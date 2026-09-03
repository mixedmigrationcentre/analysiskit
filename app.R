# Analysis Kit - Mixed Migration Centre
#
# app.R assembles the interface and connects reactive behaviour. Reading,
# validating, analysing and exporting all live in R/ and functions/, so nothing
# in this file needs a running Shiny session to be understood or tested.

# Packages first: a missing one should be a clear message on launch, not an
# error halfway through a run. ak_ensure_packages() installs what is missing
# from CRAN and stops with the exact command for anything it will not install
# on your behalf. See R/setup_packages.R for what is needed and why.
source("R/setup_packages.R")
source("R/deployment.R")

# Nothing is installed on a served deployment: the library there is built from
# manifest.json before the app starts, and is read-only once it is running, so
# an install attempt would only waste time and fail confusingly.
ak_ensure_packages(install = !ak_is_server())
ak_attach_packages()

options(shiny.maxRequestSize = 100 * 1024^2)

source("R/read_dataset.R")
source("R/read_loa.R")
source("R/ui_components.R")
source("R/export_results.R")

# The analysis and export functions are sourced from functions/, so they can be
# replaced without touching the app. If they are absent the interface says so
# rather than pretending it can run.
for (file in list.files("functions", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(file)
}

pipeline_available <- function() {
  exists("run_group_analysis_pipeline", mode = "function")
}


ui <- fluidPage(
  includeCSS("www/styles.css"),
  tags$header(
    class = "app-header",
    titlePanel("Analysis Kit"),
    tags$img(
      src = "logo.png",
      alt = "Mixed Migration Centre",
      class = "app-logo"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "dataset",
        "Upload dataset",
        accept = c(".csv", ".xlsx")
      ),
      fileInput(
        "loa",
        "Upload List of Analysis",
        accept = c(".csv", ".xlsx")
      ),
      tags$div(
        class = "ak-template",
        downloadLink("template", "Download the List of Analysis template")
      ),
      tags$div(
        class = "ak-destination",
        # The heading is rendered rather than fixed: served, there is no folder
        # to pick and "Output folder" over an empty space reads as a broken
        # control. See ak_destination_mode().
        uiOutput("folder_label"),
        uiOutput("folder_control"),
        uiOutput("folder_status")
      ),
      uiOutput("run_control"),
      uiOutput("run_hint")
    ),
    mainPanel(
      uiOutput("progress_tracker"),
      uiOutput("status"),
      tabsetPanel(
        id = "panels",
        type = "tabs",
        tabPanel(
          "Dataset",
          conditionalPanel(
            condition = "output.datasetReady",
            ak_section("Dataset overview", tableOutput("dataset_overview")),
            ak_section(
              "Column profile",
              div(class = "table-scroll", tableOutput("dataset_columns"))
            ),
            ak_section(
              "First 10 rows",
              div(class = "table-scroll", tableOutput("dataset_preview"))
            )
          )
        ),
        tabPanel(
          "List of Analysis",
          uiOutput("loa_status"),
          conditionalPanel(
            condition = "output.loaReady",
            ak_section("Sheets read", tableOutput("loa_sheets")),
            ak_section(
              "Checks",
              uiOutput("check_badges"),
              div(class = "table-scroll ak-problems", tableOutput("loa_problems"))
            ),
            ak_section(
              "Variable coverage",
              uiOutput("coverage_note"),
              div(class = "table-scroll", tableOutput("coverage_table"))
            )
          )
        ),
        tabPanel(
          "Results",
          uiOutput("results_status"),
          conditionalPanel(
            condition = "output.resultsReady",
            uiOutput("exclusive_note"),
            ak_section(
              "Analyses run",
              uiOutput("download_control"),
              tableOutput("results_summary")
            ),
            ak_section(
              "Results preview",
              div(class = "table-scroll", tableOutput("results_preview"))
            ),
            ak_section("Run log", uiOutput("run_log"))
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Dataset
  # ---------------------------------------------------------------------------

  # Reading happens in an observer rather than inside a reactive, so a 100 MB
  # upload is read exactly once, when the file arrives, and the progress bar is
  # tied to that one event. A reactive would re-read on every invalidation and
  # flash a progress bar at unrelated moments.
  dataset_state <- reactiveVal(NULL)

  observeEvent(input$dataset, {
    withProgress(message = "Reading the dataset", value = 0, {
      setProgress(0.3, detail = input$dataset$name)

      result <- tryCatch(
        read_uploaded_dataset(input$dataset),
        error = function(error) {
          structure(list(message = conditionMessage(error)), class = "dataset_error")
        }
      )

      setProgress(0.9, detail = "profiling the columns")
      dataset_state(result)
      setProgress(1, detail = "done")
    })
  })

  # req(), not a bare read: without it every dataset output would be computed
  # against NULL before anything is uploaded, and dataset_overview() would be
  # handed a dataset with no size, rows or columns.
  uploaded_dataset <- reactive({
    req(dataset_state())
    dataset_state()
  })

  dataset_ok <- reactive({
    !is.null(dataset_state()) && !inherits(dataset_state(), "dataset_error")
  })

  output$datasetReady <- reactive(dataset_ok())
  outputOptions(output, "datasetReady", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # List of Analysis
  # ---------------------------------------------------------------------------

  loa_state <- reactiveVal(NULL)

  observeEvent(input$loa, {
    withProgress(message = "Reading the List of Analysis", value = 0, {
      setProgress(0.3, detail = input$loa$name)

      result <- tryCatch(
        read_loa_workbook(input$loa$datapath, filename = input$loa$name),
        error = function(error) {
          structure(list(message = conditionMessage(error)), class = "loa_error")
        }
      )

      loa_state(result)
      setProgress(1, detail = "done")
    })
  })

  uploaded_loa <- reactive({
    req(loa_state())
    loa_state()
  })

  loa_ok <- reactive({
    !is.null(loa_state()) && !inherits(loa_state(), "loa_error")
  })

  output$loaReady <- reactive(loa_ok())
  outputOptions(output, "loaReady", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # Checks
  #
  # Validation is cheap and runs reactively, so the moment both files are in the
  # user sees every problem at once. Only the analysis itself waits for the
  # button.
  # ---------------------------------------------------------------------------

  # No progress bar here on purpose: validation runs in milliseconds and this
  # reactive feeds several outputs, so a bar would flash on every render while
  # telling the user nothing.
  analysis_spec <- reactive({
    req(dataset_ok(), loa_ok())
    build_analysis_spec(uploaded_loa(), uploaded_dataset()$data)
  })

  loa_problems <- reactive(analysis_spec()$problems)

  variable_coverage <- reactive({
    req(dataset_ok(), loa_ok())
    loa_variable_coverage(uploaded_loa(), uploaded_dataset()$data)
  })

  # ---------------------------------------------------------------------------
  # Destination folder
  #
  # shinyFiles browses the filesystem the app is running on. Analysis Kit is
  # launched on the analyst's own machine, so that is their filesystem - see
  # docs/loa-schema.md. Served from a remote host it would browse the server,
  # and a download handler would be the right answer instead.
  # ---------------------------------------------------------------------------

  destination_mode <- ak_destination_mode()

  volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)

  output$folder_label <- renderUI({
    tags$label(class = "control-label", destination_mode$label)
  })

  output$folder_control <- renderUI({
    if (!destination_mode$pick_folder) {
      return(tagList())
    }
    shinyFiles::shinyDirButton(
      "folder",
      label = "Choose folder...",
      title = "Select the folder to save the results workbook into",
      class = "btn-default"
    )
  })

  output_folder <- reactive({
    # Served, there is no folder of the user's to write into, so the workbook is
    # built in a session temp folder and reaches them as a download.
    if (!destination_mode$pick_folder) {
      return(ak_session_output_dir())
    }

    chosen <- shinyFiles::parseDirPath(volumes, input$folder)
    if (length(chosen) == 0 || !nzchar(chosen)) NULL else as.character(chosen)
  })

  folder_problem <- reactive({
    if (is.null(output_folder())) NULL else ak_check_folder(output_folder())
  })

  output$folder_status <- renderUI({
    if (!destination_mode$pick_folder) {
      return(ak_status(destination_mode$explanation, destination_mode$status_type))
    }
    if (is.null(output_folder())) {
      return(ak_status(destination_mode$explanation, destination_mode$status_type))
    }
    if (!is.null(folder_problem())) {
      return(ak_status(folder_problem(), "error"))
    }
    ak_status(paste0("Saving to ", output_folder()), "success")
  })

  # ---------------------------------------------------------------------------
  # Downloads
  # ---------------------------------------------------------------------------

  output$template <- downloadHandler(
    filename = "analysiskit_loa_template.xlsx",
    content = function(file) {
      template <- "docs/loa_template.xlsx"
      validate(need(file.exists(template), "The template is missing from docs/."))
      file.copy(template, file, overwrite = TRUE)
    }
  )

  output$results_file <- downloadHandler(
    filename = function() basename(saved_path()),
    content = function(file) file.copy(saved_path(), file, overwrite = TRUE)
  )

  # Offered wherever the app runs: served it is the only way the workbook
  # reaches the user, and locally it is a convenience next to the saved copy.
  output$download_control <- renderUI({
    # An empty tagList rather than req(): there is simply nothing to download
    # yet, which is a state to render, not a condition to abort on.
    if (is.null(saved_path())) {
      return(tagList())
    }
    tags$div(
      class = "ak-download",
      downloadButton("results_file", "Download the results workbook", class = "btn-primary")
    )
  })

  # ---------------------------------------------------------------------------
  # Progress tracker
  # ---------------------------------------------------------------------------

  running <- reactiveVal(FALSE)

  step_states <- reactive({
    ak_step_states(
      dataset_loaded = dataset_ok(),
      dataset_failed = !is.null(input$dataset) && !dataset_ok(),
      loa_loaded = loa_ok(),
      loa_failed = !is.null(input$loa) && !loa_ok(),
      problems = if (dataset_ok() && loa_ok()) loa_problems() else NULL,
      destination_chosen = destination_mode$settled ||
        (!is.null(output_folder()) && is.null(folder_problem())),
      destination_failed = !is.null(folder_problem()),
      results_ready = !is.null(analysis_results()),
      running = running()
    )
  })

  output$progress_tracker <- renderUI(
    ak_step_tracker(step_states(), destination_mode$step_label)
  )

  output$status <- renderUI({
    if (is.null(input$dataset)) {
      return(ak_status("Upload a CSV or XLSX dataset to begin."))
    }
    if (!dataset_ok()) {
      return(ak_status(uploaded_dataset()$message, "error"))
    }
    if (is.null(input$loa)) {
      return(ak_status(
        sprintf(
          "Loaded %s. Upload a List of Analysis to run the checks.",
          uploaded_dataset()$filename
        ),
        "success"
      ))
    }
    if (!loa_ok()) {
      return(ak_status(uploaded_loa()$message, "error"))
    }

    if (!is.null(run_error())) {
      return(ak_status(run_error(), "error"))
    }
    if (!is.null(saved_path())) {
      return(ak_status(
        if (destination_mode$pick_folder) {
          sprintf(
            "Analyses complete. Saved as %s in %s.",
            basename(saved_path()), dirname(saved_path())
          )
        } else {
          sprintf(
            "Analyses complete. Download %s from the Results tab.",
            basename(saved_path())
          )
        },
        "success"
      ))
    }

    counts <- ak_problem_counts(loa_problems())

    if (counts[["error"]] > 0L) {
      return(ak_status(
        sprintf(
          "The List of Analysis has %d problem(s) that must be fixed before running. See the List of Analysis tab.",
          counts[["error"]]
        ),
        "error"
      ))
    }
    if (counts[["warning"]] > 0L) {
      return(ak_status(
        sprintf(
          "Ready to run, with %d warning(s) worth reading first. See the List of Analysis tab.",
          counts[["warning"]]
        ),
        "warning"
      ))
    }

    ak_status("Every check passed. Ready to run the analyses.", "success")
  })

  # ---------------------------------------------------------------------------
  # Dataset tab
  # ---------------------------------------------------------------------------

  # substitute() + quoted = TRUE, not renderTable(expr): passing the expression
  # as an ordinary argument turns it into a promise that this wrapper's frame
  # forces on every render. Any req() inside it then leaves the promise
  # interrupted, and the next render re-forces it - which R reports as
  # "restarting interrupted promise evaluation".
  ak_table <- function(expr, spacing = "s", rownames = FALSE) {
    renderTable(
      substitute(expr),
      striped = TRUE,
      bordered = FALSE,
      spacing = spacing,
      rownames = rownames,
      env = parent.frame(),
      quoted = TRUE
    )
  }

  output$dataset_overview <- ak_table({
    result <- uploaded_dataset()
    validate(need(!inherits(result, "dataset_error"), result$message))
    dataset_overview(result)
  })

  output$dataset_columns <- ak_table({
    result <- uploaded_dataset()
    validate(need(!inherits(result, "dataset_error"), result$message))
    dataset_columns(result)
  })

  output$dataset_preview <- ak_table(
    {
      result <- uploaded_dataset()
      validate(need(!inherits(result, "dataset_error"), result$message))
      utils::head(result$data, 10L)
    },
    spacing = "xs",
    rownames = TRUE
  )

  # ---------------------------------------------------------------------------
  # List of Analysis tab
  # ---------------------------------------------------------------------------

  output$loa_status <- renderUI({
    if (is.null(input$loa)) {
      return(ak_status("Upload a List of Analysis workbook to see the checks."))
    }
    if (!loa_ok()) {
      return(ak_status(uploaded_loa()$message, "error"))
    }
    if (!dataset_ok()) {
      return(ak_status(
        "Upload a dataset as well: the variable checks compare the two files.",
        "warning"
      ))
    }

    workbook <- uploaded_loa()
    ak_status(
      sprintf("Read %s (%s).", workbook$filename, workbook$format),
      "success"
    )
  })

  output$loa_sheets <- ak_table({
    req(loa_ok())
    ak_sheet_summary(uploaded_loa())
  })

  output$check_badges <- renderUI({
    req(dataset_ok(), loa_ok())
    ak_badges(ak_problem_counts(loa_problems()))
  })

  output$loa_problems <- ak_table({
    req(dataset_ok(), loa_ok())
    display <- ak_problems_display(loa_problems())
    validate(need(nrow(display) > 0, "Nothing to fix."))
    display
  })

  output$coverage_note <- renderUI({
    req(dataset_ok(), loa_ok())

    coverage <- variable_coverage()
    if (nrow(coverage) == 0) {
      return(ak_status("The workbook does not name any dataset variables."))
    }

    missing <- sum(!coverage$present)
    total <- length(unique(coverage$variable))

    if (missing == 0) {
      return(ak_status(
        sprintf(
          "All %d variable(s) named in the workbook are in %s.",
          total, uploaded_dataset()$filename
        ),
        "success"
      ))
    }

    ak_status(
      sprintf(
        "%d of %d variable(s) named in the workbook are not in %s.",
        length(unique(coverage$variable[!coverage$present])),
        total,
        uploaded_dataset()$filename
      ),
      if (any(loa_coverage_severity(coverage$role[!coverage$present]) == "error")) {
        "error"
      } else {
        "warning"
      }
    )
  })

  output$coverage_table <- ak_table({
    req(dataset_ok(), loa_ok())
    summary <- loa_coverage_summary(variable_coverage())
    validate(need(nrow(summary) > 0, "No variables referenced."))
    summary
  })

  # ---------------------------------------------------------------------------
  # Running
  # ---------------------------------------------------------------------------

  output$run_control <- renderUI({
    states <- step_states()
    # pipeline_available() as well as ak_can_run(): offering a button that
    # cannot succeed is worse than explaining why it is not offered. The
    # observer still guards the call, in case the button is reached anyway.
    ready <- ak_can_run(states) && !running() && pipeline_available()

    button <- actionButton(
      "run",
      if (running()) "Running..." else "Run analyses",
      class = "btn-primary"
    )

    if (ready) button else tagAppendAttributes(button, disabled = NA)
  })

  output$run_hint <- renderUI({
    if (!pipeline_available()) {
      return(ak_status(
        "The analysis functions are not installed in this copy of Analysis Kit, so the run will stop before doing any work. Add them to the functions/ folder.",
        "warning"
      ))
    }
    states <- step_states()
    if (identical(unname(states[["destination"]]), "active")) {
      return(ak_status("Choose an output folder to enable the run."))
    }
    # Served, the destination step no longer needs a hint here: it is settled,
    # and its own status sits a few pixels above this. Repeating the same
    # paragraph twice in a narrow sidebar made it read as an error.
    if (!ak_can_run(states)) {
      return(ak_status("Upload both files and clear any problems to run."))
    }
    # An empty tagList rather than NULL: renderUI(NULL) reaches htmltools as
    # structure(NULL, ...), which is deprecated.
    tagList()
  })

  analysis_results <- reactiveVal(NULL)
  run_error <- reactiveVal(NULL)
  run_log <- reactiveVal(character(0))
  saved_path <- reactiveVal(NULL)

  # eventReactive/observeEvent rather than a plain reactive: uploading or
  # editing an input must never silently rerun an expensive analysis, and it
  # must never silently write a file.
  observeEvent(input$run, {
    spec <- analysis_spec()
    dataset <- uploaded_dataset()$data
    dataset_name <- uploaded_dataset()$filename
    folder <- output_folder()

    analysis_results(NULL)
    run_error(NULL)
    saved_path(NULL)
    run_log(character(0))
    running(TRUE)
    on.exit(running(FALSE), add = TRUE)

    if (!pipeline_available()) {
      run_error(paste0(
        "run_group_analysis_pipeline() is not available. Add the analysis ",
        "functions to the functions/ folder and restart the application."
      ))
      return(invisible(NULL))
    }

    folder_issue <- ak_check_folder(folder)
    if (!is.null(folder_issue)) {
      run_error(folder_issue)
      return(invisible(NULL))
    }

    withProgress(message = "Running analyses", value = 0, {
      setProgress(0.03, detail = sprintf(
        "%d analyses across %d grouping variable(s)",
        nrow(spec$loa), length(spec$group_variables)
      ))

      # The pipeline reports its own progress through message(). Those messages
      # drive the bar and are kept as a run log: they carry the diagnostics an
      # analyst needs to see - variables skipped, choices excluded, small groups
      # set aside - which would otherwise vanish into the console.
      #
      # There is no reliable count of messages to divide by, so rather than
      # invent a denominator each one closes a fraction of the remaining
      # distance to the end of the analysis phase. The bar always advances and
      # never overshoots.
      step <- 0L
      note <- function(text) {
        text <- trimws(sub("^-->\\s*", "", text))
        if (nzchar(text)) run_log(c(run_log(), text))
        text
      }

      result <- withCallingHandlers(
        tryCatch(
          run_analysis_spec(dataset, spec, verbose = TRUE),
          error = function(error) {
            structure(list(message = conditionMessage(error)), class = "run_error")
          }
        ),
        message = function(m) {
          step <<- step + 1L
          setProgress(
            0.03 + 0.72 * (1 - 0.88^step),
            detail = note(conditionMessage(m))
          )
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          note(paste0("Warning: ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      )

      if (inherits(result, "run_error")) {
        run_error(result$message)
        return(invisible(NULL))
      }

      # Only now, with a completed run in hand, is anything written to disk.
      setProgress(0.78, detail = "building the results workbook")

      written <- withCallingHandlers(
        tryCatch(
          ak_export_results(
            results = result,
            spec = spec,
            folder = folder,
            dataset_name = dataset_name,
            verbose = TRUE
          ),
          error = function(error) {
            structure(list(message = conditionMessage(error)), class = "run_error")
          }
        ),
        message = function(m) {
          note(conditionMessage(m))
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          note(paste0("Warning: ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      )

      if (inherits(written, "run_error")) {
        # The analysis succeeded; only the saving failed. Keep the results so
        # the work is not thrown away over a bad folder.
        analysis_results(result)
        run_error(paste0(
          "The analysis finished, but the results could not be saved: ",
          written$message
        ))
        return(invisible(NULL))
      }

      setProgress(1, detail = basename(written))
      analysis_results(result)
      saved_path(written)
      note(paste0("Saved to ", written))
    })
  })

  output$resultsReady <- reactive(!is.null(analysis_results()))
  outputOptions(output, "resultsReady", suspendWhenHidden = FALSE)

  output$results_status <- renderUI({
    if (!is.null(run_error())) {
      return(ak_status(run_error(), "error"))
    }
    if (running()) {
      return(ak_status("Running the analyses..."))
    }
    if (is.null(analysis_results())) {
      if (!ak_can_run(step_states())) {
        return(ak_status(
          "Upload both files, clear any problems and choose an output folder, then click Run analyses."
        ))
      }
      return(ak_status("Ready. Click Run analyses to produce the results."))
    }

    results <- analysis_results()
    ak_status(
      sprintf(
        if (destination_mode$pick_folder) {
          "Produced %d rows and %d columns. Saved as %s in %s."
        } else {
          "Produced %d rows and %d columns. Download %s using the button above.%s"
        },
        nrow(results$combined_results), ncol(results$combined_results),
        basename(saved_path()),
        if (destination_mode$pick_folder) dirname(saved_path()) else ""
      ),
      "success"
    )
  })

  output$results_summary <- ak_table({
    results <- analysis_results()
    req(results)

    data.frame(
      Measure = c(
        "Saved as", if (destination_mode$pick_folder) "Folder" else "Delivery",
        "Result rows", "Result columns",
        "Grouping variables", "Selection counts", "Choice combinations",
        "Exclusive combinations", "Excluded choices"
      ),
      Value = c(
        if (is.null(saved_path())) "not saved" else basename(saved_path()),
        if (is.null(saved_path())) {
          "-"
        } else if (destination_mode$pick_folder) {
          dirname(saved_path())
        } else {
          "Download using the button above"
        },
        format(nrow(results$combined_results), big.mark = ","),
        format(ncol(results$combined_results), big.mark = ","),
        format(length(unique(stats::na.omit(results$column_map$group_variable)))),
        format(nrow(results$selection_counts)),
        format(nrow(results$choice_combinations)),
        format(nrow(results$exclusive_combinations %||% data.frame())),
        format(nrow(results$excluded_choices))
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  output$results_preview <- ak_table(
    {
      results <- analysis_results()
      req(results)
      # Previewed the way it was exported, separator rows and all: showing the
      # raw table here would display spacer and heading markers as rows of NA
      # that are not in the file the user just received.
      wide <- ak_prepare_for_export(
        results$combined_results, ak_export_settings(results, analysis_spec())$layout
      )
      # A wide table can run to thousands of columns; showing all of them would
      # hang the browser rather than inform anyone.
      utils::head(wide[, seq_len(min(ncol(wide), 12L)), drop = FALSE], 10L)
    },
    spacing = "xs"
  )

  # Sits above the summary rather than in the run log: these percentages get
  # published, and the caveat has to be in front of whoever exports the file.
  output$exclusive_note <- renderUI({
    results <- analysis_results()
    if (is.null(results)) {
      return(tagList())
    }
    ak_exclusive_base_panel(results$exclusive_combinations)
  })

  output$run_log <- renderUI({
    entries <- run_log()
    if (length(entries) == 0) {
      return(ak_status("The pipeline reported nothing."))
    }

    tags$ul(
      class = "ak-log",
      lapply(entries, function(line) {
        tags$li(
          class = if (startsWith(line, "Warning:")) "ak-log-warning" else NULL,
          line
        )
      })
    )
  })
}

shinyApp(ui, server)
