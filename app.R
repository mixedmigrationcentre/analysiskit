library(shiny)

options(shiny.maxRequestSize = 100 * 1024^2)

# app.R assembles the interface and connects reactive behaviour. Reading,
# validating and analysing all live in R/ and functions/, so nothing in this
# file needs a running Shiny session to be understood or tested.
source("R/read_dataset.R")
source("R/read_loa.R")
source("R/ui_components.R")

# The analysis pipeline is not yet part of the repository. Sourcing whatever is
# in functions/ means the Run step starts working the moment it lands there,
# and reports honestly that it cannot run until then.
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
            ak_section("Analyses run", tableOutput("results_summary")),
            ak_section(
              "Results preview",
              div(class = "table-scroll", tableOutput("results_preview"))
            )
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
      results_ready = !is.null(analysis_results()),
      running = running()
    )
  })

  output$progress_tracker <- renderUI(ak_step_tracker(step_states()))

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
    if (!ak_can_run(step_states())) {
      return(ak_status("Upload both files and clear any problems to run."))
    }
    # An empty tagList rather than NULL: renderUI(NULL) reaches htmltools as
    # structure(NULL, ...), which is deprecated.
    tagList()
  })

  analysis_results <- reactiveVal(NULL)
  run_error <- reactiveVal(NULL)

  # eventReactive/observeEvent rather than a plain reactive: uploading or
  # editing an input must never silently rerun an expensive analysis.
  observeEvent(input$run, {
    spec <- analysis_spec()
    dataset <- uploaded_dataset()$data

    analysis_results(NULL)
    run_error(NULL)
    running(TRUE)
    on.exit(running(FALSE), add = TRUE)

    if (!pipeline_available()) {
      run_error(paste0(
        "run_group_analysis_pipeline() is not available. Add the analysis ",
        "functions to the functions/ folder and restart the application."
      ))
      return(invisible(NULL))
    }

    withProgress(message = "Running analyses", value = 0, {
      setProgress(
        0.1,
        detail = sprintf(
          "%d analyses across %d grouping variable(s)",
          nrow(spec$loa), length(spec$group_variables)
        )
      )

      # The pipeline reports its own progress through message(); routing those
      # to the progress bar is what makes a long run legible rather than a
      # frozen screen.
      step <- 0L
      total <- max(nrow(spec$loa) * length(spec$group_variables), 1L)

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
            min(0.1 + 0.85 * step / total, 0.95),
            detail = trimws(sub("^-->\\s*", "", conditionMessage(m)))
          )
          invokeRestart("muffleMessage")
        }
      )

      setProgress(1, detail = "done")

      if (inherits(result, "run_error")) {
        run_error(result$message)
      } else {
        analysis_results(result)
      }
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
        return(ak_status("Upload both files and clear any problems, then click Run analyses."))
      }
      return(ak_status("Ready. Click Run analyses to produce the results."))
    }

    results <- analysis_results()
    ak_status(
      sprintf(
        "Produced %d rows and %d columns.",
        nrow(results$combined_results), ncol(results$combined_results)
      ),
      "success"
    )
  })

  output$results_summary <- ak_table({
    results <- analysis_results()
    req(results)

    data.frame(
      Measure = c(
        "Result rows", "Result columns", "Grouping variables",
        "Selection counts", "Choice combinations", "Excluded choices"
      ),
      Value = c(
        format(nrow(results$combined_results), big.mark = ","),
        format(ncol(results$combined_results), big.mark = ","),
        format(length(unique(stats::na.omit(results$column_map$group_variable)))),
        format(nrow(results$selection_counts)),
        format(nrow(results$choice_combinations)),
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
      # A wide table can run to thousands of columns; showing all of them would
      # hang the browser rather than inform anyone.
      wide <- results$combined_results
      utils::head(wide[, seq_len(min(ncol(wide), 12L)), drop = FALSE], 10L)
    },
    spacing = "xs"
  )
}

shinyApp(ui, server)
