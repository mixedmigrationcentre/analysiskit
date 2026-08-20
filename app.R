library(shiny)

options(shiny.maxRequestSize = 100 * 1024^2)

source("R/read_dataset.R")

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
      checkboxGroupInput(
        "analyses",
        "Select analyses",
        choices = c(
          "Analysis 1" = "analysis_01",
          "Analysis 2" = "analysis_02",
          "Analysis 3" = "analysis_03"
        )
      ),
      actionButton(
        "run",
        "Run analyses"
      )
    ),
    mainPanel(
      uiOutput("status"),
      conditionalPanel(
        condition = "output.datasetReady",
        div(
          class = "dataset-section",
          h3("Dataset overview"),
          tableOutput("dataset_overview")
        ),
        # div(
        #   class = "dataset-section",
        #   h3("Column profile"),
        #   tableOutput("dataset_columns")
        # ),
        # div(
        #   class = "dataset-section",
        #   h3("First 10 rows"),
        #   div(class = "table-scroll", tableOutput("dataset_preview"))
        # )
      )
    )
  )
)

server <- function(input, output, session) {
  uploaded_dataset <- reactive({
    req(input$dataset)

    tryCatch(
      read_uploaded_dataset(input$dataset),
      error = function(error) {
        structure(list(message = conditionMessage(error)), class = "dataset_error")
      }
    )
  })

  output$datasetReady <- reactive({
    result <- uploaded_dataset()
    !inherits(result, "dataset_error")
  })
  outputOptions(output, "datasetReady", suspendWhenHidden = FALSE)

  output$status <- renderUI({
    if (is.null(input$dataset)) {
      return(div(class = "status-message", "Upload a CSV or XLSX dataset to inspect it."))
    }

    result <- uploaded_dataset()

    if (inherits(result, "dataset_error")) {
      return(div(class = "status-message status-error", result$message))
    }

    div(
      class = "status-message status-success",
      sprintf("Loaded %s successfully.", result$filename)
    )
  })

  output$dataset_overview <- renderTable(
    {
      result <- uploaded_dataset()
      validate(need(!inherits(result, "dataset_error"), result$message))
      dataset_overview(result)
    },
    striped = TRUE,
    bordered = FALSE,
    spacing = "s"
  )

  output$dataset_columns <- renderTable(
    {
      result <- uploaded_dataset()
      validate(need(!inherits(result, "dataset_error"), result$message))
      dataset_columns(result)
    },
    striped = TRUE,
    bordered = FALSE,
    spacing = "s"
  )

  output$dataset_preview <- renderTable(
    {
      result <- uploaded_dataset()
      validate(need(!inherits(result, "dataset_error"), result$message))
      utils::head(result$data, 10L)
    },
    striped = TRUE,
    bordered = FALSE,
    spacing = "xs",
    rownames = TRUE
  )
}

shinyApp(ui, server)
