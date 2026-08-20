library(shiny)

ui <- fluidPage(
  titlePanel("Analysis Tool"),

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
      textOutput("status")
    )
  )
)

server <- function(input, output, session) {
  output$status <- renderText({
    "Upload a dataset and select the analyses you want to run."
  })
}

shinyApp(ui, server)
