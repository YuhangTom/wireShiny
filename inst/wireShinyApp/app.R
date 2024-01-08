library(shiny)
library(shinyjs)
library(shinyWidgets) # showNotification
library(x3ptools)
library(tools) # file_ext
library(purrr) # map_lgl

ui <- fluidPage(
  useShinyjs(),
  titlePanel("R Shiny App for wire"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    "))
      ),
      titlePanel("File inputs"),
      fileInput("fileInput1", "Choose file (Max size: 5MB, Acceptable formats: .x3p, .rda)", accept = c(".x3p", ".rda")),
      hidden(
        div(
          id = "secondFileInput",
          fileInput("fileInput2", "Choose file (Max size: 5MB, Acceptable format: .x3p)", accept = c(".x3p"))
        )
      ),
      actionButton("clear", "Clear files"),
      conditionalPanel(
        condition = "output.strOutput !== ''",
        titlePanel("Inner polygon parameters"),
        numericInput("concavity", "Concavity (positive number)", value = 1.5, min = 1e-12),
        numericInput("b", "Block size b (positive integer)", value = 1, min = 1, step = 1)
      )
    ),
    mainPanel(
      verbatimTextOutput("strOutput")
    )
  )
)




server <- function(input, output) {
  x3ps <- reactiveValues(x3p1 = NULL, x3p2 = NULL)

  observeEvent(input$fileInput1, {
    inFile1 <- input$fileInput1

    ext1 <- file_ext(inFile1$datapath)

    if (ext1 == "x3p") {
      show("secondFileInput")

      x3p1 <- x3p_read(inFile1$datapath)
      x3p2 <- NULL
    } else if (ext1 == "rda") {
      hide("secondFileInput")

      x3prda <- get(load(inFile1$datapath))
      if (!is.list(x3prda) || length(x3prda) < 2 || !all(map_lgl(x3prda, ~ "x3p" %in% class(.)))) {
        showNotification("The uploaded file is not a valid rda file containing at least 2 x3p objects.",
          type = "error"
        )
        return(NULL)
      }
      if (length(x3prda) > 2) {
        showNotification("More than two x3p objects detected in the uploaded file. Only the first two will be used.",
          type = "warning"
        )
      }
      x3p1 <- x3prda[1]
      x3p2 <- x3prda[2]
    }

    x3ps$x3p1 <- x3p1
    x3ps$x3p2 <- x3p2
  })

  observeEvent(input$fileInput2, {
    inFile2 <- input$fileInput2

    x3ps$x3p2 <- x3p_read(inFile2$datapath)
  })

  observeEvent(input$clear, {
    reset("fileInput1")
    reset("fileInput2")
    x3ps$x3p1 <- NULL
    x3ps$x3p2 <- NULL
    hide("secondFileInput")
  })

  output$strOutput <- renderPrint({
    if (!is.null(x3ps$x3p1) && !is.null(x3ps$x3p2)) {
      cat("Structure of the first x3p object:\n")
      str(x3ps$x3p1)
      cat("\nStructure of the second x3p object:\n")
      str(x3ps$x3p2)
    }
  })
}

shinyApp(ui = ui, server = server)
