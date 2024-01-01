library(shiny)
library(shinyjs)
library(x3ptools)
library(assertthat)


ui <- fluidPage(
  useShinyjs(), # Set up shinyjs

  titlePanel("R Shiny App for wire"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput1", "Choose file (Max size: 5MB, Acceptable formats: .x3p, .rda)", accept = c(".x3p", ".rda")),
      hidden( # Hide the second file input initially
        div(
          id = "secondFileInput", # Add an id to the div so it can be referenced
          fileInput("fileInput2", "Choose file (Max size: 5MB, Acceptable format: .x3p)", accept = c(".x3p"))
        )
      ),
      actionButton("clear", "Clear files") # Add a button to clear the file inputs
    ),
    mainPanel(
      verbatimTextOutput("strOutput")
    )
  )
)




server <- function(input, output) {
  rv <- reactiveValues(x3p1 = NULL, x3p2 = NULL)

  observeEvent(input$fileInput1, {
    inFile1 <- input$fileInput1

    if (is.null(inFile1)) {
      showNotification("Please upload the first file.", type = "error")
      return(NULL)
    }

    ext1 <- tools::file_ext(inFile1$datapath)

    if (ext1 == "x3p") {
      show("secondFileInput") # Show the second file input if the first file is a x3p file
    } else {
      hide("secondFileInput") # Hide the second file input otherwise
    }

    if (ext1 == "x3p") {
      x3p1 <- x3p_read(inFile1$datapath)
      assert_that("x3p" %in% class(x3p1), msg = "The first uploaded file is not a valid x3p file.")
    } else if (ext1 == "rda") {
      x3pList <- get(load(inFile1$datapath))
      if (!is.list(x3pList) || length(x3pList) < 2 || !all(sapply(x3pList, function(x) "x3p" %in% class(x)))) {
        showNotification("The first uploaded file is not a valid rda file containing at least 2 x3p objects.", type = "error")
        return(NULL)
      }
      if (length(x3pList) > 2) {
        message("More than 2 x3p objects detected in the first file. Only the first 2 x3p objects will be used.")
      }
      x3p1 <- x3pList[1]
      x3p2 <- x3pList[2]
    } else {
      showNotification("Incorrect input format for the first file. Please upload a x3p or rda file.", type = "error")
      return(NULL)
    }

    rv$x3p1 <- x3p1
  })

  observeEvent(input$fileInput2, {
    inFile2 <- input$fileInput2

    if (is.null(inFile2)) {
      return(NULL)
    }

    if (!is.null(inFile2)) {
      ext2 <- tools::file_ext(inFile2$datapath)
      if (ext2 == "x3p") {
        x3p2 <- x3p_read(inFile2$datapath)
        assert_that("x3p" %in% class(x3p2), msg = "The second uploaded file is not a valid x3p file.")
      } else {
        showNotification("Incorrect input format for the first file. Please upload a x3p or rda file.", type = "error")
        return(NULL)
      }
    }

    rv$x3p2 <- x3p2
  })

  observeEvent(input$clear, {
    shinyjs::reset("fileInput1")
    shinyjs::reset("fileInput2")
    rv$x3p1 <- NULL
    rv$x3p2 <- NULL
    shinyjs::hide("secondFileInput") # Hide the second file input
  })

  output$strOutput <- renderPrint({
    if (!is.null(rv$x3p1) && !is.null(rv$x3p2)) {
      cat("Structure of the first x3p object:\n")
      str(rv$x3p1) # Use the x3p1 from the reactive values
      cat("\nStructure of the second x3p object:\n")
      str(rv$x3p2) # Use the x3p2 from the reactive values
    }
  })
}

shinyApp(ui = ui, server = server)
