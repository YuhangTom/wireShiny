library(shiny)
library(x3ptools)
library(assertthat)

server <- function(input, output) {
  observeEvent(input$fileInput, {
    inFile <- input$fileInput

    if (is.null(inFile)) {
      stop("Please upload a file.")
    }

    ext <- tools::file_ext(inFile$datapath)

    if (ext == "x3p") {
      x3p <- x3ptools::x3p_read(inFile$datapath)
      assert_that("x3p" %in% class(x3p), msg = "The uploaded file is not a valid x3p file.")
    } else if (ext == "rda") {
      x3pList <- get(load(inFile$datapath))
      if (!is.list(x3pList) || length(x3pList) < 2 || !all(sapply(x3pList, function(x) "x3p" %in% class(x)))) {
        stop("The uploaded file is not a valid rda file containing at least 2 x3p objects.")
      }
      if (length(x3pList) > 2) {
        message("More than 2 x3p objects detected. Only the first 2 x3p objects will be used.")
      }
      x3p <- x3pList[1:2]
    } else {
      stop("Incorrect input format. Please upload a x3p or rda file.")
    }

    output$strOutput <- renderPrint({
      str(x3p)
    })
  })
}
