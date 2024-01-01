library(shiny)
library(x3ptools)
library(assertthat)

server <- function(input, output) {
  observeEvent(c(input$fileInput1, input$fileInput2), {
    inFile1 <- input$fileInput1
    inFile2 <- input$fileInput2

    if (is.null(inFile1)) {
      stop("Please upload the first file.")
    }

    ext1 <- tools::file_ext(inFile1$datapath)

    if (ext1 == "x3p") {
      x3p1 <- x3ptools::x3p_read(inFile1$datapath)
      assert_that("x3p" %in% class(x3p1), msg = "The first uploaded file is not a valid x3p file.")
    } else if (ext1 == "rda") {
      x3pList <- get(load(inFile1$datapath))
      if (!is.list(x3pList) || length(x3pList) < 2 || !all(sapply(x3pList, function(x) "x3p" %in% class(x)))) {
        stop("The first uploaded file is not a valid rda file containing at least 2 x3p objects.")
      }
      if (length(x3pList) > 2) {
        message("More than 2 x3p objects detected in the first file. Only the first 2 x3p objects will be used.")
      }
      x3p1 <- x3pList[1]
      x3p2 <- x3pList[2]
    } else {
      stop("Incorrect input format for the first file. Please upload a x3p or rda file.")
    }

    if (!is.null(inFile2)) {
      ext2 <- tools::file_ext(inFile2$datapath)
      if (ext2 == "x3p") {
        x3p2 <- x3ptools::x3p_read(inFile2$datapath)
        assert_that("x3p" %in% class(x3p2), msg = "The second uploaded file is not a valid x3p file.")
      } else {
        stop("Incorrect input format for the second file. Please upload a x3p file.")
      }
    }

    output$strOutput <- renderPrint({
      cat("Structure of the first x3p object:\n")
      str(x3p1)
      if (exists("x3p2")) {
        cat("\nStructure of the second x3p object:\n")
        str(x3p2)
      }
    })
  })
}
