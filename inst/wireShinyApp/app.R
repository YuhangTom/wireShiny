library(shiny)
library(shinyjs)
library(shinyWidgets) # showNotification
library(shinydashboard)

library(tidyverse)
library(rgl) # rglwidgetOutput, rglwidget
library(gfonts) # use_pkg_gfont

library(x3ptools)
if (!require(wire)) devtools::install_github("YuhangTom/wire")
library(wire)

# Increase the limit to 20MB
options(shiny.maxRequestSize = 20 * 1024^2)


ui <- dashboardPage(
  dashboardHeader(title = HTML("<b>wireShiny</b>")),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML("
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #003a70;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #003a70;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #003a70;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #003a70;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #003a70;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #003a70;
                                color: #003a70;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #003a70;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #003a70;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }

                                "))),
    tabsetPanel(
      tabPanel(
        tabName = "home_tab",
        h4(HTML("<b>Home</b>")),
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
            h1(HTML("<b>File inputs</b>")),
            fileInput("fileInput1", h3("Choose file (Max size: 20MB, Acceptable formats: .x3p, .rda)"), accept = c(".x3p", ".rda")),
            hidden(
              div(
                id = "secondFileInput",
                fileInput("fileInput2", h3("Choose file (Max size: 20MB, Acceptable format: .x3p)"), accept = c(".x3p"))
              )
            ),
            actionButton("clear", h4(HTML("<b>Clear files</b>"))),
            hidden(
              div(
                id = "parametersInput",
                h1(HTML("<b>Inner polygon parameters</b>")),
                numericInput("concavity", h3("Concavity (positive number)"), value = 1.5, min = 1e-12),
                numericInput("b", h3("Block size b (positive integer)"), value = 1, min = 1, step = 1),
                h1(HTML("<b>Rotation parameters</b>")),
                sliderInput("colour_cutoff", h3("Colour cutoff"), value = c(0.3, 0.7), min = 0, max = 1, step = 0.1),
                numericInput("min_score_cut", h3("Min score cut (non-negative number)"), value = 0.1, min = 0),
                numericInput("loess_span", h3("Loess span (positive number)"), value = 0.2, min = 0),
                h1(HTML("<b>Shifting parameters</b>")),
                numericInput("delta_lower", h3("Delta lower (negative integer)"), value = -5, max = -1, step = 1),
                numericInput("delta_upper", h3("Delta upper (positive integer)"), value = 5, min = 1, step = 1),
                actionButton("run_button", h4(HTML("<b>Run</b>")))
              )
            )
          ),
          mainPanel(
            tabsetPanel(
              id = "tabsetPanel",
              selected = "Signals after aligning",
              tabPanel(
                "Signals after aligning",
                plotOutput("sig_align_plot")
              ),
              tabPanel(
                "Original x3p images",
                fluidRow(
                  column(
                    width = 6,
                    rglwidgetOutput("x3p1_plot")
                  ),
                  column(
                    width = 6,
                    rglwidgetOutput("x3p2_plot")
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    plotOutput("number_of_missing_immediate_neighbors_boxplot_1")
                  ),
                  column(
                    width = 6,
                    plotOutput("number_of_missing_immediate_neighbors_boxplot_2")
                  )
                )
              ),
              tabPanel(
                "x3p images after detrending",
                fluidRow(
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_inner_nomiss_res_1_plot")
                  ),
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_inner_nomiss_res_2_plot")
                  )
                )
              ),
              tabPanel(
                "x3p images after imputing",
                fluidRow(
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_inner_impute_1_plot")
                  ),
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_inner_impute_2_plot")
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    imageOutput("impute_gif_1_plot")
                  ),
                  column(
                    width = 6,
                    imageOutput("impute_gif_2_plot")
                  )
                )
              ),
              tabPanel(
                "x3p images after rotating",
                fluidRow(
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_bin_rotate_1_plot")
                  ),
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_bin_rotate_2_plot")
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    plotOutput("nfline_red_plot_1")
                  ),
                  column(
                    width = 6,
                    plotOutput("nfline_red_plot_2")
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    plotOutput("MLE_loess_red_plot_1")
                  ),
                  column(
                    width = 6,
                    plotOutput("MLE_loess_red_plot_2")
                  )
                )
              ),
              tabPanel(
                "x3p images after shifting",
                fluidRow(
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_bin_shift_1_plot")
                  ),
                  column(
                    width = 6,
                    rglwidgetOutput("x3p_bin_shift_2_plot")
                  )
                )
              )
            )
          )
        )
      ),
      # tabPanel(
      #   tabName = "about_tab",
      #   h4(HTML("<b>About</b>"))
      # ),
      # tabPanel(
      #   tabName = "instructions_tab",
      #   h4(HTML("<b>Instructions</b>"))
      # ),
      # tabPanel(
      #   tabName = "contact_tab",
      #   h4(HTML("<b>Contact</b>"))
      # )
    ),
    use_pkg_gfont("montserrat", selector = "*"),
    useShinyjs()
  )
)



server <- function(input, output) {
  x3ps <- reactiveValues(x3p1 = NULL, x3p2 = NULL)

  observeEvent(input$fileInput1, {
    output$sig_align_plot <- NULL

    inFile1 <- input$fileInput1

    ext1 <- tools::file_ext(inFile1$datapath)

    if (ext1 == "x3p") {
      shinyjs::show("secondFileInput")

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
      x3p1 <- x3prda[1][[1]]
      x3p2 <- x3prda[2][[1]]
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
    output$sig_align_plot <- NULL
  })

  observe({
    if (is.null(x3ps$x3p2)) {
      hide("parametersInput")
    } else {
      shinyjs::show("parametersInput")

      updateTabsetPanel(inputId = "tabsetPanel", selected = "Original x3p images")
      output$x3p1_plot <- renderRglwidget({
        x3ps$x3p1 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })
      output$x3p2_plot <- renderRglwidget({
        x3ps$x3p2 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })
    }
  })

  observeEvent(input$run_button, {
    withProgress(message = "Processing", value = 0, {
      n_step <- 9
      i_step <- 1

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Checking parameters...", i_step, n_step)
      )
      i_step <- i_step + 1

      if (is.na(input$concavity)) {
        showNotification("Concavity not provided, using default value 1.5.", type = "warning")
        concavity <- 1.5
      } else {
        concavity <- input$concavity
      }
      if (is.na(input$b)) {
        showNotification("Block size b not provided, using default value 1.", type = "warning")
        b <- 1
      } else {
        b <- input$b
      }
      if (near(input$colour_cutoff[1], input$colour_cutoff[2])) {
        showNotification("Colour cutoffs cannot be the same, using default value 0.3 and 0.7.", type = "warning")
        colour_cutoff <- c(0.3, 0.7)
      } else {
        colour_cutoff <- input$colour_cutoff
      }
      if (is.na(input$min_score_cut)) {
        showNotification("Min score cut not provided, using default value 0.1.", type = "warning")
        min_score_cut <- 0.1
      } else {
        min_score_cut <- input$min_score_cut
      }
      if (is.na(input$loess_span)) {
        showNotification("Loess span not provided, using default value 0.2.", type = "warning")
        loess_span <- 0.2
      } else {
        loess_span <- input$loess_span
      }
      if (is.na(input$delta_lower)) {
        showNotification("Delta lower not provided, using default value -5.", type = "warning")
        delta_lower <- -5
      } else {
        delta_lower <- input$delta_lower
      }
      if (is.na(input$delta_upper)) {
        showNotification("Delta upper not provided, using default value 5.", type = "warning")
        delta_upper <- 5
      } else {
        delta_upper <- input$delta_upper
      }

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Computing summaries for scans...", i_step, n_step)
      )
      i_step <- i_step + 1

      insidepoly_df_1 <- x3p_insidepoly_df(x3ps$x3p1, concavity = concavity, b = b, ifplot = TRUE)
      insidepoly_df_2 <- x3p_insidepoly_df(x3ps$x3p2, concavity = concavity, b = b, ifplot = TRUE)

      output$number_of_missing_immediate_neighbors_boxplot_1 <- renderPlot({
        attr(insidepoly_df_1, "number_of_missing_immediate_neighbors_boxplot")
      })
      output$number_of_missing_immediate_neighbors_boxplot_2 <- renderPlot({
        attr(insidepoly_df_2, "number_of_missing_immediate_neighbors_boxplot")
      })

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Detrending...", i_step, n_step)
      )
      i_step <- i_step + 1

      x3p_inner_nomiss_res_1 <- df_rmtrend_x3p(insidepoly_df_1)
      x3p_inner_nomiss_res_2 <- df_rmtrend_x3p(insidepoly_df_2)

      output$x3p_inner_nomiss_res_1_plot <- renderRglwidget({
        x3p_inner_nomiss_res_1 %>%
          x3p_delete_mask() %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })
      output$x3p_inner_nomiss_res_2_plot <- renderRglwidget({
        x3p_inner_nomiss_res_2 %>%
          x3p_delete_mask() %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Imputing for missing values...", i_step, n_step)
      )
      i_step <- i_step + 1

      dir_name <- tempdir()
      x3p_inner_impute_1 <- x3p_impute(x3p_inner_nomiss_res_1, ifsave = TRUE, dir_name = dir_name, gif_name = "focal_impute_1.gif")
      x3p_inner_impute_2 <- x3p_impute(x3p_inner_nomiss_res_2, ifsave = TRUE, dir_name = dir_name, gif_name = "focal_impute_2.gif")

      output$x3p_inner_impute_1_plot <- renderRglwidget({
        x3p_inner_impute_1 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })
      output$x3p_inner_impute_2_plot <- renderRglwidget({
        x3p_inner_impute_2 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })

      output$impute_gif_1_plot <- renderImage(
        {
          list(src = paste0(dir_name, "/focal_impute_1.gif"), contentType = "image/gif", width = 400)
        },
        deleteFile = TRUE
      )
      output$impute_gif_2_plot <- renderImage(
        {
          list(src = paste0(dir_name, "/focal_impute_2.gif"), contentType = "image/gif", width = 400)
        },
        deleteFile = TRUE
      )

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Rotating scans...", i_step, n_step)
      )
      i_step <- i_step + 1

      x3p_bin_rotate_1 <- x3p_vertical(x3p_inner_impute_1,
        freqs = c(0, colour_cutoff, 1),
        min_score_cut = min_score_cut,
        loess_span = loess_span,
        ifplot = TRUE
      )
      x3p_bin_rotate_2 <- x3p_vertical(x3p_inner_impute_2,
        freqs = c(0, colour_cutoff, 1),
        min_score_cut = min_score_cut,
        loess_span = loess_span,
        ifplot = TRUE
      )

      output$x3p_bin_rotate_1_plot <- renderRglwidget({
        x3p_bin_rotate_1 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })
      output$x3p_bin_rotate_2_plot <- renderRglwidget({
        x3p_bin_rotate_2 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })

      output$nfline_red_plot_1 <- renderPlot({
        attr(x3p_bin_rotate_1, "nfline_red_plot")
      })
      output$MLE_loess_red_plot_1 <- renderPlot({
        attr(x3p_bin_rotate_1, "MLE_loess_red_plot")
      })
      output$nfline_red_plot_2 <- renderPlot({
        attr(x3p_bin_rotate_2, "nfline_red_plot")
      })
      output$MLE_loess_red_plot_2 <- renderPlot({
        attr(x3p_bin_rotate_2, "MLE_loess_red_plot")
      })

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Shifting scans...", i_step, n_step)
      )
      i_step <- i_step + 1

      x3p_bin_shift_1 <- x3p_shift(x3p_bin_rotate_1, delta = delta_lower:delta_upper)
      x3p_bin_shift_2 <- x3p_shift(x3p_bin_rotate_2, delta = delta_lower:delta_upper)

      output$x3p_bin_shift_1_plot <- renderRglwidget({
        x3p_bin_shift_1 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })
      output$x3p_bin_shift_2_plot <- renderRglwidget({
        x3p_bin_shift_2 %>%
          x3p_image_autosize(zoom = 1.5)
        rglwidget()
      })

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Extracting signals...", i_step, n_step)
      )
      i_step <- i_step + 1

      shift_sig_1 <- x3p_raw_sig_df(x3p_bin_shift_1) %>%
        df_ccsig() %>%
        filter(between(sig, -10, 10))
      shift_sig_2 <- x3p_raw_sig_df(x3p_bin_shift_2) %>%
        df_ccsig() %>%
        filter(between(sig, -10, 10))

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Aligning signals...", i_step, n_step)
      )
      i_step <- i_step + 1

      aligned <- vec_align_sigs_list(
        shift_sig_1$sig,
        shift_sig_2$sig,
        ifplot = TRUE
      )

      output$sig_align_plot <- renderPlot({
        attr(aligned, "sig_align_plot")
      })

      updateTabsetPanel(inputId = "tabsetPanel", selected = "Signals after aligning")

      incProgress(1 / n_step,
        detail = sprintf("Step %d of %d: Complete.", i_step, n_step)
      )
      i_step <- i_step + 1

      Sys.sleep(1)
    })
  })
}

shinyApp(ui = ui, server = server)
