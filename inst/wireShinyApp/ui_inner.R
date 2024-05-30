library(shinydashboard) #dashboardPage
library(gfonts) #use_pkg_gfont

dashboardPage(
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
