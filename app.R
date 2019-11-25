library(shiny)
# devtools::install_github("milanwiedemann/lcsm")
library(lcsm)
library(lavaan)
library(tidyverse)
library(ggpubr)
library(DT)

# ui ----
ui <- tagList(navbarPage(
  "lcsm",
  
  # Overview ----
  tabPanel("Overview",
           includeMarkdown("INCLUDEME.md")),
  
  # Simulate Data ----
  # Generate univariate LCSM ----
  navbarMenu(
    "Generate lavaan Syntax",
    tabPanel(
      "Univariate LCSM",
      column(width = 3,
             h4("Options:"),
             tabsetPanel(
               tabPanel(
                 "Data Characteristics",
                 helpText(),
                 # just a placeholder for a little bit top margin
                 wellPanel(
                   numericInput(
                     "specify_uni_timepoints",
                     "Measurement Points:",
                     value = 5,
                     min = 2
                   ),
                   helpText("Note: Number of repeated measurement points."),
                   textInput("specify_uni_var_name", "Variable Name:", value = "x"),
                   helpText(
                     "Note: Variable name to be used for generating lavaan syntax, changes wont show on the path diagram.
                     Variable name should start with a letter."
                   )
                 )
               ),
               tabPanel("Parameters",
                        helpText(),  # just a placeholder for a little bit top margin
                        wellPanel(
                          checkboxGroupInput(
                            "specify_uni_param",
                            label = "Construct X",
                            choices = list(
                              "Constant change factor [alpha_g]" = "alpha_constant",
                              "Proportional change factor [beta_x]" = "beta",
                              "Autoregression of change scores [phi_x]" = "phi"
                            ),
                            selected = c("alpha_constant", "beta", "phi")
                          )
                        )
                        )
             )),
      column(9,
             h4("Results:"),
             tabsetPanel(
               tabPanel(
                 "lavaan Syntax",
                 helpText(
                   "Note: lavaan syntax for the selected data characteristics and model parameters.
                    This syntax includes comments describing the different sections of the model and can be modified by hand.
                    Modified syntax could be used in the 'model' argument of functions from the lavaan package.
                    Observed scores in the syntax are the variable name followed by a number indicating the measurement point.
                    Latent true scores have the prefix 'l' (for latent) followed by the variable name of the observed score.
                    Change scores have the prefix 'd' (for delta) followed by the variable name of the observed score."
                 ),
                 verbatimTextOutput("lavaan_uni_lcsm")
               ),
               tabPanel(
                 "Path Diagram",
                 helpText(),
                 # just a placeholder for a little bit top margin
                 # wellPanel(
                 fluidRow(column(
                   4,
                   checkboxInput(
                     "plot_specify_uni_lcsm_path_whatLabels",
                     "Show the parameter names as labels",
                     value = TRUE,
                     width = NULL
                   )
                 ),
                 column(
                   4,
                   checkboxInput(
                     "plot_specify_uni_lcsm_path_colorgroups",
                     "I like colours",
                     value = FALSE,
                     width = NULL
                   )
                 )),
                 fluidRow(column(
                   8,
                   plotOutput(
                     "plot_specify_uni_lcsm_path",
                     width = 900,
                     height = 550
                   )
                 )),
                 hr("Reference: Sacha Epskamp (2019). semPlot: Path Diagrams and Visual Analysis of Various SEM Packages' Output. R package version 1.1.2.
  https://CRAN.R-project.org/package=semPlot.")
               )
             ))
    ),
    # Generate bivariate LCSM ----
    tabPanel(
      "Bivariate LCSM",
      column(width = 3,
             h4("Options:"),
             tabsetPanel(
               tabPanel(
                 "Data Characteristics",
                 helpText(),
                 # just a placeholder for a little bit top margin
                 wellPanel(
                   numericInput(
                     "specify_bi_timepoints",
                     "Measurement Points:",
                     value = 5,
                     min = 2
                   ),
                   helpText("Note: Number of repeated measurement points for each construct.")
                 ),
                 wellPanel(
                   textInput("specify_bi_var_name_x", "Variable Name Construct X:", value = "x"),
                   textInput("specify_var_name_y", "Variable Name Construct Y:", value = "y"),
                   helpText(
                     "Note: Variable names to be used for generating lavaan syntax, changes wont show on the path diagram.
                     Variable names should start with a letter."
                   )
                 )
               ),
               tabPanel(
                 "Parameters",
                 helpText(),
                 # just a placeholder for a little bit top margin
                 wellPanel(
                   checkboxGroupInput(
                     "specify_bi_param_x",
                     label = "Construct X:",
                     choices = list(
                       "Constant change factor [alpha_g]" = "alpha_constant_x",
                       "Proportional change factor [beta_x]" = "beta_x",
                       "Autoregression of change scores [phi_x]" = "phi_x"
                     ),
                     selected = c("alpha_constant_x", "beta_x", "phi_x")
                   )
                 ),
                 wellPanel(
                   checkboxGroupInput(
                     "specify_bi_param_y",
                     label = "Construct Y:",
                     choices = list(
                       "Constant change factor [alpha_j]" = "alpha_constant_y",
                       "Proportional change factor [beta_y]" = "beta_y",
                       "Autoregression of change scores [phi_y]" = "phi_y"
                     ),
                     selected = c("alpha_constant_y", "beta_y", "phi_y")
                   )
                 ),
                 wellPanel(
                   checkboxGroupInput(
                     "specify_bi_param_coupling",
                     label = "Coupling:",
                     choices = list(
                       "Change score x (t) determined by true score y (t) [delta_con_xy]" = "delta_con_xy",
                       "Change score y (t) determined by true score x (t)  [delta_con_yx]" = "delta_con_yx",
                       "Change score x (t) determined by true score y (t-1) [delta_lag_xy]" = "delta_lag_xy",
                       "Change score y (t) determined by true score x (t-1) [delta_lag_yx]" = "delta_lag_yx",
                       "Change score x (t) determined by change score y (t) [xi_con_xy]" = "xi_con_xy",
                       "Change score y (t) determined by change score x (t) [xi_con_yx]" = "xi_con_yx",
                       "Change score x (t) determined by change score y (t-1) [xi_lag_xy]" = "xi_lag_xy",
                       "Change score y (t) determined by change score x (t-1) [xi_lag_yx]" = "xi_lag_yx"
                     )
                   )
                 )
               )
             )),
      column(9,
             h4("Results:"),
             tabsetPanel(
               tabPanel(
                 "lavaan Syntax",
                 helpText(
                   "Note: lavaan syntax for the selected data characteristics and model parameters.
                    This syntax includes comments describing the different sections of the model and can be modified by hand.
                    Modified syntax could be used in the 'model' argument of functions from the lavaan package.
                    Observed scores in the syntax are the variable name followed by a number indicating the measurement point.
                    Latent true scores have the prefix 'l' (for latent) followed by the variable name of the observed score.
                    Change scores have the prefix 'd' (for delta) followed by the variable name of the observed score."
                 ),
                 verbatimTextOutput("lavaan_bi_lcsm")
               ),
               tabPanel(
                 "Path Diagram",
                 helpText(),
                 # just a placeholder for a little bit top margin
                 # wellPanel(
                 fluidRow(column(
                   4,
                   checkboxInput(
                     "plot_specify_bi_lcsm_path_whatLabels",
                     "Show the parameter names as labels",
                     value = TRUE,
                     width = NULL
                   )
                 ),
                 column(
                   4,
                   checkboxInput(
                     "plot_specify_bi_lcsm_path_colorgroups",
                     "I like colours",
                     value = FALSE,
                     width = NULL
                   )
                 )),
                 fluidRow(column(
                   8,
                   plotOutput(
                     "plot_specify_bi_lcsm_path",
                     width = 900,
                     height = 550
                   )
                 )),
                 hr("Reference: Sacha Epskamp (2019). semPlot: Path Diagrams and Visual Analysis of Various SEM Packages' Output. R package version 1.1.2.
  https://CRAN.R-project.org/package=semPlot.")
               )
             ))
    )
  ),
  
  navbarMenu(
    "Simulate Data",
    # Simulate Univariate ----
    tabPanel(
      "Univariate LCSM",
      column(
        width = 4,
        h4("Options:"),
        tabsetPanel(
          tabPanel(
            "Data Characteristics",
            helpText(),
            # just a placeholder for a little bit top margin
            wellPanel(
              numericInput("sim_uni_timepoints", "Measurement Points:", value = 7),
              numericInput("sim_uni_samplesize", "Sample Size:", value = 500),
              sliderInput(
                "sim_uni_na_x_pct",
                "Missingness in %:",
                min = 0,
                max = 100,
                value = 0,
                step = 1
              ),
              helpText("Note: Missing values are added randomly after simulating data.")
              # Maybe have a Simulate Data button at some point?
              # actionButton("simulate_action", "Simulate Data", class = "btn-primary")
            )
          ),
          # Enter Parameters ----
          tabPanel(
            "Parameters",
            helpText(),
            # just a placeholder for a little bit top margin
            wellPanel(
              helpText(
                "Note: See 'Help' for further information about the parameters.
                       Example parameters in the input fields below were taken from Grimm, Ram & Estabrook (2017), Chapter 16."
              ),
              numericInput(
                "sim_uni_gamma_lx1",
                "gamma_lx1",
                value = 32.53,
                step = .1
              ),
              numericInput(
                "sim_uni_sigma2_lx1",
                "sigma2_lx1",
                value = 71.9,
                step = .1
              ),
              numericInput(
                "sim_uni_sigma2_ux",
                "sigma2_ux",
                value = 30.82,
                step = .1
              ),
              numericInput(
                "sim_uni_beta_x",
                "beta_x",
                value = -0.24,
                step = .1
              ),
              numericInput(
                "sim_uni_alpha_g2",
                "alpha_g2",
                value = 15.22,
                step = .1
              ),
              numericInput(
                "sim_uni_sigma2_g2",
                "sigma2_g2",
                value = 5.6,
                step = .1
              ),
              numericInput(
                "sim_uni_sigma_g2lx1",
                "sigma_g2lx1",
                value = 13.75,
                step = .1
              ),
              numericInput("sim_uni_phi_x",
                           "phi_x",
                           value = NA,
                           step = .1)
            )
          ),
          tabPanel("Help",
                   helpText(),  # just a placeholder for a little bit top margin
                   includeMarkdown("INCLUDEME_UNI.md"))
        )
      ),
      column(
        width = 8,
        h4("Results:"),
        tabsetPanel(
          tabPanel(
            "Simulated Data",
            DT::dataTableOutput("datatable_sim_uni_lcsm"),
            downloadButton("download_uni_data", "Download")
          ),
          tabPanel("Longitudinal Plot",
                   plotOutput("plot_sim_uni_lcsm"),
                   hr("Reference: Hadley Wickham (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.")),
          tabPanel(
            "lavaan Syntax",
            helpText(
              "Note: The lavaan syntax below was used to simulate the data using the function simulateData() from the R package lavaan.
                    Observed scores in the syntax are the variable name followed by a number indicating the measurement point.
                    Latent true scores have the prefix 'l' (for latent) followed by the variable name of the observed score.
                    Change scores have the prefix 'd' (for delta) followed by the variable name of the observed score."
            ),
            verbatimTextOutput("lavaan_sim_uni_lcsm")
          ),
          # Maybe include tab with simplified Path Diagram?
          tabPanel(
            "Path Diagram",
            helpText(),
            # just a placeholder for a little bit top margin
            # wellPanel(
            fluidRow(column(
              4,
              checkboxInput(
                "plot_sim_uni_lcsm_path_whatLabels",
                "Show the parameter names as labels",
                value = TRUE,
                width = NULL
              )
            ),
            column(
              4,
              checkboxInput(
                "plot_sim_uni_lcsm_path_colorgroups",
                "I like colours",
                value = FALSE,
                width = NULL
              )
            )),
            fluidRow(column(
              8,
              plotOutput(
                "plot_sim_uni_lcsm_path",
                width = 900,
                height = 550
              )
            )),
            hr("Reference: Sacha Epskamp (2019). semPlot: Path Diagrams and Visual Analysis of Various SEM Packages' Output. R package version 1.1.2.
  https://CRAN.R-project.org/package=semPlot.")
          )
        )
      )
    ),
    
    # Simulate Bivariate ----
    tabPanel(
      "Bivariate LCSM",
      column(
        width = 4,
        h4("Options:"),
        tabsetPanel(
          tabPanel(
            "Data Characteristics",
            helpText(),
            # just a placeholder for a little bit top margin
            wellPanel(
              numericInput(
                "sim_bi_timepoints",
                "Measurement Points:",
                value = 7,
                min = 2
              ),
              numericInput("sim_bi_samplesize", "Sample Size:", value = 500),
              sliderInput(
                "sim_bi_na_x_pct",
                "Missingness Construct X in % :",
                min = 0,
                max = 100,
                value = 0,
                step = 1
              ),
              sliderInput(
                "sim_bi_na_y_pct",
                "Missingness Construct Y in % :",
                min = 0,
                max = 100,
                value = 0,
                step = 1
              ),
              helpText("Note: Missing values are added randomly after simulating data."),
              # Maybe have a Simulate Data button at some point?
              # actionButton("simulate_action", "Simulate data", class = "btn-primary")
            )
          ),
          # Enter Parameters ----
          tabPanel("Parameters",
                   tabsetPanel(
                     tabPanel(
                       "Construct X",
                       helpText(),
                       # just a placeholder for a little bit top margin
                       wellPanel(
                         helpText(
                           "Note: See 'Help' for further information about the parameters.
                           Example parameters in the input fields below were taken from Grimm, Ram & Estabrook (2017), Chapter 17."
                         ),
                         numericInput(
                           "sim_bi_gamma_lx1",
                           "gamma_lx1",
                           value = 32.52,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma2_lx1",
                           "sigma2_lx1",
                           value = 72.73,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma2_ux",
                           "sigma2_ux",
                           value = 31.44,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_beta_x",
                           "beta_x",
                           value = -.30,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_alpha_g2",
                           "alpha_g2",
                           value = 15.20,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma2_g2",
                           "sigma2_g2",
                           value = 5.79,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma_g2lx1",
                           "sigma_g2lx1",
                           value = 14.46,
                           step = .1
                         ),
                         numericInput("sim_bi_phi_x",
                                      "phi_x",
                                      value = NA,
                                      step = .1)
                       )
                     ),
                     tabPanel(
                       "Construct Y",
                       helpText(),
                       # just a placeholder for a little bit top margin
                       wellPanel(
                         helpText(
                           "Note: See 'Help' for further information about the parameters.
                           Example parameters in the input fields below were taken from Grimm, Ram & Estabrook (2017), Chapter 17."
                         ),
                         numericInput(
                           "sim_bi_gamma_ly1",
                           "gamma_ly1",
                           value = 34.36,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma2_ly1",
                           "sigma2_ly1",
                           value = 72.26,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma2_uy",
                           "sigma2_uy",
                           value = 33.34,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_beta_y",
                           "beta_y",
                           value = -.49,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_alpha_j2",
                           "alpha_j2",
                           value = 10.99,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma2_j2",
                           "sigma2_j2",
                           value = 17.85,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma_j2ly1",
                           "sigma_j2ly1",
                           value = 25.98,
                           step = .1
                         ),
                         numericInput("sim_bi_phi_y",
                                      "phi_y",
                                      value = NA,
                                      step = .1)
                       )
                     ),
                     tabPanel(
                       "Coupling",
                       helpText(),
                       # just a placeholder for a little bit top margin
                       wellPanel(
                         helpText(
                           "Note: See 'Help' for further information about the parameters.
                           Example parameters in the input fields below were taken from Grimm, Ram & Estabrook (2017), Chapter 17."
                         ),
                         numericInput(
                           "sim_bi_sigma_su",
                           "sigma_su",
                           value = 6.46,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma_ly1lx1",
                           "sigma_ly1lx1",
                           value = 57.34,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma_g2ly1",
                           "sigma_g2ly1",
                           value = 10.04,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma_j2lx1",
                           "sigma_j2lx1",
                           value = 25.98,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_sigma_j2g2",
                           "sigma_j2g2",
                           value = .79,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_delta_lag_xy",
                           "delta_lag_xy",
                           value = 0.05,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_delta_lag_yx",
                           "delta_lag_yx",
                           value = 0.38,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_xi_lag_xy",
                           "xi_lag_xy",
                           value = NA,
                           step = .1
                         ),
                         numericInput(
                           "sim_bi_xi_lag_yx",
                           "xi_lag_yx",
                           value = NA,
                           step = .1
                         ),
                       )
                     )
                   )),
          tabPanel("Help",
                   tabsetPanel(
                     tabPanel(
                       "Construct X",
                       helpText(),
                       # just a placeholder for a little bit top margin
                       includeMarkdown("INCLUDEME_BI_X.md")
                     ),
                     tabPanel(
                       "Construct Y",
                       helpText(),
                       # just a placeholder for a little bit top margin
                       includeMarkdown("INCLUDEME_BI_Y.md")
                     ),
                     tabPanel("Coupling",
                              helpText(),  # just a placeholder for a little bit top margin
                              includeMarkdown("INCLUDEME_BI_C.md"))
                   ))
        )
      ),
      column(
        width = 8,
        h4("Results:"),
        tabsetPanel(
          tabPanel(
            "Simulated Data",
            DT::dataTableOutput("datatable_sim_bi_lcsm"),
            downloadButton("download_bi_data", "Download")
          ),
          tabPanel(
            "Longitudinal Plots",
            plotOutput("plot_sim_bi_lcsm", width = 850, height = 550),
            hr("Reference: Hadley Wickham (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.")
          ),
          tabPanel(
            "lavaan Syntax",
            helpText(
              "Note: The lavaan syntax below was used to simulate the data using the function simulateData() from the R package lavaan.
                    Observed scores in the syntax are the variable name followed by a number indicating the measurement point.
                    Latent true scores have the prefix 'l' (for latent) followed by the variable name of the observed score.
                    Change scores have the prefix 'd' (for delta) followed by the variable name of the observed score."
            ),
            verbatimTextOutput("lavaan_sim_bi_lcsm")
          ),
          # Maybe include tab with simplified Path Diagram?
          tabPanel(
            "Path Diagram",
            helpText(),
            # just a placeholder for a little bit top margin
            # wellPanel(
            fluidRow(column(
              4,
              checkboxInput(
                "plot_sim_bi_lcsm_path_whatLabels",
                "Show the parameter names as labels",
                value = TRUE,
                width = NULL
              )
            ),
            column(
              4,
              checkboxInput(
                "plot_sim_bi_lcsm_path_colorgroups",
                "I like colours",
                value = FALSE,
                width = NULL
              )
            )),
            fluidRow(column(
              8,
              plotOutput("plot_sim_bi_lcsm_path", width = 900, height = 550)
            )),
            hr("Reference: Sacha Epskamp (2019). semPlot: Path Diagrams and Visual Analysis of Various SEM Packages' Output. R package version 1.1.2.
  https://CRAN.R-project.org/package=semPlot.")
          )
        )
      )
    )
  ),
  # Fit univariate LCSM ----
  navbarMenu("Fit Model",
  tabPanel(
    "Univariate LCSM",
    column(width = 4, h4("Options:"),
           tabsetPanel(
             tabPanel("Load Data",
                      helpText(),
                      wellPanel(
                        # Input: Select a file ---
                        fileInput("file1", "Select CSV File",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        # Input: Checkbox if file has header ---
                        checkboxInput("header", "Variable names included", TRUE),
                        
                        # Input: Select separator ---
                        radioButtons("sep", "Separator",
                                     choices = c("Comma" = ",",
                                                 "Semicolon" = ";",
                                                 "Tab" = "\t"),
                                     selected = ","),
                        
                        # Input: Select quotes ---
                        radioButtons("quote", "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = '"'),
                        
                        # Horizontal line ---
                        tags$hr(),
                          checkboxInput("uni_sample_data_check", "Select Example Data", T)
      )
      ),
      tabPanel("Select Variables",
               helpText(),  # just a placeholder for a little bit top margin
               wellPanel(
                 # numericInput(
                 #   "fit_uni_timepoints",
                 #   "Measurement Points:",
                 #   value = 10,
                 #   min = 2
                 # ),
                 
                 selectInput("uni_select_id", "Select ID Variable:", c("Variable 1", "Variable 2", "Variable 3"),  multiple = FALSE
                 ),
                 
                 selectInput("uni_select_vars", "Select Construct x Variables:", c("Variable 1", "Variable 2", "Variable 3"),  multiple = TRUE
                              )

               )
               ),
      tabPanel("Select Parameters",
               helpText(),  # just a placeholder for a little bit top margin
               wellPanel(
                 checkboxGroupInput(
                   "fit_uni_param",
                   label = "Construct X",
                   choices = list(
                     "Constant change factor [alpha_g]" = "alpha_constant",
                     "Proportional change factor [beta_x]" = "beta",
                     "Autoregression of change scores [phi_x]" = "phi"
                   )
                 )
               ),
               actionButton("fit-uni-lcsm", "Fit model", class = "btn-primary")
      )
      
      )
    ),
    column(8, h4("Results:"),
      tabsetPanel(
      tabPanel("Data",
               helpText("Note: Values are rounded to the third decimal place."),  # just a placeholder for a little bit top margin
               
               DT::dataTableOutput("contents")),
      tabPanel(
        "lavaan Syntax",
        helpText(
          "Note: lavaan syntax for the selected data characteristics and model parameters.
                    This syntax includes comments describing the different sections of the model and can be modified by hand.
                    Modified syntax could be used in the 'model' argument of functions from the lavaan package.
                    Observed scores in the syntax are the variable name followed by a number indicating the measurement point.
                    Latent true scores have the prefix 'l' (for latent) followed by the variable name of the observed score.
                    Change scores have the prefix 'd' (for delta) followed by the variable name of the observed score."
        ),
        verbatimTextOutput("lavaan_fit_uni_lcsm")
      ),
      tabPanel("Estimated Parameters",
               helpText(),
               DT::dataTableOutput("fit_uni_lcsm_param"),
               hr("Reference: David Robinson and Alex Hayes (2019). broom: Convert Statistical Analysis Objects into Tidy Tibbles. R package version 0.5.2.
  https://CRAN.R-project.org/package=broom.")
               ),
      tabPanel("Fit Statistics",
               helpText(),
               fluidRow(column(
                                  4,
                                  checkboxInput(
                                    "fit_uni_lcsm_fit_stats_details",
                                    "Show details",
                                    value = FALSE,
                                    width = NULL
                                  )
                                )
                        ),
                                fluidRow(column(
                                  8,

               DT::dataTableOutput("fit_uni_lcsm_fit_stats"),
               hr("Reference: David Robinson and Alex Hayes (2019). broom: Convert Statistical Analysis Objects into Tidy Tibbles. R package version 0.5.2.
  https://CRAN.R-project.org/package=broom.")
               ))),
      tabPanel("Longitudinal Plot",
               plotOutput("plot_fit_uni_lcsm"),
               hr("Reference: Hadley Wickham (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.")),
      tabPanel(
        "Path Diagram",
        helpText(),
        # just a placeholder for a little bit top margin
        # wellPanel(
        fluidRow(column(
          4,
          checkboxInput(
            "plot_fit_uni_lcsm_path_whatLabels",
            "Show the parameter names as labels",
            value = TRUE,
            width = NULL
          )
        ),
        column(
          4,
          checkboxInput(
            "plot_fit_uni_lcsm_path_colorgroups",
            "I like colours",
            value = FALSE,
            width = NULL
          )
        )),
        fluidRow(column(
          8,
          plotOutput("plot_fit_uni_lcsm_path", width = 900, height = 550)
        )),
        hr("Reference: Sacha Epskamp (2019). semPlot: Path Diagrams and Visual Analysis of Various SEM Packages' Output. R package version 1.1.2.
  https://CRAN.R-project.org/package=semPlot.")
      )
    ))
  ),
  # Fit bivariate LCSM ----
  tabPanel("Bivariate LCSM",
           )
  )
))

# server ----
server <- function(input, output, session) {
  # Specify univariate syntax ----
  output$lavaan_uni_lcsm <- renderText({
    specify_uni_param <- input$specify_uni_param
    # alpha_constant
    if ("alpha_constant" %in% specify_uni_param) {
      alpha_constant <- TRUE
    } else {
      alpha_constant <- FALSE
    }
    # beta
    if ("beta" %in% specify_uni_param) {
      beta <- TRUE
    } else {
      beta <- FALSE
    }
    # phi
    if ("phi" %in% specify_uni_param) {
      phi <- TRUE
    } else {
      phi <- FALSE
    }
    
    # Create lavaan syntax
    specify_uni_lcsm(
      timepoints = input$specify_uni_timepoints,
      model = list(
        alpha_constant = alpha_constant,
        beta = beta,
        phi = phi
      ),
      var = input$specify_uni_var_name,
      change_letter = "g"
    )
  })
  
  # Specify bivariate lavaan syntax ----
  output$lavaan_bi_lcsm <- renderText({
    specify_bi_param_x <- input$specify_bi_param_x
    # alpha_constant_x
    if ("alpha_constant_x" %in% specify_bi_param_x) {
      alpha_constant_x <- TRUE
    } else {
      alpha_constant_x <- FALSE
    }
    # beta_x
    if ("beta_x" %in% specify_bi_param_x) {
      beta_x <- TRUE
    } else {
      beta_x <- FALSE
    }
    # phi_x
    if ("phi_x" %in% specify_bi_param_x) {
      phi_x <- TRUE
    } else {
      phi_x <- FALSE
    }
    
    specify_bi_param_y <- input$specify_bi_param_y
    # alpha_constant_y
    if ("alpha_constant_y" %in% specify_bi_param_y) {
      alpha_constant_y <- TRUE
    } else {
      alpha_constant_y <- FALSE
    }
    # beta_y
    if ("beta_y" %in% specify_bi_param_y) {
      beta_y <- TRUE
    } else {
      beta_y <- FALSE
    }
    # phi_x
    if ("phi_y" %in% specify_bi_param_y) {
      phi_y <- TRUE
    } else {
      phi_y <- FALSE
    }
    
    specify_bi_param_coupling <- input$specify_bi_param_coupling
    # delta_con_xy
    if ("delta_con_xy" %in% specify_bi_param_coupling) {
      delta_con_xy <- TRUE
    } else {
      delta_con_xy <- FALSE
    }
    # delta_con_yx
    if ("delta_con_yx" %in% specify_bi_param_coupling) {
      delta_con_yx <- TRUE
    } else {
      delta_con_yx <- FALSE
    }
    # xi_con_xy
    if ("xi_con_xy" %in% specify_bi_param_coupling) {
      xi_con_xy <- TRUE
    } else {
      xi_con_xy <- FALSE
    }
    # xi_con_yx
    if ("xi_con_yx" %in% specify_bi_param_coupling) {
      xi_con_yx <- TRUE
    } else {
      xi_con_yx <- FALSE
    }
    
    # delta_lag_xy
    if ("delta_lag_xy" %in% specify_bi_param_coupling) {
      delta_lag_xy <- TRUE
    } else {
      delta_lag_xy <- FALSE
    }
    # delta_lag_yx
    if ("delta_lag_yx" %in% specify_bi_param_coupling) {
      delta_lag_yx <- TRUE
    } else {
      delta_lag_yx <- FALSE
    }
    # xi_lag_xy
    if ("xi_lag_xy" %in% specify_bi_param_coupling) {
      xi_lag_xy <- TRUE
    } else {
      xi_lag_xy <- FALSE
    }
    # xi_lag_yx
    if ("xi_lag_yx" %in% specify_bi_param_coupling) {
      xi_lag_yx <- TRUE
    } else {
      xi_lag_yx <- FALSE
    }
    
    specify_bi_lcsm(
      timepoints = input$specify_bi_timepoints,
      var_x = input$specify_bi_var_name_x,
      model_x = list(
        alpha_constant = alpha_constant_x,
        beta = beta_x,
        phi = phi_x
      ),
      var_y = input$specify_var_name_y,
      model_y = list(
        alpha_constant = alpha_constant_y,
        beta = beta_y,
        phi = phi_y
      ),
      coupling = list(
        delta_con_xy = delta_con_xy,
        delta_con_yx = delta_con_yx,
        xi_con_xy = xi_con_xy,
        xi_con_yx = xi_con_yx,
        delta_lag_xy = delta_lag_xy,
        delta_lag_yx = delta_lag_yx,
        xi_lag_xy = xi_lag_xy,
        xi_lag_yx = xi_lag_yx
      ),
      change_letter_x = "g",
      change_letter_y = "j"
    )
  })
  
  # Simulate data ----
  # Univariate ----
  # Reactive environment to simulate data
  simulate_uni_lcsm <- reactive({
    sim_uni_gamma_lx1 <- input$sim_uni_gamma_lx1
    sim_uni_sigma2_lx1 <- input$sim_uni_sigma2_lx1
    sim_uni_sigma2_ux <- input$sim_uni_sigma2_ux
    sim_uni_beta_x <- input$sim_uni_beta_x
    sim_uni_alpha_g2 <- input$sim_uni_alpha_g2
    sim_uni_sigma2_g2 <- input$sim_uni_sigma2_g2
    sim_uni_sigma_g2lx1 <- input$sim_uni_sigma_g2lx1
    sim_uni_phi_x <- input$sim_uni_phi_x
    
    # set constant change parameter for simulating data
    if (base::is.na(sim_uni_alpha_g2) == TRUE |
        base::is.na(sim_uni_sigma2_g2) == TRUE |
        base::is.na(sim_uni_sigma_g2lx1) == TRUE) {
      sim_uni_model_alpha_constant_x <- FALSE
    } else {
      sim_uni_model_alpha_constant_x <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_uni_beta_x) == TRUE) {
      sim_uni_model_beta_x <- FALSE
    } else {
      sim_uni_model_beta_x <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_uni_phi_x) == TRUE) {
      sim_uni_model_phi_x <- FALSE
    } else {
      sim_uni_model_phi_x <- TRUE
    }
    
    sim_uni_lcsm(
      timepoints = input$sim_uni_timepoints,
      model = list(
        alpha_constant = sim_uni_model_alpha_constant_x,
        beta = sim_uni_model_beta_x,
        phi = sim_uni_model_phi_x
      ),
      model_param = list(
        gamma_lx1 = sim_uni_gamma_lx1,
        sigma2_lx1 = sim_uni_sigma2_lx1,
        sigma2_ux = sim_uni_sigma2_ux,
        alpha_g2 = sim_uni_alpha_g2,
        beta_x = sim_uni_beta_x,
        sigma2_g2 = sim_uni_sigma2_g2,
        sigma_g2lx1 = sim_uni_sigma_g2lx1,
        phi_x = sim_uni_phi_x
      ),
      sample.nobs = input$sim_uni_samplesize,
      na_pct = input$sim_uni_na_x_pct / 100
    )
  })
  
  # Downloadable csv of selected dataset ----
  output$download_uni_data <- downloadHandler(
    filename = function() {
      paste("uni_lcsm_sim_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(simulate_uni_lcsm(), file, row.names = FALSE)
    }
  )
  
  # Create data table
  output$datatable_sim_uni_lcsm <-
    DT::renderDataTable(
      DT::datatable(
        simulate_uni_lcsm(),
        options = list(searching = FALSE),
        rownames = FALSE
      ) %>%
        DT::formatRound(
          digits = 2,
          columns = 2:ncol(simulate_uni_lcsm())
        )
    )
  
  # simulate data syntax ----
  # univariate ----
  output$lavaan_sim_uni_lcsm <- renderText({
    # extract input variables
    sim_uni_gamma_lx1 <- input$sim_uni_gamma_lx1
    sim_uni_sigma2_lx1 <- input$sim_uni_sigma2_lx1
    sim_uni_sigma2_ux <- input$sim_uni_sigma2_ux
    sim_uni_beta_x <- input$sim_uni_beta_x
    sim_uni_alpha_g2 <- input$sim_uni_alpha_g2
    sim_uni_sigma2_g2 <- input$sim_uni_sigma2_g2
    sim_uni_sigma_g2lx1 <- input$sim_uni_sigma_g2lx1
    sim_uni_phi_x <- input$sim_uni_phi_x
    
    # set constant change parameter for simulating data
    if (base::is.na(sim_uni_alpha_g2) == TRUE |
        base::is.na(sim_uni_sigma2_g2) == TRUE |
        base::is.na(sim_uni_sigma_g2lx1) == TRUE) {
      sim_uni_model_alpha_constant_x <- FALSE
    } else {
      sim_uni_model_alpha_constant_x <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_uni_beta_x) == TRUE) {
      sim_uni_model_beta_x <- FALSE
    } else {
      sim_uni_model_beta_x <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_uni_phi_x) == TRUE) {
      sim_uni_model_phi_x <- FALSE
    } else {
      sim_uni_model_phi_x <- TRUE
    }
    
    # create lavaan syntax
    sim_uni_lcsm(
      timepoints = input$sim_uni_timepoints,
      return_lavaan_syntax = TRUE,
      return_lavaan_syntax_string = TRUE,
      model = list(
        alpha_constant = sim_uni_model_alpha_constant_x,
        beta = sim_uni_model_beta_x,
        phi = sim_uni_model_phi_x
      ),
      model_param = list(
        gamma_lx1 = sim_uni_gamma_lx1,
        sigma2_lx1 = sim_uni_sigma2_lx1,
        sigma2_ux = sim_uni_sigma2_ux,
        alpha_g2 = sim_uni_alpha_g2,
        beta_x = sim_uni_beta_x,
        sigma2_g2 = sim_uni_sigma2_g2,
        sigma_g2lx1 = sim_uni_sigma_g2lx1,
        phi_x = sim_uni_phi_x
      ),
      sample.nobs = input$sim_uni_samplesize,
      na_pct = input$sim_uni_na_x_pct / 100
    )
  })
  
  # Longitudinal plots
  output$plot_sim_uni_lcsm <- renderPlot({
    simulate_uni_lcsm() %>%
      plot_trajectories(
        id_var = names(simulate_uni_lcsm())[1],
        var_list = names(simulate_uni_lcsm())[2:ncol(simulate_uni_lcsm())],
        xlab = "Time",
        ylab = "Construct X",
        connect_missing = FALSE,
        random_sample_frac = 1
      ) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 16, face = "bold"))
  })
  
  
  # Path diagram ----
  output$plot_sim_uni_lcsm_path <- renderPlot({
    withProgress(message = "Making plot", value = 0, {
      # extract input variables
      sim_uni_gamma_lx1 <- input$sim_uni_gamma_lx1
      sim_uni_sigma2_lx1 <- input$sim_uni_sigma2_lx1
      sim_uni_sigma2_ux <- input$sim_uni_sigma2_ux
      sim_uni_beta_x <- input$sim_uni_beta_x
      sim_uni_alpha_g2 <- input$sim_uni_alpha_g2
      sim_uni_sigma2_g2 <- input$sim_uni_sigma2_g2
      sim_uni_sigma_g2lx1 <- input$sim_uni_sigma_g2lx1
      sim_uni_phi_x <- input$sim_uni_phi_x
      
      # set constant change parameter for simulating data
      if (base::is.na(sim_uni_alpha_g2) == TRUE |
          base::is.na(sim_uni_sigma2_g2) == TRUE |
          base::is.na(sim_uni_sigma_g2lx1) == TRUE) {
        sim_uni_model_alpha_constant_x <- FALSE
      } else {
        sim_uni_model_alpha_constant_x <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_uni_beta_x) == TRUE) {
        sim_uni_model_beta_x <- FALSE
      } else {
        sim_uni_model_beta_x <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_uni_phi_x) == TRUE) {
        sim_uni_model_phi_x <- FALSE
      } else {
        sim_uni_model_phi_x <- TRUE
      }
      
      incProgress(1 / 6)
      
      uni_lavaan_results <- fit_uni_lcsm(
        data = simulate_uni_lcsm(),
        var = names(simulate_uni_lcsm())[-1],
        model = list(
          alpha_constant = sim_uni_model_alpha_constant_x,
          beta = sim_uni_model_beta_x,
          phi = sim_uni_model_phi_x
        )
      )
      
      incProgress(2 / 3)
      
      uni_lavaan_syntax <- fit_uni_lcsm(
        data = simulate_uni_lcsm(),
        var = names(simulate_uni_lcsm())[-1],
        model = list(
          alpha_constant = sim_uni_model_alpha_constant_x,
          beta = sim_uni_model_beta_x,
          phi = sim_uni_model_phi_x
        ),
        return_lavaan_syntax = TRUE,
        return_lavaan_syntax_string = TRUE
      )
      
      incProgress(3 / 3)
    })
    
    if (input$plot_sim_uni_lcsm_path_whatLabels == FALSE) {
      plot_sim_uni_lcsm_path_whatLabels <- "invisible"
    } else {
      plot_sim_uni_lcsm_path_whatLabels <- "label"
    }
    
    if (input$plot_sim_uni_lcsm_path_colorgroups == FALSE) {
      plot_lcsm(
        lavaan_object = uni_lavaan_results,
        lavaan_syntax = uni_lavaan_syntax,
        lcsm = "univariate",
        whatLabels = plot_sim_uni_lcsm_path_whatLabels
      )
    } else {
      plot_lcsm(
        lavaan_object = uni_lavaan_results,
        lavaan_syntax = uni_lavaan_syntax,
        lcsm = "univariate",
        whatLabels = plot_sim_uni_lcsm_path_whatLabels,
        groups = "latents",
        borders = FALSE
      )
    }
    
  })
  
  # Path diagram ----
  output$plot_specify_uni_lcsm_path <- renderPlot({
    withProgress(message = "Making plot", value = 0, {
      # extract input variables
      sim_uni_gamma_lx1 <- input$sim_uni_gamma_lx1
      sim_uni_sigma2_lx1 <- input$sim_uni_sigma2_lx1
      sim_uni_sigma2_ux <- input$sim_uni_sigma2_ux
      sim_uni_beta_x <- input$sim_uni_beta_x
      sim_uni_alpha_g2 <- input$sim_uni_alpha_g2
      sim_uni_sigma2_g2 <- input$sim_uni_sigma2_g2
      sim_uni_sigma_g2lx1 <- input$sim_uni_sigma_g2lx1
      sim_uni_phi_x <- input$sim_uni_phi_x
      
      
      specify_uni_param <- input$specify_uni_param
      # alpha_constant
      if ("alpha_constant" %in% specify_uni_param) {
        alpha_constant <- TRUE
      } else {
        alpha_constant <- FALSE
      }
      # beta
      if ("beta" %in% specify_uni_param) {
        beta <- TRUE
      } else {
        beta <- FALSE
      }
      # phi
      if ("phi" %in% specify_uni_param) {
        phi <- TRUE
      } else {
        phi <- FALSE
      }
      
      incProgress(1 / 6)
      
      sim_uni_gamma_lx1 <- input$sim_uni_gamma_lx1
      sim_uni_sigma2_lx1 <- input$sim_uni_sigma2_lx1
      sim_uni_sigma2_ux <- input$sim_uni_sigma2_ux
      sim_uni_beta_x <- input$sim_uni_beta_x
      sim_uni_alpha_g2 <- input$sim_uni_alpha_g2
      sim_uni_sigma2_g2 <- input$sim_uni_sigma2_g2
      sim_uni_sigma_g2lx1 <- input$sim_uni_sigma_g2lx1
      sim_uni_phi_x <- input$sim_uni_phi_x
      
      # set constant change parameter for simulating data
      if (base::is.na(sim_uni_alpha_g2) == TRUE |
          base::is.na(sim_uni_sigma2_g2) == TRUE |
          base::is.na(sim_uni_sigma_g2lx1) == TRUE) {
        sim_uni_model_alpha_constant_x <- FALSE
      } else {
        sim_uni_model_alpha_constant_x <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_uni_beta_x) == TRUE) {
        sim_uni_model_beta_x <- FALSE
      } else {
        sim_uni_model_beta_x <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_uni_phi_x) == TRUE) {
        sim_uni_model_phi_x <- FALSE
      } else {
        sim_uni_model_phi_x <- TRUE
      }
      
      # Simulate data here for plot using same parameters as specified in simulate tab
      simulate_uni_lcsm_path <- sim_uni_lcsm(
        var = input$specify_uni_var_name,
        sample.nobs = 100,
        timepoints = input$specify_uni_timepoints,
        model = list(
          alpha_constant = sim_uni_model_alpha_constant_x,
          beta = sim_uni_model_beta_x,
          phi = sim_uni_model_phi_x
        ),
        model_param = list(
          gamma_lx1 = sim_uni_gamma_lx1,
          sigma2_lx1 = sim_uni_sigma2_lx1,
          sigma2_ux = sim_uni_sigma2_ux,
          alpha_g2 = sim_uni_alpha_g2,
          beta_x = sim_uni_beta_x,
          sigma2_g2 = sim_uni_sigma2_g2,
          sigma_g2lx1 = sim_uni_sigma_g2lx1,
          phi_x = sim_uni_phi_x
        )
      )
      
      uni_lavaan_results <- fit_uni_lcsm(
        data = simulate_uni_lcsm_path,
        var = names(simulate_uni_lcsm_path)[-1],
        model = list(
          alpha_constant = alpha_constant,
          beta = beta,
          phi = phi
        )
      )
      
      incProgress(2 / 3)
      
      uni_lavaan_syntax <- fit_uni_lcsm(
        data = simulate_uni_lcsm_path,
        var = names(simulate_uni_lcsm_path)[-1],
        model = list(
          alpha_constant = alpha_constant,
          beta = beta,
          phi = phi
        ),
        return_lavaan_syntax = TRUE,
        return_lavaan_syntax_string = TRUE
      )
      
      incProgress(3 / 3)
    })
    
    if (input$plot_specify_uni_lcsm_path_whatLabels == FALSE) {
      plot_specify_uni_lcsm_path_whatLabels <- "invisible"
    } else {
      plot_specify_uni_lcsm_path_whatLabels <- "label"
    }
    
    
    if (input$plot_specify_uni_lcsm_path_colorgroups == FALSE) {
      plot_lcsm(
        lavaan_object = uni_lavaan_results,
        lavaan_syntax = uni_lavaan_syntax,
        lcsm = "univariate",
        whatLabels = plot_specify_uni_lcsm_path_whatLabels
      )
    } else {
      plot_lcsm(
        lavaan_object = uni_lavaan_results,
        lavaan_syntax = uni_lavaan_syntax,
        lcsm = "univariate",
        whatLabels = plot_specify_uni_lcsm_path_whatLabels,
        groups = "latents",
        borders = FALSE
      )
    }
    
  })
  
  # Bivariate ----
  # Reactive environment to simulate data
  simulate_bi_lcsm <- reactive({
    sim_bi_gamma_lx1 <- input$sim_bi_gamma_lx1
    sim_bi_sigma2_lx1 <- input$sim_bi_sigma2_lx1
    sim_bi_sigma2_ux <- input$sim_bi_sigma2_ux
    sim_bi_beta_x <- input$sim_bi_beta_x
    sim_bi_alpha_g2 <- input$sim_bi_alpha_g2
    sim_bi_sigma2_g2 <- input$sim_bi_sigma2_g2
    sim_bi_sigma_g2lx1 <- input$sim_bi_sigma_g2lx1
    sim_bi_phi_x <- input$sim_bi_phi_x
    
    sim_bi_gamma_ly1 <- input$sim_bi_gamma_ly1
    sim_bi_sigma2_ly1 <- input$sim_bi_sigma2_ly1
    sim_bi_sigma2_uy <- input$sim_bi_sigma2_uy
    sim_bi_beta_y <- input$sim_bi_beta_y
    sim_bi_alpha_j2 <- input$sim_bi_alpha_j2
    sim_bi_sigma2_j2 <- input$sim_bi_sigma2_j2
    sim_bi_sigma_j2ly1 <- input$sim_bi_sigma_j2ly1
    sim_bi_phi_y <- input$sim_bi_phi_y
    
    sim_bi_sigma_su <- input$sim_bi_sigma_su
    sim_bi_sigma_ly1lx1 <- input$sim_bi_sigma_ly1lx1
    sim_bi_sigma_g2ly1 <- input$sim_bi_sigma_g2ly1
    sim_bi_sigma_j2lx1 <- input$sim_bi_sigma_j2lx1
    sim_bi_sigma_j2g2 <- input$sim_bi_sigma_j2g2
    
    sim_bi_delta_lag_xy <- input$sim_bi_delta_lag_xy
    sim_bi_delta_lag_yx <- input$sim_bi_delta_lag_yx
    sim_bi_xi_lag_xy <- input$sim_bi_xi_lag_xy
    sim_bi_xi_lag_yx <- input$sim_bi_xi_lag_yx
    
    # Construct X
    # set constant change parameter for simulating data
    if (base::is.na(sim_bi_alpha_g2) == TRUE |
        base::is.na(sim_bi_sigma2_g2) == TRUE |
        base::is.na(sim_bi_sigma_g2lx1) == TRUE) {
      sim_bi_model_alpha_constant_x <- FALSE
    } else {
      sim_bi_model_alpha_constant_x <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_bi_beta_x) == TRUE) {
      sim_bi_model_beta_x <- FALSE
    } else {
      sim_bi_model_beta_x <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_phi_x) == TRUE) {
      sim_bi_model_phi_x <- FALSE
    } else {
      sim_bi_model_phi_x <- TRUE
    }
    
    # Construct y
    # set constant change parameter for simulating data
    if (base::is.na(sim_bi_alpha_j2) == TRUE |
        base::is.na(sim_bi_sigma2_j2) == TRUE |
        base::is.na(sim_bi_sigma_j2ly1) == TRUE) {
      sim_bi_model_alpha_constant_y <- FALSE
    } else {
      sim_bi_model_alpha_constant_y <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_bi_beta_y) == TRUE) {
      sim_bi_model_beta_y <- FALSE
    } else {
      sim_bi_model_beta_y <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_phi_y) == TRUE) {
      sim_bi_model_phi_y <- FALSE
    } else {
      sim_bi_model_phi_y <- TRUE
    }
    
    # Coupling
    # set constant change parameter for simulating data
    if (base::is.na(sim_bi_delta_lag_xy) == TRUE) {
      sim_bi_model_delta_lag_xy <- FALSE
    } else {
      sim_bi_model_delta_lag_xy <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_bi_delta_lag_yx) == TRUE) {
      sim_bi_model_delta_lag_yx <- FALSE
    } else {
      sim_bi_model_delta_lag_yx <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_xi_lag_yx) == TRUE) {
      sim_bi_model_xi_lag_yx <- FALSE
    } else {
      sim_bi_model_xi_lag_yx <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_xi_lag_xy) == TRUE) {
      sim_bi_model_xi_lag_xy <- FALSE
    } else {
      sim_bi_model_xi_lag_xy <- TRUE
    }
    
    sim_bi_lcsm(
      timepoints = input$sim_bi_timepoints,
      na_x_pct = input$sim_bi_na_x_pct / 100,
      na_y_pct = input$sim_bi_na_y_pct / 100,
      model_x = list(
        alpha_constant = sim_bi_model_alpha_constant_x,
        beta = sim_bi_model_beta_x,
        phi = sim_bi_model_phi_x
      ),
      model_x_param = list(
        gamma_lx1 = sim_bi_gamma_lx1,
        sigma2_lx1 = sim_bi_sigma2_lx1,
        sigma2_ux = sim_bi_sigma2_ux,
        alpha_g2 = sim_bi_alpha_g2,
        sigma2_g2 = sim_bi_sigma2_g2,
        sigma_g2lx1 = sim_bi_sigma_g2lx1,
        beta_x = sim_bi_beta_x,
        phi_x = sim_bi_phi_x
      ),
      model_y = list(
        alpha_constant = sim_bi_model_alpha_constant_y,
        beta = sim_bi_model_beta_y,
        phi = sim_bi_model_phi_y
      ),
      model_y_param = list(
        gamma_ly1 = sim_bi_gamma_ly1,
        sigma2_ly1 = sim_bi_sigma2_ly1,
        sigma2_uy = sim_bi_sigma2_uy,
        alpha_j2 = sim_bi_alpha_j2,
        sigma2_j2 = sim_bi_sigma2_j2,
        sigma_j2ly1 = sim_bi_sigma_j2ly1,
        beta_y = sim_bi_beta_y,
        phi_y = sim_bi_phi_y
      ),
      coupling = list(
        delta_lag_xy = sim_bi_model_delta_lag_xy,
        delta_lag_yx = sim_bi_model_delta_lag_yx,
        xi_lag_yx = sim_bi_model_xi_lag_yx,
        xi_lag_xy = sim_bi_model_xi_lag_xy
      ),
      coupling_param = list(
        sigma_su = sim_bi_sigma_su,
        sigma_ly1lx1 = sim_bi_sigma_ly1lx1,
        sigma_g2ly1 = sim_bi_sigma_g2ly1,
        sigma_j2lx1 = sim_bi_sigma_j2lx1,
        sigma_j2g2 = sim_bi_sigma_j2g2,
        
        delta_lag_xy = sim_bi_delta_lag_xy,
        delta_lag_yx = sim_bi_delta_lag_yx,
        xi_lag_xy = sim_bi_xi_lag_xy,
        xi_lag_yx = sim_bi_xi_lag_yx
      ),
      sample.nobs = input$sim_bi_samplesize
    )
  })
  
  # Downloadable csv of selected dataset ----
  output$download_bi_data <- downloadHandler(
    filename = function() {
      paste("bi_lcsm_sim_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(simulate_bi_lcsm(), file, row.names = FALSE)
    }
  )
  
  # Create data table
  output$datatable_sim_bi_lcsm <-
    DT::renderDataTable(
      DT::datatable(
        simulate_bi_lcsm(),
        options = list(searching = FALSE),
        rownames = FALSE
      ) %>%
        DT::formatRound(
          digits = 2,
          columns = 2:ncol(simulate_bi_lcsm())
        )
    )
  
  # lavaan syntax
  output$lavaan_sim_bi_lcsm <- renderText({
    # extract input variables
    sim_bi_gamma_lx1 <- input$sim_bi_gamma_lx1
    sim_bi_sigma2_lx1 <- input$sim_bi_sigma2_lx1
    sim_bi_sigma2_ux <- input$sim_bi_sigma2_ux
    sim_bi_beta_x <- input$sim_bi_beta_x
    sim_bi_alpha_g2 <- input$sim_bi_alpha_g2
    sim_bi_sigma2_g2 <- input$sim_bi_sigma2_g2
    sim_bi_sigma_g2lx1 <- input$sim_bi_sigma_g2lx1
    sim_bi_phi_x <- input$sim_bi_phi_x
    
    sim_bi_gamma_ly1 <- input$sim_bi_gamma_ly1
    sim_bi_sigma2_ly1 <- input$sim_bi_sigma2_ly1
    sim_bi_sigma2_uy <- input$sim_bi_sigma2_uy
    sim_bi_beta_y <- input$sim_bi_beta_y
    sim_bi_alpha_j2 <- input$sim_bi_alpha_j2
    sim_bi_sigma2_j2 <- input$sim_bi_sigma2_j2
    sim_bi_sigma_j2ly1 <- input$sim_bi_sigma_j2ly1
    sim_bi_phi_y <- input$sim_bi_phi_y
    
    sim_bi_sigma_su <- input$sim_bi_sigma_su
    sim_bi_sigma_ly1lx1 <- input$sim_bi_sigma_ly1lx1
    sim_bi_sigma_g2ly1 <- input$sim_bi_sigma_g2ly1
    sim_bi_sigma_j2lx1 <- input$sim_bi_sigma_j2lx1
    sim_bi_sigma_j2g2 <- input$sim_bi_sigma_j2g2
    
    sim_bi_delta_lag_xy <- input$sim_bi_delta_lag_xy
    sim_bi_delta_lag_yx <- input$sim_bi_delta_lag_yx
    sim_bi_xi_lag_xy <- input$sim_bi_xi_lag_xy
    sim_bi_xi_lag_yx <- input$sim_bi_xi_lag_yx
    
    # Construct X
    # set constant change parameter for simulating data
    if (base::is.na(sim_bi_alpha_g2) == TRUE |
        base::is.na(sim_bi_sigma2_g2) == TRUE |
        base::is.na(sim_bi_sigma_g2lx1) == TRUE) {
      sim_bi_model_alpha_constant_x <- FALSE
    } else {
      sim_bi_model_alpha_constant_x <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_bi_beta_x) == TRUE) {
      sim_bi_model_beta_x <- FALSE
    } else {
      sim_bi_model_beta_x <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_phi_x) == TRUE) {
      sim_bi_model_phi_x <- FALSE
    } else {
      sim_bi_model_phi_x <- TRUE
    }
    
    # Construct y
    # set constant change parameter for simulating data
    if (base::is.na(sim_bi_alpha_j2) == TRUE |
        base::is.na(sim_bi_sigma2_j2) == TRUE |
        base::is.na(sim_bi_sigma_j2ly1) == TRUE) {
      sim_bi_model_alpha_constant_y <- FALSE
    } else {
      sim_bi_model_alpha_constant_y <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_bi_beta_y) == TRUE) {
      sim_bi_model_beta_y <- FALSE
    } else {
      sim_bi_model_beta_y <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_phi_y) == TRUE) {
      sim_bi_model_phi_y <- FALSE
    } else {
      sim_bi_model_phi_y <- TRUE
    }
    
    # Coupling
    # set constant change parameter for simulating data
    if (base::is.na(sim_bi_delta_lag_xy) == TRUE) {
      sim_bi_model_delta_lag_xy <- FALSE
    } else {
      sim_bi_model_delta_lag_xy <- TRUE
    }
    
    # set beta parameter for simulating data
    if (base::is.na(sim_bi_delta_lag_yx) == TRUE) {
      sim_bi_model_delta_lag_yx <- FALSE
    } else {
      sim_bi_model_delta_lag_yx <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_xi_lag_yx) == TRUE) {
      sim_bi_model_xi_lag_yx <- FALSE
    } else {
      sim_bi_model_xi_lag_yx <- TRUE
    }
    
    # set phi parameter for simulating data
    if (base::is.na(sim_bi_xi_lag_xy) == TRUE) {
      sim_bi_model_xi_lag_xy <- FALSE
    } else {
      sim_bi_model_xi_lag_xy <- TRUE
    }
    
    # create lavaan syntax
    sim_bi_lcsm(
      timepoints = input$sim_bi_timepoints,
      return_lavaan_syntax = TRUE,
      return_lavaan_syntax_string = TRUE,
      na_x_pct = input$sim_bi_na_x_pct / 100,
      na_y_pct = input$sim_bi_na_y_pct / 100,
      model_x = list(
        alpha_constant = sim_bi_model_alpha_constant_x,
        beta = sim_bi_model_beta_x,
        phi = sim_bi_model_phi_x
      ),
      model_x_param = list(
        gamma_lx1 = sim_bi_gamma_lx1,
        sigma2_lx1 = sim_bi_sigma2_lx1,
        sigma2_ux = sim_bi_sigma2_ux,
        alpha_g2 = sim_bi_alpha_g2,
        sigma2_g2 = sim_bi_sigma2_g2,
        sigma_g2lx1 = sim_bi_sigma_g2lx1,
        beta_x = sim_bi_beta_x,
        phi_x = sim_bi_phi_x
      ),
      model_y = list(
        alpha_constant = sim_bi_model_alpha_constant_y,
        beta = sim_bi_model_beta_y,
        phi = sim_bi_model_phi_y
      ),
      model_y_param = list(
        gamma_ly1 = sim_bi_gamma_ly1,
        sigma2_ly1 = sim_bi_sigma2_ly1,
        sigma2_uy = sim_bi_sigma2_uy,
        alpha_j2 = sim_bi_alpha_j2,
        sigma2_j2 = sim_bi_sigma2_j2,
        sigma_j2ly1 = sim_bi_sigma_j2ly1,
        beta_y = sim_bi_beta_y,
        phi_y = sim_bi_phi_y
      ),
      coupling = list(
        delta_lag_xy = sim_bi_model_delta_lag_xy,
        delta_lag_yx = sim_bi_model_delta_lag_yx,
        xi_lag_yx = sim_bi_model_xi_lag_yx,
        xi_lag_xy = sim_bi_model_xi_lag_xy
      ),
      coupling_param = list(
        sigma_su = sim_bi_sigma_su,
        sigma_ly1lx1 = sim_bi_sigma_ly1lx1,
        sigma_g2ly1 = sim_bi_sigma_g2ly1,
        sigma_j2lx1 = sim_bi_sigma_j2lx1,
        sigma_j2g2 = sim_bi_sigma_j2g2,
        
        delta_lag_xy = sim_bi_delta_lag_xy,
        delta_lag_yx = sim_bi_delta_lag_yx,
        xi_lag_xy = sim_bi_xi_lag_xy,
        xi_lag_yx = sim_bi_xi_lag_yx
      ),
      sample.nobs = input$sim_bi_samplesize
    )
  })
  
  # Longitudinal plots
  
  # Create plot for x
  output$plot_sim_bi_lcsm <- renderPlot({
    plot_x <- simulate_bi_lcsm() %>%
      plot_trajectories(
        id_var = names(simulate_bi_lcsm())[1],
        var_list = names(simulate_bi_lcsm())[2:(input$sim_bi_timepoints + 1)],
        xlab = "Time",
        ylab = "Construct X",
        connect_missing = FALSE,
        random_sample_frac = 1
      ) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 16, face = "bold"))
    
    # Create plot for y
    plot_y <- simulate_bi_lcsm() %>%
      plot_trajectories(
        id_var = names(simulate_bi_lcsm())[1],
        var_list = names(simulate_bi_lcsm())[(input$sim_bi_timepoints + 2):ncol(simulate_bi_lcsm())],
        xlab = "Time",
        ylab = "Construct Y",
        connect_missing = FALSE,
        random_sample_frac = 1
      ) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 16, face = "bold"))
    
    # Combine plots
    ggpubr::ggarrange(
      plot_x,
      plot_y,
      labels = c("a", "b"),
      ncol = 1,
      nrow = 2
    )
  })
  
  
  # Path diagram ----
  output$plot_sim_bi_lcsm_path <- renderPlot({
    withProgress(message = "Making plot", value = 0, {
      # extract input variables
      sim_bi_gamma_lx1 <- input$sim_bi_gamma_lx1
      sim_bi_sigma2_lx1 <- input$sim_bi_sigma2_lx1
      sim_bi_sigma2_ux <- input$sim_bi_sigma2_ux
      sim_bi_beta_x <- input$sim_bi_beta_x
      sim_bi_alpha_g2 <- input$sim_bi_alpha_g2
      sim_bi_sigma2_g2 <- input$sim_bi_sigma2_g2
      sim_bi_sigma_g2lx1 <- input$sim_bi_sigma_g2lx1
      sim_bi_phi_x <- input$sim_bi_phi_x
      
      sim_bi_gamma_ly1 <- input$sim_bi_gamma_ly1
      sim_bi_sigma2_ly1 <- input$sim_bi_sigma2_ly1
      sim_bi_sigma2_uy <- input$sim_bi_sigma2_uy
      sim_bi_beta_y <- input$sim_bi_beta_y
      sim_bi_alpha_j2 <- input$sim_bi_alpha_j2
      sim_bi_sigma2_j2 <- input$sim_bi_sigma2_j2
      sim_bi_sigma_j2ly1 <- input$sim_bi_sigma_j2ly1
      sim_bi_phi_y <- input$sim_bi_phi_y
      
      sim_bi_sigma_su <- input$sim_bi_sigma_su
      sim_bi_sigma_ly1lx1 <- input$sim_bi_sigma_ly1lx1
      sim_bi_sigma_g2ly1 <- input$sim_bi_sigma_g2ly1
      sim_bi_sigma_j2lx1 <- input$sim_bi_sigma_j2lx1
      sim_bi_sigma_j2g2 <- input$sim_bi_sigma_j2g2
      
      sim_bi_delta_lag_xy <- input$sim_bi_delta_lag_xy
      sim_bi_delta_lag_yx <- input$sim_bi_delta_lag_yx
      sim_bi_xi_lag_xy <- input$sim_bi_xi_lag_xy
      sim_bi_xi_lag_yx <- input$sim_bi_xi_lag_yx
      
      # Construct X
      # set constant change parameter for simulating data
      if (base::is.na(sim_bi_alpha_g2) == TRUE |
          base::is.na(sim_bi_sigma2_g2) == TRUE |
          base::is.na(sim_bi_sigma_g2lx1) == TRUE) {
        sim_bi_model_alpha_constant_x <- FALSE
      } else {
        sim_bi_model_alpha_constant_x <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_bi_beta_x) == TRUE) {
        sim_bi_model_beta_x <- FALSE
      } else {
        sim_bi_model_beta_x <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_phi_x) == TRUE) {
        sim_bi_model_phi_x <- FALSE
      } else {
        sim_bi_model_phi_x <- TRUE
      }
      
      # Construct y
      # set constant change parameter for simulating data
      if (base::is.na(sim_bi_alpha_j2) == TRUE |
          base::is.na(sim_bi_sigma2_j2) == TRUE |
          base::is.na(sim_bi_sigma_j2ly1) == TRUE) {
        sim_bi_model_alpha_constant_y <- FALSE
      } else {
        sim_bi_model_alpha_constant_y <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_bi_beta_y) == TRUE) {
        sim_bi_model_beta_y <- FALSE
      } else {
        sim_bi_model_beta_y <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_phi_y) == TRUE) {
        sim_bi_model_phi_y <- FALSE
      } else {
        sim_bi_model_phi_y <- TRUE
      }
      
      # Coupling
      # set constant change parameter for simulating data
      if (base::is.na(sim_bi_delta_lag_xy) == TRUE) {
        sim_bi_model_delta_lag_xy <- FALSE
      } else {
        sim_bi_model_delta_lag_xy <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_bi_delta_lag_yx) == TRUE) {
        sim_bi_model_delta_lag_yx <- FALSE
      } else {
        sim_bi_model_delta_lag_yx <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_xi_lag_yx) == TRUE) {
        sim_bi_model_xi_lag_yx <- FALSE
      } else {
        sim_bi_model_xi_lag_yx <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_xi_lag_xy) == TRUE) {
        sim_bi_model_xi_lag_xy <- FALSE
      } else {
        sim_bi_model_xi_lag_xy <- TRUE
      }
      
      incProgress(1 / 6)
      
      # Fit bivariate lcsm and save the results
      bi_lavaan_results <- fit_bi_lcsm(
        data = simulate_bi_lcsm(),
        var_x = names(simulate_bi_lcsm())[2:(input$sim_bi_timepoints + 1)],
        var_y = names(simulate_bi_lcsm())[(input$sim_bi_timepoints + 2):ncol(simulate_bi_lcsm())],
        model_x = list(
          alpha_constant = sim_bi_model_alpha_constant_x,
          beta = sim_bi_model_beta_x,
          phi = sim_bi_model_phi_x
        ),
        model_y = list(
          alpha_constant = sim_bi_model_alpha_constant_y,
          beta = sim_bi_model_beta_y,
          phi = sim_bi_model_phi_y
        ),
        coupling = list(
          delta_lag_xy = sim_bi_model_delta_lag_xy,
          delta_lag_yx = sim_bi_model_delta_lag_yx,
          xi_lag_yx = sim_bi_model_xi_lag_yx,
          xi_lag_xy = sim_bi_model_xi_lag_xy
        )
      )
      
      incProgress(2 / 3)
      
      # Save the lavaan syntax that was used to create the layout matrix for semPlot
      bi_lavaan_syntax <- fit_bi_lcsm(
        data = simulate_bi_lcsm(),
        var_x = names(simulate_bi_lcsm())[2:(input$sim_bi_timepoints + 1)],
        var_y = names(simulate_bi_lcsm())[(input$sim_bi_timepoints + 2):ncol(simulate_bi_lcsm())],
        model_x = list(
          alpha_constant = sim_bi_model_alpha_constant_x,
          beta = sim_bi_model_beta_x,
          phi = sim_bi_model_phi_x
        ),
        model_y = list(
          alpha_constant = sim_bi_model_alpha_constant_y,
          beta = sim_bi_model_beta_y,
          phi = sim_bi_model_phi_y
        ),
        coupling = list(
          delta_lag_xy = sim_bi_model_delta_lag_xy,
          delta_lag_yx = sim_bi_model_delta_lag_yx,
          xi_lag_yx = sim_bi_model_xi_lag_yx,
          xi_lag_xy = sim_bi_model_xi_lag_xy
        ),
        return_lavaan_syntax = TRUE,
        return_lavaan_syntax_string = TRUE
      )
      
      incProgress(3 / 3)
    })
    
    if (input$plot_sim_bi_lcsm_path_whatLabels == FALSE) {
      plot_sim_bi_lcsm_path_whatLabels <- "invisible"
    } else {
      plot_sim_bi_lcsm_path_whatLabels <- "label"
    }
    
    
    if (input$plot_sim_bi_lcsm_path_colorgroups == FALSE) {
      plot_lcsm(
        lavaan_object = bi_lavaan_results,
        lavaan_syntax = bi_lavaan_syntax,
        lcsm = "bivariate",
        whatLabels = plot_sim_bi_lcsm_path_whatLabels
      )
    } else {
      plot_lcsm(
        lavaan_object = bi_lavaan_results,
        lavaan_syntax = bi_lavaan_syntax,
        lcsm = "bivariate",
        whatLabels = plot_sim_bi_lcsm_path_whatLabels,
        groups = "latents",
        borders = FALSE
      )
    }
  })
  
  
  
  
  # Path diagram ----
  output$plot_specify_bi_lcsm_path <- renderPlot({
    withProgress(message = "Making plot", value = 0, {
      # extract input variables
      sim_bi_gamma_lx1 <- input$sim_bi_gamma_lx1
      sim_bi_sigma2_lx1 <- input$sim_bi_sigma2_lx1
      sim_bi_sigma2_ux <- input$sim_bi_sigma2_ux
      sim_bi_beta_x <- input$sim_bi_beta_x
      sim_bi_alpha_g2 <- input$sim_bi_alpha_g2
      sim_bi_sigma2_g2 <- input$sim_bi_sigma2_g2
      sim_bi_sigma_g2lx1 <- input$sim_bi_sigma_g2lx1
      sim_bi_phi_x <- input$sim_bi_phi_x
      
      sim_bi_gamma_ly1 <- input$sim_bi_gamma_ly1
      sim_bi_sigma2_ly1 <- input$sim_bi_sigma2_ly1
      sim_bi_sigma2_uy <- input$sim_bi_sigma2_uy
      sim_bi_beta_y <- input$sim_bi_beta_y
      sim_bi_alpha_j2 <- input$sim_bi_alpha_j2
      sim_bi_sigma2_j2 <- input$sim_bi_sigma2_j2
      sim_bi_sigma_j2ly1 <- input$sim_bi_sigma_j2ly1
      sim_bi_phi_y <- input$sim_bi_phi_y
      
      sim_bi_sigma_su <- input$sim_bi_sigma_su
      sim_bi_sigma_ly1lx1 <- input$sim_bi_sigma_ly1lx1
      sim_bi_sigma_g2ly1 <- input$sim_bi_sigma_g2ly1
      sim_bi_sigma_j2lx1 <- input$sim_bi_sigma_j2lx1
      sim_bi_sigma_j2g2 <- input$sim_bi_sigma_j2g2
      
      sim_bi_delta_lag_xy <- input$sim_bi_delta_lag_xy
      sim_bi_delta_lag_yx <- input$sim_bi_delta_lag_yx
      sim_bi_xi_lag_xy <- input$sim_bi_xi_lag_xy
      sim_bi_xi_lag_yx <- input$sim_bi_xi_lag_yx
      
      # Construct X
      # set constant change parameter for simulating data
      if (base::is.na(sim_bi_alpha_g2) == TRUE |
          base::is.na(sim_bi_sigma2_g2) == TRUE |
          base::is.na(sim_bi_sigma_g2lx1) == TRUE) {
        sim_bi_model_alpha_constant_x <- FALSE
      } else {
        sim_bi_model_alpha_constant_x <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_bi_beta_x) == TRUE) {
        sim_bi_model_beta_x <- FALSE
      } else {
        sim_bi_model_beta_x <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_phi_x) == TRUE) {
        sim_bi_model_phi_x <- FALSE
      } else {
        sim_bi_model_phi_x <- TRUE
      }
      
      # Construct y
      # set constant change parameter for simulating data
      if (base::is.na(sim_bi_alpha_j2) == TRUE |
          base::is.na(sim_bi_sigma2_j2) == TRUE |
          base::is.na(sim_bi_sigma_j2ly1) == TRUE) {
        sim_bi_model_alpha_constant_y <- FALSE
      } else {
        sim_bi_model_alpha_constant_y <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_bi_beta_y) == TRUE) {
        sim_bi_model_beta_y <- FALSE
      } else {
        sim_bi_model_beta_y <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_phi_y) == TRUE) {
        sim_bi_model_phi_y <- FALSE
      } else {
        sim_bi_model_phi_y <- TRUE
      }
      
      # Coupling
      # set constant change parameter for simulating data
      if (base::is.na(sim_bi_delta_lag_xy) == TRUE) {
        sim_bi_model_delta_lag_xy <- FALSE
      } else {
        sim_bi_model_delta_lag_xy <- TRUE
      }
      
      # set beta parameter for simulating data
      if (base::is.na(sim_bi_delta_lag_yx) == TRUE) {
        sim_bi_model_delta_lag_yx <- FALSE
      } else {
        sim_bi_model_delta_lag_yx <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_xi_lag_yx) == TRUE) {
        sim_bi_model_xi_lag_yx <- FALSE
      } else {
        sim_bi_model_xi_lag_yx <- TRUE
      }
      
      # set phi parameter for simulating data
      if (base::is.na(sim_bi_xi_lag_xy) == TRUE) {
        sim_bi_model_xi_lag_xy <- FALSE
      } else {
        sim_bi_model_xi_lag_xy <- TRUE
      }
      
      incProgress(1 / 6)
      
      # Simulate data here using the same parameters as in simulate bi tab
      simulate_bi_lcsm_path <- sim_bi_lcsm(
        timepoints = input$specify_bi_timepoints,
        model_x = list(
          alpha_constant = sim_bi_model_alpha_constant_x,
          beta = sim_bi_model_beta_x,
          phi = sim_bi_model_phi_x
        ),
        model_x_param = list(
          gamma_lx1 = sim_bi_gamma_lx1,
          sigma2_lx1 = sim_bi_sigma2_lx1,
          sigma2_ux = sim_bi_sigma2_ux,
          alpha_g2 = sim_bi_alpha_g2,
          sigma2_g2 = sim_bi_sigma2_g2,
          sigma_g2lx1 = sim_bi_sigma_g2lx1,
          beta_x = sim_bi_beta_x,
          phi_x = sim_bi_phi_x
        ),
        model_y = list(
          alpha_constant = sim_bi_model_alpha_constant_y,
          beta = sim_bi_model_beta_y,
          phi = sim_bi_model_phi_y
        ),
        model_y_param = list(
          gamma_ly1 = sim_bi_gamma_ly1,
          sigma2_ly1 = sim_bi_sigma2_ly1,
          sigma2_uy = sim_bi_sigma2_uy,
          alpha_j2 = sim_bi_alpha_j2,
          sigma2_j2 = sim_bi_sigma2_j2,
          sigma_j2ly1 = sim_bi_sigma_j2ly1,
          beta_y = sim_bi_beta_y,
          phi_y = sim_bi_phi_y
        ),
        coupling = list(
          delta_lag_xy = sim_bi_model_delta_lag_xy,
          delta_lag_yx = sim_bi_model_delta_lag_yx,
          xi_lag_yx = sim_bi_model_xi_lag_yx,
          xi_lag_xy = sim_bi_model_xi_lag_xy
        ),
        coupling_param = list(
          sigma_su = sim_bi_sigma_su,
          sigma_ly1lx1 = sim_bi_sigma_ly1lx1,
          sigma_g2ly1 = sim_bi_sigma_g2ly1,
          sigma_j2lx1 = sim_bi_sigma_j2lx1,
          sigma_j2g2 = sim_bi_sigma_j2g2,
          
          delta_lag_xy = sim_bi_delta_lag_xy,
          delta_lag_yx = sim_bi_delta_lag_yx,
          xi_lag_xy = sim_bi_xi_lag_xy,
          xi_lag_yx = sim_bi_xi_lag_yx
        ),
        sample.nobs = input$sim_bi_samplesize
      )
      
      specify_bi_param_x <- input$specify_bi_param_x
      # alpha_constant_x
      if ("alpha_constant_x" %in% specify_bi_param_x) {
        alpha_constant_x <- TRUE
      } else {
        alpha_constant_x <- FALSE
      }
      # beta_x
      if ("beta_x" %in% specify_bi_param_x) {
        beta_x <- TRUE
      } else {
        beta_x <- FALSE
      }
      # phi_x
      if ("phi_x" %in% specify_bi_param_x) {
        phi_x <- TRUE
      } else {
        phi_x <- FALSE
      }
      
      specify_bi_param_y <- input$specify_bi_param_y
      # alpha_constant_y
      if ("alpha_constant_y" %in% specify_bi_param_y) {
        alpha_constant_y <- TRUE
      } else {
        alpha_constant_y <- FALSE
      }
      # beta_y
      if ("beta_y" %in% specify_bi_param_y) {
        beta_y <- TRUE
      } else {
        beta_y <- FALSE
      }
      # phi_x
      if ("phi_y" %in% specify_bi_param_y) {
        phi_y <- TRUE
      } else {
        phi_y <- FALSE
      }
      
      specify_bi_param_coupling <- input$specify_bi_param_coupling
      # delta_con_xy
      if ("delta_con_xy" %in% specify_bi_param_coupling) {
        delta_con_xy <- TRUE
      } else {
        delta_con_xy <- FALSE
      }
      # delta_con_yx
      if ("delta_con_yx" %in% specify_bi_param_coupling) {
        delta_con_yx <- TRUE
      } else {
        delta_con_yx <- FALSE
      }
      # xi_con_xy
      if ("xi_con_xy" %in% specify_bi_param_coupling) {
        xi_con_xy <- TRUE
      } else {
        xi_con_xy <- FALSE
      }
      # xi_con_yx
      if ("xi_con_yx" %in% specify_bi_param_coupling) {
        xi_con_yx <- TRUE
      } else {
        xi_con_yx <- FALSE
      }
      
      # delta_lag_xy
      if ("delta_lag_xy" %in% specify_bi_param_coupling) {
        delta_lag_xy <- TRUE
      } else {
        delta_lag_xy <- FALSE
      }
      # delta_lag_yx
      if ("delta_lag_yx" %in% specify_bi_param_coupling) {
        delta_lag_yx <- TRUE
      } else {
        delta_lag_yx <- FALSE
      }
      # xi_lag_xy
      if ("xi_lag_xy" %in% specify_bi_param_coupling) {
        xi_lag_xy <- TRUE
      } else {
        xi_lag_xy <- FALSE
      }
      # xi_lag_yx
      if ("xi_lag_yx" %in% specify_bi_param_coupling) {
        xi_lag_yx <- TRUE
      } else {
        xi_lag_yx <- FALSE
      }
      
      
      # Fit bivariate lcsm and save the results
      bi_lavaan_results <- fit_bi_lcsm(
        data = simulate_bi_lcsm_path,
        var_x = names(simulate_bi_lcsm_path)[2:(input$specify_bi_timepoints + 1)],
        var_y = names(simulate_bi_lcsm_path)[(input$specify_bi_timepoints + 2):ncol(simulate_bi_lcsm_path)],
        model_x = list(
          alpha_constant = alpha_constant_x,
          beta = beta_x,
          phi = phi_x
        ),
        model_y = list(
          alpha_constant = alpha_constant_y,
          beta = beta_y,
          phi = phi_y
        ),
        coupling = list(
          delta_con_xy = delta_con_xy,
          delta_con_yx = delta_con_yx,
          delta_lag_xy = delta_lag_xy,
          delta_lag_yx = delta_lag_yx,
          xi_con_yx = xi_con_yx,
          xi_con_xy = xi_con_xy,
          xi_lag_yx = xi_lag_yx,
          xi_lag_xy = xi_lag_xy
        )
      )
      
      incProgress(2 / 3)
      
      # Save the lavaan syntax that was used to create the layout matrix for semPlot
      bi_lavaan_syntax <- fit_bi_lcsm(
        data = simulate_bi_lcsm_path,
        var_x = names(simulate_bi_lcsm_path)[2:(input$specify_bi_timepoints + 1)],
        var_y = names(simulate_bi_lcsm_path)[(input$specify_bi_timepoints + 2):ncol(simulate_bi_lcsm_path)],
        model_x = list(
          alpha_constant = alpha_constant_x,
          beta = beta_x,
          phi = phi_x
        ),
        model_y = list(
          alpha_constant = alpha_constant_y,
          beta = beta_y,
          phi = phi_y
        ),
        coupling = list(
          delta_con_xy = delta_con_xy,
          delta_con_yx = delta_con_yx,
          delta_lag_xy = delta_lag_xy,
          delta_lag_yx = delta_lag_yx,
          xi_con_yx = xi_con_yx,
          xi_con_xy = xi_con_xy,
          xi_lag_yx = xi_lag_yx,
          xi_lag_xy = xi_lag_xy
        ),
        return_lavaan_syntax = TRUE,
        return_lavaan_syntax_string = TRUE
      )
      
      incProgress(3 / 3)
    })
    
    if (input$plot_specify_bi_lcsm_path_whatLabels == FALSE) {
      plot_specify_bi_lcsm_path_whatLabels <- "invisible"
    } else {
      plot_specify_bi_lcsm_path_whatLabels <- "label"
    }
    
    if (input$plot_specify_bi_lcsm_path_colorgroups == FALSE) {
      plot_lcsm(
        lavaan_object = bi_lavaan_results,
        lavaan_syntax = bi_lavaan_syntax,
        lcsm = "bivariate",
        whatLabels = plot_specify_bi_lcsm_path_whatLabels
      )
    } else {
      plot_lcsm(
        lavaan_object = bi_lavaan_results,
        lavaan_syntax = bi_lavaan_syntax,
        lcsm = "bivariate",
        whatLabels = plot_specify_bi_lcsm_path_whatLabels,
        groups = "latents",
        borders = FALSE
      )
    }
    
  })
  
  
  
  # Fit models ----
  # Univariate ----
  
  fit_uni_data <- reactive({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if (input$uni_sample_data_check == FALSE) {
      
      req(input$file1)
      
      read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      
    } else {
      lcsm::data_uni_lcsm
    }
    })
  
  output$contents <- DT::renderDataTable({
    
   df <-  fit_uni_data() %>% 
      select(input$uni_select_id, input$uni_select_vars)

    DT::datatable(df,
                  rownames = FALSE,
                  extensions = 'FixedColumns',
                  options = list(pageLength = 10,
                                 scrollX = TRUE,
                                 fixedColumns = TRUE)) %>%
      DT::formatRound(digits = 3, columns = 2:ncol(df)
      )
  })
  
  # Longitudinal plot ----
  
  output$plot_fit_uni_lcsm <- renderPlot({
     fit_uni_data() %>% 
    plot_trajectories(.,
      id_var = input$uni_select_id,
      var_list = input$uni_select_vars,
      xlab = "Time",
      ylab = "Construct X",
      connect_missing = FALSE,
      random_sample_frac = 1
    ) +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16, face = "bold"))
  })
  
  output$lavaan_fit_uni_lcsm <- renderText({
    fit_uni_param <- input$fit_uni_param
    # alpha_constant
    if ("alpha_constant" %in% fit_uni_param) {
      alpha_constant <- TRUE
    } else {
      alpha_constant <- FALSE
    }
    # beta
    if ("beta" %in% fit_uni_param) {
      beta <- TRUE
    } else {
      beta <- FALSE
    }
    # phi
    if ("phi" %in% fit_uni_param) {
      phi <- TRUE
    } else {
      phi <- FALSE
    }
    
    # Create lavaan syntax
    specify_uni_lcsm(
      timepoints = length(input$uni_select_vars),
      model = list(
        alpha_constant = alpha_constant,
        beta = beta,
        phi = phi
      ),
      var = "x",
      change_letter = "g"
    )
  })
  
  
  
  
  
  fit_uni_data_lcsm <- reactive({ 
    
    fit_uni_param <- input$fit_uni_param
    # alpha_constant
    if ("alpha_constant" %in% fit_uni_param) {
      alpha_constant <- TRUE
    } else {
      alpha_constant <- FALSE
    }
    # beta
    if ("beta" %in% fit_uni_param) {
      beta <- TRUE
    } else {
      beta <- FALSE
    }
    # phi
    if ("phi" %in% fit_uni_param) {
      phi <- TRUE
    } else {
      phi <- FALSE
    }

    fit_uni_data() %>% 
    fit_uni_lcsm(data = ., 
                 var =  input$uni_select_vars,
                 model = list(alpha_constant = alpha_constant, 
                              beta = beta, 
                              phi = phi))
  })
  
  
  
  # parameter table ----
  output$fit_uni_lcsm_param <- DT::renderDataTable({
    
    withProgress(message = "Extracting parameters", value = 0, {
      incProgress(1 / 6)
    df <- extract_param(fit_uni_data_lcsm())[ , 1:7]
    incProgress(6 / 6)
    }) 
    DT::datatable(df,
                  rownames = FALSE,
                  extensions = 'FixedColumns',
                  options = list(pageLength = 10,
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 searching = FALSE,
                                 dom = 'ft'))%>%
      DT::formatRound(digits = 3, columns = 2:ncol(df)
      )
  })
  
  
  # fit table ----
  output$fit_uni_lcsm_fit_stats <- DT::renderDataTable({
    
    withProgress(message = "Extracting fit statistics", value = 0, {
    incProgress(1 / 6)
    df <- extract_fit(fit_uni_data_lcsm(), details = input$fit_uni_lcsm_fit_stats_details)[-1]
    incProgress(6 / 6)
    }) 
    DT::datatable(df,
                  rownames = FALSE,
                  extensions = 'FixedColumns',
                  options = list(pageLength = 10,
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 searching = FALSE,
                                 dom = 'ft'))%>%
      DT::formatRound(digits = 3, columns = 2:ncol(df)
      )

    })
  
  # Path diagram ----
  output$plot_fit_uni_lcsm_path <- renderPlot({
    withProgress(message = "Making plot", value = 0, {
     
      fit_uni_param <- input$fit_uni_param
      # alpha_constant
      if ("alpha_constant" %in% fit_uni_param) {
        alpha_constant <- TRUE
      } else {
        alpha_constant <- FALSE
      }
      # beta
      if ("beta" %in% fit_uni_param) {
        beta <- TRUE
      } else {
        beta <- FALSE
      }
      # phi
      if ("phi" %in% fit_uni_param) {
        phi <- TRUE
      } else {
        phi <- FALSE
      }
      
      incProgress(1 / 6)
      
      uni_lavaan_results <- fit_uni_data() %>% 
        fit_uni_lcsm(data = ., 
                     var =  input$uni_select_vars,
                     model = list(alpha_constant = alpha_constant, 
                                  beta = beta, 
                                  phi = phi))
      
      incProgress(2 / 3)
      
      uni_lavaan_syntax <- fit_uni_data() %>% 
        fit_uni_lcsm(data = ., 
                     var =  input$uni_select_vars,
                     model = list(alpha_constant = alpha_constant, 
                                  beta = beta, 
                                  phi = phi),
                     return_lavaan_syntax = TRUE,
                     return_lavaan_syntax_string = TRUE)

      incProgress(3 / 3)
      
      if (input$plot_fit_uni_lcsm_path_whatLabels == FALSE) {
        plot_fit_uni_lcsm_path_whatLabels <- "invisible"
      } else {
        plot_fit_uni_lcsm_path_whatLabels <- "label"
      }
      
      if (input$plot_fit_uni_lcsm_path_colorgroups == FALSE) {
        plot_lcsm(
          lavaan_object = uni_lavaan_results,
          lavaan_syntax = uni_lavaan_syntax,
          lcsm = "univariate",
          whatLabels = plot_fit_uni_lcsm_path_whatLabels
        )
      } else {
        plot_lcsm(
          lavaan_object = uni_lavaan_results,
          lavaan_syntax = uni_lavaan_syntax,
          lcsm = "univariate",
          whatLabels = plot_fit_uni_lcsm_path_whatLabels,
          groups = "latents",
          borders = FALSE
        )
      }
      
    })
  })

  
  
  
  # OBSERVE -----
  # I got errors when these ovserve were further up so I'll leave them down here for now
  
  observe({

      
      x <- names(fit_uni_data())
      
      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- character(0)
      
      # Can also set the label and select items
      updateSelectInput(session, "uni_select_vars",
                        # label = paste("Select input label", length(x)),
                        choices = x,
                        selected = x[2]
      )

  })
  
  observe({
    x <- names(fit_uni_data())
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "uni_select_id",
                      # label = paste("Select input label", length(x)),
                      choices = x,
                      selected = x[1]
    )
  })
  
  
  # update time points based on number of vars selected ... doesnt really work at the moment, dont knw why
  # observe({
  #   updateNumericInput(session, "fit_uni_timepoints",
  #                      value = length(names(fit_uni_data())) - 1
  #                      )
  # 
  # })
  
  
  
  
  
  
  
  
  
  
}






shinyApp(ui = ui, server = server)
