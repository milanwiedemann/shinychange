library(shiny)
library(lcsm)
library(lavaan)
library(tidyverse)
library(ggpubr)
library(DT)

# ui ----
ui <-  tagList(
    navbarPage("lcsm", collapsable = FALSE,
               
      # Overview ----
      tabPanel("Overview",
               includeMarkdown("INCLUDEME.md")),

      # Simulate univariate LCSM ----
      navbarMenu("Simulate Data", 
      tabPanel("Univariate LCSM",
               column(width = 4, h4("Options:"),
                      tabsetPanel(
                        tabPanel("Data Characteristics",
                                 wellPanel(
                                 numericInput("sim_uni_timepoints", "Measurement Points:", value = 10),
                                 numericInput("sim_uni_samplesize", "Sample Size:", value = 500),
                                 sliderInput("sim_uni_na_x_pct", "Missingness:", min = 0, max = 100, value = 0, step = 1),
                                 # actionButton("simulate_action", "Simulate Data", class = "btn-primary")
                                 )
                                 ),
                        tabPanel("Parameters",
                                 wellPanel(
                                   numericInput("sim_uni_gamma_lx1", "gamma_lx1", value = 0, step = .1),
                                   numericInput("sim_uni_sigma2_lx1", "sigma2_lx1", value = .5, step = .1),
                                   numericInput("sim_uni_sigma2_ux", "sigma2_ux", value = .2, step = .1),
                                   numericInput("sim_uni_beta", "beta", value = -.1, step = .1),
                                   numericInput("sim_uni_alpha_g2", "alpha_g2", value = -.4, step = .1),
                                   numericInput("sim_uni_sigma2_g2", "sigma2_g2", value = .4, step = .1),
                                   numericInput("sim_uni_sigma_g2lx1", "sigma_g2lx1", value = .2, step = .1),
                                   numericInput("sim_uni_phi", "phi", value = NA, step = .1)
                                 )
                                 ),
                        tabPanel("Help",
                                 includeMarkdown("INCLUDEME_UNI.md"))

          )),
          column(width = 8, h4("Results:"),
          tabsetPanel(

            tabPanel("Simulated Data",
                     DT::dataTableOutput('datatable_sim_uni_lcsm')),
            downloadButton("download_bi_data", "Download"),
          
            tabPanel("Longitudinal Plot",
                     plotOutput("plot_sim_uni_lcsm")),
            tabPanel("lavaan Code",
                     verbatimTextOutput("lavaan_sim_uni_lcsm"))
            # tabPanel("Path diagram",
            #          "Not working yet.")
          )
        )
      ),
      
      # Simulate bivariate LCSM ----
      tabPanel("Bivariate LCSM",
               column(width = 4, h4("Options:"),
                      tabsetPanel(
                        tabPanel("Data Characteristics",
                                 wellPanel(
          numericInput("sim_bi_timepoints", "Measurement Points:", value = 6, min = 2),
          numericInput("sim_bi_samplesize", "Sample Size:", value = 500),
          sliderInput("sim_bi_na_x_pct", "Missingness Construct X:", min = 0, max = 100, value = 0, step = 1),
          sliderInput("sim_bi_na_y_pct", "Missingness Construct Y:", min = 0, max = 100, value = 0, step = 1),
          # actionButton("simulate_action", "Simulate data", class = "btn-primary")
                                 )
        ),
        tabPanel("Parameters",
                 tabsetPanel(
                   tabPanel("Construct X",
                            wellPanel(
                                      numericInput("sim_bi_gamma_lx1", "gamma_lx1", value = 0, step = .1),
                                      numericInput("sim_bi_sigma2_lx1", "sigma2_lx1", value = .5, step = .1),
                                      numericInput("sim_bi_sigma2_ux", "sigma2_ux", value = .2, step = .1),
                                      numericInput("sim_bi_beta_x", "beta_x", value = -.1, step = .1),
                                      numericInput("sim_bi_alpha_g2", "alpha_g2", value = -.4, step = .1),
                                      numericInput("sim_bi_sigma2_g2", "sigma2_g2", value = .4, step = .1),
                                      numericInput("sim_bi_sigma_g2lx1", "sigma_g2lx1", value = .2, step = .1),
                                      numericInput("sim_bi_phi_x", "phi_x", value = NA, step = .1)
                            )),
                   tabPanel("Construct Y",
                            wellPanel(
                                      numericInput("sim_bi_gamma_ly1", "gamma_ly1", value = 5, step = .1),
                                      numericInput("sim_bi_sigma2_ly1", "sigma2_ly1", value = .2, step = .1),
                                      numericInput("sim_bi_sigma2_uy", "sigma2_uy", value = .2, step = .1),
                                      numericInput("sim_bi_beta_y", "beta_y", value = -.2, step = .1),
                                      numericInput("sim_bi_alpha_j2", "alpha_j2", value = -.2, step = .1),
                                      numericInput("sim_bi_sigma2_j2", "sigma2_j2", value = .1, step = .1),
                                      numericInput("sim_bi_sigma_j2ly1", "sigma_j2ly1", value = .02, step = .1),
                                      numericInput("sim_bi_phi_y", "phi_y", value = .1, step = .1)
                            )),
                   tabPanel("Coupling",
                            wellPanel(
                                      numericInput("sim_bi_sigma_su", "sigma_su", value = .01, step = .1),
                                      numericInput("sim_bi_sigma_ly1lx1", "sigma_ly1lx1", value = .2, step = .1),
                                      numericInput("sim_bi_sigma_g2ly1", "sigma_g2ly1", value = .1, step = .1),
                                      numericInput("sim_bi_sigma_j2lx1", "sigma_j2lx1", value = .1, step = .1),
                                      numericInput("sim_bi_sigma_j2g2", "sigma_j2g2", value = .01, step = .1),
                                      numericInput("sim_bi_delta_lag_xy", "delta_lag_xy", value = .13, step = .1),
                                      numericInput("sim_bi_delta_lag_yx", "delta_lag_yx", value = NA, step = .1),
                                      numericInput("sim_bi_xi_lag_xy", "xi_lag_xy", value = NA, step = .1),
                                      numericInput("sim_bi_xi_lag_yx", "xi_lag_yx", value = .4, step = .1),
                            ))
                   )

        ),
        tabPanel("Help",
                 tabsetPanel(
                   tabPanel("Construct X",
                            includeMarkdown("INCLUDEME_BI_X.md")),
                   tabPanel("Construct Y",
                            includeMarkdown("INCLUDEME_BI_Y.md")),
                   tabPanel("Coupling",
                            includeMarkdown("INCLUDEME_BI_C.md"))
                   )
                 )
        )),
        column(width = 8, h4("Results:"),
               tabsetPanel(
            tabPanel("Simulated Data",
                     DT::dataTableOutput('datatable_sim_bi_lcsm'),
                     downloadButton("download_bi_data", "Download")),
            tabPanel("Longitudinal Plots",
                     plotOutput("plot_sim_bi_lcsm", width = 850, height = 550),
                     downloadButton('download_plot_sim_bi_lcsm', "Download")),
            tabPanel("lavaan Code",
                     verbatimTextOutput("lavaan_sim_bi_lcsm"))
            # tabPanel("Path diagram",
            #          "Not working yet.")
          )
        )
      )),
      # Fit univariate LCSM ----
      # navbarMenu("Fit Model",
      # tabPanel(
      #   "Univariate LCSM",
      #   sidebarPanel(
      #     fileInput("file", "Load data:"),
      #     actionButton("fit-uni-lcsm", "Fit model", class = "btn-primary")
      #   ),
      #   mainPanel(tabsetPanel(
      #     tabPanel("Estimated parameters",
      #              "This panel is intentionally left blank"),
      #     tabPanel("Fit statistics",
      #              "This panel is intentionally left blank"),
      #     tabPanel("Longitudinal plot",
      #              "This panel is intentionally left blank"),
      #     # tabPanel("Path diagram",
      #     #          "Not working yet."),
      #     tabPanel("lavaan Code",
      #              "This panel is intentionally left blank")
      #   ))
      # ),
      # # Fit bivariate LCSM ----
      # tabPanel("Bivariate LCSM",
      #          sidebarPanel(
      #            fileInput("file", "Load data:"),
      #            actionButton("fit-bi-lcsm", "Fit model", class = "btn-primary")
      #            ),
      #          mainPanel(tabsetPanel(
      #            tabPanel("Estimated parameters",
      #                     "This panel is intentionally left blank"),
      #            tabPanel("Fit statistics",
      #                     "This panel is intentionally left blank"),
      #            tabPanel("Longitudinal plot",
      #                     "This panel is intentionally left blank"),
      #            # tabPanel("Path diagram",
      #            #          "Not working yet."),
      #            tabPanel("lavaan Code",
      #                     "This panel is intentionally left blank")
      #            )
      #            )
      #          )
      # ),
      # Specify univariate LCSM ----
      navbarMenu("lavaan Syntax",
      tabPanel("Univariate LCSM",
               fluidPage(
                 fluidRow(
               column(width = 2, h4("Data Characteristics:"),
                      wellPanel(numericInput("specify_uni_timepoints", "Measurement Points:", value = 5, min = 2),
                                helpText("Note: Number of repeated measurement points.")),
                      wellPanel(textInput("specify_uni_var_name", "Variable Name:", value = "x"),
                                helpText("Note: The variable name should start with a letter.")),
                      ),
               column(3, h4("Select Parameters:"),
                      wellPanel(checkboxGroupInput("specify_uni_param",
                                                   label = "",
                                                   choices = list("alpha_constant" = "alpha_constant",
                                                                  "beta" = "beta",
                                                                  "phi" = "phi"),
                                                   selected = c("alpha_constant", "beta", "phi")),
                                helpText("Note: alpha_constant (Constant change factor); 
                                          beta (Proportional change factor);
                                          phi (Autoregression of change scores).")),
               ),
               column(6, h4("lavaan Syntax:"),
                      mainPanel(helpText("Note: lavaan syntax with comments for the selected data characteristics and model parameters. 
                                  This syntax can be modified by hand and used in the 'model' argument of the function 'lavaan::lavaan()'"),
                                verbatimTextOutput("lavaan_uni_lcsm")
                      )
               ))
               )),
      # Specify bivariate LCSM ----
      tabPanel("Bivariate LCSM",
               column(width = 2, h4("Data Characteristics:"),
                      wellPanel(numericInput("specify_bi_timepoints", "Measurement Points:", value = 5, min = 2),
                                helpText("Note: Number of repeated measurement points for each construct.")),
                      wellPanel(textInput("specify_bi_var_name_x", "Variable Name Construct X:", value = "x"),
                                helpText("Note: Variable name for construct X. The variable name should start with a letter.")),
                      wellPanel(textInput("specify_var_name_y", "Variable Name Construct Y:", value = "y"),
                                helpText("Note: Variable name for construct X. The variable name should start with a letter."))),
               column(3, h4("Select Parameters:"),
                      wellPanel(checkboxGroupInput("specify_bi_param_x",
                                                   label = "Construct X:",
                                                   choices = list("alpha_constant_x" = "alpha_constant_x",
                                                                  "beta_x" = "beta_x",
                                                                  "phi_x" = "phi_x"),
                                                   selected = c("alpha_constant_x", "beta_x", "phi_x")),
                                helpText("Note: alpha_constant_x (Constant change factor); 
                                          beta_x (Proportional change factor);
                                          phi_x (Autoregression of change scores).")),
                      wellPanel(checkboxGroupInput("specify_bi_param_y",
                                                   label = "Construct Y:",
                                                   choices = list("alpha_constant_y" = "alpha_constant_y",
                                                                  "beta_y" = "beta_y",
                                                                  "phi_y" = "phi_y"),
                                                   selected = c("alpha_constant_y", "beta_y", "phi_y")),
                                helpText("Note: alpha_constant_y (Constant change factor); 
                                          beta_y (Proportional change factor);
                                          phi_y (Autoregression of change scores).")),
                      wellPanel(checkboxGroupInput("specify_bi_param_coupling",
                                                   label = "Coupling:",
                                                   choices = list("delta_con_xy" = "delta_con_xy",
                                                                  "delta_con_yx" = "delta_con_yx",
                                                                  "delta_lag_xy" = "delta_lag_xy",
                                                                  "delta_lag_yx" = "delta_lag_yx",
                                                                  "xi_con_xy" = "xi_con_xy",
                                                                  "xi_con_yx" = "xi_con_yx",
                                                                  "xi_lag_xy" = "xi_lag_xy",
                                                                  "xi_lag_yx" = "xi_lag_yx")),
                                helpText("Note: delta_con_xy (True score y predicting concurrent change score x);
                                          delta_con_yx (True score x predicting concurrent change score y);
                                          delta_lag_xy (True score y predicting subsequent change score x);
                                          delta_lag_yx (True score x predicting subsequent change score y);
                                          xi_con_xy (Change score y predicting concurrent change score x);
                                          xi_con_yx (Change score x predicting concurrent change score y);
                                          xi_lag_xy (Change score y predicting subsequent change score x);
                                          xi_lag_yx (Change score x predicting subsequent change score y)."))
               ),
               column(6, h4("lavaan Syntax:"),
               mainPanel(helpText("Note: lavaan syntax with comments for the selected data characteristics and model parameters. 
                                  This syntax can be modified by hand and used in the 'model' argument of the function 'lavaan::lavaan()'"),
                 verbatimTextOutput("lavaan_bi_lcsm")
               )
               ))
      )
    )
  )
  

# server ----
server <-  function(input, output) {
  
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
   specify_uni_lcsm(timepoints = input$specify_uni_timepoints, 
                    model = list(alpha_constant = alpha_constant, 
                                 beta = beta, 
                                 phi = phi), 
                    var = input$specify_uni_var_name,  
                    change_letter = "g")
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

    specify_bi_lcsm(timepoints = input$specify_bi_timepoints, 
                    var_x = input$specify_bi_var_name_x,
                    model_x = list(alpha_constant = alpha_constant_x, 
                                   beta = beta_x, 
                                   phi = phi_x),
                    var_y = input$specify_var_name_y,  
                    model_y = list(alpha_constant = alpha_constant_y, 
                                   beta = beta_y, 
                                   phi = phi_y),  
                    coupling = list(delta_con_xy = delta_con_xy, 
                                    delta_con_yx = delta_con_yx,
                                    xi_con_xy = xi_con_xy, 
                                    xi_con_yx = xi_con_yx,
                                    delta_lag_xy = delta_lag_xy, 
                                    delta_lag_yx = delta_lag_yx,
                                    xi_lag_xy = xi_lag_xy, 
                                    xi_lag_yx = xi_lag_yx),
                    change_letter_x = "g",
                    change_letter_y = "j")
  
  })
  
 # Simulate data ----
  
  # Univariate ----
  
  # Reactive environment to simulate data
  simulate_uni_lcsm <- reactive({
    
    sim_uni_gamma_lx1 <- input$sim_uni_gamma_lx1
    sim_uni_sigma2_lx1 <- input$sim_uni_sigma2_lx1
    sim_uni_sigma2_ux <- input$sim_uni_sigma2_ux
    sim_uni_beta <- input$sim_uni_beta
    sim_uni_alpha_g2 <- input$sim_uni_alpha_g2
    sim_uni_sigma2_g2 <- input$sim_uni_sigma2_g2
    sim_uni_sigma_g2lx1 <- input$sim_uni_sigma_g2lx1
    sim_uni_phi <- input$sim_uni_phi
    
    sim_uni_lcsm(timepoints = input$sim_uni_timepoints, 
                 model = list(alpha_constant = TRUE, 
                              beta = TRUE, 
                              phi = TRUE), 
                 model_param = list(gamma_lx1 = sim_uni_gamma_lx1, 
                                    sigma2_lx1 = sim_uni_sigma2_lx1,
                                    sigma2_ux = sim_uni_sigma2_ux,
                                    alpha_g2 = sim_uni_alpha_g2,
                                    beta_x = sim_uni_beta,
                                    sigma2_g2 = sim_uni_sigma2_g2,
                                    sigma_g2lx1 = sim_uni_sigma_g2lx1,
                                    phi_x = sim_uni_phi),
                 sample.nobs = input$sim_uni_samplesize,
                 na_pct = input$sim_uni_na_x_pct / 100)
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
  output$datatable_sim_uni_lcsm <- DT::renderDataTable(
  
    DT::datatable(simulate_uni_lcsm()) %>% 
      DT::formatRound(digits = 2, columns = 2:ncol(simulate_uni_lcsm()))
  )
  
  # lavaan syntax
  output$lavaan_sim_uni_lcsm <- renderText({
    
    sim_uni_gamma_lx1 <- input$sim_uni_gamma_lx1
    sim_uni_sigma2_lx1 <- input$sim_uni_sigma2_lx1
    sim_uni_sigma2_ux <- input$sim_uni_sigma2_ux
    sim_uni_beta <- input$sim_uni_beta
    sim_uni_alpha_g2 <- input$sim_uni_alpha_g2
    sim_uni_sigma2_g2 <- input$sim_uni_sigma2_g2
    sim_uni_sigma_g2lx1 <- input$sim_uni_sigma_g2lx1
    sim_uni_phi <- input$sim_uni_phi
    
    sim_uni_lcsm(timepoints = input$sim_uni_timepoints, 
                 return_lavaan_syntax = TRUE,
                 return_lavaan_syntax_string = TRUE,
                 model = list(alpha_constant = TRUE, 
                              beta = TRUE, 
                              phi = TRUE), 
                 model_param = list(gamma_lx1 = sim_uni_gamma_lx1, 
                                    sigma2_lx1 = sim_uni_sigma2_lx1,
                                    sigma2_ux = sim_uni_sigma2_ux,
                                    alpha_g2 = sim_uni_alpha_g2,
                                    beta_x = sim_uni_beta,
                                    sigma2_g2 = sim_uni_sigma2_g2,
                                    sigma_g2lx1 = sim_uni_sigma_g2lx1,
                                    phi_x = sim_uni_phi),
                 sample.nobs = input$sim_uni_samplesize,
                 na_pct = input$sim_uni_na_x_pct / 100)
    
  })
    
  # Longitudinal plots
  output$plot_sim_uni_lcsm <- renderPlot({
    
    simulate_uni_lcsm() %>% 
      plot_trajectories(id_var = names(simulate_uni_lcsm())[1], 
                        var_list = names(simulate_uni_lcsm())[2:ncol(simulate_uni_lcsm())],
                        xlab = "Time", ylab = "Y Score",
                        connect_missing = FALSE, 
                        random_sample_frac = 1) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=16,face="bold"))
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
    
    sim_bi_lcsm(timepoints = input$sim_bi_timepoints,
                na_x_pct = input$sim_bi_na_x_pct / 100,
                na_y_pct = input$sim_bi_na_y_pct / 100,
                model_x = list(alpha_constant = TRUE, 
                               beta = TRUE, 
                               phi = FALSE),
                model_x_param = list(gamma_lx1 = sim_bi_gamma_lx1,
                                     sigma2_lx1 = sim_bi_sigma2_lx1,
                                     sigma2_ux = sim_bi_sigma2_ux,
                                     alpha_g2 = sim_bi_alpha_g2,
                                     sigma2_g2 = sim_bi_sigma2_g2,
                                     sigma_g2lx1 = sim_bi_sigma_g2lx1,
                                     beta_x = sim_bi_beta_x,
                                     phi_x = sim_bi_phi_x),
                model_y = list(alpha_constant = TRUE, 
                               beta = TRUE, 
                               phi = TRUE),
                model_y_param = list(gamma_ly1 = sim_bi_gamma_ly1,
                                     sigma2_ly1 = sim_bi_sigma2_ly1,
                                     sigma2_uy = sim_bi_sigma2_uy,
                                     alpha_j2 = -sim_bi_alpha_j2,
                                     sigma2_j2 = sim_bi_sigma2_j2,
                                     sigma_j2ly1 = sim_bi_sigma_j2ly1,
                                     beta_y = sim_bi_beta_y,
                                     phi_y = sim_bi_phi_y),
                coupling = list(delta_lag_xy = TRUE, 
                                delta_lag_yx = TRUE,
                                xi_lag_yx = TRUE,
                                xi_lag_xy = TRUE),
                coupling_param = list(sigma_su = sim_bi_sigma_su,
                                      sigma_ly1lx1 = sim_bi_sigma_ly1lx1,
                                      sigma_g2ly1 = sim_bi_sigma_g2ly1,
                                      sigma_j2lx1 = sim_bi_sigma_j2lx1,
                                      sigma_j2g2 = sim_bi_sigma_j2g2,
                                      
                                      delta_lag_xy = sim_bi_delta_lag_xy,
                                      delta_lag_yx = sim_bi_delta_lag_yx,
                                      xi_lag_xy = sim_bi_xi_lag_xy,
                                      xi_lag_yx = sim_bi_xi_lag_yx),
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
  output$datatable_sim_bi_lcsm <- DT::renderDataTable(
    
    DT::datatable(simulate_bi_lcsm()) %>% 
      DT::formatRound(digits = 2, columns = 2:ncol(simulate_bi_lcsm()))
  )
  
  # lavaan syntax
  output$lavaan_sim_bi_lcsm <- renderText({
    
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
    
    sim_bi_lcsm(timepoints = input$sim_bi_timepoints,
                return_lavaan_syntax = TRUE,
                return_lavaan_syntax_string = TRUE,
                na_x_pct = input$sim_bi_na_x_pct / 100,
                na_y_pct = input$sim_bi_na_y_pct / 100,
                model_x = list(alpha_constant = TRUE, 
                               beta = TRUE, 
                               phi = FALSE),
                model_x_param = list(gamma_lx1 = sim_bi_gamma_lx1,
                                     sigma2_lx1 = sim_bi_sigma2_lx1,
                                     sigma2_ux = sim_bi_sigma2_ux,
                                     alpha_g2 = sim_bi_alpha_g2,
                                     sigma2_g2 = sim_bi_sigma2_g2,
                                     sigma_g2lx1 = sim_bi_sigma_g2lx1,
                                     beta_x = sim_bi_beta_x,
                                     phi_x = sim_bi_phi_x),
                model_y = list(alpha_constant = TRUE, 
                               beta = TRUE, 
                               phi = TRUE),
                model_y_param = list(gamma_ly1 = sim_bi_gamma_ly1,
                                     sigma2_ly1 = sim_bi_sigma2_ly1,
                                     sigma2_uy = sim_bi_sigma2_uy,
                                     alpha_j2 = -sim_bi_alpha_j2,
                                     sigma2_j2 = sim_bi_sigma2_j2,
                                     sigma_j2ly1 = sim_bi_sigma_j2ly1,
                                     beta_y = sim_bi_beta_y,
                                     phi_y = sim_bi_phi_y),
                coupling = list(delta_lag_xy = TRUE, 
                                delta_lag_yx = TRUE,
                                xi_lag_yx = TRUE,
                                xi_lag_xy = TRUE),
                coupling_param = list(sigma_su = sim_bi_sigma_su,
                                      sigma_ly1lx1 = sim_bi_sigma_ly1lx1,
                                      sigma_g2ly1 = sim_bi_sigma_g2ly1,
                                      sigma_j2lx1 = sim_bi_sigma_j2lx1,
                                      sigma_j2g2 = sim_bi_sigma_j2g2,
                                      
                                      delta_lag_xy = sim_bi_delta_lag_xy,
                                      delta_lag_yx = sim_bi_delta_lag_yx,
                                      xi_lag_xy = sim_bi_xi_lag_xy,
                                      xi_lag_yx = sim_bi_xi_lag_yx),
                sample.nobs = input$sim_bi_samplesize
    )
  })
  
  # Longitudinal plots
  output$plot_sim_bi_lcsm <- renderPlot({
    
    plot_x <- simulate_bi_lcsm() %>% 
      plot_trajectories(id_var = names(simulate_bi_lcsm())[1], 
                        var_list = names(simulate_bi_lcsm())[2:(input$sim_bi_timepoints + 1)],
                        xlab = "Time", ylab = "Construct X",
                        connect_missing = FALSE, 
                        random_sample_frac = 1) +
      theme(axis.text=element_text(size = 16),
            axis.title=element_text(size = 16, face = "bold"))
    
    
    plot_y <- simulate_bi_lcsm() %>% 
      plot_trajectories(id_var = names(simulate_bi_lcsm())[1], 
                        var_list = names(simulate_bi_lcsm())[(input$sim_bi_timepoints + 2):ncol(simulate_bi_lcsm())],
                        xlab = "Time", ylab = "Construct Y",
                        connect_missing = FALSE, 
                        random_sample_frac = 1) +
      theme(axis.text=element_text(size = 16),
            axis.title=element_text(size = 16, face = "bold"))
    
    ggpubr::ggarrange(plot_x, plot_y, 
                      labels = c("a", "b"), 
                      ncol = 1, nrow = 2)
    
  })
  

}

shinyApp(ui = ui, server = server)
