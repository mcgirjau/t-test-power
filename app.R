library(dplyr)
library(ggplot2)
library(kableExtra)
library(shiny)
library(shinycssloaders)
library(shinyWidgets)

# Theme for ggplot2
theme_set(theme_bw()) 

# -------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  # Some customization
  tags$head(
    tags$style(
      HTML('#sidebar {
              color: white;
              background-color: #1b3752;
              transition: transform 0.3s;
              border-radius: 10px;
            }
            
            body, label, input, button, select { 
              font-family: "Arial";
            }
            
            button {
              left-margin: auto;
              right-margin: auto;
            }
           
            h1 h2 {
              color: #1b3752;
              font-family: "Verdana";
            }
           
            p {
              text-align: justify;
              font-size: 0.87em;
            }
            
            .inline label { 
              display: table-cell; 
              text-align: left; 
              vertical-align: middle; 
              table-layout: fixed;
              width: 200px;
            } 
            
            .inline .form-group { 
              display: table-row;
            }
            
            .inline .form-control {
              margin-top: 7px;
            }
            
            input {
              width: 250px;
              padding: 5px;
            }
           
           .shiny-output-error,
           .shiny-output-error:before { 
              visibility: hidden; 
           }')),
  
  tags$title("Power Shiny App")
  ),
  
  h1("Power of \\(t\\)-Test for Slope"),
  
  br(),
  
  # Sidebar layout with input and output
  sidebarLayout(
    
    # Inputs in the sidebar panel
    sidebarPanel(id = "sidebar", width = 3,
                 
                 h2("Parameters"),
                 
                 p(withMathJax("Estimate the power of a test of \\(H_0: \\beta_1 = 0\\) in a simple linear 
                               regression model, with the \\(x\\) values evenly spaced within some interval, 
                               and the linear model correctly specified.")),
                 
                 div(class = "inline",
                     # true intercept (beta_0)
                     numericInput(inputId = "beta_0", label = "True Intercept \\(\\beta_0\\):", 
                                  value = 2, min = -1000000, max = 1000000, step = 0.01),
                     
                     # true slope (beta_1)
                     numericInput(inputId = "beta_1", label = "True Slope \\(\\beta_1\\):", 
                                  value = 4, min = -1000000, max = 1000000, step = 0.01),
                     
                     # significance level
                     numericInput(inputId = "alpha", label = "Significance Level \\(\\alpha\\):", 
                                  value = 0.05, min = 0, max = 1, step = 0.01),
                     
                     # sample size
                     numericInput(inputId = "n", label = "Sample Size \\(n\\):", 
                                  value = 12, min = 1, max = 1000000, step = 1),
                     
                     # error standard deviation
                     numericInput(inputId = "sigma", label = "Error Standard Deviation \\(\\sigma_\\varepsilon\\):", 
                                  value = 1, min = 0, max = 1000, step = 0.01)),
                 
                 br(),
                 
                 # minimum and maximum x values
                 chooseSliderSkin("Simple"),
                 sliderInput(inputId = "x_range", label = "Interval for \\(x\\) Values:", 
                             min = -100, max = 100, value = c(0, 10)),
                 
                 br(),
                 
                 div(class = "inline",
                     # number of simulations
                     numericInput(inputId = "nsim", label = "Number of Trials:",
                                  value = 10000, min = 1, max = 1000000, step = 1),
                     
                     # seed
                     numericInput(inputId = "seed", label = "Random Seed:",
                                  value = 23, min = -1000000, max = 1000000, step = 1)),

                 br(),
                 
                 div(class = "text-center", actionButton(inputId = "go", label = "Run Simulation"))
    ),
    
    # Outputs in the main panel
    mainPanel(id = "main", 
              br(),
              h2("Simulation Results"),
              br(),
              withSpinner(plotOutput("plots"), size = 2, color = "#1b3752"),
              br(),
              htmlOutput("results"))
  )
)

# -------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  
  params <- eventReactive(input$go, {
    list(
      "beta_0" = input$beta_0, 
      "beta_1" = input$beta_1,
      "alpha" = input$alpha,
      "n" = input$n,
      "sigma" = input$sigma,
      "min" = input$x_range[1],
      "max" = input$x_range[2],
      "nsim" = input$nsim,
      "seed" = input$seed
    )
  }, ignoreNULL = FALSE)
  
  # run simulations and return collected p-value data
  simulated_data <- eventReactive(input$go, {
    
    set.seed(params()$seed)
    
    x <- seq(from = params()$min, to = params()$max, length = params()$n)
    
    # run simulations
    p_values <- replicate(params()$nsim, {
      err <- rnorm(params()$n, mean = 0, sd = params()$sigma) # OLS regression errors are normal and centered at 0
      y <- params()$beta_0 + params()$beta_1 * x + err # the linear model is correctly specified
      model <- lm(y ~ x) # fit OLS model
      p_value <- summary(model)$coefficients[2, 4] # extract the slope p-value from the model
      return(p_value)
    })
    
    return(p_values)
  
  }, ignoreNULL = FALSE)
  
  output$plots <- renderPlot({
    
    p_values <- simulated_data()
    data <- data.frame(p_values)
  
    p1 <- ggplot(data, aes(x = p_values)) +
      geom_density(fill = "#1b3752", alpha = 0.5) +
      geom_vline(xintercept = params()$alpha, color = "red", linetype = 2) +
      xlim(c(min(p_values) * 1.001, max(p_values) * 1.001)) +
      labs(x = "P-value", y = "Density", title = "Distribution of P-Values") +
      theme(text = element_text(size = 15))
    
    p2 <- ggplot(data, aes(x = p_values)) +
      geom_density(fill = "#1b3752", alpha = 0.5) +
      geom_vline(xintercept = params()$alpha, color = "red", linetype = 2) +
      scale_x_log10() +
      labs(x = "P-value (log 10)", y = "Density", title = "Distribution of P-Values (log 10)") +
      theme(text = element_text(size = 15))
    
    cowplot::plot_grid(p1, p2, nrow = 1)
  })
  
  output$results <- renderText({
    
    p_values <- simulated_data()
    num_rejections <- sum(p_values <= params()$alpha)
    power <- mean(p_values <= params()$alpha)
    confint <- round(binom.test(num_rejections, params()$nsim, conf.level = 1 - params()$alpha)$conf.int, 4)
    
    data.frame(trials = params()$nsim, rejections = num_rejections, power = power,
               confint = paste0("(", confint[1], ", ", confint[2], ")")) %>%
    kable(col.names = c("Number of Trials", "Number of Rejections", "Estimated Power", 
                        paste0((1 - params()$alpha) * 100, "% Confidence Interval")),
          align = "l") %>%
    kable_styling(full_width = FALSE)
  })
}

# -------------------------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
