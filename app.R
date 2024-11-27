# *** app.R. ***

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Probability Distributions Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Select Distribution:",
                  choices = c("Normal", "Binomial", "Poisson", "Exponential", "Uniform")),
      uiOutput("parameters"),  # Dynamic UI for distribution parameters
      actionButton("simulate", "Simulate"),
      hr(),
      numericInput("sample_size", "Sample Size:", value = 1000, min = 1),
      checkboxInput("show_theory", "Show Theoretical Distribution", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Distribution Plot", plotOutput("distPlot")),
        tabPanel("Simulation Results", plotOutput("simPlot"), verbatimTextOutput("summaryStats"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Dynamic UI for parameters based on selected distribution
  output$parameters <- renderUI({
    switch(input$distribution,
           "Normal" = tagList(
             numericInput("mean", "Mean (μ):", value = 0),
             numericInput("sd", "Standard Deviation (σ):", value = 1, min = 0.01)
           ),
           "Binomial" = tagList(
             numericInput("size", "Number of Trials (n):", value = 10, min = 1),
             numericInput("prob", "Probability of Success (p):", value = 0.5, min = 0, max = 1)
           ),
           "Poisson" = numericInput("lambda", "Rate (λ):", value = 1, min = 0),
           "Exponential" = numericInput("rate", "Rate (λ):", value = 1, min = 0.01),
           "Uniform" = tagList(
             numericInput("min", "Minimum (a):", value = 0),
             numericInput("max", "Maximum (b):", value = 1)
           )
    )
  })
  
  # Reactive expression for distribution data
  distData <- reactive({
    x <- seq(-10, 10, length.out = 1000)
    data.frame(
      x = x,
      y = switch(input$distribution,
                 "Normal" = dnorm(x, mean = input$mean, sd = input$sd),
                 "Binomial" = dbinom(x = 0:input$size, size = input$size, prob = input$prob),
                 "Poisson" = dpois(x = 0:max(20, input$lambda * 3), lambda = input$lambda),
                 "Exponential" = {
                   x_pos <- x[x >= 0]
                   y_vals <- dexp(x_pos, rate = input$rate)
                   data.frame(x = x_pos, y = y_vals)
                 },
                 "Uniform" = dunif(x, min = input$min, max = input$max)
      )
    )
  })
  
  # Distribution plot
  output$distPlot <- renderPlot({
    data <- distData()
    ggplot(data, aes(x = x, y = y)) +
      geom_line(color = "blue") +
      labs(title = paste(input$distribution, "Distribution"),
           x = "x", y = "Density") +
      theme_minimal()
  })
  
  # Simulation based on user input
  simulation <- eventReactive(input$simulate, {
    n <- input$sample_size
    dist <- input$distribution
    switch(dist,
           "Normal" = rnorm(n, mean = input$mean, sd = input$sd),
           "Binomial" = rbinom(n, size = input$size, prob = input$prob),
           "Poisson" = rpois(n, lambda = input$lambda),
           "Exponential" = rexp(n, rate = input$rate),
           "Uniform" = runif(n, min = input$min, max = input$max)
    )
  })
  
  # Simulation plot
  output$simPlot <- renderPlot({
    req(simulation())
    data <- data.frame(Value = simulation())
    p <- ggplot(data, aes(x = Value)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
      labs(title = paste("Simulation of", input$distribution, "Distribution"),
           x = "Value", y = "Density") +
      theme_minimal()
    
    # Overlay theoretical distribution if selected
    if (input$show_theory) {
      p <- p + stat_function(fun = switch(input$distribution,
                                          "Normal" = function(x) dnorm(x, mean = input$mean, sd = input$sd),
                                          "Exponential" = function(x) dexp(x, rate = input$rate),
                                          "Uniform" = function(x) dunif(x, min = input$min, max = input$max),
                                          NULL
      ), color = "red", size = 1)
    }
    p
  })
  
  # Summary statistics
  output$summaryStats <- renderPrint({
    req(simulation())
    summary(simulation())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
