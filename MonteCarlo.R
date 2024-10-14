# Load required libraries
library(shiny)
library(quantmod)
library(ggplot2)
library(tidyquant)
library(PerformanceAnalytics)

# Define UI for Retirement Portfolio Optimizer
ui <- fluidPage(
  titlePanel("Retirement Portfolio Optimizer"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("salary", "Annual Salary ($)", value = 50000, step = 1000),
      numericInput("age", "Current Age", value = 30, step = 1),
      numericInput("savings", "Current Savings ($)", value = 10000, step = 500),
      numericInput("desired_amount", "Desired Amount at Retirement ($)", value = 1000000, step = 5000),
      sliderInput("risk_tolerance", "Risk Tolerance (1 = Low, 5 = Medium, 10 = High)", 1, 10, 5),
      dateInput("start_date", "Start Date", value = "2014-01-01"),
      dateInput("end_date", "End Date", value = Sys.Date()),
      
      # Stock selection input
      checkboxGroupInput("assets", "Select Stocks", 
                         choices = c("AAPL", "GOOG", "MSFT", "AMZN", "TSLA"),
                         selected = c("AAPL", "GOOG"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Price Chart", plotOutput("pricePlot")),
        tabPanel("Portfolio Statistics", tableOutput("statsTable")),
        tabPanel("Optimized Portfolio Allocation", plotOutput("allocationPie"))
      )
    )
  )
)

# Define server logic for Retirement Portfolio Optimizer
server <- function(input, output) {
  
  # Fetch stock data using quantmod
  get_data <- reactive({
    symbols <- input$assets
    stock_data <- new.env()
    
    for (symbol in symbols) {
      getSymbols(symbol, from = input$start_date, to = input$end_date, env = stock_data, auto.assign = TRUE)
    }
    
    stock_data
  })
  
  # Render Stock Price Chart
  output$pricePlot <- renderPlot({
    stock_data <- get_data()
    stock_prices <- do.call(merge, eapply(stock_data, Cl))
    
    chart_data <- data.frame(Date = index(stock_prices), coredata(stock_prices))
    melted_data <- reshape2::melt(chart_data, id = "Date")
    
    ggplot(melted_data, aes(x = Date, y = value, color = variable)) +
      geom_line() +
      labs(title = "Stock Prices Over Time", x = "Date", y = "Price")
  })
  
  # Render Portfolio Statistics
  output$statsTable <- renderTable({
    stock_data <- get_data()
    stock_prices <- do.call(merge, eapply(stock_data, Cl))
    
    # Calculate returns and portfolio statistics
    returns <- Return.calculate(stock_prices)
    portfolio_return <- colMeans(returns, na.rm = TRUE)
    portfolio_volatility <- apply(returns, 2, sd, na.rm = TRUE)
    
    stats_df <- data.frame(
      Stock = colnames(returns),
      Return = portfolio_return * 252,  # Annualized return
      Volatility = portfolio_volatility * sqrt(252)  # Annualized volatility
    )
    
    stats_df
  })
  
  # Render Optimized Portfolio Allocation (Pie Chart)
  output$allocationPie <- renderPlot({
    stock_data <- get_data()
    stock_prices <- do.call(merge, eapply(stock_data, Cl))
    
    # Placeholder for optimized weights
    optimized_weights <- rep(1 / length(input$assets), length(input$assets))
    
    allocation_df <- data.frame(
      Stock = input$assets,
      Weight = optimized_weights
    )
    
    ggplot(allocation_df, aes(x = "", y = Weight, fill = Stock)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = "Optimized Portfolio Allocation")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
