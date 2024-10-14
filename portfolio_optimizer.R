library(shiny)
library(quantmod)
library(PortfolioAnalytics)
library(tidyverse)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Retirement Portfolio Optimizer"),
  sidebarLayout(
    sidebarPanel(
      numericInput("salary", "Annual Salary ($):", value = 50000, step = 1000),
      numericInput("age", "Current Age:", value = 30, step = 1),
      numericInput("savings", "Current Savings ($):", value = 10000, step = 500),
      numericInput("desired_amount", "Desired Amount at Retirement ($):", value = 1000000, step = 5000),
      sliderInput("risk_tolerance", "Risk Tolerance (1 = Low, 5 = Medium, 10 = High)", min = 1, max = 10, value = 5),
      dateInput("start_date", "Start Date", value = "2014-01-01"),
      dateInput("end_date", "End Date", value = Sys.Date()),
      
      checkboxGroupInput("stocks", "Select Stocks", 
                         choices = list("AAPL", "GOOG", "AMZN", "TSLA", "MSFT"), 
                         selected = c("AAPL", "GOOG")),
      
      actionButton("optimize", "Optimize Portfolio")
    ),
    mainPanel(
      h3("Stock Price Chart"),
      plotlyOutput("price_chart"),
      h3("Portfolio Statistics"),
      verbatimTextOutput("portfolio_stats"),
      h3("Optimized Portfolio Allocation"),
      plotlyOutput("optimized_pie"),
      h3("Monte Carlo Simulation"),
      plotlyOutput("monte_carlo")
    )
  )
)

# Define server
server <- function(input, output) {
  
  get_data <- reactive({
    req(input$stocks)
    start <- input$start_date
    end <- input$end_date
    data_list <- lapply(input$stocks, function(stock) {
      getSymbols(stock, src = 'yahoo', from = start, to = end, auto.assign = FALSE)[, 6] # Adjusted close
    })
    stock_data <- do.call(merge, data_list)
    stock_data <- na.omit(stock_data)
    colnames(stock_data) <- input$stocks
    stock_data
  })
  
  output$price_chart <- renderPlotly({
    df <- get_data()
    df_long <- as.data.frame(df) %>%
      rownames_to_column(var = "Date") %>%
      gather(key = "Stock", value = "Price", -Date) %>%
      mutate(Date = as.Date(Date))
    
    plot_ly(df_long, x = ~Date, y = ~Price, color = ~Stock, type = 'scatter', mode = 'lines') %>%
      layout(title = "Stock Prices Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
  })
  
  monte_carlo_sim <- reactive({
    stock_data <- get_data()
    returns <- ROC(stock_data, type = "discrete")[-1, ]
    num_portfolios <- 5000
    port_returns <- numeric(num_portfolios)
    port_risks <- numeric(num_portfolios)
    sharpe_ratios <- numeric(num_portfolios)
    weights_list <- vector("list", num_portfolios)
    
    for (i in 1:num_portfolios) {
      weights <- runif(n = ncol(returns))
      weights <- weights / sum(weights)
      port_return <- sum(colMeans(returns) * weights)
      port_risk <- sqrt(t(weights) %*% cov(returns) %*% weights)
      sharpe_ratio <- port_return / port_risk
      
      port_returns[i] <- port_return
      port_risks[i] <- port_risk
      sharpe_ratios[i] <- sharpe_ratio
      weights_list[[i]] <- weights
    }
    
    data.frame(Returns = port_returns, Risk = port_risks, Sharpe = sharpe_ratios)
  })
  
  output$monte_carlo <- renderPlotly({
    mc_data <- monte_carlo_sim()
    plot_ly(mc_data, x = ~Risk, y = ~Returns, color = ~Sharpe, type = 'scatter', mode = 'markers') %>%
      layout(title = "Monte Carlo Simulation", xaxis = list(title = "Risk"), yaxis = list(title = "Returns"))
  })
  
  optimize_portfolio <- reactive({
    stock_data <- get_data()
    returns <- ROC(stock_data, type = "discrete")[-1, ]
    
    port <- portfolio.spec(assets = colnames(returns))
    port <- add.constraint(portfolio = port, type = "full_investment")
    port <- add.constraint(portfolio = port, type = "long_only")
    port <- add.objective(portfolio = port, type = "return", name = "mean")
    port <- add.objective(portfolio = port, type = "risk", name = "StdDev")
    
    opt_port <- optimize.portfolio(R = returns, portfolio = port, optimize_method = "random", trace = TRUE)
    extractWeights(opt_port)
  })
  
  output$optimized_pie <- renderPlotly({
    weights <- optimize_portfolio()
    weights_df <- data.frame(Stock = names(weights), Weight = as.numeric(weights))
    
    plot_ly(weights_df, labels = ~Stock, values = ~Weight, type = 'pie') %>%
      layout(title = 'Optimized Portfolio Allocation')
  })
  
  output$portfolio_stats <- renderPrint({
    weights <- optimize_portfolio()
    stock_data <- get_data()
    returns <- ROC(stock_data, type = "discrete")[-1, ]
    
    portfolio_return <- sum(colMeans(returns) * weights)
    portfolio_risk <- sqrt(t(weights) %*% cov(returns) %*% weights)
    sharpe_ratio <- portfolio_return / portfolio_risk
    
    cat("Expected Portfolio Return: ", round(portfolio_return, 4), "\n")
    cat("Portfolio Risk (Volatility): ", round(portfolio_risk, 4), "\n")
    cat("Sharpe Ratio: ", round(sharpe_ratio, 4), "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
