#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 
#
#
# Makayla Lerner 
# BAS 475 
# Final App
# 
#

library(shiny)
library(fpp3)
library(bslib) 

get_my_plot <- function(i = "Seasonality") {
  if (i == "Seasonality") {
    
    return(gg_season(souvenirs))
    
  }
  
  else if (i == "Autocorrelation") {
    
    return(souvenirs %>%
             ACF(Sales) %>%
             autoplot())
    
  }
  
  
  else {
    return(souvenirs %>%
             model(classical_decomposition(Sales)) %>%
             components() %>%
             autoplot())
    
    
  }
}

get_int_1 <- function(i = "Seasonality") {
  if (i == "Seasonality") {
    
    return("Year over year, there is a large jump 
        in sales starting in November and going through December. 
        We also see a moderate jump in sales in March every year. 
        This could be due to Christian holidays of Easter and Christmas, 
        as well as those traveling for school breaks during those times.")
  }
  
  
  
  else if (i == "Autocorrelation") {
    
    
    
    return("In this autocorrelation graph, we see the seasonality 
      of the data around month 12, with the autocorrelation being much higher 
      around the 12th month mark compared to the other lags. We see something 
      very similar around lag 3, which correlates with additional seasonality 
      in March.")
    
  }
  
  
  else {
    
    return("Through classical decomposition, we can break the series 
      into parts. Overall, random or residual effects appears to be the most 
      prominent aspect of the series, with seasonality and trend coming after, 
      respectively. Classical decomposition assumes that seasonality repeats 
      every year and observations at the head and tail ends do not appear in 
      the trend cycle.")
  }
}

get_my_fc <- function(i = "Mean", h_out = 10, ar1 = 1, i1 = 1, ma1 = 1, ar2 = 0, i2 = 1, ma2 = 1){
  
  if(i  == "Mean" ) {
    fit <- souvenirs %>%
      model(MEAN(Sales))
    
    return(fit %>%
             forecast(h = h_out) %>%
             autoplot(souvenirs))
    
  }
  else if(i  == "Naive") {
    
    fit <- souvenirs %>% 
      model(NAIVE(Sales))
    
    return(fit %>%
             forecast(h = h_out) %>%
             autoplot(souvenirs))
  }
  
  else if(i == "Seasonal Naive") {
    
    
    fit <- souvenirs %>%
      model(SNAIVE(Sales))
    
    return(fit %>%
             forecast(h = h_out) %>%
             autoplot(souvenirs))
  }
  
  else if (i == 'Drift') {
    fit <- souvenirs %>%
      model(NAIVE(Sales ~ drift()))
    
    return(fit %>%
             forecast(h = h_out) %>%
             autoplot(souvenirs))
  }
  
  else if (i == 'Exponential Smoothing - Holts') {
    fit <- souvenirs %>% 
      model(ETS(Sales))
    
    return(fit %>%
             forecast(h = h_out) %>%
             autoplot(souvenirs))
    
  }
  
  else if (i == 'Exponential Smoothing - Holts/Winters')  {
    fit <- souvenirs %>% 
      model(ETS(Sales ~ error("M") + trend("A") + season("M")))
    
    return (fit %>% 
              forecast (h = h_out) %>%
              autoplot(souvenirs))
  }
  
  
  else if (i == 'ARIMA - Default Paramters') {
    fit <- souvenirs %>% 
      model(ARIMA())
    
    return(fit %>% 
             forecast(h = h_out) %>% 
             autoplot(souvenirs))
  }
  
  else {
    fit <- souvenirs %>% 
      model(ARIMA(Sales ~ pdq(ar1, i1, ma1) + PDQ(ar2, i2, ma2)))
    
    return(fit %>% 
             forecast(h = h_out) %>%
             autoplot(souvenirs))
  }
}

get_int_2 <- function(i = "Mean") {
  
  if(i == "Mean" ) {
    
    return("The mean model predicts that sales will continue to be
      around $14,315.59 per month.")
    
  }
  
  else if(i == "Naive") {
    
    return("The Naive model predicts that sales will continue to be
      around the last value observed $104,660.67 every month.")
  }
  
  else if(i == "Seasonal Naive") {
    return("The seasonal naive model predicts that every season will stay 
      the same and we will continue to see spikes in sales in March and 
      December.")
    
  }
  else if (i == "Drift") {
    return("The drift model predicts that Sales will continue to increase
      as they did between the first and last observation.")
  }
  
  else if (i == "Exponetial Smoothing - Holts") {
    return("The Holts Exponential Smoothing model predicts that sales will fall within the blue range")
    
  }
  
  else if (i == "Exponential Smoothing - Holts/Winters") {
    return("The Holts/Winters Exponential Smoothing model uses multiplicative error, additive trend, and multiplicative seasonality.This model predicts that sales will fall with the blue range")
    
  }
  else if (i == "ARIMA - Default Parameters") {
    return("The ARIMA or Autoregressive Integrated Moving Average model with default parameters predicts that sales will fall within the blue range")
  }
  
  else {
    return("The ARIMA or Autoregressive Integrated Moving Average model with custom parameters predicts that sales will fall within the blue range")
  }
  
}

get_my_report <- function(i = "Mean", h_out = 12) {
  
  if(i  == "Mean" ) {
    fit <- souvenirs %>%
      model(MEAN(Sales))
    
    return(fit %>%
             forecast(h = h_out) %>%
             report)
  }
  
  else {
    return("Hi")
  }
  
}


plot_list <- c(
  "Seasonality", 
  "Autocorrelation", 
  "Decomposition"
)

fc_list <- c(
  "Mean", 
  "Naive", 
  "Seasonal Naive", 
  "Drift",
  "Exponetial Smoothing - Holts", 
  "Exponential Smoothing - Holts/Winters", 
  "ARIMA - Default Parameters", 
  "ARIMA - Custom Parameters"
)


# Define UI 
ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "lux"),
  
  # Application title
  titlePanel("Souvenir Sales from Shop on Queensland Wharf 1987 - 1993"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      helpText(
        "Use the dropdowns to select a time series graph and forecasting model. 
            Then, use the slider to set how many months to forecast."
      ), 
      
      selectInput(
        inputId = "graph",
        label = "Choose a plot: ",
        choices = plot_list
      ), 
      
      selectInput(
        inputId = "fc",
        label = "Choose a forecasting model: ",
        choices = fc_list
      ), 
      
      sliderInput(
        inputId = "hi", 
        label = "Set how many months to forecast: ",
        min = 8, 
        max = 16,
        value = 10, 
        step = 2
      ), 
      
      
      helpText (
        "Manually select ARIMA parameters: "
      ),
      
      sliderInput( 
        inputId = "AR",
        label = "Select the number of lag observations: ", 
        min = 0, 
        max = 5, 
        value = 1,
        step = 1
      ), 
      
      sliderInput(
        inputId = "I", 
        label = "Select the number of times to difference: ", 
        min = 0, 
        max = 5, 
        value = 1, 
        step = 1
      ), 
      
      sliderInput(
        inputId = "MA", 
        label = "Select the size of the moving average window: ", 
        min = 0, 
        max = 5,
        value = 1, 
        step = 1
      ), 
      
      sliderInput(
        inputId = "AR2", 
        label = "Select the number of seasonal lag obseravations: ", 
        min = 0, 
        max = 5, 
        value = 0, 
        step = 1
      ), 
      
      sliderInput(
        inputId = "I2", 
        label = "Select the number of times to difference the seasonal observations: ", 
        min = 0, 
        max = 5, 
        value = 1, 
        step = 1
      ), 
      
      sliderInput(
        inputId = "MA2", 
        label = "Select the size of the seasonal moving average window: ", 
        min = 0, 
        max = 5, 
        value = 1, 
        step = 1
      ),
      
      helpText(
        "Created by Makayla Lerner for BAS 475 on 12.07.22"
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs", 
                  
                  tabPanel("Exploratory Analysis", 
                           
                           plotOutput(outputId = "plot"),
                           h4("Time Series Interpretation: "),
                           textOutput(outputId = "int") ), 
                  
                  tabPanel("Forecasting Model", 
                           
                           plotOutput(outputId = "fco"), 
                           h4("Forecasting Interpretation: "),
                           textOutput(outputId = "int2")), 
                  textOutput(outputId = "rep")),
      
      h1("       "), 
      h1("       ")
      
    )
    
  )
)

# Define server logic required to show plots and interpretations
server <- function(input, output) {
  
  output$plot <- renderPlot(
    get_my_plot(input$graph),
    width = "auto",
    height = "auto")
  
  output$int <- renderText(get_int_1(input$graph))
  
  
  output$fco <- renderPlot(
    get_my_fc(input$fc, input$hi, input$AR, input$I, input$MA, input$AR2, input$I2, input$MA2), 
    width = "auto", 
    height = "auto" )
  
  output$int2 <- renderText(get_int_2(input$fc))
  
  output$rep <- renderText(get_my_report(input$fc, input$hi))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
