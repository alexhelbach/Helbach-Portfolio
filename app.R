#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
####################################
### Stock Prediction Application ###
####################################
#rm(list = ls())
#-------------------------------------------------------------------------------
# Upload packages 
library(shinydashboard)
library(shiny)
library(tidyverse)
library(magrittr)
library(DT)
library(dplyr)
library(shinydashboardPlus)
library(data.table)
library(BatchGetSymbols)
library(forecast)
library(timetk)
library(lubridate)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Setting Dashboard User Interface ###
#-------------------------------------------------------------------------------
# Header of dashboard
#------------------------------------------------------------------------------
header <- dashboardHeader(title = "Stock Prediction")
 #------------------------------------------------------------------------------
# Sidebar of dashboard 
 #------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Home Page", tabName = "HomePage", icon = NULL),
  menuItem("Graphs", tabName = "DataVis", icon = NULL),
  menuItem("Model Prediction", tabName = "NNETAR", icon = NULL),
  menuItem("Prediction", tabName = "ML", icon = NULL)))
#------------------------------------------------------------------------------
# Dashboard body
#------------------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    # New tab
    tabItem(tabName = "HomePage",
         fluidRow(  
           # Stock symbol look up box
           box(title = "Enter Stock Symbol", width = 4, solidHeader = T, status = "primary",
        textInput("Stock1", "Stock Symbol", value = "AAPL"),
        actionButton(inputId = "click", label = "Predict")),
        # Last couple days metrics 
        box(dataTableOutput("HomeTable1"))
        )),
  # Data Vis Tab
     tabItem(tabName = "DataVis",
             fluidRow(
               box(title = "Enter Dates EX: 2018-01-01", width = 3, status = "primary",
             dateInput("StartDate", "Start Date", value = "2015-01-01"),
             dateInput("EndDate", "End Date", value = "2020-01-01")),
             box(
                 selectInput("Metric", "Select Metric", 
                             c("Open" = "Open", 
                               "High" = "High", "Low" = "Low",
                               "Close " = "Close"),multiple = TRUE))),
           box(width = 12, plotOutput("plot1", hover = TRUE))),
  # Auto Arima Model Output
  tabItem(tabName = "NNETAR",
          fluidRow(
          box(title = "Prediction Period", width = 3, status = "primary",
              numericInput("Days", "", value = 30)),
          box(width = 3, status = "primary",
            selectInput("ModelMetric", "Select Metric", 
                        c("Open" = "Open", 
                          "High" = "High", "Low" = "Low",
                          "Close " = "Close")))),
          fluidRow(box(width = 3, height = "400px", title = "ANN Model Forecast", 
              DT::dataTableOutput("PredTable1")),
  box(width = 8, plotOutput("MLPlot")))),
  # Machine Learning Results and Interface
     tabItem(tabName = "ML",
          box(),
          box())
  ))
#-------------------------------------------------------------------------------
# Zipping all user facing components together
ui <- dashboardPage(header = header, sidebar =sidebar, body = body)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Back End ###
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Declaring Functions
#-------------------------------------------------------------------------------
# Pulls stock data from yahoo and returns a data frame of high low open close 
# of user typed stock. Default is apply AAPL
#-------------------------------------------------------------------------------
server <- function(input, output) {
  Stock_Pull <- function(Variable) {
    data1 <- as.character(Variable)
    FirstDate <- Sys.Date() - 2500
    LastDate <- Sys.Date()
    FreqData <- "daily"
    # Pulling stock data using BatchGetSymbols
    stocks <- data.frame(BatchGetSymbols(tickers = data1,
                                         first.date = FirstDate,
                                         last.date = LastDate,
                                         freq.data = FreqData,
                                         do.cache = FALSE,
                                         thresh.bad.data = 0))
    # Selecting values  for table 
    HomeTable <- data.frame(stocks$df.control.ticker,
                            stocks$df.tickers.ref.date,
                            stocks$df.tickers.price.open,
                            stocks$df.tickers.price.high, 
                            stocks$df.tickers.price.low,
                            stocks$df.tickers.price.close)
    # Tidying up data
    colnames(HomeTable) <- c("Symbol", "Date", "Open", "High", "Low", "Close")
    HomeTable$Date <- as.Date(HomeTable$Date)
    HomeTable[3:6] <- lapply(HomeTable[3:6], round, digits = 2)
    return(HomeTable)
  }
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# First Page of Dashboard: Stock selection box and table of most recent 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
      output$HomeTable1 <- renderDataTable({
        data <- eventReactive(input$click, {
      (input$Stock1) 
    })
      HomeTable <- Stock_Pull(input$Stock1)
      HomeTable1 <- tail(HomeTable)
      })
#------------------------------------------------------------------------------- 
# Time Series Plot with Date Adjustment for the Data Vis page 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    output$plot1 <- renderPlot({
        data <- eventReactive(input$click, {
          (input$Stock1)
          })
      StartDate <- eventReactive(input$Filter,{
        (input$StartDate)
        })
      EndDate <- eventReactive(input$Filter,{
        (input$EndDate)
        })
      Metric <- eventReactive(input$Metric, {
        (input$Metric)
        })
      HomeTable <- Stock_Pull(input$Stock1)
#-------------------------------------------------------------------------------
# Start and Enter Date Logic 
 HomeTable2 <-  HomeTable %>% select(Date, input$Metric) %>% 
        filter(Date >= input$StartDate & Date <= input$EndDate) 
      Metric <- as.character(input$Metric)
#-------------------------------------------------------------------------------
# ggplot logic 
      ggplot(HomeTable2, aes(Date)) +
        {if("High" %in% input$Metric)geom_line(aes(Date, High, color = "High"))} + 
        {if("Close" %in% input$Metric)geom_line(aes(Date, Close, color = "Close"))} + 
        {if("Open" %in% input$Metric)geom_line(aes(Date, Open, color = "Open"))} + 
        {if("Low" %in% input$Metric)geom_line(aes(Date, Low, color = "Low"))} + 
        theme_classic() + 
        ggtitle("Low, High, Open, and Close Value Over Time") +
        ylab("Value of Stock") + 
        theme(
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, face = "bold", hjust=.5))
    })
#-------------------------------------------------------------------------------
# NNETAR Model Plot
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    output$MLPlot <- renderPlot({
      data <- eventReactive(input$click, {
        (input$Stock1)
        })
      Days <- eventReactive(input$Days,{
        (input$Days)
      })
      ModelMetric <- eventReactive(input$ModelMetric, {
        (input$Metric)
      })
      
      HomeTable <- Stock_Pull(input$Stock1)
      HomeTable2 <- HomeTable %>% select(Date, input$ModelMetric) %>%  
        tibble() %>% filter_by_time(Date, .start_date = "2018-01-01")
#-------------------------------------------------------------------------------
      T1 <- ts(HomeTable2[2], start = c(2018, 01), frequency = 252)
      fit <- nnetar(T1)
      fit.forecast <- forecast(fit, h = input$Days) 
      autoplot(fit.forecast) + ggtitle("NNETAR Prediction") + theme_classic()
})
#-------------------------------------------------------------------------------    
# NNETAR Prediction Table 
#-------------------------------------------------------------------------------
output$PredTable1 <- renderDataTable({
  data <- eventReactive(input$click, {
    (input$Stock1)
  })
  Days <- eventReactive(input$Days,{
    (input$Days)
  })
  ModelMetric <- eventReactive(input$ModelMetric, {
    (input$Metric)
  })
#-------------------------------------------------------------------------------
  HomeTable <- Stock_Pull(input$Stock1)
  HomeTable2 <- HomeTable %>% select(Date, input$ModelMetric) %>%  
    tibble() %>% filter_by_time(Date, .start_date = "2018-01-01")
#-------------------------------------------------------------------------------
  T1 <- ts(HomeTable2[2], start = c(2018, 01), frequency = 252)
  fit <- nnetar(T1)
  fit.forecast <- forecast(fit, h = input$Days) 
  date <-format(date_decimal(as.numeric(row.names(as.data.frame(fit.forecast)))),"%Y-%m-%d")
  x <- cbind(date,as.data.frame(fit.forecast))
#-------------------------------------------------------------------------------
# Formatting output table 
  table2 <- data.frame(cbind(x$date, x$`Point Forecast`))
  colnames(table2) <- c("Date", "Value")
  table2$Value <- as.double(table2$Value)
  table2$Value <- round(table2$Value, digits = 2)
  DT::datatable(table2, options = list(lengthMenue = c(5), pageLength = 5))
})    
    
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
shinyApp(ui, server)
#-------------------------------------------------------------------------------


