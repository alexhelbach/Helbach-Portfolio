##########################################
### Corn Value Prediction Application ###
#########################################
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
library(forecast)
library(timetk)
library(lubridate)
################################################################################
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Setting Dashboard User Interface ###
#-------------------------------------------------------------------------------
# Header of dashboard
#------------------------------------------------------------------------------
header <- dashboardHeader(title = "Corn Price Prediction")
#------------------------------------------------------------------------------
# Sidebar of dashboard 
#------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        fileInput("file1", "Load CSV File"),
        menuItem("Home Page", tabName = "HomePage", icon = NULL),
        menuItem("Graphs", tabName = "DataVis", icon = NULL),
        menuItem("ANN Plot", tabName = "ANNPlot", icon = NULL),
        menuItem("ARIMA Plot", tabName = "ARIMAPlot", icon = NULL)))
#------------------------------------------------------------------------------
# Dashboard body
#------------------------------------------------------------------------------
body <- dashboardBody(
    tabItems(
        # New tab
        tabItem(tabName = "HomePage",
                fluidRow(
                    # Last couple days metrics 
                    box(dataTableOutput("WeekPrice")))),
        # Data Vis Tab
        tabItem(tabName = "DataVis",
                box(title = "Enter Dates EX: 2018-01-01", width = 3, status = "primary",
                    dateInput("StartDate", "Start Date", value = "2015-01-01"),
                    dateInput("EndDate", "End Date", value = "2020-01-01")),
                box(width = 12, plotOutput("PricePlot"))),
        # Ann Plot and Table Tab 
        tabItem(tabName = "ANNPlot",
                fluidRow(box(title = "Prediction Period", width = 3, status = "primary",
                    numericInput("Days", "", value = 30))),
                fluidRow(
                box(width = 4, height = "400px", title = "ANN Model Forecast", 
                    DT::dataTableOutput("PredTable1")),
                box(width = 8, height = "400px", plotOutput("ANNplot"))))))
#-------------------------------------------------------------------------------
ui <- dashboardPage(header = header, sidebar =sidebar, body = body)
# Zipping all user facing components together
#-------------------------------------------------------------------------------
### Back End ###
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# File upload and HomePage Tab Table 
#-------------------------------------------------------------------------------
server <- function(input, output) {
    file_data <- reactive({
        file <- input$file1 
        if(!is.null(file)){read.csv(file$datapath)}
    })
    
output$WeekPrice <- renderDataTable({
    req(file_data())
    req(tail(file_data()))
})
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Data Visualizations Tab # 
#-------------------------------------------------------------------------------
output$PricePlot <- renderPlot({
    df <- req(file_data())
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
#-------------------------------------------------------------------------------
# Date sorting logic
    HomeTable2 <-  df %>% select(date, value) %>% 
        filter(date >= input$StartDate & date <= input$EndDate) 
#-------------------------------------------------------------------------------
# ggplot of corn value over time 
    ggplot(HomeTable2, aes(date)) + geom_line(aes(date, value)) +
        theme_classic() + 
        ggtitle("Value of Corn Over Time") +
        ylab("Value of Corn") + 
        theme(
            axis.title.x = element_blank(),
            plot.title = element_text(size = 14, face = "bold", hjust=.5))
})
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# NNETAR Predictions Graph and Table 
#-------------------------------------------------------------------------------
output$ANNplot <- renderPlot({
    Days <- eventReactive(input$Days,{
        (input$Days)
    })
    df <- req(file_data())
#-------------------------------------------------------------------------------
    # Formatting data for modeling 
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
    HomeTable2 <- df %>% select(date, value) %>%  
        tibble() %>% filter_by_time(date, .start_date = "2010-01-01")
    T1 <- ts(HomeTable2[2], start = c(2010,01), frequency = 252)
    # Model building and forecasting 
    fit <- nnetar(T1)
    fit.forecast <- forecast(fit, h = input$Days) 
#-------------------------------------------------------------------------------
    autoplot(fit.forecast) + ggtitle("NNETAR Prediction") + theme_classic()
})
output$PredTable1 <- renderDataTable({
    Days <- eventReactive(input$Days,{
        (input$Days)
    })
    df <- req(file_data())
    #-------------------------------------------------------------------------------
    # Formatting data for modeling 
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
    HomeTable2 <- df %>% select(date, value) %>%  
        tibble() %>% filter_by_time(date, .start_date = "2010-01-01")
    T1 <- ts(HomeTable2[2], start = c(2010,01), frequency = 252)
    # Model building and forecasting 
    fit <- nnetar(T1)
    fit.forecast <- forecast(fit, h = input$Days) 
    date <-format(date_decimal(as.numeric(row.names(as.data.frame(fit.forecast)))),"%Y-%m-%d")
    x <- cbind(date,as.data.frame(fit.forecast))
#-------------------------------------------------------------------------------
    # Formatting output table 
    table2 <- data.frame(cbind(x$date, x$`Point Forecast`))
    colnames(table2) <- c("Date", "Value")
    table2$Value <- as.double(table2$Value)
    table2$Value <- round(table2$Value, digits = 3)
    datatable(table2, options = list(lengthMenue = c(5), pageLength = 5))
})

}
#-------------------------------------------------------------------------------
shinyApp(ui, server)
#-------------------------------------------------------------------------------


