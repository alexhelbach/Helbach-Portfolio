##########################################
# EE Survey Data Shiny Application Scrip #
##########################################
# Authored by: Alexander Helbach 
# Last updated: 10/9/21
# #-------------------------------------------------------------------------------
# Takes data from Qualtrics survey and cleans, formats, and calculates needed 
# variables. After the latter, results are displayed on an interactive dashboard. 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(tidyverse)
library(magrittr)
library(DT)
library(dplyr)
library(shinydashboardPlus)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Select the newest data aka that days data :) 
df <- read.csv("Express+Employment_October+6,+2021_13.03.csv")
# Omitting unneeded columns
df1 <- df[-c(1:2), -c(1:18)]
#-------------------------------------------------------------------------------
# Re-coding variables
df1[df1 == "Strongly agree"] <- 7
df1[df1 == "Agree"] <- 6
df1[df1 == "Slightly agree"] <- 5
df1[df1 == "Neither agree nor disagree"] <- 4
df1[df1 == "Slightly disagree"] <- 3
df1[df1 == "Disagree"] <- 2
df1[df1 == "Strongly disagree"] <- 1
# Formatting scale variables 
df1[1:14] <- lapply(df1[1:14], as.double)
# zipping it back into a data frame 
df1 <- data.frame(df1)
#-------------------------------------------------------------------------------
# Adding in composite scale scores 
df1 <- df1 %>% 
    mutate(WFCS_Score = rowSums(df1[1:5], na.rm = T)) %>% 
    mutate(FWCS_Score = rowSums(df1[6:10], na.rm = T)) %>% 
    mutate(POWLBS_Score = rowSums(df1[11:14], na.rm = T))
#-------------------------------------------------------------------------------
# Mean table 
Mean_Tabulations <- data.frame((colSums(df1[1:14], na.rm = T)/length(df$WFCS2)))
colnames(Mean_Tabulations) <- c("Mean")
Mean_Tabulations$Mean <- round(Mean_Tabulations$Mean, digits = 2)
Mean_Tabulations$SD <- lapply(df1[1:14], sd, na.rm = T)
Mean_Tabulations <- lapply(Mean_Tabulations, as.double)
Mean_Tabulations <- data.frame(Mean_Tabulations)
Mean_Tabulations$SD <- round(Mean_Tabulations$SD, digits = 2)
Mean_Tabulations$Count <- colSums(!is.na(df1[1:14]))
Mean_Tabulations$Questions <- c(cbind(df[1, c(20:33)]))
Mean_Tabulations$Variable <- c(names(df1[1:14]))
Mean_Tabulations$Variable  <- ordered(Mean_Tabulations$Variable,
                                      levels = c("WFCS1", "WFCS2", "WFCS3", 
                                                 "WFCS4", "WFCS5", "FWCS1",
                                                 "FWCS2", "FWCS3", "FWCS4",
                                                 "FWCS5", "HoursNeed", "Culture",
                                                 "Policies", "TimeRespect"))
Mean_Tabulations <- Mean_Tabulations[c(5,4,1,2,3)]
#-------------------------------------------------------------------------------
# Scale Scores 
Sum_Score <- data.frame(colSums(df1[24:26], na.rm = T)/length(df1$Culture))
colnames(Sum_Score) <- c("Sum")
Sum_Score$Sum <- round(Sum_Score$Sum, digits = 2)
Sum_Score <- lapply(Sum_Score, as.double)
Sum_Score <- data.frame(Sum_Score)
Sum_Score$Count <- colSums(!is.na(df1[24:26]))
Sum_Score$Scale <- factor(c("WFCS", "FWCS", "POWLBS"))
#-------------------------------------------------------------------------------
# Back end side 
#-------------------------------------------------------------------------------
    ui <- dashboardPage(
        dashboardHeader(title = "EE Dashboard"),
        # Side bar select menu 
        dashboardSidebar(selectInput("Demographics", label = "Demographic", 
                                     choices = c("Age", "Ethnicity", "Gender",
                                                 "Education", "PursueEducation", "WorkClass",
                                                 "MultiPositions", "Hours")),
                         selectInput("Score", label = "Score", 
                                     choices = c("WFCS_Score", "FWCS_Score", "POWLBS_Score"))),
        dashboardBody(
            fluidRow(width = 6,
                       box(
                           plotOutput(width = NULL, height = "350px","DemoScore")),
            box(plotOutput(width = NULL, height = "350px","Sum_Score"))),
            box(plotOutput(width = NULL, height = "350px", "Item_Mean")),
            box(dataTableOutput(width = NULL, height = "300px","table"))))
        
server <- function(input, output) {
 # Mean of each scale item table    
output$table <- renderDataTable({datatable(Mean_Tabulations, 
                                           options = list(pageLength = 3))
})
# Interactive plot of scales by demographic measures 
output$DemoScore <- renderPlot({
        ggplot(df1, aes_string(input$Demographics, input$Score, fill = input$Demographics)) + 
            stat_summary(aes_string(input$Demographics,input$Score), fun = "mean", geom = "bar") +
            ggtitle("Scale Sum Score by Demographic Variable") + 
            theme_classic() + theme(
                axis.text.x = element_blank(),
                plot.title = element_text()
            )
    })
# Static plot of scale mean score
output$Sum_Score <- renderPlot({
    ggplot(Sum_Score, aes(Scale, Sum, fill = Scale)) +
        geom_col() + theme_classic() + geom_text(aes(label = Sum), size = 6) +
        ggtitle("Scale Mean Score") + theme(
            plot.title = element_text(hjust = .5),
            axis.text.x = element_blank())
})
# Static plot of scale mean scores 
output$Item_Mean <- renderPlot({
    ggplot(Mean_Tabulations, aes(Variable, Mean, fill = Variable)) +
        geom_col() + theme_classic() +
        ggtitle("Scale Items Mean Score") + theme(
            plot.title = element_text(hjust = .5),
            axis.text.x = element_blank())
})
}

#-------------------------------------------------------------------------------
shinyApp(ui, server)
#-------------------------------------------------------------------------------

