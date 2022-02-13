# --------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(tidyverse)
# --------------------------------------------------------------
# Run once when app is launched
# --------------------------------------------------------------

# list of years
yearsList <- c(2001:2021)
stationsList <- c("UIC-Halsted", "O'Hare", "Medical Center")

# read in data-file
halstedData <- read.table(file="./halsted.tsv", sep="\t", header=TRUE)
ohareData <- read.table(file="./ohare.tsv", sep="\t", header=TRUE)
medicalData <- read.table(file="./medical.tsv", sep="\t", header=TRUE)

newDate <- as.Date(halstedData$date, "%m/%d/%Y")
halstedData$nDate <- newDate
halstedData$date <-NULL
newDate <- as.Date(ohareData$date, "%m/%d/%Y")
ohareData$nDate <- newDate
ohareData$date <- NULL
newDate <- as.Date(medicalData$date, "%m/%d/%Y")
medicalData$nDate <- newDate
medicalData$date <- NULL

# # # Entries per day over 2021
# yearToLookFor <- 2021
# daysHalsted <- subset(halstedData, year(nDate) == yearToLookFor )
# daysHalsted <- subset(ohareData, year(nDate) == yearToLookFor )
# daysMedical2021 <- subset(medicalData, year(nDate) == yearToLookFor )

# look at the data loaded
# View(halstedData)
# View(daysHalsted)


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title="Shoaib's Project 1"),
  dashboardSidebar(disable=FALSE, collapsed=FALSE,
                   sidebarMenu(
                     id = "tabsMenu",
                     menuItem("Home", tabName = "homeTab", icon = NULL),
                     menuItem("Compare", tabName = "compareStationsTab", icon = NULL),
                     menuItem("About", tabName = "aboutMe", icon = NULL)
                     )
                   ),
  dashboardBody(
    tabItems( 
      tabItem(tabName = "homeTab", # --------------------------------------------------------------------------------------------
              fluidRow(
                column(6,
                       # 
                       box( title = "Station Entries Per Year", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("halstedYears", height=800)
                       )
                ),
                column(6,
                       box( title = "Station Entries Per Year", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("ohareYears", height=800)
                       )
                )
              )
            ),
    
    
      tabItem(tabName = "compareStationsTab", # ---------------------------------------------------------------------------------
              fluidRow(
                column(6,
                       selectInput("yearInput1", "Choose a year", yearsList, selected = 2021),
                       selectInput("stationInput", "Choose a station", stationsList, selected = "UIC-Halsted"),

                       box( title = "Station Entries for the Year", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("halstedDays", height=200)
                       ),
                       box( title = "Station Entries by Month", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("halstedMonths", height=200)
                       ),
                       box( title = "Station Entries by Day of the Week", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("halstedDaysNamed", height=200)
                       )
                ),
                column(6,
                       selectInput("yearInput2", "Choose a year", yearsList, selected = 2021),
                       selectInput("stationInput", "Choose a station", stationsList, selected = "O'Hare"),
                       
                       box( title = "Station Entries for the Year", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("ohareDays", height=200)
                       ),
                       box( title = "Station Entries by Month", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("ohareMonths", height=200)
                       ),
                       box( title = "Station Entries by Day of the Week", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("ohareDaysNamed", height=200)
                       )
                )
              )
      ),
      
      
      tabItem(tabName = "aboutMe", # ---------------------------------------------------------------------------------
              fluidRow(
                h2("About Page"),
                p("Data given from CS424 class at UIC during Spring 2022"),
                p("Author:  Shoaib Jakvani")
              )
      )
    ),
  ),
 skin = "green"
)

# --------------------------------------------------------------
# Run once each time user visits app
# --------------------------------------------------------------
# Define server logic
server <- function(input, output) {
  
  values <- reactiveValues()
  # values$yearsToLookFor1 <- selectInput$yearInput1
  # values$yearsToLookFor2 <- selectInput$yearInput2
  values$yearsToLookFor1 <- 2021
  values$yearsToLookFor2 <- 2021
  
  # # Entries per day over 2021
  # yearToLookFor <- 2021
  daysHalsted <- reactive(subset(halstedData, year(nDate) == isolate(values$yearsToLookFor1) ))
  daysOhare <- reactive(subset(ohareData, year(nDate) == isolate(values$yearsToLookFor1) ))
  

  # output$map renderPlot({
  #   # ----------------------------------------------------------
  #   # Run each time widget changes that output$map depends on
  #   # ----------------------------------------------------------
  # })
  output$halstedYears <- renderPlot({
    # ggplot(noons, aes(x=newDate, y=S2)) + geom_point(color="blue") +  labs(title="Room Temperature in room ???", x="Day", y = "Degrees F") + geom_line()
    ggplot(halstedData, aes(x=year(nDate), y=rides))+
      labs(x="Years", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  output$ohareYears <- renderPlot({
    ggplot(ohareData, aes(x=year(nDate), y=rides))+
      labs(x="Years", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  
  
  output$halstedDays <- renderPlot({
    ggplot(isolate(daysHalsted()), aes(x=nDate, y=rides))+
      labs(x="Days", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  output$halstedMonths <- renderPlot({
    ggplot(isolate(daysHalsted()), aes(x=month(nDate, label=TRUE), y=rides))+
      labs(x="Days", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  output$halstedDaysNamed <- renderPlot({
    ggplot(isolate(daysHalsted()), aes(x=wday(nDate, label=TRUE), y=rides))+
      labs(x="Days", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  # 
  
  output$ohareDays <- renderPlot({
    ggplot(isolate(daysOhare()), aes(x=nDate, y=rides))+
      labs(x="Days", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  output$ohareMonths <- renderPlot({
    ggplot(isolate(daysOhare()), aes(x=month(nDate, label=TRUE), y=rides))+
      labs(x="Days", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  output$ohareDaysNamed <- renderPlot({
    ggplot(isolate(daysOhare()), aes(x=wday(nDate, label=TRUE), y=rides))+
      labs(x="Days", y="Amount of Entries")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

