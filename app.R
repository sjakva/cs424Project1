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
years<-c(2001:2021)

# read in data-file
halstedData <- read.table(file="./halsted.tsv", sep="\t", header=TRUE)

newDate <- as.Date(halstedData$date, "%m/%d/%Y")
halstedData$nDate <- newDate
halstedData$date <-NULL

# Entries per day over 2021
days2021 <- subset(halstedData, year(nDate) == 2021 )

# look at the data loaded
# View(halstedData)
# View(days2021)


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title="Shoaib's Project 1"),
  dashboardSidebar(disable=FALSE, collapsed=FALSE,
                   sidebarMenu(
                     id = "tabsMenu",
                     menuItem("Home", tabName = "homeTab", icon = NULL),
                     menuItem("Compare", tabName = "compareStationsTab", icon = NULL)
                     )
                   ),
  dashboardBody(
    # selectInput("stationInput", "Choose a station", c("UIC-Halsted", "[station]", "[station]")),
    tabItems(
      tabItem(tabName = "homeTab",
              fluidRow(
                column(6,
                       # "row"
                       box( title = "Station Entries Per Year", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("halstedYears", height=800)
                       )
                       # dateInput("day2021", "Enter a date:", 
                       #           format="mm-dd-yy", value="2021-01-01", 
                       #           min="2021-01-01", max="2021-12-31",
                       #           startview="month", autoclose=FALSE),
                       # box( title = "Station Entries Per Year", solidHeader=TRUE, status="success", width=12, background="purple",
                       #      plotOutput("halstedDays", height=250)
                       # )
                ),
                column(6,
                       "oharee"
                )
              )
            ),
    
    
      tabItem(tabName = "compareStationsTab",
              fluidRow(
                column(6,
                       box( title = "Station Entries for 2021", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("halstedDays", height=300)
                       ),
                       box( title = "Station Entries for 2021 by Month", solidHeader=TRUE, status="success", width=12, background="purple",
                            plotOutput("halstedMonths", height=300)
                       )
                ),
                column(6,
                       "bobo"
                )
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

  # output$map renderPlot({
  #   # ----------------------------------------------------------
  #   # Run each time widget changes that output$map depends on
  #   # ----------------------------------------------------------
  # })
  output$halstedYears <- renderPlot({
    # ggplot(noons, aes(x=newDate, y=S2)) + geom_point(color="blue") +  labs(title="Room Temperature in room ???", x="Day", y = "Degrees F") + geom_line()
    ggplot(halstedData, aes(x=year(nDate), y=rides))+
      labs(x="Years", y="Amount of Entries", title="Entries over the Years")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  output$halstedDays <- renderPlot({
    ggplot(days2021, aes(x=nDate, y=rides))+
      labs(x="Days", y="Amount of Entries", title="Entries per day over 2021")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  output$halstedMonths <- renderPlot({
    ggplot(days2021, aes(x=month(nDate, label=TRUE), y=rides))+
      labs(x="Days", y="Amount of Entries", title="Entries per day over 2021")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

