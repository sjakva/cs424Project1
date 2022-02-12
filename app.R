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

# read in data-file
halstedData <- read.table(file="./halsted.tsv", sep="\t", header=TRUE)

# convert the dates to internal format and remove the original dates
# newDates <- as.Date(evl2006$Date, "%m/%d/%Y")
# evl2006$newDate<-newDates
# evl2006$Date <- NULL
nDate <- as.Date(halstedData$date, "%m/%d/%Y")
halstedData$nDate<-nDate
halstedData$date<-NULL

# look at the data loaded
# View(halstedData)


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title="Shoaib's Project 1"),
  dashboardSidebar(disable=FALSE, collapsed=FALSE,
                   sidebarMenu(
                     menuItem("Tab 1", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Tab 2", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Tab 3", tabName = "cheapBlankSpace", icon = NULL)
                     )
                   ),
  dashboardBody(
    selectInput("stationInput", "Choose a station", c("UIC-Halsted", "[station]", "[station]")),
    fluidRow(
      # "row"
      box( title = "Station Entries Per Year", solidHeader=TRUE, status="success", width=12, background="purple",
           plotOutput("newPlot", height=250)
      )
    )
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
  output$newPlot <- renderPlot({
    # ggplot(noons, aes(x=newDate, y=S2)) + geom_point(color="blue") +  labs(title="Room Temperature in room ???", x="Day", y = "Degrees F") + geom_line()
    ggplot(halstedData, aes(x=year(nDate), y=rides))+
      labs(x="Years", y="Amount of Entries", title="Entries over the Years")+
      geom_bar(stat="identity", fill="palegreen")+
      theme_dark()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

