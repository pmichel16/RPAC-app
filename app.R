#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(httr)
library(XML)

#The url of the schedules
url<-'https://recsports.osu.edu/schedule.aspx'

#Use GET to get the html format of the website, and the pointer to HTMLInternalDocument.
htmlURL<- GET(url)
doc <- htmlParse(htmlURL,asText = TRUE)

#Get the content out of getURL, and convert it from raw form to text. Then get the table of times.
htmlText <- htmlParse(rawToChar(htmlURL$content))
times <- readHTMLTable(htmlText)

#Get the names of the facilities, stored in 3rd level headers on the page. 
root <- xmlRoot(doc)
facility.names <- xpathSApply(root,"//h3",xmlValue)
facility.numberOrig <- length(facility.names)
facility.number <- facility.numberOrig


#Reduce the number by the ones with a title but no reservations. 
facility.hasReservations = c()
for(i in 1:facility.numberOrig) {
  if(!is.null(times[[i]][[3]])){
    facility.hasReservations <-  c(facility.hasReservations,i)
  } else {
    facility.number = facility.number - 1
  } 
}

rooms <- list()
for(i in 1:facility.number) {
  index <- facility.hasReservations[i]
      rooms[[index]] <- unique(times[[index]][[3]])
      rooms[[index]] <- sort(rooms[[index]])
}


#Create the mainframe for the app
shinyApp(
  
  #Aesthetic effects
  ui = dashboardPage(
    
    #Application title
    dashboardHeader(title = "RPAC Facility Schedule"),
    
    dashboardSidebar(
      selectInput("facility","Choose facility:",choices = facility.names),
      uiOutput("roomSelector")
    ),
    
      
    #Body
    dashboardBody(
      fluidRow(
        box(tableOutput("scheduleTable"))
      )
    )
  ),
  
  #Server for the app
  server = function(input,output) {
    
    #Output the ui to select room in facility.
    output$roomSelector <- renderUI({
      checkboxGroupInput("rooms","Choose room:",
                  choices = rooms[[
                    which(facility.names==input$facility)
                  ]]
      )
    })
    
    output$scheduleTable <- renderTable(
      #Show the time table for the selected facility
      times[[
        which(facility.names==input$facility)
      ]]
      
      #Only show the times for the selected room.
      [
          which(times[[
            which(facility.names==input$facility)
          ]][[3]] %in% 
            input$rooms),
      ]
      
      , colnames = FALSE
    )
    
  }
)

