library(shiny)
library(dplyr)
library(readr)
library(shinydashboard)
setwd("~/zrc/HHS_CostAnalysis/data")

#LOAD DATA 
hosp_cost <- read.csv("HospitalCosts.csv")



#DEFINE UI
ui <- dashboardPage(
  dashboardHeader( title="Hospital Cost Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("correlation_plot"), width=8),
    box(
      selectInput("Features", "Features:",
                  c( "AGE")), width=4
    ),
    box(selectInput("Features", "Features:", 
                    c("RACE")), width=4
        )
  )
)

server <- function(input, output){
  output$correlation_plot <- renderPlot({
  hist(hosp_cost$AGE, main="Cost Distribution by Age")
    hist(hosp_cost$RACE, main="Cost Distribution by Race")
    #hist(hosp_cost$FEMALE, main="Cost Distribution by Gender")
    
    
  })
  
}

shinyApp(ui,server)


