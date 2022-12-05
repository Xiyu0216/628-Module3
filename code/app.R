#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinythemes)
library(bslib)
library(shinydashboard)

Chinese <- read_csv("E:/WISC/Fall 2022/STAT 628/Module3/Restaurant_Shinyapp/Chinese_restaurant_new.csv")
Korean <- read_csv("E:/WISC/Fall 2022/STAT 628/Module3/Restaurant_Shinyapp/Korean_restaurant_new.csv")

barplot <- function(df) {
  ggplot(df, aes(x=stars)) +
    geom_histogram(fill="lightblue", aes(y = after_stat(count / sum(count))),binwidth=.25) +
    scale_y_continuous(labels = scales::percent) +
    labs( x = 'Stars', y = 'Percent of Total') +
    theme_minimal()
  
}

ui <-shinyUI(
  
  navbarPage("Restaurants",
             theme = shinytheme("flatly"),
             tabPanel("Chinese Restaurants",
                      titlePanel("Restaurant Ratings"),
                      sidebarLayout( 
                        sidebarPanel(
                          
                          selectInput("wifi",
                                      "Restaurants WiFi",
                                      choices = unique(Chinese$WiFi), 
                                      multiple = FALSE),
                          selectInput(
                            inputId = "Reservations",
                            label = "Restaurants Reservations",
                            choices = unique(Chinese$RestaurantsReservations),
                            selected = NULL,
                            multiple = FALSE,
                            selectize = FALSE
                          ),
                          selectInput("Wheelchair",
                                      "Wheelchair Accessible",
                                      choices = unique(Chinese$WheelchairAccessible), 
                                      multiple = FALSE),
                          selectInput("TableService",
                                      "Restaurants Table Service",
                                      choices = unique(Chinese$RestaurantsTableService), 
                                      multiple = FALSE),
                          selectInput("DriveThru",
                                      "Drive Thru",
                                      choices = unique(Chinese$DriveThru), 
                                      multiple = FALSE)
                          
                          
                        ),
                        
                        mainPanel(
                          # Output: Tabset w/ plot, summary
                          tabsetPanel(type = "tabs",
                                      tabPanel("Plot", 
                                               h4("Restaunrant Star Ratings"),
                                               plotOutput("barplot")),
                                      tabPanel("Summary", verbatimTextOutput("summary"))
                          ) 
                        )
                        
                      )
             ),
             tabPanel("Korean Restaurants",
                      titlePanel("Restaurant Ratings"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("GoodForKids",
                                      "Good For Kids",
                                      choices = unique(Korean$GoodForKids), 
                                      multiple = FALSE),
                          selectInput(
                            inputId = "Reservations2",
                            label = "Restaurants Reservations",
                            choices = unique(Korean$RestaurantsReservations),
                            selected = NULL,
                            multiple = FALSE,
                            selectize = FALSE
                          ),
                          selectInput("GoodForGroups",
                                      "Good For Groups",
                                      choices = unique(Korean$RestaurantsGoodForGroups), 
                                      multiple = FALSE),
                          selectInput("BYOB",
                                      "BYOB",
                                      choices = unique(Korean$BYOB), 
                                      multiple = FALSE),
                          selectInput("is_open",
                                      "Open",
                                      choices = unique(Korean$is_open), 
                                      multiple = FALSE)
                          
                          
                        ),
                        
                        mainPanel(
                          # Output: Tabset w/ plot, summary
                          tabsetPanel(type = "tabs",
                                      tabPanel("Plot", 
                                               h4("Restaunrant Star Ratings"),
                                               plotOutput("barplot2")),
                                      tabPanel("Summary", verbatimTextOutput("summary2"))
                          ) 
                        )
                        
                      )
             ),
             tabPanel("More",
                      h4("GitHub: https://github.com/Xiyu0216/628-Module3"),
                      h4("For more information, please contect:"),
                      "DUOHAN ZHANG (dzhang357@wisc.edu)", br(),
                      "SHUWEI LIU (sliu777@wisc.edu)",br(),
                      "XINYAN WANG (xwang2587@wisc.edu)",br(),
                      "XIYU HAO (xhao33@wisc.edu)"
             )
  ))

server <- function(input, output) {
  # Return the requested dataset

  current_data <- reactive({
    Chinese %>%
      filter(
        WiFi %in% input$wifi,
        RestaurantsReservations %in% input$Reservations,
        WheelchairAccessible %in% input$Wheelchair,
        RestaurantsTableService %in% input$TableService,
        is_open %in% input$is_open,
        
      )
  })
  current_data2 <- reactive({
    Korean %>%
      filter(
        
        GoodForKids %in% input$GoodForKids,
        RestaurantsReservations %in% input$Reservations2,
        RestaurantsGoodForGroups %in% input$GoodForGroups,
        BYOB %in% input$BYOB,
        DriveThru %in% input$DriveThru2
        
      )
  })
  output$barplot <- renderPlot({
    barplot(current_data())}, height = 500, width = 700)
  output$barplot2 <- renderPlot({
    barplot(current_data2())}, height = 500, width = 700)
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- current_data()
    summary(dataset$stars)
  })
  output$summary2 <- renderPrint({
      dataset <- current_data2()
      summary(dataset$stars)
})
}
shinyApp(ui, server)
