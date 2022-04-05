
library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(DT)

AB_NYC_2019 <- read_csv("AB_NYC_2019.csv")

ui <- dashboardPage(
  dashboardHeader(title = "NYC Airbnb Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview"),
      menuItem("Correlations", tabName = "Correlations"),
      menuItem("Neighborhood_Group", tabName = "Neighborhood_Group"),
      menuItem("Room_Types", tabName = "Room_Types")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Overview",
              h1("Welcome to NYC Airbnb Price Analysis"),
              p("This Dataset was taken from the following source:
                  https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data" ),
              h2("Context:"),  
              p( "Since 2008, guests and hosts have used Airbnb to expand on traveling possibilities and present more unique, personalized way of experiencing the world. 
                 This dataset describes the listing activity and metrics in NYC, NY for 2019."),
              h2("Content:") ,
                p("
                This dashboard includes needed information to find out more about hosts, geographical availability, necessary metrics to make predictions and draw conclusions."
              )
                ),
      tabItem("Correlations",
              h1("Correlations"),
              p("This section is to get to know the correlation between the different metrics with the number of reviews left on the Airbnb property"),
              box(plotOutput("correlation_plot"), width = 8),
              box(
                selectInput("features", "Features:",
                            c("host_id", "minimum_nights", "prices")), width = 4 
      )
    
    ),
    tabItem("Neighborhood_Group",
            h1("Neighborhood_Group"),
            p("This Bar Chart is to explore the price changes according to the neighborhood area chosen"),
            box(plotOutput("Neighborhood_plot"), width = 10),
            box(
              selectInput("neighbourhoods", "Neighbourhoods:",
                          AB_NYC_2019$neighbourhood_group, selectize = TRUE), width = 10
            )
    ),
    tabItem("Room_Types",
            h1("Room_Types"),
            p("This Bar chart shows the price variation according to the room type chosen for NYC Airbnb"),
            box(plotOutput("Room_Types_plot"), width = 10),
                
    )
  )
)
)

server <- function(input,output){
  output$correlation_plot <- renderPlot({
    plot(AB_NYC_2019$number_of_reviews, AB_NYC_2019[[input$features]],
         xlab = "Number of Reviews", ylab = "Feature", fill = "#FF6666" ) 
    
  }) 

  output$Neighborhood_plot <- renderPlot({
  
      AB_NYC_2019 %>% 
      ggplot(aes(x = price, fill = input$neighbourhoods)) + geom_bar() +
      xlim (0,1000)+ ylim (0, 700)
  }, height = 400, width =1000)
  
  output$Room_Types_plot <- renderPlot({
    
    AB_NYC_2019 %>% 
      ggplot(aes(x = room_type, fill = price)) + geom_bar(fill = "#FF6666")
  }, height = 400, width =1000)
}

shinyApp(ui, server)