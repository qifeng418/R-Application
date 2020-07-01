#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



dashboardPage(
  dashboardHeader(title = "Airline Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Airline Overview", tabName = "overview", icon = icon("upload")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("plane")),
      menuItem("Customer Feedback", tabName = "feedback", icon = icon("comment")),
      menuItem("Application", tabName = "Application", icon = icon("comment"))
      
    )
  ),
  
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
      # First tap content ----- Airline Overview
      
      tabItem(tabName = "overview",
              fluidRow(
                column(12, tags$h2("Airline Overview")),
                # Create a new Row in the UI for selectInputs
                column(4, selectInput("select_airline_page1","Airline", c("All", unique(sort(as.character(AirlineData$airline_name)))))),
                column(4, selectInput("select_star_page1","Star Level", c("All", unique(sort(as.character(AirlineData$star)))))),
                column(4, selectInput("select_country_page1", "Country of Airline", c("All", unique(sort(as.character(AirlineData$airline_country)))))),
                column(12, DT::dataTableOutput("table"))
              )     
      ),
      
      
      
      # Second tab content ------ Rating comparison
      tabItem(tabName = "dashboard",
              fluidRow(
                column(12, tags$h2("Customer Rating Analysis")),
                column(4, selectInput("select_cabin_page2","Cabin Flown", c("All", unique(sort(as.character(AirlineData$cabin_flown)))))),
                column(4, selectInput("select_star2_page2","Star Level", c("All", unique(sort(as.character(AirlineData$star)))))),
                column(4, selectInput("select_airline_page2","Airline", c("All", unique(sort(as.character(AirlineData$airline_name))))))

                              ),     
              
                mainPanel(
                  
                  plotOutput("likert"),
                  plotOutput("likert2")
  
                )
      ),
      
      # Third tab content
      tabItem(tabName = "feedback",
              fluidRow(
                column(12, tags$h2("Customer Feedback Analysis"))
              ),     
              
              sidebarLayout(
                sidebarPanel(
                  
                  selectInput("select_cabin_page3", choices=c("All",sort(as.character(AirlineData$cabin_flown))),multiple = FALSE,"Please select cabin type",selected = "All"),
                  selectInput("select_star_page3", choices=c("All",sort(as.character(AirlineData$star))), multiple = FALSE,"Please select airline star",selected = "All"),
                  selectInput("select_seat_score_page3", choices=c("All",sort(as.character(AirlineData$seat_comfort_rating))),multiple = FALSE, "Please select seat rating",selected = "All"),
                  selectInput("select_staff_score_page3", choices=c("All",sort(as.character(AirlineData$cabin_staff_rating))), multiple = FALSE,"Please select staff rating",selected = "All"),                      
                  selectInput("select_food_score_page3", choices=c("All",sort(as.character(AirlineData$food_beverages_rating))), multiple = FALSE,"Please select food rating",selected = "All"),                      
                  selectInput("select_entertainment_score_page3", choices=c("All",sort(as.character(AirlineData$inflight_entertainment_rating))), multiple = FALSE,"Please select entertainment rating",selected = "All"),                      
                  selectInput("select_money_value_score_page3", choices=c("All",sort(as.character(AirlineData$value_money_rating))), multiple = FALSE,"Please select money value rating",selected = "All")                    
    #              column(4, selectInput("select_cabin_page3","Cabin Flown", c("All", unique(sort(as.character(AirlineData$cabin_flown)))))),
    #              column(4, selectInput("select_star2_page3","Star Level", c("All", unique(sort(as.character(AirlineData$star)))))),
    #              column(4, selectInput("select_seat_score_page3","Seat Rating", c("All", unique(sort(as.character(AirlineData$seat_comfort_rating)))))),
    #              column(4, selectInput("select_staff_score_page3","Staff Rating", c("All", unique(sort(as.character(AirlineData$cabin_staff_rating)))))),
    #              column(4, selectInput("select_food_score_page3","Food Rating", c("All", unique(sort(as.character(AirlineData$food_beverages_rating)))))),
    #              column(4, selectInput("select_entertainment_score_page3","Entertainment Rating", c("All", unique(sort(as.character(AirlineData$inflight_entertainment_rating)))))),
    #              column(4, selectInput("select_money_value_score_page3","Money Value Rating", c("All", unique(sort(as.character(AirlineData$value_money_rating))))))
                  
                  
                ),
                tabBox(
                  tabPanel("Most Frequent Words", plotOutput("Recommend_or_not") ),
                  tabPanel("Word Cloud", plotOutput("wordcloudPositive3") ),
                  tabPanel("Word Correlation", plotOutput("correlation") ),
                  tabPanel("Sentiment1", plotlyOutput("sentiment1") ),
                  tabPanel("Sentiment2", plotlyOutput("sentiment2") )
                )
              )
              
      ),
      tabItem(tabName = "Application",
              fluidRow(
                column(12, tags$h2("Customer Feedback Analysis"))
              ),     
              
              sidebarLayout(
                sidebarPanel(
                  textInput("TextInput", label="",value = "", width = NULL, placeholder = NULL)
                ),
                mainPanel(
                  plotOutput("TextOutput")
                  
                )
                
              )
              
      )
    )
  )
)
