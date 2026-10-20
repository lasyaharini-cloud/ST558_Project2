
library(shiny)
library(tidyverse)
library(DT)

# Load data
housing <- readRDS("housing_clean.rds")

# App layout
ui <- fluidPage(
  
  titlePanel("Melbourne Housing Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filters"),
      
      # Property type
      selectInput(
        "type",
        "Property Type",
        choices = c("All", sort(unique(housing$Type))),
        selected = "All"
      ),
      
      # Region
      selectInput(
        "region",
        "Region",
        choices = c("All", sort(unique(housing$Regionname))),
        selected = "All"
      ),
      
      # First numeric variable
      selectInput(
        "num1",
        "First Numeric Variable",
        choices = c("Price", "Rooms", "Distance", "Landsize"),
        selected = "Price"
      ),
      
      # First range
      sliderInput(
        "range1",
        "First Range",
        min = 0,
        max = 10000000,
        value = c(0, 10000000)
      ),
      
      # Second numeric variable
      selectInput(
        "num2",
        "Second Numeric Variable",
        choices = c("Price", "Rooms", "Distance", "Landsize"),
        selected = "Rooms"
      ),
      
      # Second range
      sliderInput(
        "range2",
        "Second Range",
        min = 0,
        max = 20,
        value = c(0, 20)
      ),
      
      # Apply button
      actionButton("apply", "Apply Filters")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # About tab
        tabPanel(
          "About",
          
          h3("About This App"),
          
          p("This app explores the Melbourne Housing dataset.
             It allows users to look at housing prices,
             property types, and different regions."),
          
          p("Use the sidebar to select property types,
             regions, and numeric ranges."),
          
          p("The Data Download tab will allow users to
             view and download filtered data."),
          
          p("The Data Exploration tab will allow users
             to explore tables and graphs."),
          
          tags$a(
            "Melbourne Housing Dataset on Kaggle",
            href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
            target = "_blank"
          )
        ),
        
        # Download tab
        tabPanel(
          "Data Download",
          
          h3("Housing Data"),
          
          p("The filtered data table and download option
             will be added here.")
        ),
        
        # Exploration tab
        tabPanel(
          "Data Exploration",
          
          h3("Explore Housing Data"),
          
          p("Tables and plots will be added here.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
}

# Run app
shinyApp(ui = ui, server = server)
