
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

# Server
server <- function(input, output, session) {
  
  # Update first slider
  observeEvent(input$num1, {
    values <- housing[[input$num1]]
    values <- values[!is.na(values)]
    
    updateSliderInput(
      session, "range1",
      label = paste(input$num1, "Range"),
      min = min(values),
      max = max(values),
      value = c(min(values), max(values))
    )
  })
  
  # Update second slider
  observeEvent(input$num2, {
    values <- housing[[input$num2]]
    values <- values[!is.na(values)]
    
    updateSliderInput(
      session, "range2",
      label = paste(input$num2, "Range"),
      min = min(values),
      max = max(values),
      value = c(min(values), max(values))
    )
  })
  
  # Apply filters when button is clicked
  filtered_data <- eventReactive(input$apply, {
    
    data <- housing
    
    # Filter property type
    if (input$type != "All") {
      data <- data |>
        filter(Type == input$type)
    }
    
    # Filter region
    if (input$region != "All") {
      data <- data |>
        filter(Regionname == input$region)
    }
    
    # Filter first numeric variable
    data <- data |>
      filter(
        !is.na(.data[[input$num1]]),
        .data[[input$num1]] >= input$range1[1],
        .data[[input$num1]] <= input$range1[2]
      )
    
    # Filter second numeric variable
    data <- data |>
      filter(
        !is.na(.data[[input$num2]]),
        .data[[input$num2]] >= input$range2[1],
        .data[[input$num2]] <= input$range2[2]
      )
    
    data
  }, ignoreNULL = FALSE)
  
}

# Run app
shinyApp(ui = ui, server = server)
