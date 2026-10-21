
library(shiny)
library(tidyverse)
library(DT)

# Load data
housing <- readRDS("housing_clean.rds")

# Numeric variables
num_vars <- c("Price", "Rooms", "Distance", "Landsize")

# Categorical variables
cat_vars <- c("Type", "Regionname", "Method")

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
        choices = num_vars,
        selected = "Price"
      ),
      
      # First range
      sliderInput(
        "range1",
        "Price Range",
        min = min(housing$Price, na.rm = TRUE),
        max = max(housing$Price, na.rm = TRUE),
        value = range(housing$Price, na.rm = TRUE)
      ),
      
      # Second numeric variable
      selectInput(
        "num2",
        "Second Numeric Variable",
        choices = num_vars,
        selected = "Rooms"
      ),
      
      # Second range
      sliderInput(
        "range2",
        "Rooms Range",
        min = min(housing$Rooms, na.rm = TRUE),
        max = max(housing$Rooms, na.rm = TRUE),
        value = range(housing$Rooms, na.rm = TRUE)
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
             regions, and numeric ranges.
             Click Apply Filters to update the data."),
          
          p("The Data Download tab allows users to
             view and download filtered data."),
          
          p("The Data Exploration tab allows users to
             explore tables and graphs."),
          
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
          
          p("View and download the filtered housing data."),
          
          # Number of rows
          textOutput("row_count"),
          
          # Data table
          DTOutput("housing_table"),
          
          br(),
          
          # Download button
          downloadButton("download_data", "Download CSV")
        ),
        
        # Exploration tab
        tabPanel(
          "Data Exploration",
          
          h3("Explore Housing Data"),
          
          h4("Categorical Summaries"),
          
          # First category
          selectInput(
            "cat1",
            "First Categorical Variable",
            choices = cat_vars,
            selected = "Type"
          ),
          
          # Second category
          selectInput(
            "cat2",
            "Second Categorical Variable",
            choices = c("None", cat_vars),
            selected = "None"
          ),
          
          # Categorical table
          tableOutput("cat_table"),
          
          hr(),
          
          h4("Numerical Summaries"),
          
          # Numeric variable
          selectInput(
            "summary_num",
            "Numeric Variable",
            choices = num_vars,
            selected = "Price"
          ),
          
          # Grouping variable
          selectInput(
            "summary_group",
            "Group By",
            choices = cat_vars,
            selected = "Type"
          ),
          
          # Numerical table
          tableOutput("num_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update first slider
  observeEvent(input$num1, {
    
    values <- housing[[input$num1]]
    values <- values[!is.na(values)]
    
    updateSliderInput(
      session,
      "range1",
      label = paste(input$num1, "Range"),
      min = min(values),
      max = max(values),
      value = range(values)
    )
  })
  
  # Update second slider
  observeEvent(input$num2, {
    
    values <- housing[[input$num2]]
    values <- values[!is.na(values)]
    
    updateSliderInput(
      session,
      "range2",
      label = paste(input$num2, "Range"),
      min = min(values),
      max = max(values),
      value = range(values)
    )
  })
  
  # Filter data
  filtered_data <- eventReactive(input$apply, {
    
    data <- housing
    
    # Filter type
    if (input$type != "All") {
      data <- data |>
        filter(Type == input$type)
    }
    
    # Filter region
    if (input$region != "All") {
      data <- data |>
        filter(Regionname == input$region)
    }
    
    # First numeric filter
    data <- data |>
      filter(
        !is.na(.data[[input$num1]]),
        .data[[input$num1]] >= input$range1[1],
        .data[[input$num1]] <= input$range1[2]
      )
    
    # Second numeric filter
    data <- data |>
      filter(
        !is.na(.data[[input$num2]]),
        .data[[input$num2]] >= input$range2[1],
        .data[[input$num2]] <= input$range2[2]
      )
    
    data
    
  }, ignoreNULL = FALSE)
  
  # Number of rows
  output$row_count <- renderText({
    paste("Number of properties:", nrow(filtered_data()))
  })
  
  # Show data table
  output$housing_table <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # Download data
  output$download_data <- downloadHandler(
    
    filename = function() {
      "melbourne_housing_filtered.csv"
    },
    
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Categorical summary
  output$cat_table <- renderTable({
    
    data <- filtered_data()
    
    if (input$cat2 == "None") {
      
      # One-way table
      table(data[[input$cat1]])
      
    } else {
      
      # Two-way table
      table(data[[input$cat1]], data[[input$cat2]])
      
    }
    
  }, rownames = TRUE)
  
  # Numerical summary
  output$num_table <- renderTable({
    
    # Grouped summary
    filtered_data() |>
      group_by(.data[[input$summary_group]]) |>
      summarise(
        Mean = mean(.data[[input$summary_num]], na.rm = TRUE),
        Median = median(.data[[input$summary_num]], na.rm = TRUE),
        SD = sd(.data[[input$summary_num]], na.rm = TRUE),
        .groups = "drop"
      )
    
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
