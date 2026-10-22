
library(shiny)
library(tidyverse)
library(DT)
library(shinycssloaders)

# Load data
housing <- readRDS("housing_clean.rds")

# Variables
num_vars <- c("Price", "Rooms", "Distance", "Landsize")
cat_vars <- c("Type", "Regionname", "Method")

# App layout
ui <- fluidPage(
  
  titlePanel("Melbourne Housing Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filters"),
      
      # Property type
      selectInput(
        "type", "Property Type",
        choices = c("All", sort(unique(housing$Type)))
      ),
      
      # Region
      selectInput(
        "region", "Region",
        choices = c("All", sort(unique(housing$Regionname)))
      ),
      
      # First numeric variable
      selectInput(
        "num1", "First Numeric Variable",
        choices = num_vars,
        selected = "Price"
      ),
      
      sliderInput(
        "range1", "Price Range",
        min = min(housing$Price, na.rm = TRUE),
        max = max(housing$Price, na.rm = TRUE),
        value = range(housing$Price, na.rm = TRUE)
      ),
      
      # Second numeric variable
      selectInput(
        "num2", "Second Numeric Variable",
        choices = num_vars,
        selected = "Rooms"
      ),
      
      sliderInput(
        "range2", "Rooms Range",
        min = min(housing$Rooms, na.rm = TRUE),
        max = max(housing$Rooms, na.rm = TRUE),
        value = range(housing$Rooms, na.rm = TRUE)
      ),
      
      actionButton("apply", "Apply Filters")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # About tab
        tabPanel(
          "About",
          
          h3("Melbourne Housing Explorer"),
          
          p("Welcome to the Melbourne Housing Explorer!
             This app helps users explore housing data
             from Melbourne, Australia."),
          
          # Housing image
          tags$img(
            src = "melbourne_house.jpg",
            width = "100%",
            style = "max-width: 650px; border-radius: 8px;"
          ),
          
          br(),
          br(),
          
          h4("About the Dataset"),
          
          p("The Melbourne Housing dataset contains
             information about properties in Melbourne,
             including prices, number of rooms,
             property types, land sizes, and locations."),
          
          p("The dataset was obtained from Kaggle."),
          
          tags$a(
            "Melbourne Housing Dataset - Kaggle",
            href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
            target = "_blank"
          ),
          
          hr(),
          
          h4("How to Use the App"),
          
          tags$ol(
            tags$li("Use the sidebar to select a property type and region."),
            tags$li("Choose two numeric variables and adjust their ranges."),
            tags$li("Click Apply Filters to update the data."),
            tags$li("Use the Data Download tab to view and download the filtered data."),
            tags$li("Use the Data Exploration tab to explore tables and graphs.")
          ),
          
          hr(),
          
          h4("App Features"),
          
          tags$ul(
            tags$li("Filter properties by type, region, and numeric ranges."),
            tags$li("View and download housing data as a CSV file."),
            tags$li("Explore categorical and numerical summaries."),
            tags$li("Create six different types of graphs."),
            tags$li("Customize graph variables, colors, and faceting.")
          )
        ),
        
        # Download tab
        tabPanel(
          "Data Download",
          
          h3("Housing Data"),
          
          p("View and download the filtered housing data."),
          
          textOutput("row_count"),
          
          DTOutput("housing_table"),
          
          br(),
          
          downloadButton("download_data", "Download CSV")
        ),
        
        # Exploration tab
        tabPanel(
          "Data Exploration",
          
          h3("Explore Housing Data"),
          
          h4("Categorical Summaries"),
          
          selectInput(
            "cat1", "First Categorical Variable",
            choices = cat_vars,
            selected = "Type"
          ),
          
          selectInput(
            "cat2", "Second Categorical Variable",
            choices = c("None", cat_vars),
            selected = "None"
          ),
          
          tableOutput("cat_table"),
          
          hr(),
          
          h4("Numerical Summaries"),
          
          selectInput(
            "summary_num", "Numeric Variable",
            choices = num_vars,
            selected = "Price"
          ),
          
          selectInput(
            "summary_group", "Group By",
            choices = cat_vars,
            selected = "Type"
          ),
          
          tableOutput("num_table"),
          
          hr(),
          
          h4("Graphical Summaries"),
          
          # Plot type
          selectInput(
            "plot_type", "Choose Plot",
            choices = c(
              "Bar Chart",
              "Histogram",
              "Boxplot",
              "Scatterplot",
              "Density Plot",
              "Heatmap"
            ),
            selected = "Bar Chart"
          ),
          
          # X variable
          conditionalPanel(
            condition = "input.plot_type == 'Bar Chart' ||
                         input.plot_type == 'Boxplot' ||
                         input.plot_type == 'Scatterplot'",
            
            selectInput(
              "plot_x", "X Variable",
              choices = cat_vars,
              selected = "Type"
            )
          ),
          
          # Y variable
          conditionalPanel(
            condition = "input.plot_type != 'Bar Chart' &&
                         input.plot_type != 'Heatmap'",
            
            selectInput(
              "plot_y", "Y Variable",
              choices = num_vars,
              selected = "Price"
            )
          ),
          
          # Color variable
          conditionalPanel(
            condition = "input.plot_type != 'Heatmap'",
            
            selectInput(
              "plot_color", "Color By",
              choices = c("None", cat_vars),
              selected = "None"
            )
          ),
          
          # Facet variable
          conditionalPanel(
            condition = "input.plot_type != 'Heatmap'",
            
            selectInput(
              "plot_facet", "Facet By",
              choices = c("None", cat_vars),
              selected = "None"
            )
          ),
          
          # Loading spinner
          withSpinner(
            plotOutput("housing_plot", height = "450px")
          )
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
      session, "range1",
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
      session, "range2",
      label = paste(input$num2, "Range"),
      min = min(values),
      max = max(values),
      value = range(values)
    )
  })
  
  # Filter data
  filtered_data <- eventReactive(input$apply, {
    
    data <- housing
    
    if (input$type != "All") {
      data <- data |>
        filter(Type == input$type)
    }
    
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
  
  # Data table
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
    
    validate(
      need(nrow(data) > 0, "No data available.")
    )
    
    if (input$cat2 == "None") {
      table(data[[input$cat1]])
    } else {
      table(data[[input$cat1]], data[[input$cat2]])
    }
    
  }, rownames = TRUE)
  
  # Numerical summary
  output$num_table <- renderTable({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available.")
    )
    
    data |>
      group_by(.data[[input$summary_group]]) |>
      summarise(
        Mean = mean(.data[[input$summary_num]], na.rm = TRUE),
        Median = median(.data[[input$summary_num]], na.rm = TRUE),
        SD = sd(.data[[input$summary_num]], na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Update X variable choices
  observeEvent(input$plot_type, {
    
    if (input$plot_type == "Scatterplot") {
      
      updateSelectInput(
        session, "plot_x",
        choices = num_vars,
        selected = "Rooms"
      )
      
    } else {
      
      updateSelectInput(
        session, "plot_x",
        choices = cat_vars,
        selected = "Type"
      )
    }
  })
  
  # Graphical summary
  output$housing_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0,
           "No data available for these filters.")
    )
    
    # Bar chart
    if (input$plot_type == "Bar Chart") {
      
      p <- ggplot(
        data,
        aes(x = .data[[input$plot_x]])
      ) +
        geom_bar(fill = "steelblue")
      
      # Histogram
    } else if (input$plot_type == "Histogram") {
      
      data <- data |>
        filter(!is.na(.data[[input$plot_y]]))
      
      validate(
        need(nrow(data) > 0, "No numeric data available.")
      )
      
      p <- ggplot(
        data,
        aes(x = .data[[input$plot_y]])
      ) +
        geom_histogram(
          bins = 30,
          fill = "steelblue"
        )
      
      # Boxplot
    } else if (input$plot_type == "Boxplot") {
      
      data <- data |>
        filter(!is.na(.data[[input$plot_y]]))
      
      validate(
        need(nrow(data) > 0, "No numeric data available.")
      )
      
      p <- ggplot(
        data,
        aes(
          x = .data[[input$plot_x]],
          y = .data[[input$plot_y]]
        )
      ) +
        geom_boxplot(fill = "lightblue")
      
      # Scatterplot
    } else if (input$plot_type == "Scatterplot") {
      
      data <- data |>
        filter(
          !is.na(.data[[input$plot_x]]),
          !is.na(.data[[input$plot_y]])
        )
      
      validate(
        need(nrow(data) > 0, "No numeric data available.")
      )
      
      p <- ggplot(
        data,
        aes(
          x = .data[[input$plot_x]],
          y = .data[[input$plot_y]]
        )
      ) +
        geom_point(alpha = 0.4)
      
      # Density plot
    } else if (input$plot_type == "Density Plot") {
      
      data <- data |>
        filter(!is.na(.data[[input$plot_y]]))
      
      validate(
        need(
          nrow(data) >= 2 &&
            length(unique(data[[input$plot_y]])) >= 2,
          "Not enough numeric data for a density plot."
        )
      )
      
      p <- ggplot(
        data,
        aes(x = .data[[input$plot_y]])
      ) +
        geom_density(
          fill = "lightblue",
          alpha = 0.5
        )
      
      # Heatmap
    } else {
      
      # Average price by type and region
      avg_data <- data |>
        filter(!is.na(Price)) |>
        group_by(Type, Regionname) |>
        summarise(
          AvgPrice = mean(Price),
          .groups = "drop"
        )
      
      validate(
        need(nrow(avg_data) > 0,
             "No price data available.")
      )
      
      p <- ggplot(
        avg_data,
        aes(
          x = Type,
          y = Regionname,
          fill = AvgPrice
        )
      ) +
        geom_tile(color = "white") +
        labs(
          title = "Average Housing Price by Type and Region",
          x = "Property Type",
          y = "Region",
          fill = "Average Price"
        )
    }
    
    # Add colors
    if (input$plot_type != "Heatmap" &&
        input$plot_color != "None") {
      
      if (input$plot_type %in%
          c("Bar Chart", "Histogram",
            "Boxplot", "Density Plot")) {
        
        p <- p +
          aes(fill = .data[[input$plot_color]])
        
      } else {
        
        p <- p +
          aes(color = .data[[input$plot_color]])
      }
    }
    
    # Add facets
    if (input$plot_type != "Heatmap" &&
        input$plot_facet != "None") {
      
      p <- p +
        facet_wrap(vars(.data[[input$plot_facet]]))
    }
    
    # Plot labels
    if (input$plot_type != "Heatmap") {
      
      x_label <- if (input$plot_type %in%
                     c("Histogram", "Density Plot")) {
        input$plot_y
      } else {
        input$plot_x
      }
      
      y_label <- if (input$plot_type == "Density Plot") {
        "Density"
      } else if (input$plot_type %in%
                 c("Bar Chart", "Histogram")) {
        "Count"
      } else {
        input$plot_y
      }
      
      p <- p +
        labs(
          title = input$plot_type,
          x = x_label,
          y = y_label
        )
    }
    
    p + theme_minimal()
    
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
