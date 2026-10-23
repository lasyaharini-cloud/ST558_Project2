library(shiny)
library(tidyverse)
library(DT)
library(shinycssloaders)

# Load data
housing <- readRDS("housing_clean.rds")
# Use full names for property types
housing <- housing |>
  mutate(
    Type = case_when(
      Type == "h" ~ "House",
      Type == "t" ~ "Townhouse",
      Type == "u" ~ "Unit / Apartment",
      TRUE ~ as.character(Type)
    )
  )

# Use full names for sale methods
housing <- housing |>
  mutate(
    Method = case_when(
      Method == "S" ~ "Sold",
      Method == "SP" ~ "Sold Prior",
      Method == "PI" ~ "Passed In",
      Method == "VB" ~ "Vendor Bid",
      Method == "SA" ~ "Sold After Auction",
      Method == "SS" ~ "Sold",
      Method == "SN" ~ "Sold Not Disclosed",
      Method == "PN" ~ "Passed In Not Disclosed",
      Method == "W" ~ "Withdrawn",
      TRUE ~ as.character(Method)
    )
  )
# Numeric variables
num_vars <- c(
  "Price" = "Price",
  "Rooms" = "Rooms",
  "Bedrooms" = "Bedroom2",
  "Bathrooms" = "Bathroom",
  "Car Spaces" = "Car",
  "Distance from City (km)" = "Distance",
  "Land Size (sq m)" = "Landsize",
  "Building Area (sq m)" = "BuildingArea",
  "Year Built" = "YearBuilt",
  "Properties in Suburb" = "Propertycount"
)

# Categorical variables
cat_vars <- c(
  "Property Type" = "Type",
  "Sale Method" = "Method",
  "Region" = "Regionname"
)
# Get the full name of a variable
get_label <- function(variable) {
  labels <- c(num_vars, cat_vars)
  names(labels)[match(variable, labels)]
}

# Starting ranges
price_range <- range(housing$Price, na.rm = TRUE)
rooms_range <- range(housing$Rooms, na.rm = TRUE)
# App layout
ui <- fluidPage(
  # Basic colors
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f9fc;
      }

      .well {
        background-color: white;
        border-radius: 8px;
      }
      h2, h3, h4 {
        color: #284d72;
      }

      .btn-primary {
        background-color: #327ba8;
        border-color: #327ba8;
      }
      .nav-tabs > li.active > a {
        color: #284d72;
        font-weight: bold;
      }
    "))
  ),
  titlePanel("Melbourne Housing Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      
      # Property type
      selectInput(
        "type", "Property Type",
        choices = c(
          "All",
          sort(unique(na.omit(housing$Type)))
        )
      ),
      # Region
      selectInput(
        "region", "Region",
        choices = c(
          "All",
          sort(unique(na.omit(housing$Regionname)))
        )
      ),
      
      # First numeric variable
      selectInput(
        "num1", "First Numeric Variable",
        choices = num_vars,
        selected = "Price"
      ),
      sliderInput(
        "range1", "Price Range",
        min = price_range[1],
        max = price_range[2],
        value = price_range
      ),
      
      # Second numeric variable
      selectInput(
        "num2", "Second Numeric Variable",
        choices = num_vars,
        selected = "Rooms"
      ),
      sliderInput(
        "range2", "Rooms Range",
        min = rooms_range[1],
        max = rooms_range[2],
        value = rooms_range
      ),
      actionButton(
        "apply", "Apply Filters",
        class = "btn-primary"
      ),
      br(),
      br(),
      tags$small(
        "Click Apply Filters to update the data."
      )
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
          p(
            "Image: Taylors Lakes, Melbourne. Photo by ",
            tags$a(
              "Graeme Bartlett",
              href = "https://commons.wikimedia.org/wiki/File:Taylors_Lakes_aerial.jpg",
              target = "_blank"
            ),
            " (CC BY-SA 3.0)."
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
            tags$li(
              "Use the sidebar to select a property type and region."
            ),
            tags$li(
              "Choose two numeric variables and adjust their ranges."
            ),
            tags$li(
              "Click Apply Filters to update the data."
            ),
            tags$li(
              "Use the Data Download tab to view and download the filtered data."
            ),
            tags$li(
              "Use the Data Exploration tab to explore tables and graphs."
            )
          ),
          hr(),
          h4("App Features"),
          tags$ul(
            tags$li(
              "Filter properties by type, region, and numeric ranges."
            ),
            tags$li(
              "View and download housing data as a CSV file."
            ),
            tags$li(
              "Explore categorical and numerical summaries."
            ),
            tags$li(
              "Create six different types of graphs."
            ),
            tags$li(
              "Customize graph variables, colors, and faceting."
            )
          ),
          p("Note: The dataset contains historical property
             sales, not homes currently listed for sale.")
        ),
        # Download tab
        tabPanel(
          "Data Download",
          h3("Housing Data"),
          p("View and download the filtered housing data."),
          textOutput("row_count"),
          DT::dataTableOutput("housing_table"),
          br(),
          downloadButton(
            "download_data", "Download CSV"
          )
        ),
        
        # Exploration tab
        tabPanel(
          "Data Exploration",
          h3("Explore Housing Data"),
          h4("Housing Summary"),
          textOutput("housing_summary"),
          hr(),
          h4("Categorical Summaries"),
          selectInput(
            "cat1", "First Categorical Variable",
            choices = cat_vars,
            selected = "Type"
          ),
          selectInput(
            "cat2", "Second Categorical Variable",
            choices = c("None" = "None", cat_vars),
            selected = "None"
          ),
          p("Choose None for a one-way table or
             another variable for a two-way table."),
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
          
          # X variable for bar charts and boxplots
          conditionalPanel(
            condition = "input.plot_type == 'Bar Chart' ||
                         input.plot_type == 'Boxplot'",
            selectInput(
              "plot_x", "X Variable",
              choices = cat_vars,
              selected = "Type"
            )
          ),
          
          # X variable for scatterplots
          conditionalPanel(
            condition = "input.plot_type == 'Scatterplot'",
            selectInput(
              "scatter_x", "X Variable",
              choices = num_vars,
              selected = "Rooms"
            )
          ),
          
          # Numeric variable for histograms and density plots
          conditionalPanel(
            condition = "input.plot_type == 'Histogram' ||
                         input.plot_type == 'Density Plot'",
            selectInput(
              "plot_num", "Numeric Variable",
              choices = num_vars,
              selected = "Price"
            )
          ),
          
          # Y variable for boxplots and scatterplots
          conditionalPanel(
            condition = "input.plot_type == 'Boxplot' ||
                         input.plot_type == 'Scatterplot'",
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
              choices = c("None" = "None", cat_vars),
              selected = "None"
            )
          ),
          
          # Facet variable
          conditionalPanel(
            condition = "input.plot_type != 'Heatmap'",
            selectInput(
              "plot_facet", "Facet By",
              choices = c("None" = "None", cat_vars),
              selected = "None"
            )
          ),
          p("Color By compares groups using different colors.
             Facet By creates separate graphs for each group."),
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
      label = paste(get_label(input$num1), "Range"),
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
      label = paste(get_label(input$num2), "Range"),
      min = min(values),
      max = max(values),
      value = range(values)
    )
  })
  
  # Do not allow the same categorical variable twice
  observeEvent(input$cat1, {
    choices2 <- cat_vars[cat_vars != input$cat1]
    updateSelectInput(
      session,
      "cat2",
      choices = c("None" = "None", choices2),
      selected = "None"
    )
  })
  
  # Store filtered data until the button is clicked
  housing_values <- reactiveValues(data = housing)
  filtered_data <- reactive({
    housing_values$data
  })
  
  # Filter data
  observeEvent(input$apply, {
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
    full1 <- range(housing[[input$num1]], na.rm = TRUE)
    
    if (input$range1[1] > full1[1] ||
        input$range1[2] < full1[2]) {
      data <- data |>
        filter(
          !is.na(.data[[input$num1]]),
          .data[[input$num1]] >= input$range1[1],
          .data[[input$num1]] <= input$range1[2]
        )
    }
    # Second numeric filter
    full2 <- range(housing[[input$num2]], na.rm = TRUE)
    
    if (input$range2[1] > full2[1] ||
        input$range2[2] < full2[2]) {
      data <- data |>
        filter(
          !is.na(.data[[input$num2]]),
          .data[[input$num2]] >= input$range2[1],
          .data[[input$num2]] <= input$range2[2]
        )
    }
    housing_values$data <- data
  })
  
  # Housing summary
  output$housing_summary <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return("No properties match the selected filters.")
    }
    
    median_price <- median(data$Price, na.rm = TRUE)
    average_distance <- mean(data$Distance, na.rm = TRUE)
    price_text <- if (is.finite(median_price)) {
      paste0(
        "$",
        format(round(median_price), big.mark = ",")
      )
    } else {
      "Not available"
    }
    
    distance_text <- if (is.finite(average_distance)) {
      paste0(round(average_distance, 1), " km")
    } else {
      "Not available"
    }
    paste(
      "Matching Properties:",
      format(nrow(data), big.mark = ","),
      "| Median Price:",
      price_text,
      "| Average Distance from City:",
      distance_text
    )
  })
  
  # Number of rows
  output$row_count <- renderText({
    paste(
      "Number of properties:",
      nrow(filtered_data())
    )
  })
  # Data table
  output$housing_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      "melbourne_housing_filtered.csv"
    },
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
  
  # Categorical summary
  output$cat_table <- renderTable({
    data <- filtered_data()
    validate(
      need(nrow(data) > 0, "No data available.")
    )
    
    # One-way table
    if (input$cat2 == "None") {
      result <- as.data.frame(
        table(data[[input$cat1]])
      )
      names(result) <- c(
        get_label(input$cat1),
        "Count"
      )
      
      # Two-way table
    } else {
      validate(
        need(
          input$cat1 != input$cat2,
          "Please choose two different variables."
        )
      )
      result <- as.data.frame(
        table(
          data[[input$cat1]],
          data[[input$cat2]]
        )
      )
      
      names(result) <- c(
        get_label(input$cat1),
        get_label(input$cat2),
        "Count"
      )
      
      # Remove combinations with zero properties
      result <- result |>
        filter(Count > 0)
    }
    
    result
  }, rownames = FALSE)
  
  # Numerical summary
  output$num_table <- renderTable({
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available.")
    )
    
    data |>
      group_by(.data[[input$summary_group]]) |>
      summarise(
        Mean = round(
          mean(.data[[input$summary_num]], na.rm = TRUE), 2
        ),
        Median = round(
          median(.data[[input$summary_num]], na.rm = TRUE), 2
        ),
        SD = round(
          sd(.data[[input$summary_num]], na.rm = TRUE), 2
        ),
        .groups = "drop"
      )
  })
  
  # Graphical summary
  output$housing_plot <- renderPlot({
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No data available for these filters."
      )
    )
    
    plot_type <- input$plot_type
    color <- input$plot_color
    facet <- input$plot_facet
    
    # Bar chart
    if (plot_type == "Bar Chart") {
      x <- input$plot_x
      
      p <- ggplot(
        data,
        aes(x = .data[[x]])
      )
      
      if (color == "None" || color == x) {
        p <- p +
          geom_bar(
            aes(fill = .data[[x]]),
            color = "white"
          )
      } else {
        p <- p +
          geom_bar(
            aes(fill = .data[[color]]),
            position = "dodge",
            color = "white"
          )
      }
      
      p <- p +
        labs(
          title = paste("Properties by", get_label(x)),
          x = get_label(x),
          y = "Number of Properties"
        )
      
      # Histogram
    } else if (plot_type == "Histogram") {
      x <- input$plot_num
      
      data <- data |>
        filter(!is.na(.data[[x]]))
      
      validate(
        need(nrow(data) > 0, "No numeric data available.")
      )
      
      p <- ggplot(
        data,
        aes(x = .data[[x]])
      )
      
      if (color == "None") {
        p <- p +
          geom_histogram(
            bins = 30,
            fill = "#4e9cc9",
            color = "white"
          )
      } else {
        p <- p +
          geom_histogram(
            aes(fill = .data[[color]]),
            bins = 30,
            alpha = 0.7,
            position = "identity"
          )
      }
      
      p <- p +
        labs(
          title = paste("Distribution of", get_label(x)),
          x = get_label(x),
          y = "Number of Properties"
        )
      
      # Boxplot
    } else if (plot_type == "Boxplot") {
      x <- input$plot_x
      y <- input$plot_y
      
      data <- data |>
        filter(!is.na(.data[[y]]))
      
      validate(
        need(nrow(data) > 0, "No numeric data available.")
      )
      
      p <- ggplot(
        data,
        aes(
          x = .data[[x]],
          y = .data[[y]]
        )
      )
      
      if (color == "None" || color == x) {
        p <- p +
          geom_boxplot(
            aes(fill = .data[[x]])
          )
      } else {
        p <- p +
          geom_boxplot(
            aes(
              fill = .data[[color]],
              group = interaction(
                .data[[x]],
                .data[[color]]
              )
            ),
            position = position_dodge(width = 0.8)
          )
      }
      
      p <- p +
        labs(
          title = paste(
            get_label(y), "by", get_label(x)
          ),
          x = get_label(x),
          y = get_label(y)
        )
      
      # Scatterplot
    } else if (plot_type == "Scatterplot") {
      x <- input$scatter_x
      y <- input$plot_y
      
      data <- data |>
        filter(
          !is.na(.data[[x]]),
          !is.na(.data[[y]])
        )
      
      validate(
        need(nrow(data) > 0, "No numeric data available.")
      )
      
      p <- ggplot(
        data,
        aes(
          x = .data[[x]],
          y = .data[[y]]
        )
      )
      
      if (color == "None") {
        p <- p +
          geom_point(
            color = "#9370db",
            alpha = 0.4
          )
      } else {
        p <- p +
          geom_point(
            aes(color = .data[[color]]),
            alpha = 0.5
          )
      }
      
      p <- p +
        labs(
          title = paste(
            get_label(y), "vs", get_label(x)
          ),
          x = get_label(x),
          y = get_label(y)
        )
      
      # Density plot
    } else if (plot_type == "Density Plot") {
      x <- input$plot_num
      
      data <- data |>
        filter(!is.na(.data[[x]]))
      
      validate(
        need(
          nrow(data) >= 2 &&
            n_distinct(data[[x]]) >= 2,
          "Not enough numeric data for a density plot."
        )
      )
      
      p <- ggplot(
        data,
        aes(x = .data[[x]])
      )
      
      if (color == "None") {
        p <- p +
          geom_density(
            fill = "#f4a261",
            alpha = 0.6
          )
      } else {
        p <- p +
          geom_density(
            aes(
              fill = .data[[color]],
              group = .data[[color]]
            ),
            alpha = 0.5,
            na.rm = TRUE
          )
      }
      
      p <- p +
        labs(
          title = paste("Density of", get_label(x)),
          x = get_label(x),
          y = "Density"
        )
      
      # Heatmap
    } else {
      avg_data <- data |>
        filter(
          !is.na(Price),
          !is.na(Type),
          !is.na(Regionname)
        ) |>
        group_by(Type, Regionname) |>
        summarise(
          AvgPrice = mean(Price),
          .groups = "drop"
        )
      
      validate(
        need(
          nrow(avg_data) > 0,
          "No price data available."
        )
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
        scale_fill_gradient(
          low = "#a8dadc",
          high = "#457b9d",
          labels = scales::label_dollar()
        ) +
        labs(
          title = "Average Housing Price by Type and Region",
          x = "Property Type",
          y = "Region",
          fill = "Average Price"
        )
    }
    
    # Add facets and categorical colors
    if (plot_type != "Heatmap") {
      if (facet != "None") {
        p <- p +
          facet_wrap(
            vars(.data[[facet]])
          )
      }
      
      p <- p +
        scale_fill_brewer(palette = "Set2") +
        scale_color_brewer(palette = "Set2")
    }
    
    # Basic graph theme
    p +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(
          face = "bold",
          color = "#284d72"
        ),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })
}

# Run app
shinyApp(ui = ui, server = server)
