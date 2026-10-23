# ST 558 Project 2 – Melbourne Housing Explorer

## About the Project

This project uses the Melbourne Housing dataset to explore housing prices and property information in Melbourne, Australia. It includes an interactive R Shiny app and a separate exploratory analysis using Quarto.
Website URL: https://lasyaharini.shinyapps.io/ST558_Project2/ 
## Dataset

The dataset contains information about property prices, rooms, property types, regions, land sizes and distances.

**Source:** [Melbourne Housing Dataset – Kaggle](https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market)

## Shiny App Features

The app includes three main tabs:

- **About:** Provides information about the dataset and instructions for using the app.
- **Data Download:** Displays a searchable table and allows users to download filtered data as a CSV file.
- **Data Exploration:** Includes categorical frequency tables, grouped numerical summaries, and six types of graphs.

Users can filter the data by property type, region, and two numerical ranges. The graphs include bar charts, histograms, boxplots, scatterplots, density plots, and heatmaps, with options for colors and faceting.

## Exploratory Analysis

The Quarto analysis includes frequency tables, grouped numerical summaries and seven graphs exploring relationships between housing variables.

## Project Files

- `app.R` – Shiny app code
- `exploration.qmd` – Exploratory analysis
- `exploration.html` – Rendered analysis
- `housing_clean.rds` – Cleaned dataset
- `Melbourne_housing_FULL.csv` – Original dataset
- `www/melbourne_house.jpg` – About tab image

## Running the App

Open `app.R` in RStudio and click **Run App**. The required packages are `shiny`, `tidyverse`, `DT` and `shinycssloaders`.

## Links

**GitHub:** https://github.com/lasyaharini-cloud/ST558_Project2

**Shiny App:** To be added after deployment.
