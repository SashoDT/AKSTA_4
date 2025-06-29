packages_needed <- c("shiny", "tidyverse", "plotly", "DT", "countrycode", "viridis", "jsonlite", "here", "maps")

for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(countrycode)
library(viridis)
library(ggplot2)
library(jsonlite)
library(here)

# Load and clean data
data <- fromJSON(here("data_cia2.json"))
data <- data %>%
  rename(
    "Expenditure on education" = expenditure,
    "Youth Unemployment Rate" = youth_unempl_rate,
    "Net Migration Rate" = net_migr_rate,
    "Electricity from Fossil Fuels" = electricity_fossil_fuel,
    "Population Growth Rate" = pop_growth_rate,
    "Life Expectancy" = life_expectancy,
    "Continent" = continent,
    "Population" = population, 
    "Area" = area
  )

# Prepare world map
data_map <- map_data("world") %>%
  mutate(ISO3 = countrycode(region, "country.name", "iso3c"))

data_merged <- left_join(data_map, data, by = "ISO3")

# Variables for selection
var_choices <- c("Expenditure on education", "Youth Unemployment Rate", "Net Migration Rate",
                 "Population Growth Rate", "Electricity from Fossil Fuels", "Life Expectancy")

ui <- fluidPage(
  titlePanel("Global CIA Factbook Explorer"),
  p("Welcome! This interactive app allows you to visualize variables from the CIA World Factbook (2020), generate descriptive statistics and statistical graphics"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "analysis_tab",
                  tabPanel("Univariate Analysis",
                           value = "univ",
                           selectInput("var_univ", "Select a variable:", choices = var_choices),
                           actionButton("view_data", "View raw data"),
                           DTOutput("raw_data")
                  ),
                  tabPanel("Multivariate Analysis",
                           value = "multivar", 
                           selectInput("xvar", "X-axis variable:", choices = var_choices),
                           selectInput("yvar", "Y-axis variable:", choices = var_choices),
                           selectInput("sizevar", "Size by:", choices = c("Population", "Area"))
                  )
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.analysis_tab == 'univ'",
        tabsetPanel(
          tabPanel("Map",
                   p("The map contains values of the selected variables. The countries with gray areas have a missing value for the visualized variable."),
                   plotlyOutput("map_plot")),
          tabPanel("Global Analysis", 
                   plotlyOutput("boxplot_global"),
                   plotlyOutput("hist_global")),
          tabPanel("Analysis per Continent", 
                   plotlyOutput("boxplot_cont"),
                   plotlyOutput("density_cont"))
        )
      ),
      conditionalPanel(
        condition = "input.analysis_tab == 'multivar'",
          tabPanel("Multivariate Plot",
                    plotlyOutput("scatter_plot"))
      )
    )
  )
)


server <- function(input, output, session) {
  # Reactive selection
  selected_var <- reactive({ input$var_univ })
  selected_data <- reactive({
    req(input$view_data)
    data %>% select(country, Continent, !!sym(input$var_univ))
  })
  
  # Raw data output
  output$raw_data <- renderDT({
    req(input$view_data > 0)
    datatable(selected_data(), options = list(pageLength = 15))
  })
  
  # Map Plot
  output$map_plot <- renderPlotly({
    req(input$var_univ)
    
    # Determine the country name column
    country_col <- if ("region" %in% colnames(data_merged)) "region" else if ("country" %in% colnames(data_merged)) "country" else NULL
    req(country_col, "No country name column found in data_merged")
    
    gg <- ggplot(data_merged, aes(x = long, y = lat, group = group,text = paste("Country:", region))) +
      geom_polygon(aes(fill = !!sym(input$var_univ)), color = "white") +
      scale_fill_viridis_c(option = "D") +
      labs(fill = input$var_univ) +
      theme_void()
    ggplotly(gg, tooltip = c("text", "fill")) 
  })
  
  # Global Boxplot
  output$boxplot_global <- renderPlotly({
    filtered_data <- data %>% filter(!is.na(.data[[input$var_univ]]))
    gg <- ggplot(filtered_data, aes(y = .data[[input$var_univ]])) +
      geom_boxplot() + ylab(input$var_univ)
    ggplotly(gg)
  })
  
  # Global Histogram + Density
  output$hist_global <- renderPlotly({
    filtered_data <- data %>%
      filter(!is.na(.data[[input$var_univ]]),
             is.finite(.data[[input$var_univ]]))
    gg <- ggplot(filtered_data, aes(x = !!sym(input$var_univ))) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +
      geom_density(color = "blue", size = 1.2)
    ggplotly(gg)
  })
  
  # Boxplot by Continent
  output$boxplot_cont <- renderPlotly({
    filtered_data <- data %>%
      filter(!is.na(.data[[input$var_univ]]), is.finite(.data[[input$var_univ]]))
    gg <- ggplot(filtered_data, aes(x = Continent, y = !!sym(input$var_univ))) +
      geom_boxplot() + xlab("Continent") + ylab(input$var_univ)
    ggplotly(gg)
  })
  
  # Density by Continent
  output$density_cont <- renderPlotly({
    filtered_data <- data %>%
      filter(!is.na(.data[[input$var_univ]]), is.finite(.data[[input$var_univ]]))
    gg <- ggplot(filtered_data, aes(x = !!sym(input$var_univ), color = Continent)) +
      geom_density() + xlab(input$var_univ)
    ggplotly(gg)
  })
  
  # Multivariate Scatter
  output$scatter_plot <- renderPlotly({
    req(input$xvar, input$yvar, input$sizevar)
    
    filtered_data <- data %>%
      filter(!is.na(.data[[input$xvar]]),
             !is.na(.data[[input$yvar]]))
    
    size_var <- input$sizevar
    
    gg <- ggplot(filtered_data, aes(
      x = .data[[input$xvar]],
      y = .data[[input$yvar]],
      color = .data[["Continent"]],
      text = paste("Country:", country),
      size = .data[[size_var]]
    )) +
      geom_point(alpha = 0.7) +
      
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        data = filtered_data,
        mapping = aes(
          x = .data[[input$xvar]],
          y = .data[[input$yvar]],
          color = .data[["Continent"]]
        ),
        se = FALSE,
        inherit.aes = FALSE
      ) +
      
      theme_minimal() +
      scale_size(range = c(1, 10)) +
      labs(
        title = "Scatterplot",
        x = input$xvar,
        y = input$yvar
      )
    
    ggplotly(gg, tooltip = c("x", "y", "color", "size", "text"))
  })
}

shinyApp(ui, server)
