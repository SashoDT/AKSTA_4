packages_needed <- c("shiny", "tidyverse", "plotly", "DT", "countrycode", "viridis", "jsonlite", "here")

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
    Education = expenditure,
    "Youth Unemployment Rate" = youth_unempl_rate,
    "Net Migration Rate" = net_migr_rate,
    "Electricity from Fossil Fuels" = electricity_fossil_fuel,
    "Population Growth Rate" = pop_growth_rate,
    "Life Expectancy" = life_expectancy
  )

# Prepare world map
data_map <- map_data("world") %>%
  mutate(ISO3 = countrycode(region, "country.name", "iso3c"))

data_merged <- left_join(data_map, data, by = "ISO3")

# Variables for selection
var_choices <- c("Education", "Youth Unemployment Rate", "Net Migration Rate",
                 "Population Growth Rate", "Electricity from Fossil Fuels", "Life Expectancy")

ui <- fluidPage(
  titlePanel("Global CIA Factbook Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Welcome!"),
      p("This interactive app allows you to explore global indicators from the CIA World Factbook (2020)."),
      tabsetPanel(
        tabPanel("Univariate Analysis",
                 selectInput("var_univ", "Select a variable:", choices = var_choices),
                 actionButton("view_data", "View raw data"),
                 DTOutput("raw_data")
        ),
        tabPanel("Multivariate Analysis",
                 selectInput("xvar", "X-axis variable:", choices = var_choices),
                 selectInput("yvar", "Y-axis variable:", choices = var_choices),
                 selectInput("sizevar", "Size by:", choices = c("Population", "Area"))
        )
      )
    ),
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Map",
                           plotlyOutput("map_plot")
                  ),
                  tabPanel("Global Analysis",
                           plotlyOutput("boxplot_global"),
                           plotlyOutput("hist_global")
                  ),
                  tabPanel("Analysis per Continent",
                           plotlyOutput("boxplot_cont"),
                           plotlyOutput("density_cont")
                  ),
                  tabPanel("Multivariate Plot",
                           plotlyOutput("scatter_plot")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive selection
  selected_var <- reactive({ input$var_univ })
  selected_data <- reactive({
    req(input$view_data)
    data %>% select(country, continent, !!sym(input$var_univ))
  })
  
  # Raw data output
  output$raw_data <- renderDT({
    req(input$view_data > 0)
    datatable(selected_data(), options = list(pageLength = 15))
  })
  
  # Map Plot
  output$map_plot <- renderPlotly({
    req(input$var_univ)
    gg <- ggplot(data_merged, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = !!sym(input$var_univ)), color = "white") +
      scale_fill_viridis_c(option = "D") +
      labs(fill = input$var_univ) +
      theme_void()
    ggplotly(gg, tooltip = c("region", input$var_univ))
  })
  
  # Global Boxplot
  output$boxplot_global <- renderPlotly({
    gg <- ggplot(data, aes(y = !!sym(input$var_univ))) +
      geom_boxplot() + ylab(input$var_univ)
    ggplotly(gg)
  })
  
  # Global Histogram + Density
  output$hist_global <- renderPlotly({
    gg <- ggplot(data, aes(x = !!sym(input$var_univ))) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +
      geom_density(color = "blue", size = 1.2)
    ggplotly(gg)
  })
  
  # Boxplot by Continent
  output$boxplot_cont <- renderPlotly({
    gg <- ggplot(data, aes(x = continent, y = !!sym(input$var_univ))) +
      geom_boxplot() + xlab("Continent") + ylab(input$var_univ)
    ggplotly(gg)
  })
  
  # Density by Continent
  output$density_cont <- renderPlotly({
    gg <- ggplot(data, aes(x = !!sym(input$var_univ), color = continent)) +
      geom_density() + xlab(input$var_univ)
    ggplotly(gg)
  })
  
  # Multivariate Scatter
  output$scatter_plot <- renderPlotly({
    req(input$xvar, input$yvar, input$sizevar)
    gg <- ggplot(data, aes(
      x = !!sym(input$xvar),
      y = !!sym(input$yvar),
      color = continent,
      size = !!sym(tolower(input$sizevar))
    )) +
      geom_point(alpha = 0.7) +
      geom_smooth(aes(group = continent), method = "loess", se = FALSE, inherit.aes = FALSE,
                  data = data, mapping = aes(x = !!sym(input$xvar), y = !!sym(input$yvar), color = continent)) +
      scale_size(range = c(1, 10))
    ggplotly(gg)
  })
}

shinyApp(ui, server)