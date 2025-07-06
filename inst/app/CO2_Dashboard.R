library(shiny)
library(shinydashboard)
library(ggplot2)

# /////////////////////////////////////////////////////////////////////////////////////////////////

# [Data Preparations]

emissions_data_path <- file.path("..", "..", "data-raw", "interim", "asean_co2_emissions.csv")
emissions_data <- read.csv(emissions_data_path)

# Preprocessing
emissions_data$Year <- as.integer(emissions_data$Year)

# /////////////////////////////////////////////////////////////////////////////////////////////////

# [UI]

ui <- dashboardPage(
    dashboardHeader(title = "ASEAN CO2 Emissions Dashboard"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
            menuItem("Explore", tabName = "explore", icon = icon("industry")),
            menuItem("Docs", tabName = "docs", icon = icon("industry"))
        ),
        selectInput("country", "Select Country:",
                    choices = unique(emissions_data$country),
                    selected = "Philippines",
                    multiple = TRUE),

        selectInput("sector", "Select Sector:",
                    choices = unique(emissions_data$Sector),
                    selected = unique(emissions_data$Sector)[1],
                    multiple = FALSE),

        sliderInput("year", "Select Year Range:",
                    min = min(emissions_data$Year, na.rm = TRUE),
                    max = max(emissions_data$Year, na.rm = TRUE),
                    value = c(min(emissions_data$Year, na.rm = TRUE), max(emissions_data$Year, na.rm = TRUE)),
                    step = 1,
                    sep = "")
    ),

    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",
                fluidRow(
                    box(title = "CO2 Emissions Over Time", width = 12, status = "primary", solidHeader = TRUE,
                        plotOutput("emissions_plot"))
                )
            )
        )
    )
)

# /////////////////////////////////////////////////////////////////////////////////////////////////

# [Server]

server <- function(input, output) {

    # Reactive filtered data
    filtered_data <- reactive({
        subset(emissions_data,
               country %in% input$country &
                   Sector %in% input$sector &
                   Year >= input$year[1] &
                   Year <= input$year[2])
    })

    # Plot
    output$emissions_plot <- renderPlot({
        ggplot(filtered_data(), aes(x = Year, y = co2_mt, color = country)) +
            geom_line(size = 1) +
            facet_wrap(~ Sector) +
            labs(title = "CO2 Emissions Over Time",
                 x = "Year", y = "CO2 Emissions (Mt)") +
            theme_minimal()
    })
}

# Run the app
shinyApp(ui, server)
