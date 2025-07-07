library(shiny)
library(shinydashboard)
library(ggplot2)

# /////////////////////////////////////////////////////////////////////////////////////////////////
# [Data Preparations]

emissions_data_path <- file.path("..", "..", "data-raw", "interim", "asean_co2_emissions.csv")
asean_merged_data_path <- file.path("..", "..", "data", "asean_merged.csv")
asean_summary_data_path <- file.path("..", "..", "data", "asean_summary_final.csv")

emissions_data <- read.csv(emissions_data_path)
asean_merged_data <- read.csv(asean_merged_data_path)
asean_summary_data <- read.csv(asean_summary_data_path)

emissions_data$Year <- as.integer(emissions_data$Year)
print(getwd())

# Helper Function
compute_abatement_helper_path <- file.path("..", "..", "R", "03_compute_solar_abatement.R")
source(compute_abatement_helper_path)

# /////////////////////////////////////////////////////////////////////////////////////////////////
# [UI]

ui <- dashboardPage(
    dashboardHeader(title = "ASEAN CO2 Emissions Dashboard"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
            menuItem("Explore", tabName = "explore", icon = icon("industry")),
            menuItem("Docs", tabName = "docs", icon = icon("book"))
        )
    ),

    dashboardBody(
        tabItems(
            tabItem(tabName = "explore",
                    fluidRow(
                        box(
                            title = "CO2 Emissions Over Time", width = 12, status = "primary", solidHeader = TRUE,
                            fluidRow(
                                column(4,
                                       selectInput("country", "Select Country:",
                                                   choices = unique(emissions_data$country),
                                                   selected = "Philippines",
                                                   multiple = TRUE)
                                ),
                                column(4,
                                       selectInput("sector", "Select Sector:",
                                                   choices = unique(emissions_data$Sector),
                                                   selected = unique(emissions_data$Sector)[1],
                                                   multiple = FALSE)
                                ),
                                column(4,
                                       sliderInput("year", "Select Year Range:",
                                                   min = min(emissions_data$Year, na.rm = TRUE),
                                                   max = max(emissions_data$Year, na.rm = TRUE),
                                                   value = c(min(emissions_data$Year, na.rm = TRUE), max(emissions_data$Year, na.rm = TRUE)),
                                                   step = 1,
                                                   sep = "")
                                )
                            ),
                            plotOutput("emissions_plot")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Total CO2 Emissions by Sector", width = 12, status = "info", solidHeader = TRUE,
                            fluidRow(
                                column(6,
                                       selectInput("bar_country", "Select Country for Bar Chart:",
                                                   choices = unique(emissions_data$country),
                                                   selected = "Philippines",
                                                   multiple = FALSE)
                                ),
                                column(6,
                                       sliderInput("bar_year", "Select Year Range for Bar Chart:",
                                                   min = min(emissions_data$Year, na.rm = TRUE),
                                                   max = max(emissions_data$Year, na.rm = TRUE),
                                                   value = c(min(emissions_data$Year, na.rm = TRUE), max(emissions_data$Year, na.rm = TRUE)),
                                                   step = 1,
                                                   sep = "")
                                )
                            ),
                            plotOutput("sector_bar_plot")
                        )
                    )
            ),

            tabItem(tabName = "overview",
                    valueBoxOutput("homesRequiredBox")
            )
        )
    )
)

# /////////////////////////////////////////////////////////////////////////////////////////////////
# [Server]

server <- function(input, output) {

    # Line Chart: Filtered data
    filtered_data <- reactive({
        subset(emissions_data,
               country %in% input$country &
                   Sector %in% input$sector &
                   Year >= input$year[1] &
                   Year <= input$year[2])
    })

    # Bar Chart: Aggregated by sector
    sector_data_for_bar <- reactive({
        subset(emissions_data,
               country %in% input$bar_country &
                   Year >= input$bar_year[1] &
                   Year <= input$bar_year[2]) |>
            aggregate(co2_mt ~ Sector, data = _, FUN = sum, na.rm = TRUE)
    })

    # Line plot output
    output$emissions_plot <- renderPlot({
        ggplot(filtered_data(), aes(x = Year, y = co2_mt, color = country)) +
            geom_line(size = 1) +
            facet_wrap(~ Sector) +
            labs(title = "CO2 Emissions Over Time",
                 x = "Year", y = "CO2 Emissions (Mt)") +
            theme_minimal()
    })

    # Bar plot output
    output$sector_bar_plot <- renderPlot({
        ggplot(sector_data_for_bar(), aes(x = reorder(Sector, co2_mt), y = co2_mt, fill = Sector)) +
            geom_bar(stat = "identity", show.legend = FALSE) +
            coord_flip() +
            labs(title = "Total CO2 Emissions by Sector",
                 x = "Sector", y = "Total CO2 Emissions (Mt)") +
            theme_minimal()
    })

    # ValueBox in overview
    output$homesRequiredBox <- renderValueBox({
        h <- compute_abatement(
            df             = asean_merged_data,
            pv_size        = 5,
            target_reduc   = 0.05,
            target_country = "Philippines",
            round_to       = 0
        )
        valueBox(
            format(h, big.mark = ","),
            subtitle = paste("Homes required in", "Philippines"),
            icon     = icon("home"),
            color    = "teal"
        )
    })
}

# Run the app
shinyApp(ui, server)
