library(shiny)
library(shinydashboard)
library(ggplot2)

# /////////////////////////////////////////////////////////////////////////////////////////////////
# [Data Preparations]

country_list <- c(
    "Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia",
    "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam"
)

emissions_data_path <- file.path("..", "..", "data-raw", "interim", "asean_co2_emissions.csv")
asean_merged_data_path <- file.path("..", "..", "data", "asean_merged.csv")
asean_summary_data_path <- file.path("..", "..", "data", "asean_summary_final.csv")
asean_grid_emission_data_path <- file.path("..", "..", "data-raw", "interim", "asean_GridEmission.csv")

emissions_data <- read.csv(emissions_data_path)
asean_merged_data <- read.csv(asean_merged_data_path)
asean_summary_data <- read.csv(asean_summary_data_path)
asean_grid_emission_data <- read.csv(asean_grid_emission_data_path)

emissions_data$Year <- as.integer(emissions_data$Year)
print(getwd())

compute_abatement_helper_path <- file.path("..", "..", "R", "03_compute_solar_abatement.R")
source(compute_abatement_helper_path)

# /////////////////////////////////////////////////////////////////////////////////////////////////
# [UI]

ui <- dashboardPage(
    skin = "green",
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
            tabItem(tabName = "overview",
                    fluidRow(
                        box(
                            title = "Scenario Controls", width = 12, status = "success", solidHeader = TRUE,
                            column(4, selectInput("ov_country", "Country", choices = country_list, selected = "Philippines")),
                            column(4, selectInput("ov_pv_size", "PV system size (kW)", choices = c(3, 5, 10))),
                            column(4, selectInput("ov_co2_target", "CO₂ reduction target (%)", choices = c(1, 5, 10)))
                        )
                    ),
                    fluidRow(
                        valueBoxOutput("homesRequiredBox"),
                        valueBoxOutput("co2Box"),
                        valueBoxOutput("capacityBox")
                    ),
                    fluidRow(
                        box(
                            title = "Homes Required by Country", width = 12, solidHeader = TRUE, status = "success",
                            plotOutput("homesBar", height = "600px")
                        )
                    )
            ),
            tabItem(tabName = "explore",
                    fluidRow(
                        box(
                            title = "CO2 Emissions Over Time", width = 12, status = "success", solidHeader = TRUE,
                            fluidRow(
                                column(4, selectInput("country", "Select Country:", choices = unique(emissions_data$country), selected = "Philippines", multiple = TRUE)),
                                column(4, selectInput("sector", "Select Sector:", choices = unique(emissions_data$Sector), selected = unique(emissions_data$Sector)[1])),
                                column(4, sliderInput("year", "Select Year Range:", min = min(emissions_data$Year, na.rm = TRUE), max = max(emissions_data$Year, na.rm = TRUE), value = c(min(emissions_data$Year, na.rm = TRUE), max(emissions_data$Year, na.rm = TRUE)), step = 1, sep = ""))
                            ),
                            plotOutput("emissions_plot")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Total CO2 Emissions by Sector", width = 12, status = "success", solidHeader = TRUE,
                            fluidRow(
                                column(6, selectInput("bar_country", "Select Country:", choices = unique(emissions_data$country), selected = "Philippines")),
                                column(6, sliderInput("bar_year", "Select Year Range:", min = min(emissions_data$Year, na.rm = TRUE), max = max(emissions_data$Year, na.rm = TRUE), value = c(min(emissions_data$Year, na.rm = TRUE), max(emissions_data$Year, na.rm = TRUE)), step = 1, sep = ""))
                            ),
                            plotOutput("sector_bar_plot")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Homes vs Grid Emission", width = 12, solidHeader = TRUE, status = "success",
                            fluidRow(
                                column(6, selectInput("scatter_pv_size", "Select PV System Size (kW):", choices = sort(unique(asean_summary_data$PV_SYSTEM_SIZE)), selected = sort(unique(asean_summary_data$PV_SYSTEM_SIZE))[1])),
                                column(6, selectInput("scatter_target", "Select CO₂ Reduction Target (%):", choices = sort(unique(asean_summary_data$TARGET_REDUCTION * 100)), selected = sort(unique(asean_summary_data$TARGET_REDUCTION * 100))[1]))
                            ),
                            plotOutput("scatter_plot", height = "500px", hover = hoverOpts(id = "plot_hover")),
                            textOutput("hover_info")
                        )
                    )
            )
        )
    )
)

# /////////////////////////////////////////////////////////////////////////////////////////////////
# [Server]

server <- function(input, output) {

    filtered_data <- reactive({
        subset(emissions_data, country %in% input$country & Sector %in% input$sector & Year >= input$year[1] & Year <= input$year[2])
    })

    sector_data_for_bar <- reactive({
        subset(emissions_data, country %in% input$bar_country & Year >= input$bar_year[1] & Year <= input$bar_year[2]) |>
            aggregate(co2_mt ~ Sector, data = _, FUN = sum, na.rm = TRUE)
    })

    output$emissions_plot <- renderPlot({
        ggplot(filtered_data(), aes(x = Year, y = co2_mt, color = country)) +
            geom_line(size = 1) +
            facet_wrap(~ Sector) +
            labs(title = "CO2 Emissions Over Time", x = "Year", y = "CO2 Emissions (Mt)") +
            theme_minimal()
    })

    output$sector_bar_plot <- renderPlot({
        ggplot(sector_data_for_bar(), aes(x = reorder(Sector, co2_mt), y = co2_mt, fill = Sector)) +
            geom_bar(stat = "identity", show.legend = FALSE) +
            coord_flip() +
            labs(title = "Total CO2 Emissions by Sector", x = "Sector", y = "Total CO2 Emissions (Mt)") +
            theme_minimal()
    })

    output$homesBar <- renderPlot({
        homes_tbl <- lapply(country_list, function(cty) {
            data.frame(
                country = cty,
                homes_required = compute_abatement(
                    df = asean_merged_data,
                    pv_size = as.numeric(input$ov_pv_size),
                    target_reduc = as.numeric(input$ov_co2_target) / 100,
                    target_country = cty
                )
            )
        }) |> dplyr::bind_rows()

        ggplot(homes_tbl, aes(x = reorder(country, -homes_required), y = homes_required / 1e6)) +
            geom_col(fill = "#609870") +
            labs(x = "Country", y = "Homes required (million)", subtitle = paste0("PV ", input$ov_pv_size, " kW  •  ", input$ov_co2_target, "% CO₂ cut")) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            theme_minimal(base_size = 13)
    })

    output$homesRequiredBox <- renderValueBox({
        h <- compute_abatement(
            df = asean_merged_data,
            pv_size = as.numeric(input$ov_pv_size),
            target_reduc = as.numeric(input$ov_co2_target) / 100,
            target_country = input$ov_country,
            round_to = 0
        )
        valueBox(format(h, big.mark = ","), subtitle = paste("Homes required in", input$ov_country), icon = icon("home"), color = "teal")
    })

    output$co2Box <- renderValueBox({
        homes_required <- compute_abatement(
            df = asean_merged_data,
            pv_size = as.numeric(input$ov_pv_size),
            target_reduc = as.numeric(input$ov_co2_target) / 100,
            target_country = input$ov_country,
            round_to = 0
        )
        mt_abated_idx <- which(
            asean_summary_data$PV_SYSTEM_SIZE == as.numeric(input$ov_pv_size) &
                asean_summary_data$TARGET_REDUCTION == as.numeric(input$ov_co2_target) / 100 &
                asean_summary_data$country == input$ov_country
        )
        mt_abated <- (homes_required * asean_summary_data$co2_avoided_per_home[[mt_abated_idx[1]]]) / 1e12
        valueBox(paste0(format(round(mt_abated, 2), big.mark = ","), " Mt"), subtitle = "Annual CO₂ abated", icon = icon("cloud"), color = "olive")
    })

    output$capacityBox <- renderValueBox({
        homes_required <- compute_abatement(
            df = asean_merged_data,
            pv_size = as.numeric(input$ov_pv_size),
            target_reduc = as.numeric(input$ov_co2_target) / 100,
            target_country = input$ov_country,
            round_to = 0
        )
        GW_needed <- (homes_required * as.numeric(input$ov_pv_size)) / 1000
        valueBox(paste0(format(round(GW_needed, 2), big.mark = ","), " GW"), subtitle = "Installed capacity needed", icon = icon("solar-panel"), color = "yellow")
    })

    output$scatter_plot <- renderPlot({
        selected_size <- as.numeric(input$scatter_pv_size)
        selected_target <- as.numeric(input$scatter_target) / 100

        filtered_data <- asean_summary_data[
            asean_summary_data$PV_SYSTEM_SIZE == selected_size &
                asean_summary_data$TARGET_REDUCTION == selected_target, ]

        matching_grid <- asean_grid_emission_data[
            match(filtered_data$country, asean_grid_emission_data$country), ]

        filtered_data$grid_emission <- matching_grid$ef_gco2_per_kwh

        ggplot(filtered_data, aes(x = grid_emission, y = homes_required, label = country)) +
            geom_point(aes(color = country), size = 5) +
            geom_text(vjust = -0.8, size = 4, color = "black") +
            labs(x = "Grid Emission Factor (gCO2/kWh)", y = "Homes Required", title = "Homes Required vs Grid Emissions", subtitle = paste("PV", input$scatter_pv_size, "kW •", input$scatter_target, "% CO2 reduction")) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none")
    })

    output$hover_info <- renderPrint({
        hover <- input$plot_hover
        if (is.null(hover)) return("Hover over a point to see details.")

        selected_size <- as.numeric(input$scatter_pv_size)
        selected_target <- as.numeric(input$scatter_target) / 100

        filtered_data <- asean_summary_data[
            asean_summary_data$PV_SYSTEM_SIZE == selected_size &
                asean_summary_data$TARGET_REDUCTION == selected_target, ]

        matching_grid <- asean_grid_emission_data[
            match(filtered_data$country, asean_grid_emission_data$country), ]

        filtered_data$grid_emission <- matching_grid$ef_gco2_per_kwh

        dist <- sqrt((hover$x - filtered_data$grid_emission)^2 +
                         (hover$y - filtered_data$homes_required)^2)
        near_idx <- which.min(dist)
        nearest <- filtered_data[near_idx, ]

        paste0("Country: ", nearest$country,
               " | Grid Emission: ", round(nearest$grid_emission, 2), " gCO2/kWh",
               " | Homes Required: ", format(round(nearest$homes_required), big.mark = ","))
    })
}

shinyApp(ui, server)
