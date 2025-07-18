# use tidyverse (i.e., dplyr, tidyr, readr, magrittr) for data manipulation
# use readxl for reading excel files
# use janitor for cleaning missing values
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(janitor)

# Read dataset carbon emission
dataset1 <- file.path("data-raw", "original", "IEA-EDGAR fossil CO2 emissions.xlsx")
read_dataset <- read_excel(dataset1, sheet = "fossil_CO2_by_sector_country_su")

head(read_dataset) # sample viewing


# Create a vector of ASEAN country names
asean_countries <- c(
    "Brunei",
    "Cambodia",
    "Indonesia",
    "Laos",
    "Malaysia",
    "Myanmar/Burma",
    "Philippines",
    "Singapore",
    "Thailand",
    "Viet Nam"
)

# filtering the data to display ASEAN Countries only
asean_co2_data <- read_dataset %>%
    filter(Country %in% asean_countries)

# Use arrange for dealing multiple country but different sectors
sorted_asean_data <- asean_co2_data %>%
    arrange(Country, Sector)

# Convert from wide format to long format
long_format_data <- sorted_asean_data %>%
    pivot_longer(
        cols = -c(Substance, Sector, `EDGAR Country Code`, Country),
        names_to = "Year",
        values_to = "co2_mt"
    ) %>%
    rename(
        country = Country
    ) %>%
    mutate(
        co2_emissions_g = co2_mt * 1e12
    )

# View the spreadsheet extract
View(long_format_data)

# Export it to csv format
out_csv <- file.path("data-raw", "interim", "asean_co2_emissions.csv")
if (!file.exists(out_csv)) {
    write_csv(long_format_data, out_csv)
    message(out_csv, " successfully created")
} else {
    message("Skipped writing ", out_csv, "; file already exists.")
}

#//////////////////////////////////////////////////////////////////

# Read dataset Grid Emission
grid_emission_path <- file.path("data-raw", "original", "IGES_GRID_EF_v11.6_20250226.xlsx")
target_sheet_name <- "SummaryEFfromCDM"

asean_countries2 <- c(
    "Brunei Darussalam",
    "Cambodia",
    "Indonesia",
    "Lao PDR",
    "Malaysia",
    "Myanmar",
    "Philippines",
    "Singapore",
    "Thailand",
    "Viet Nam"
)

ds_grid_emission <- read_excel(grid_emission_path, sheet = target_sheet_name, skip = 1, col_names = TRUE)

ds_grid_emission_asean <- ds_grid_emission %>%
    select(
        country = "Host Party...2",
        ef_kgco2_per_kwh = "Combined Margin EF (Average)"
    ) %>%
    mutate(
        ef_gco2_per_kwh = ef_kgco2_per_kwh * 1000 # from kg to g
    ) %>%
    filter(country %in% asean_countries2)

View(ds_grid_emission_asean)

# Export it to csv format file

out_csv <- file.path("data-raw", "interim", "asean_GridEmission.csv")
if (!file.exists(out_csv)) {
    write_csv(ds_grid_emission_asean, out_csv)
    message(out_csv, " successfully created")
} else {
    message("Skipped writing ", out_csv, "; file already exists.")
}

#//////////////////////////////////////////////////////////////////

# Read dataset HouseHold Size
dataset3<- file.path("data-raw", "original", "Household Size and Composition.xlsx")
sheet_name3 <- "HH size and composition 2022"

asean_countries3 <- c(
    "Brunei Darussalam",
    "Cambodia",
    "Indonesia",
    "Lao People's Dem. Republic",
    "Malaysia",
    "Myanmar",
    "Philippines",
    "Singapore",
    "Thailand",
    "Viet Nam"
)

asean_household_data <- read_excel(
    dataset3,
    sheet = sheet_name3,
    # Skip the first 4 rows of titles and blank lines.
    # Row 5 will be used as the header.
    skip = 4
) %>%
    # Use clean_names() to automatically fix all the messy, multi-line headers.
    clean_names() %>%
    filter(country_or_area %in% asean_countries3)

View(asean_household_data)

# Export the data in csv format file
out_csv <- file.path("data-raw", "interim", "asean_Household.csv")
if (!file.exists(out_csv)) {
    write_csv(asean_household_data, out_csv)
    message(out_csv, " successfully created")
} else {
    message("Skipped writing ", out_csv, "; file already exists.")
}

#//////////////////////////////////////////////////////////////////

# Read dataset Global Electricity Demand
dataset4 <- file.path("data-raw", "original", "Global Electricity Demand and Generation Dataset.csv")

asean_countries4 <- c(
    "Cambodia",
    "Indonesia",
    "Laos",
    "Malaysia",
    "Myanmar",
    "Philippines",
    "Singapore",
    "Thailand",
    "VietNam"
)

asean_electricity_data <- read_csv(dataset4) %>%
    clean_names() %>%
    filter(entity %in% asean_countries4) %>%
    arrange(entity, year)

View(asean_electricity_data)

# Export to csv file format
out_csv <- file.path("data-raw", "interim", "asean_Electricity.csv")
if (!file.exists(out_csv)) {
    write_csv(asean_electricity_data, out_csv)
    message(out_csv, " successfully created")
} else {
    message("Skipped writing ", out_csv, "; file already exists.")
}

#//////////////////////////////////////////////////////////////////

ds_pv_dir <- file.path("data-raw", "original", "asean_pv_output")
target_sheet_name <- "PVOUT_stats"
cell_ref <- "B4"

# Function to extract PVOUTPUT average from GSA data
get_pv_out_avg <- function(path, sheet = target_sheet_name, cell = cell_ref) {
    if (!(sheet %in% excel_sheets(path)))
        return(NA)

    read_excel(
        path,
        sheet = sheet,
        range = cell,
        col_names = FALSE
    ) %>%
        pull(1)
}

asean_pv_data <- list.files(
    ds_pv_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE
    ) %>%
    map_dfr(
        ~ tibble(
            country = basename(.x) %>%
                str_remove("^GSA_Report_") %>%
                str_remove(".xlsx$"),
            daily_pv_average = get_pv_out_avg(.x)
        ),
        .id = NULL
    ) %>%
    mutate(
        country = recode(
            country,
            "Brunei Darussalam"                 = "Brunei",
            "Lao People's Democratic Republic"  = "Laos",
            "Viet Nam"                          = "Vietnam",
            .default = country
        ),
        yearly_pv = daily_pv_average * 365,
        solar_capacity_factor = yearly_pv / 8760 # 8760 -> number of hours in a (non‑leap) year
    )

print(asean_pv_data)
View(asean_pv_data)

# Export the data in csv format file
out_csv <- file.path("data-raw", "interim", "asean_PV.csv")
if (!file.exists(out_csv)) {
    write_csv(asean_pv_data, out_csv)
    message(out_csv, " successfully created")
} else {
    message("Skipped writing ", out_csv, "; file already exists.")
}

#//////////////////////////////////////////////////////////////////

# Cleaning and preprocessing "land area" data

land_area_data_path <- file.path("data-raw", "original", "API_AG.LND.TOTL.K2_DS2_en_csv_v2_21556.csv")

asean_countries5 <- c(
    "Cambodia",
    "Brunei Darussalam",
    "Indonesia",
    "Lao PDR",
    "Malaysia",
    "Myanmar",
    "Philippines",
    "Singapore",
    "Thailand",
    "Viet Nam"
)

land_area_data <- read_csv(land_area_data_path, col_names=TRUE, skip=4) %>%
    filter(`Country Name` %in% asean_countries5) %>%
    mutate(
        `Country Name` = recode(
            `Country Name`,
            "Lao PDR" = "Laos",
            "Viet Nam" = "Vietnam",
            "Brunei Darussalam" = "Brunei"
        )
    ) %>%
    select(
        country = `Country Name`,
        land_area_2022 = `2022`
    )
View(land_area_data)

# Export the data in csv format file
out_csv <- file.path("data-raw", "interim", "asean_land_area.csv")
if (!file.exists(out_csv)) {
    write_csv(land_area_data, out_csv)
    message(out_csv, " successfully created")
} else {
    message("Skipped writing ", out_csv, "; file already exists.")
}

#//////////////////////////////////////////////////////////////////

# Merge all dataset into one

# Ensure no duplicates in columns
co2_emissions_clean <- long_format_data %>%
    mutate(
        country = recode(
            country,
            "Myanmar/Burma" = "Myanmar",
            "Viet Nam" = "Vietnam",
            .default = country
        ),
        # Convert Year to a numeric type for joining
        Year = as.numeric(Year)
    ) %>%
    filter(
        Year == max(Year, na.rm = TRUE) &
        Sector == "Power Industry"
    ) %>%
    select(
        country,
        co2_emissions_g
    )

grid_emission_clean <- ds_grid_emission_asean %>%
    mutate(
        country = recode(
            country,
            "Brunei Darussalam" = "Brunei",
            "Lao PDR" = "Laos",
            "Viet Nam" = "Vietnam",
            .default = country
        )
    )

## Joining the dataset now
merged_dataset <- full_join(
    co2_emissions_clean,
    grid_emission_clean,
    by = "country"
)

merged_dataset <- full_join(
    merged_dataset,
    land_area_data,
    by = "country"
)

merged_dataset <- full_join(
    merged_dataset,
    asean_pv_data,
    by = "country"
)

## Finalized the dataset
final_merged_data <- merged_dataset %>%
    arrange(country)

# View the merge data
View(final_merged_data)

#Export the dataset in csv format
out_csv <- file.path("data", "asean_merged.csv")
if (!file.exists(out_csv)) {
    write_csv(final_merged_data, out_csv)
    message(out_csv, " successfully created")
} else {
    message("Skipped writing ", out_csv, "; file already exists.")
}
