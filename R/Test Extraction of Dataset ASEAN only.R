# use tidyverse for data manipulation
# use readxl for reading excel files
# use janitor for cleaning missing values
library("readxl")
library("tidyverse")
library("janitor")

# Read dataset carbon emission
dataset1 <- "IEA-EDGAR fossil CO2 emissions.xlsx"
read_dataset <- read_excel(dataset1, sheet = "fossil_CO2_by_sector_country_su")

head(read_dataset) # sample viewing


# Create a vector of ASEAN country names
asean_countries <- c(
    "Brunei Darussalam",
    "Cambodia",
    "Indonesia",
    "Laos",
    "Malaysia",
    "Myanmar",
    "Philippines",
    "Singapore",
    "Thailand",
    "Vietnam"
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
        values_to = "CO2_Emissions"
    )

# View the spreadsheet extract
View(long_format_data)

# Export it to csv format
write_csv(long_format_data, "asean_co2_emissions.csv")



# Read dataset Grid Emission
dataset2 <- "IGES_GRID_EF_v11.6_20250226.xlsx"
sheet_name2 <- "EFfromCountriesOrSB"

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

all_data <- read_excel(dataset2, sheet = sheet_name2, skip = 3)

asean_data <- all_data %>%
    fill(...1, ...2, .direction = "down") %>%

    rename(
        Country = ...1,
        Region_Grid = ...2,
        Publisher = ...3,
        Data_Vintage = ...4,
        Method = ...5,
        Applicability_Wind_Solar = `Wind and\r\nsolar power`,
        Applicability_Other = `Other than wind and solar power`,
        Crediting_Period_1 = `First crediting period`,
        Crediting_Period_2 = `Second crediting period`,
        Crediting_Period_3 = `Third crediting period`,
        Notes = ...31,
        Sources = ...32,
        URLs = ...33
    ) %>%

    # Now the filter works perfectly on the cleaned 'Country' column
    filter(Country %in% asean_countries2)

view(asean_data)

# Export it to csv format file
write_csv(asean_data, "asean_GridEmission.csv")

# Read dataset HouseHold Size
dataset3<- "Household Size and Composition.xlsx"
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

view(asean_household_data)

# Export the data in csv format file
write_csv(asean_household_data, "asean_Household.csv")

# Read dataset Global Electricity Demand
dataset4<- "Global Electricity Demand and Generation Dataset.csv"

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

view(asean_electricity_data)

# Export to csv file format
write_csv(asean_electricity_data, "asean_Electricity.csv")
