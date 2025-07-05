# use tidyverse for data manipulation
# use readxl for reading excel files
library("readxl")
library("tidyverse")

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



# Read dataset Grid Emission
dataset2 <- "IGES_GRID_EF_v11.6_20250226.xlsx"





