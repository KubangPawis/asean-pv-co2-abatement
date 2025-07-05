# use tidyverse for data manipulation
# use readxl for reading excel files
library("readxl")
library("tidyverse")

# Read dataset carbon emission
dataset1 <- "IEA-EDGAR fossil CO2 emissions.xlsx"
read_dataset1 <- read_excel(dataset1, sheet = "fossil_CO2_totals_by_country")
read_dataset2 <- read_excel(dataset1, sheet = "fossil_CO2_by_sector_country_su")
read_dataset3 <- read_excel(dataset1, sheet = "fossil_CO2_per_GDP_by_country")
read_dataset4 <- read_excel(dataset1, sheet = "fossil_CO2_per_capita_by_countr")

head(read_dataset1) # sample viewing


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

# Filter the main data frame to get only the rows where the 'Country' column
asean_co2_data <- read_dataset1 %>%
    filter(Country %in% asean_countries)
asean_co2_data2 <- read_dataset2 %>%
    filter(Country %in% asean_countries)
# Use arrange for dealing multiple country but different sectors
sorted_asean_data <- asean_co2_data2 %>%
    arrange(Country, Sector)
asean_co2_data3 <- read_dataset3 %>%
    filter(Country %in% asean_countries)
asean_co2_data4 <- read_dataset4 %>%
    filter(Country %in% asean_countries)

# View the spreadsheet extract
View(asean_co2_data)
View(sorted_asean_data)
View(asean_co2_data3)
View(asean_co2_data4)








