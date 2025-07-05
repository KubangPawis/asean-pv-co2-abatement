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



# Read dataset Grid Emission
dataset_file <- "IGES_GRID_EF_v11.6_20250226.xlsx"
sheet_name <- "EFfromCountriesOrSB"

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

all_data <- read_excel(dataset_file, sheet = sheet_name, skip = 3)

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

