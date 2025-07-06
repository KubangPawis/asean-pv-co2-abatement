# Need to use library("readxl") for reading excel files if you don't have download the package.
library("readxl")

# Download dataset 1 CO2 emission per country
url <- "https://edgar.jrc.ec.europa.eu/booklet/EDGAR_2024_GHG_booklet_2024_fossilCO2only.xlsx"

download.file(url, destfile = "IEA-EDGAR fossil CO2 emissions.xlsx", mode = "wb")

iea_edgar_data <- read_excel("IEA-EDGAR fossil CO2 emissions.xlsx", 
                             sheet = "fossil_CO2_totals_by_country", 
                             skip = 10)
# Display the header
head(iea_edgar_data)


# Download the dataset 2 Grid emission factors 
url1 <- "https://www.iges.or.jp/en/publication_documents/pub/data/en/1215/IGES_GRID_EF_v11.6_20250226.xlsx"

download.file(url1, destfile = "IGES_GRID_EF_v11.6_20250226.xlsx", mode = "wb")

iges_grid_data <- read_excel("IGES_GRID_EF_v11.6_20250226.xlsx", 
                             sheet = "EFfromCountriesOrSB", 
                             skip = 10)
# Display the header
head(iges_grid_data)

# Download the dataset 3 Number of households per country
url2 <- "https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/undesa_pd_2022_hh-size-composition.xlsx"

download.file(url2, destfile = "Household Size and Composition.xlsx", mode = "wb")

Household_data <- read_excel("Household Size and Composition.xlsx", 
                             sheet = "HH size and composition 2022", 
                             skip = 10)
# Display the header
head(Household_data)

# Download the dataset 4 Average residential electricity it's a csv forma
# Note that it cannot download directly using the link because its in the kaggle and it doesnt provide a link address directly
url3 <- "https://www.kaggle.com/datasets/shahriarkabir/global-electricity-demand-and-generation-dataset?select=Global+Electricity+Demand+and+Generation+Dataset.csv"

AverageResidentElectricity <- read.csv("Global Electricity Demand and Generation Dataset.csv")

# Display the header
head(AverageResidentElectricity)

