library("readxl")

download_if_missing <- function(url, destfile, mode = "wb") {
    if (!file.exists(destfile)) {
        message("Downloading ", destfile, " …")
        download.file(url, destfile = destfile, mode = mode)
    } else {
        message(destfile, " already exists; skipping download.")
    }
}

# ////////////////////////////////////////////////////////////////////////////////////////////////

# Dataset 1: CO2 emission per country
url1 <- "https://edgar.jrc.ec.europa.eu/booklet/EDGAR_2024_GHG_booklet_2024_fossilCO2only.xlsx"
file1 <- file.path("data-raw", "original", "IEA-EDGAR fossil CO2 emissions.xlsx")
download_if_missing(url1, file1)
iea_edgar_data <- read_excel(file1,
                             sheet = "fossil_CO2_totals_by_country",
                             skip = 10)
head(iea_edgar_data)

# ////////////////////////////////////////////////////////////////////////////////////////////////

# Dataset 2: Grid emission factors
url2 <- "https://www.iges.or.jp/en/publication_documents/pub/data/en/1215/IGES_GRID_EF_v11.6_20250226.xlsx"
file2 <- file.path("data-raw", "original", "IGES_GRID_EF_v11.6_20250226.xlsx")
download_if_missing(url2, file2)
iges_grid_data <- read_excel(file2,
                             sheet = "EFfromCountriesOrSB",
                             skip = 10)
head(iges_grid_data)

# ////////////////////////////////////////////////////////////////////////////////////////////////

# Dataset 3: Number of households per country
url3 <- "https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/undesa_pd_2022_hh-size-composition.xlsx"
file3 <- file.path("data-raw", "original", "Household Size and Composition.xlsx")
download_if_missing(url3, file3)
Household_data <- read_excel(file3,
                             sheet = "HH size and composition 2022",
                             skip = 10)
head(Household_data)

# ////////////////////////////////////////////////////////////////////////////////////////////////

# Dataset 4: Average residential electricity (CSV from Kaggle)
# Note that it cannot download directly using the link because its in the kaggle and it doesnt provide a link address directly
url4 <- "https://www.kaggle.com/datasets/shahriarkabir/global-electricity-demand-and-generation-dataset?select=Global+Electricity+Demand+and+Generation+Dataset.csv"
file4 <- file.path("data-raw", "original", "Global Electricity Demand and Generation Dataset.csv")
AverageResidentElectricity <- read.csv(file4)

# Display the header
head(AverageResidentElectricity)

