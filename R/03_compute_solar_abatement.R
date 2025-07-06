library(readr)
library(dplyr)
library(magrittr)

# [Constants]

PV_SYSTEM_SIZE <- 5         # kW
TARGET_REDUCTION <- 0.05    # Percentage
HOURS_PER_YEAR <- 8760

# [Data Load]

ds_path = file.path("data", "asean_merged.csv")
ds <- read_csv(ds_path, col_names = TRUE)

# [Solar Abatement Computations per ASEAN Country]

ds <- ds %>%
    mutate(
        annual_pv_electricity_per_home = PV_SYSTEM_SIZE * HOURS_PER_YEAR * solar_capacity_factor,
        co2_avoided_per_home = annual_pv_electricity_per_home * ef_gco2_per_kwh,
        homes_required = (TARGET_REDUCTION * co2_emissions_g) / co2_avoided_per_home
    )

View(ds)

out_path <- file.path("data", "asean_summary_final.csv")
write.csv(ds, out_path)
