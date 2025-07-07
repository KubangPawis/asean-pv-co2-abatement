library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(here)

# [Variables]

HOURS_PER_YEAR <- 8760
pv_system_sizes <- c(3, 5, 10)                  # kW
target_reduction_values <- c(0.01, 0.05, 0.10)  # Percentage

scenarios <- crossing(PV_SYSTEM_SIZE = pv_system_sizes,
                      TARGET_REDUCTION = target_reduction_values)

# [Data Load]

ds_path = here(file.path("data", "asean_merged.csv"))
ds <- read_csv(ds_path, col_names = TRUE)
View(ds)

# [Solar Abatement Computations per ASEAN Country]

## Data Export for Analysis

results_ds <- scenarios %>%
    crossing(ds) %>%
    mutate(
        annual_pv_electricity_per_home = PV_SYSTEM_SIZE * HOURS_PER_YEAR * solar_capacity_factor,
        co2_avoided_per_home = annual_pv_electricity_per_home * ef_gco2_per_kwh,
        homes_required = (TARGET_REDUCTION * co2_emissions_g) / co2_avoided_per_home
    )

View(results_ds)

## Helper Function for Dashboard
compute_abatement <- function(df, pv_size, target_reduc, target_country, hours_per_yr=8760, round_to=0) {
    df %>%
        mutate(
            annual_pv_electricity_per_home = pv_size * hours_per_yr * solar_capacity_factor,
            co2_avoided_per_home = annual_pv_electricity_per_home * ef_gco2_per_kwh,
            homes_required = (target_reduc * co2_emissions_g) / co2_avoided_per_home
        ) %>%
        filter(country == target_country) %>%
        pull(homes_required) %>%
        # anonymous function to ensure NA when country not found
        {
            if (length(.) == 0) NA_real_
            else if (!is.na(round_to)) round(., round_to)
            else .
        }
}

# [Data Export]

out_path <- here(file.path("data", "asean_summary_final.csv"))
if (!file.exists(out_path)) {
    write.csv(results_ds, out_path)
    message(out_path, " successfully created")
} else {
    message("Skipped writing ", out_path, "; file already exists.")
}
