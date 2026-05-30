library(tidyverse)


# ---- EA ----
ea_data <- read_csv("https://raw.githubusercontent.com/rtotbagi/ClimateXSupernatural/refs/heads/main/datasets/EA/data.csv")
ea_soc  <- read_csv("https://raw.githubusercontent.com/rtotbagi/ClimateXSupernatural/refs/heads/main/datasets/EA/societies.csv")

ea <- ea_data %>%
  filter(var_id == "EA034") %>%
  select(soc_id, year, var_id, code) %>%
  rename(EA034 = code) %>%
  left_join(
    ea_soc %>% select(id, pref_name_for_society, glottocode, Lat, Long),
    by = c("soc_id" = "id")
  )

# ---- SCCS ----
sccs_data <- read_csv("https://raw.githubusercontent.com/rtotbagi/ClimateXSupernatural/refs/heads/main/datasets/SCCS/data.csv")
sccs_soc  <- read_csv("https://raw.githubusercontent.com/rtotbagi/ClimateXSupernatural/refs/heads/main/datasets/SCCS/societies.csv")

sccs_vars <- c(
  "SCCS645", "SCCS647", "SCCS648", "SCCS649",
  "SCCS652", "SCCS653", "SCCS654", "SCCS655", "SCCS656"
)

sccs <- sccs_data %>%
  filter(var_id %in% sccs_vars) %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  left_join(
    sccs_soc %>% select(id, pref_name_for_society, glottocode),
    by = c("soc_id" = "id")
  )

# ---- ecoClimate ----
climate_data <- read_csv("https://raw.githubusercontent.com/rtotbagi/ClimateXSupernatural/refs/heads/main/datasets/ecoClimate/data.csv")

climate_vars <- c(
  "AnnualTemperatureVariance",
  "AnnualPrecipitationVariance",
  "TemperaturePredictability",
  "PrecipitationPredictability"
)

climate <- climate_data %>%
  filter(var_id %in% climate_vars) %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code)

# ---- ??sszekapcsol??s ----
# EA ??s SCCS: glottocode alapj??n
# SCCS ??s ecoClimate: soc_id alapj??n, ha ecoClimate SCCS-azonos??t??kat haszn??l

d_wide <- ea %>%
  left_join(
    sccs,
    by = "glottocode",
    suffix = c("_ea", "_sccs")
  ) %>%
  left_join(
    climate,
    by = c("soc_id_sccs" = "soc_id")
  )

# ---- Long verzi?? a besz??mol??hoz ----

d_long <- d_wide %>%
  pivot_longer(
    cols = c(
      EA034,
      all_of(sccs_vars),
      all_of(climate_vars)
    ),
    names_to = "variable",
    values_to = "value"
  )

# ---- Ment??s ----

write_csv(d_wide, "D:/Source/github.com/rtotbagi/ClimateXSupernatural/d_wide.csv")
write_csv(d_long, "D:/Source/github.com/rtotbagi/ClimateXSupernatural/d_long.csv")
