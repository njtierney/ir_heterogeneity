source("R/functions.R")

download_ir_bioassay_data(
  doi = "10.5061/dryad.dn4676s",
  file_name = "2_standard-WHO-susc-test_species.csv",
  directory = "data"
)

# now we can read in the data
library(tidyverse)

ir_data_raw <- read_bioassay_data(
  filepath = "data/2_standard-WHO-susc-test_species.csv"
  )

ir_data <- clean_bioassay_data(ir_data_raw = ir_data_raw)


# how many observations of each insecticide and concentration (note each
# insecticide is only with one concentration)
ir_data %>%
  group_by(
    `Insecticide class`,
    `Insecticide tested`,
    `Concentration (%)`
  ) %>%
  summarise(
    Records = n(),
    .groups = "drop"
  ) %>%
  arrange(
    `Insecticide class`,
    desc(Records)
  )

# find point locations, years, species, and insecticides with multiple
# records
ir_data %>%
  group_by(
    Species,
    `Insecticide tested`,
    Country,
    `Site name`,
    Longitude,
    Latitude,
    Year,
  ) %>%
  summarise(
    Records = n(),
    .groups = "drop"
  ) %>%
  filter(
    Records > 1
  ) %>%
  arrange(
    desc(Records)
  )

