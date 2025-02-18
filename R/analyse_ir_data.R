# Download the IR bioassay data for specific species from this research article:
# https://doi.org/10.1038/s41597-019-0134-2

# The data are hosted here in the Dryad data repository:
# https://doi.org/10.5061/dryad.dn4676s
repo_doi <- "10.5061/dryad.dn4676s"

# There are several files, we want the csv file named
# 2_standard-WHO-susc-test_species.csv
file_name <- "2_standard-WHO-susc-test_species.csv"

# we can download the files manually, or automatically using the rdryad R
# package. This uses caching, so if we download it once, it shouldn't download
# again, even if we run the code again.
# install.packages("rdryad")
library(rdryad)

# this returns a data object pointing to the downloaded files
download_info <- rdryad::dryad_download(dois = repo_doi)
downloaded_file_paths <- download_info[[repo_doi]]

# now we want to find the specific file we want
file_index <- grep(pattern = file_name,
                   x = downloaded_file_paths)
file_path <- downloaded_file_paths[file_index]

# let's copy it to our data folder
new_file_path <- file.path("data", file_name)
file.copy(file_path, new_file_path)

# now we can read in the data
library(tidyverse)
ir_data_raw <- read_csv(file = new_file_path)
# spec(ir_data_raw)

# let's subset this down to the data we want to work with
ir_data <- ir_data_raw %>%
  # we'll only keep the records that have specific point locations, and the percent mortality is given (most of them)
  filter(
    `Site type` == "point",
    !is.na(`Percent mortality`)
  ) %>%
  select(
    Species,
    Generation,
    `Test protocol`,
    `Insecticide tested`,
    `Insecticide class`,
    `Concentration (%)`,
    `No. mosquitoes tested`,
    `Percent mortality`,
    Country,
    `Site name`,
    Longitude,
    Latitude
  )

summary(ir_data)

