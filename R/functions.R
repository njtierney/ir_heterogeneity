download_ir_bioassay_data <- function(doi = "10.5061/dryad.dn4676s",
                                      file_name = "2_standard-WHO-susc-test_species.csv",
                                      directory){
  # Download the IR bioassay data for specific species from this research article:
  # https://doi.org/10.1038/s41597-019-0134-2
  
  
  # The data are hosted here in the Dryad data repository:
  # https://doi.org/10.5061/dryad.dn4676s
  repo_doi <- doi
  
  # There are several files, we want the csv file named
  # 2_standard-WHO-susc-test_species.csv
  file_name <- file_name
  
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
  new_file_path <- file.path(directory, file_name)
  file.copy(file_path, new_file_path)
}


read_bioassay_data <- function(filepath){
  
  ir_data_raw <- read_csv(file = filepath,
                          na = c("", "NA", "NR", "NF"),
                          col_types = cols(
                            `Start month` = col_integer(),
                            `Start year` = col_integer(),
                            `End month` = col_integer(),
                            `End year` = col_integer(),
                            Species = col_character(),
                            `Identification method 1` = col_character(),
                            Generation = col_character(),
                            `Test protocol` = col_character(),
                            `Insecticide tested` = col_character(),
                            `Insecticide class` = col_character(),
                            `Concentration (%)` = col_double(),
                            `No. mosquitoes tested` = col_integer(),
                            `Percent mortality` = col_double(),
                            Country = col_character(),
                            `Site type` = col_character(),
                            `Site name` = col_character(),
                            Latitude = col_double(),
                            Longitude = col_double(),
                            .default = col_guess()
                          ))
  
  return(ir_data_raw)
  
}

clean_bioassay_data <- function(ir_data_raw){
  
  # let's subset this down to the data we want to work with
  ir_data <- ir_data_raw %>%
    # we'll only keep the records that have specific point locations, and the
    # percent mortality is given (this is most of them)
    filter(
      # only records with specific point locations
      `Site type` == "point",
      !is.na(Latitude),
      !is.na(Longitude),
      # make sure we know the mortality and the concentration
      !is.na(`Percent mortality`),
      !is.na(`Concentration (%)`),
      # only records we are sure were collected in one year
      !is.na(`Start year`),
      !is.na(`End year`),
      `Start year` == `End year`
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
      `Start year`,
      Country,
      `Site name`,
      Longitude,
      Latitude
    ) %>%
    # we're only using single-year data, so rename this column to somehting more
    # meaningful
    rename(
      Year = `Start year`
    )
  
  return(ir_data)
  
}