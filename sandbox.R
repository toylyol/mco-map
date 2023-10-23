
# ___  ___         _ _           _     _  ___  ___                                 _   _____                
# |  \/  |        | (_)         (_)   | | |  \/  |                                | | /  __ \               
# | .  . | ___  __| |_  ___ __ _ _  __| | | .  . | __ _ _ __   __ _  __ _  ___  __| | | /  \/ __ _ _ __ ___ 
# | |\/| |/ _ \/ _` | |/ __/ _` | |/ _` | | |\/| |/ _` | '_ \ / _` |/ _` |/ _ \/ _` | | |    / _` | '__/ _ \
# | |  | |  __/ (_| | | (_| (_| | | (_| | | |  | | (_| | | | | (_| | (_| |  __/ (_| | | \__/\ (_| | | |  __/
# \_|  |_/\___|\__,_|_|\___\__,_|_|\__,_| \_|  |_/\__,_|_| |_|\__,_|\__, |\___|\__,_|  \____/\__,_|_|  \___|
#                                                                    __/ |                                  
#                                                                   |___/                                   
# ______                                         ______         _____ _        _                            
# | ___ \                                        | ___ \       /  ___| |      | |                           
# | |_/ / __ ___   __ _ _ __ __ _ _ __ ___  ___  | |_/ /_   _  \ `--.| |_ __ _| |_ ___                      
# |  __/ '__/ _ \ / _` | '__/ _` | '_ ` _ \/ __| | ___ \ | | |  `--. \ __/ _` | __/ _ \                     
# | |  | | | (_) | (_| | | | (_| | | | | | \__ \ | |_/ / |_| | /\__/ / || (_| | ||  __/                     
# \_|  |_|  \___/ \__, |_|  \__,_|_| |_| |_|___/ \____/ \__, | \____/ \__\__,_|\__\___|                     
#                  __/ |                                 __/ |                                              
#                 |___/                                 |___/                                               


# Load packages ----

packages <- c("jsonlite", "dplyr", "stringr")

invisible(lapply(packages, library, character.only = TRUE))


# Retrieve program dataset from Medicaid site ----

url1 <- "https://data.medicaid.gov/api/1/datastore/query/7459d190-e592-42e0-9b66-6f4e7e05eb9b/0"

raw_program_data <- fromJSON(url1)


## Convert and streamline raw data ----

program_data <- tibble::as_tibble(raw_program_data$results) |> 
  select(features, program_type, statewide_or_regionspecific, state)


## Identify all program types ----

all_programs <- program_data$program_type |> 
  unique() |> 
  sort()


## Specify program types to include ----

eligible_programs <- program_data |> 
  filter(stringr::str_detect(program_type,"MCO")) |> 
  select(program_type) |> 
  pull() |> 
  unique()


## Subset data ----

program_data <- program_data |> 
  filter(program_type %in% eligible_programs)


# References ----

# See landing Page: https://www.medicaid.gov/medicaid/managed-care/enrollment-report/index.html

# See data source url1: https://data.medicaid.gov/dataset/7459d190-e592-42e0-9b66-6f4e7e05eb9b
