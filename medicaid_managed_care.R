
# ___  ___         _ _           _     _  ___  ___                                 _   _____                
# |  \/  |        | (_)         (_)   | | |  \/  |                                | | /  __ \               
# | .  . | ___  __| |_  ___ __ _ _  __| | | .  . | __ _ _ __   __ _  __ _  ___  __| | | /  \/ __ _ _ __ ___ 
# | |\/| |/ _ \/ _` | |/ __/ _` | |/ _` | | |\/| |/ _` | '_ \ / _` |/ _` |/ _ \/ _` | | |    / _` | '__/ _ \
# | |  | |  __/ (_| | | (_| (_| | | (_| | | |  | | (_| | | | | (_| | (_| |  __/ (_| | | \__/\ (_| | | |  __/
# \_|  |_/\___|\__,_|_|\___\__,_|_|\__,_| \_|  |_/\__,_|_| |_|\__,_|\__, |\___|\__,_|  \____/\__,_|_|  \___|
#                                                                    __/ |                                  
#                                                                   |___/                                   

# Load packages ----

packages <- c("jsonlite", "dplyr")

invisible(lapply(packages, library, character.only = TRUE))


# Retrieve data from Medicaid site ----

url <- "https://data.medicaid.gov/api/1/datastore/query/7459d190-e592-42e0-9b66-6f4e7e05eb9b/0"

raw_data <- fromJSON(url)


# Convert and streamline raw data ----

data <- tibble::as_tibble(raw_data$results) |> 
  select(features, program_type, statewide_or_regionspecific, state)


# Identify all program types 

all_programs <- data$program_type |> 
  unique() |> 
  sort()


# Specify program types to include ----

eligible_programs <- data |> 
  filter(stringr::str_detect(program_type,"MCO")) |> 
  select(program_type) |> 
  pull() |> 
  unique()


# Subset data ----

data <- data |> 
  filter(program_type %in% eligible_programs)


# References ----

# See landing Page: https://www.medicaid.gov/medicaid/managed-care/enrollment-report/index.html

# See data source: https://data.medicaid.gov/dataset/7459d190-e592-42e0-9b66-6f4e7e05eb9b

# See KFF aggregation: https://www.kff.org/medicaid/state-indicator/total-medicaid-mcos/
