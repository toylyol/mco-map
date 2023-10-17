
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


#*************************************#
# 2021 Managed Care Programs By State #
#*************************************#

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


#*******************************************************************************#
# 2021 Managed Care Enrollment by Program and Plan                              #
# This is the dataset used by KFF: It is Table 5 in the 2020 enrollment report. #
#*******************************************************************************#

# Retrieve plan dataset from Medicaid site ----

url2 <- "https://data.medicaid.gov/api/1/datastore/query/0bef7b8a-c663-5b14-9a46-0b5c2b86b0fe/0?conditions[0][property]=year&conditions[0][value]=2021"

raw_plan_data <- fromJSON(url2)


## Convert to df ----

plan_data <- tibble::as_tibble(raw_plan_data$results)


## Identify unique program names ----

plan_data$program_name |> 
  unique() |> 
  sort() 

# There are 161...


## Subset dataset to comprehensive MCOs only ----

plan_data <- plan_data |> 
  filter(stringr::str_detect(program_name, "Comprehensive MCO"))


# See states with comprehensive MCOs ----

plan_data$state |> 
  unique() |> 
  sort()

# There are 54. Some states are duplicated with an odd suffix: Colorado and Colorado3, Ohio and Ohio6, etc.
# There is only one HI, but it has the odd suffix: Hawaii5. Same for OK and VT: Oklahoma7 and Vermont9, respectively.


# Identify states with odd numeric suffix ---

odd_states <- plan_data |> 
  filter(stringr::str_detect(state, "[0-9]")) |> 
  pull(state) |> 
  unique()

plan_data |> 
  filter(state %in% odd_states) |> 
  View()

# See the explanations in the notes column.


# References ----

# See landing Page: https://www.medicaid.gov/medicaid/managed-care/enrollment-report/index.html

# See data source url1: https://data.medicaid.gov/dataset/7459d190-e592-42e0-9b66-6f4e7e05eb9b

# See data source url2: https://data.medicaid.gov/dataset/0bef7b8a-c663-5b14-9a46-0b5c2b86b0fe/data?conditions[0][property]=year&conditions[0][value]=2021&conditions[0][operator]=%3D

# See 2021 enrollment report: https://www.medicaid.gov/sites/default/files/2023-07/2021-medicaid-managed-care-enrollment-report.pdf

# See KFF aggregation: https://www.kff.org/medicaid/state-indicator/total-medicaid-mcos/
