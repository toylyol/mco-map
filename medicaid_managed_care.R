
# ___  ___         _ _           _     _  ___  ___                                 _   _____                                            
# |  \/  |        | (_)         (_)   | | |  \/  |                                | | /  __ \                                           
# | .  . | ___  __| |_  ___ __ _ _  __| | | .  . | __ _ _ __   __ _  __ _  ___  __| | | /  \/ __ _ _ __ ___                             
# | |\/| |/ _ \/ _` | |/ __/ _` | |/ _` | | |\/| |/ _` | '_ \ / _` |/ _` |/ _ \/ _` | | |    / _` | '__/ _ \                            
# | |  | |  __/ (_| | | (_| (_| | | (_| | | |  | | (_| | | | | (_| | (_| |  __/ (_| | | \__/\ (_| | | |  __/                            
# \_|  |_/\___|\__,_|_|\___\__,_|_|\__,_| \_|  |_/\__,_|_| |_|\__,_|\__, |\___|\__,_|  \____/\__,_|_|  \___|                            
#                                                                    __/ |                                                              
#                                                                   |___/                                                               
#  _____                _ _                      _    ______        ______                                        _______ _             
# |  ___|              | | |                    | |   | ___ \       | ___ \                                      / / ___ \ |            
# | |__ _ __  _ __ ___ | | |_ __ ___   ___ _ __ | |_  | |_/ /_   _  | |_/ / __ ___   __ _ _ __ __ _ _ __ ___    / /| |_/ / | __ _ _ __  
# |  __| '_ \| '__/ _ \| | | '_ ` _ \ / _ \ '_ \| __| | ___ \ | | | |  __/ '__/ _ \ / _` | '__/ _` | '_ ` _ \  / / |  __/| |/ _` | '_ \ 
# | |__| | | | | | (_) | | | | | | | |  __/ | | | |_  | |_/ / |_| | | |  | | | (_) | (_| | | | (_| | | | | | |/ /  | |   | | (_| | | | |
# \____/_| |_|_|  \___/|_|_|_| |_| |_|\___|_| |_|\__| \____/ \__, | \_|  |_|  \___/ \__, |_|  \__,_|_| |_| |_/_/   \_|   |_|\__,_|_| |_|
#                                                             __/ |                  __/ |                                              
#                                                            |___/                  |___/                                               
#                                 

# NOTE: This is the dataset used by KFF: It was Table 5 in the 2020 enrollment report.


# Load packages ----

packages <- c("jsonlite", "dplyr", "stringr", "sf")

invisible(lapply(packages, library, character.only = TRUE))


# Retrieve plan dataset from Medicaid site ----

url2 <- "https://data.medicaid.gov/api/1/datastore/query/0bef7b8a-c663-5b14-9a46-0b5c2b86b0fe/0?conditions[0][property]=year&conditions[0][value]=2021"

raw_plan_data <- fromJSON(url2)


# Convert to df ----

plan_data <- tibble::as_tibble(raw_plan_data$results)


## Identify unique program names ----
# 
# plan_data$program_name |> 
#   unique() |> 
#   sort() 
# 
# There are 161...


# Subset dataset to remove irrelevant plans ----

plan_data <- plan_data |> 
  filter(!stringr::str_detect(program_name, "BHO|BHSO|Dental|PACE|Senior"))

# Remove behavioral health/dental/senior only


# See states with managed care ----

plan_data$state |> 
  unique() |> 
  sort()

# There are 54. Some states are duplicated with an odd suffix: Colorado and Colorado3, Ohio and Ohio6, etc.
# There is only one HI, but it has the odd suffix: Hawaii5. Same for OK and VT: Oklahoma7 and Vermont9, respectively.


# Identify states with odd numeric suffix ---
# 
# odd_states <- plan_data |> 
#   filter(stringr::str_detect(state, "[0-9]")) |> 
#   pull(state) |> 
#   unique()
# 
# plan_data |> 
#   filter(state %in% odd_states) |> 
#   View()
# 
# See the explanations in the notes column.


## Remove odd state suffixes ----

plan_data <- plan_data |> 
  mutate(state = case_when(str_detect(state, "[0-9]") ~ str_replace_all(state, "[0-9]", ""),
                           TRUE ~ state))


# Add ID column so that user can compare map and reactable ----
 
plan_data <- plan_data |> 
   tibble::rowid_to_column("id")


# Retrieve US counties shapefiles ----
# 
# us_counties <- tigris::counties(cb = TRUE, year = "2021", resolution = "500k")
# 
# ## Remove US territories from shapefile ----
# 
# territory_fips <- c("60", "66", "69", "78") # specify state FIPS codes not needed; note PR is "72"
# 
# `%not_in%` <- Negate( `%in%` )
# 
# us_counties <- us_counties %>%
#   filter(STATEFP %not_in% territory_fips)
#
# ## Save as GPKG ----
# 
# sf::st_write(obj = us_counties, dsn = "us_counties_2021.gpkg")


# Test ----

arizona_data <- plan_data |> 
  filter(state == "Arizona") |> 
  tidyr::separate_wider_delim(cols = geographic_region, # specify col to separate
                              delim = ", ",
                              names_sep = "", # name as many cols as needed automatically
                              too_few = "align_start") |>  # make as many cols as needed
  mutate(across(starts_with("geographic_region"), 
                ~ str_replace_all(., "and | counties", ""))
  )|> 
  tidyr::pivot_longer(cols = starts_with("geographic_region"),
                      values_to = "geo_region") |> 
  mutate(name = row_number(),
         geo_region = ifelse(geo_region == "Statewide", state, geo_region)
         )|> 
  tidyr::pivot_wider(id_cols = c("id", "geo_region"),
                     values_from = "plan_name",
                     names_from = name,
                     names_prefix = "plan") |> 
  tidyr::pivot_wider(id_cols = "geo_region",
                     values_from = starts_with("plan"),
                     names_from = "id") |> 
  tidyr::unite("plans", starts_with("plan"), sep = ",")|> 
  mutate(plans =  
           str_replace_all(plans, "NA,|,NA", "")
  ) |> 
  filter(!is.na(geo_region))


# Read in GPKG ----

us_counties <- sf::st_read(dsn = "us_counties_2021.gpkg")

## Reproject to leaflet's desired CRS

us_counties <- st_transform(us_counties, crs = 4326)

## Check 

st_crs(us_counties)


# Test 2 ----

arizona_data <- plan_data |> filter(state == "Arizona")

arizona_data <- arizona_data |> 
  mutate(geographic_region = str_replace_all(geographic_region, ", ", "|")) |> 
  mutate(geographic_region = str_replace_all(geographic_region, "and | counties", ""))

arizona_shp1 <- us_counties |> 
  filter(str_detect(NAME,arizona_data$geographic_region[1])) |> 
  st_union() |> 
  st_as_sf()

arizona_shp2 <- us_counties |> 
  filter(str_detect(NAME,arizona_data$geographic_region[1])) |> 
  bind_rows() 

mapview::mapview(arizona_shp1)

mapview::mapview(arizona_shp2)


# References ----

# See landing Page: https://www.medicaid.gov/medicaid/managed-care/enrollment-report/index.html

# See data source url2: https://data.medicaid.gov/dataset/0bef7b8a-c663-5b14-9a46-0b5c2b86b0fe/data?conditions[0][property]=year&conditions[0][value]=2021&conditions[0][operator]=%3D

# See 2021 enrollment report: https://www.medicaid.gov/sites/default/files/2023-07/2021-medicaid-managed-care-enrollment-report.pdf

# See KFF aggregation: https://www.kff.org/medicaid/state-indicator/total-medicaid-mcos/
