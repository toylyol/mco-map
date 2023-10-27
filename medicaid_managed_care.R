
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

packages <- c("jsonlite", "dplyr", "stringr", "sf", "leaflet")

invisible(lapply(packages, library, character.only = TRUE))


# Retrieve plan dataset from Medicaid site ----

url2 <- "https://data.medicaid.gov/api/1/datastore/query/0bef7b8a-c663-5b14-9a46-0b5c2b86b0fe/0?conditions[0][property]=year&conditions[0][value]=2021"

raw_plan_data <- fromJSON(url2)


# Convert to df ----

plan_data <- tibble::as_tibble(raw_plan_data$results) |> 
  tibble::rowid_to_column("id") # add ID column


# Clean up data frame ----

plan_data <- plan_data |> 
  # remove behavioral health/dental/senior only
  filter(!str_detect(program_name, "BHO|BHSO|Dental|PACE|Senior")) |> 
  # remove odd state suffixes denoting notes
  mutate(state = case_when(str_detect(state, "[0-9]") ~ str_replace_all(state, "[0-9]", ""),
                           TRUE ~ state)) |> 
  # replace commas with or operator to facilitate subsetting geometries
  mutate(geographic_region = str_replace_all(geographic_region, ", ", "|")) |> 
  mutate(geographic_region = str_replace_all(geographic_region, "and | counties", ""))

# TODO: Exclude geographic regions that are non-counties or statewide.


# Read in GPKG ----

us_counties <- sf::st_read(dsn = "us_counties_2021.gpkg")

## Reproject to leaflet's desired CRS

us_counties <- st_transform(us_counties, crs = 4326)

## Check 

st_crs(us_counties)


# Iterate test ----

## create function

createCoverageArea <- function(df_subset){
  
  identifier <- df_subset$id
  
  state <- df_subset$state
  
  area <- df_subset$geographic_region
  
  if(area == "Statewide"){
    
    us_counties |> 
      filter(str_detect(STATE_NAME, state)) |> 
      st_combine() |> 
      st_as_sf() |> 
      mutate(id = identifier) |> 
      left_join(df_subset, by = "id") } else {
        
        us_counties |> 
          filter(STATE_NAME == state) |>  # in case there are counties with the same name
          filter(str_detect(NAME,area)) |> 
          st_combine() |> 
          st_as_sf() |> 
          mutate(id = identifier) |> 
          left_join(df_subset, by = "id")
      }
  
}

## subset data

data <- filter(plan_data, state == "Arizona")

## create two vectors to prep for iterate

input_list <- vector("list", length(data))

output_shp <- vector("list", length(data))

## use for loop to create list to isolat each plan's data

for (i in seq_along(data)){
  
  temp <- data[i, ]
   
  input_list[[i]] <- temp
  
}

## iterate over each plan's data to add multipolygon coverage area

output_list <- purrr::map(input_list, createCoverageArea)

## combine list with each plan's data and coverage area into one sf df

map_df <- bind_rows(output_list)

## make multi-layer map

leaflet() %>%
  addTiles() %>%   # add base map
  addPolygons(data = map_df,
              weight = 1,
              opacity = 1.0, fillOpacity = 0.25, 
              group = map_df$plan_name,
              stroke = TRUE,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)
  ) %>%
  addLayersControl(
    overlayGroups = map_df$plan_name,
    options = layersControlOptions(collapsed = FALSE)
  ) 


# References ----

# See landing Page: https://www.medicaid.gov/medicaid/managed-care/enrollment-report/index.html

# See data source url2: https://data.medicaid.gov/dataset/0bef7b8a-c663-5b14-9a46-0b5c2b86b0fe/data?conditions[0][property]=year&conditions[0][value]=2021&conditions[0][operator]=%3D

# See 2021 enrollment report: https://www.medicaid.gov/sites/default/files/2023-07/2021-medicaid-managed-care-enrollment-report.pdf

# See KFF aggregation: https://www.kff.org/medicaid/state-indicator/total-medicaid-mcos/
