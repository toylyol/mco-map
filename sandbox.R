
# Medicaid Managed Care Programs by State ----

## Load packages ----

packages <- c("jsonlite", "dplyr", "stringr")

invisible(lapply(packages, library, character.only = TRUE))


## Retrieve program dataset from Medicaid site ----

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


## References ----

# See landing Page: https://www.medicaid.gov/medicaid/managed-care/enrollment-report/index.html

# See data source url1: https://data.medicaid.gov/dataset/7459d190-e592-42e0-9b66-6f4e7e05eb9b


# Retrieve US counties shapefiles ----

us_counties <- tigris::counties(cb = TRUE, year = "2021", resolution = "500k")

## Remove US territories from shapefile ----

territory_fips <- c("60", "66", "69", "78") # specify state FIPS codes not needed; note PR is "72"

`%not_in%` <- Negate( `%in%` )

us_counties <- us_counties %>%
  filter(STATEFP %not_in% territory_fips)

## Save as GPKG ----

sf::st_write(obj = us_counties, dsn = "us_counties_2021.gpkg")


# Restructure data test ----

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


# See states with managed care ----

plan_data$state |> 
  unique() |> 
  sort()

# There are 54. Some states are duplicated with an odd suffix: Colorado and Colorado3, Ohio and Ohio6, etc.
# There is only one HI, but it has the odd suffix: Hawaii5. Same for OK and VT: Oklahoma7 and Vermont9, respectively.


## Identify states with odd numeric suffix ----

odd_states <- plan_data |>
  filter(stringr::str_detect(state, "[0-9]")) |>
  pull(state) |>
  unique()

plan_data |>
  filter(state %in% odd_states) |>
  View()

# See the explanations in the notes column.


# Visualize each plan as a map layer test ----

## Filter data

az_data <- plan_data |> filter(state == "Arizona")

## Create function

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
          filter(str_detect(NAME,area)) |> 
          st_combine() |> 
          st_as_sf() |> 
          mutate(id = identifier) |> 
          left_join(df_subset, by = "id")
        
      }
}

## Subset geometries

az_shp1 <- az_data[1, ] |> 
  createCoverageArea()

az_shp2 <- az_data[2, ] |> 
  createCoverageArea()

## Combine into one sf df

az_mcos <- az_shp1 |> 
  bind_rows(az_shp2)

## Make a multi-layer leaflet map

leaflet() %>%
  addTiles() %>%   # add base map
  addPolygons(data = az_mcos,
              weight = 1,
              opacity = 1.0, fillOpacity = 0.7, 
              group = az_mcos$plan_name,
              stroke = TRUE,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)
  ) %>%
  addLayersControl(
    overlayGroups = az_mcos$plan_name,
    options = layersControlOptions(collapsed = FALSE)
  ) 
