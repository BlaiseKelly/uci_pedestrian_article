library(openxlsx)
library(reshape2)
library(dplyr)
library(stats19)
library(sf)
library(purrr)
library(lubridate)
library(geographr)

# download homicide rates in England and Wales
download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/appendixtableshomicideinenglandandwales/current/homicideinenglandandwalesappendixtablesyemar2025finalv2.xlsx",
              destfile = "data/homicideinenglandandwalesappendixtablesyemar2025finalv2.xlsx", mode = "wb")


# homicide data only, useful as it is broken down by location
homicide_streets = openxlsx::read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/appendixtableshomicideinenglandandwales/current/homicideinenglandandwalesappendixtablesyemar2025finalv2.xlsx",
                                       sheet = "Worksheet 19",startRow = 9) [1:33,] |> 
  reshape2::melt(c("Sex.of.victim","Location.type","Location")) |> 
  mutate(reshape2::colsplit(variable, "\\.",c("month", "year", "rest"))) |> 
  filter(Location == "Street, footpath, alleyway" & year >=2021) |> 
  select(sex = Sex.of.victim,
         location = Location,
         year,
         killed = value) |> 
  group_by(sex) |> 
  summarise(total = sum(as.numeric(killed),na.rm = TRUE))

outdoor_spaces = c("Street, footpath, alleyway","Open outdoor area [note 42]")

# all public space
homicide_public = openxlsx::read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/appendixtableshomicideinenglandandwales/current/homicideinenglandandwalesappendixtablesyemar2025finalv2.xlsx",
                                      sheet = "Worksheet 19",startRow = 9) [1:33,] |> 
  reshape2::melt(c("Sex.of.victim","Location.type", "Location")) |> 
  mutate(reshape2::colsplit(variable, "\\.",c("month", "year", "rest"))) |> 
  filter(Location %in% outdoor_spaces & year >=2021) |> 
  select(sex = Sex.of.victim,
         location = Location,
         year,
         killed = value) |> 
  ungroup() |> 
  group_by(sex) |> 
  summarise(total = sum(as.numeric(killed),na.rm = TRUE))

# Recalculate STATS19 data to match Police crime data.
# define the year range of analysis
base_year = 2021
upper_year = 2025

yrs2get = ifelse(base_year < year(Sys.Date())-2 & base_year >= year(Sys.Date())-6,"5 years",
                 ifelse(base_year < year(Sys.Date())-6,"2004",base_year))

# import crashes and trim to temporal parameters and make spatial
crashes_gb = get_stats19(yrs2get, type = "collision") |> 
  filter(collision_year >= base_year & collision_year <= upper_year) |> 
  mutate(junction_detail = ifelse(is.na(junction_detail),"unknown (self reported)",junction_detail)) |> 
  format_sf()

# import casualties, add fatal column to match serious and slight and include e-scooters from vehicle data, add in dft age bands (different to those included in data)
casualties_gb = get_stats19(yrs2get, type = "casualty")|> 
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) 

# side calculation for casualties by sex
casualties_pv_fat = casualties_gb |> 
  filter(pedestrian_location == "On footway or verge" & casualty_type == "Pedestrian" & fatal_count > 0)

crashes_ew = crashes_gb |> 
  filter(collision_index %in% casualties_pv_fat$collision_index) |> 
  filter(date >= "2021-04-01" & date <= "2025-03-31")

# import country boundaries
countries <- geographr::boundaries_countries20 |>
  st_transform(27700)

# make the crash data sf to intersect with 
cra_country <- crashes_ew |>
  st_join(countries) |> 
  filter(country20_name %in% c("England","Wales"))

cas_pv_fat_eng_wales = casualties_pv_fat |> 
  filter(collision_index %in% cra_country$collision_index) |> 
  group_by(sex_of_casualty) |> 
  summarise(all = n())

# female pavement fatalities
cas_pv_fat_eng_wales$all[1]

# side calculation for casualties by sex
casualties_ped_fat = casualties_gb |> 
  filter(casualty_type == "Pedestrian" & fatal_count > 0)

# filter for same period as homicide data
crashes_ew = crashes_gb |> 
  filter(collision_index %in% casualties_ped_fat$collision_index) |> 
  filter(date >= "2021-04-01" & date <= "2025-03-31")

# import country boundaries
countries <- geographr::boundaries_countries20 |>
  st_transform(27700)

# filter only data for England and Wales
cra_country <- crashes_ew |>
  st_join(countries) |> 
  filter(country20_name %in% c("England","Wales"))

# summarise pedestrian fatalities by sex
cas_ped_fat_eng_wales = casualties_ped_fat |> 
  filter(collision_index %in% cra_country$collision_index) |> 
  group_by(sex_of_casualty) |> 
  summarise(all = n())

# total pedestrian fatalities
sum(cas_ped_fat_eng_wales$all)

However, in England and Wales between April 2021 and March 2025 47 women were murdered on “streets, footpaths, alleyways” compared to 72 killed by road violence whilst on a pavement. For all outdoor spaces 784 people were murdered in “open outdoor areas”. In the same period nearly double this number (1417) of pedestrians were killed on streets, mostly by motor vehicles.
