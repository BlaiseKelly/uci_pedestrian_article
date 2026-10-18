library(sf)
library(dplyr)
library(lubridate)
devtools::load_all("C:/Users/blais/Desktop/stats19")
#library(stats19)
library(bbplot)
library(ggplot2)
library(treemapify)
library(glue)
library(ltc)
library(gt)
#pak::pak("activeanalytics-nl/stats19reports")
#devtools::load_all("../stats19reports")

dir.create("outputs/stats19/",recursive = TRUE)

#summarise casualties function
summarise_casualties_per_collision <- function(casualties) {
  cas_summary <- casualties %>%
    dplyr::mutate(fatal_count = dplyr::if_else(casualty_severity == "Fatal", 1, 0)) %>%
    dplyr::select(collision_index, collision_year, casualty_type,
                  pedestrian_location, fatal_count,
                  casualty_adjusted_severity_serious,
                  casualty_adjusted_severity_slight) %>%
    dplyr::group_by(collision_index, collision_year) %>%
    dplyr::summarise(
      Fatal = sum(fatal_count),
      Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
      Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
      .groups = "drop"
    )
  
  cas_summary
}

casualty_type_lookup <- data.frame(
  casualty_type = c(
    "Car occupant", "Motorcycle 125cc and under rider or passenger",
    "Cyclist", "Pedestrian", "Motorcycle over 500cc rider or passenger",
    "Motorcycle over 125cc and up to 500cc rider or  passenger",
    "Motorcycle 50cc and under rider or passenger",
    "Bus or coach occupant (17 or more pass seats)",
    "Taxi/Private hire car occupant",
    "Van / Goods vehicle (3.5 tonnes mgw or under) occupant",
    "Other vehicle occupant", "Data missing or out of range",
    "Motorcycle - unknown cc rider or passenger",
    "Goods vehicle (7.5 tonnes mgw and over) occupant",
    "Electric motorcycle rider or passenger",
    "Minibus (8 - 16 passenger seats) occupant",
    "Mobility scooter rider", "Horse rider",
    "Goods vehicle (over 3.5t. and under 7.5t.) occupant",
    "Goods vehicle (unknown weight) occupant",
    "Agricultural vehicle occupant", "E-scooter rider","Personal Powered Transporter"
  ),
  short_name = c(
    "Car occupant", "Motorcyclist", "Cyclist", "Pedestrian", "Motorcyclist",
    "Motorcyclist", "Motorcyclist", "Bus occupant", "Taxi occupant",
    "Goods vehicle occupant", "Other vehicle", "Data missing",
    "Motorcyclist", "Goods vehicle occupant", "Motorcyclist",
    "Bus occupant", "Mobility scooter rider", "Horse rider",
    "Goods vehicle occupant", "Goods vehicle occupant",
    "Agricultural vehicle occupant", "E-scooter rider","Personal Powered Transporter rider"
  ),
  in_or_on = c(
    "Car", "Motorcyclist", "Bicycle", "Foot", "Motorcyclist", "Motorcyclist",
    "Motorcyclist", "Bus", "Taxi", "Goods vehicle", "Other vehicle",
    "Data missing", "Motorcyclist", "Goods vehicle", "Motorcyclist",
    "Bus", "Mobility scooter", "Horse", "Goods vehicle", "Goods vehicle",
    "Agricultural vehicle", "E-scooter","Personal Powered Transporter"
  )
)

vehicle_type_lookup <- data.frame(
  vehicle_type = c(
    "Car", "Motorcycle 125cc and under", "Taxi/Private hire car",
    "Motorcycle over 500cc", "Motorcycle over 125cc and up to 500cc",
    "Goods 7.5 tonnes mgw and over", "Goods over 3.5t. and under 7.5t",
    "Bus or coach (17 or more pass seats)",
    "Van / Goods 3.5 tonnes mgw or under", "Motorcycle 50cc and under",
    "Other vehicle", "Pedal cycle", "Motorcycle - unknown cc",
    "Electric motorcycle", "e-scooter", "Minibus (8 - 16 passenger seats)",
    "Mobility scooter", "Unknown vehicle type (self rep only)",
    "Agricultural vehicle", "Goods vehicle - unknown weight",
    "Data missing or out of range", "Ridden horse", "Tram","Personal powered transporter"
  ),
  short_name = c(
    "Car", "Motorcycle", "Taxi", "Motorcycle", "Motorcycle",
    "Goods vehicle", "Goods vehicle", "Bus", "Goods vehicle", "Motorcycle",
    "Other vehicle", "Pedal cycle", "Motorcycle", "Motorcycle", "e-scooter",
    "Bus", "Mobility scooter", "Other vehicle", "Agricultural vehicle",
    "Goods vehicle", "Data missing or out of range", "Ridden horse", "Tram","Personal powered transporter"
  ),
  driver_type = c(
    "Car driver", "Motorcyclist", "Taxi driver", "Motorcyclist",
    "Motorcyclist", "Goods vehicle driver", "Goods vehicle driver",
    "Bus driver", "Goods vehicle driver", "Motorcyclist", "Other vehicle",
    "Cyclist", "Motorcyclist", "Motorcyclist", "E-scooter driver",
    "Bus driver", "Mobility scooter rider", "Other vehicle",
    "Agricultural vehicle driver", "Goods vehicle driver", "Data missing",
    "Horse rider", "Tram driver","Personal powered transporter"
  )
)

summarise_vehicle_types <- function(vehicles, summary_type = c("short_name", "driver_type")){
  
  summary_type <- match.arg(summary_type)
  
  vehicle_type_lookup <- vehicle_type_lookup
  
  veh_new_types <- vehicles |>
    left_join(vehicle_type_lookup, by = "vehicle_type")
  #mutate(vehicle_type = !!sym(summary_type))
  
  return(veh_new_types)
  
}

plot_pavement_severity_by_year <- function(casualties_pv, base_year, 
                                           upper_year, save_path = NULL,
                                           theme = "ltc") {
  
  plot_dat <- casualties_pv |>
    dplyr::filter(collision_year >= base_year &
                    collision_year <= upper_year) |>
    dplyr::select(collision_year, Fatal, Serious, Slight) |>
    tidyr::pivot_longer(-collision_year, names_to = "variable",
                        values_to = "number_of_casualties") |>
    dplyr::group_by(collision_year, variable) |>
    dplyr::summarise(number_of_casualties = sum(number_of_casualties,
                                                na.rm = TRUE),
                     .groups = "drop")
  
  if(theme[1] == "bbc"){
    pal = rev(c("#B0B2B4", "#141414","#DC2878"))
  }
  if(theme[1] == "ltc"){
    pal = ltc::ltc(trio3, 3)
  }
  if(NROW(theme)>1){
    pal = theme
  }
  
  
  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(x = collision_year,
                                    y = number_of_casualties,
                                    fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      title = "Pedestrian pavement casualties",
      subtitle = paste0("Between ", base_year, " and ", upper_year,
                        " by severity")) +
    bbplot::bbc_style()
  
  if (!is.null(save_path)) {
    bbplot::finalise_plot(plot_name = p, source_name = "Source: STATS19",
                          save_filepath = save_path)
  }
  invisible(p)
}

# define the year range of analysis
base_year = 2021
upper_year = 2025

# request the correct period in stats19
yrs2get = ifelse(base_year < year(Sys.Date())-2 & base_year >= year(Sys.Date())-6,"5 years",
                 ifelse(base_year < year(Sys.Date())-6,"2004",base_year))

# import crashes and trim to temporal parameters and make spatial
crashes_gb = get_stats19(yrs2get, type = "collision") |> 
  filter(collision_year >= base_year & collision_year <= upper_year) |> 
  mutate(junction_detail = ifelse(is.na(junction_detail),"unknown (self reported)",junction_detail)) |> 
  format_sf()

# import vehicles and use c index from crashes
vehicles_gb = get_stats19(yrs2get, type = "vehicle")|> 
  mutate(vehicle_type = ifelse(is.na(vehicle_type),"Other vehicle",vehicle_type)) |> 
  mutate(vehicle_type = if_else(escooter_flag == "Vehicle was an e-scooter", "e-scooter", vehicle_type)) # add in escooters

# e scooter collisions from vehicle description
e_scooter_collisions = filter(vehicles_gb, vehicle_type == "e-scooter")

# import casualties, add fatal column to match serious and slight and include e-scooters from vehicle data, add in dft age bands (different to those included in data)
casualties_gb = get_stats19(yrs2get, type = "casualty")|> 
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>  # there is a column for serious and slight, so add one for fatal to make analysis consistent
  mutate(
    casualty_type = ifelse(
      collision_index %in% e_scooter_collisions$collision_index & # add e_scooters
      !is.na(casualty_type),
      "E-scooter rider",
      casualty_type
    ))



cas_u16 = casualties_gb |> 
  filter(age_of_casualty < 17) |> 
  group_by(casualty_type) |> 
  summarise(fatal = round(sum(fatal_count, na.rm = TRUE)),
            serious = round(sum(as.numeric(casualty_adjusted_severity_serious), na.rm = TRUE)),
            ksi = fatal+serious) |> 
  select(-fatal,-serious)

# collisions with escooters involved
veh_e = vehicles_gb |> 
  filter(vehicle_type == "e-scooter")

#
veh_e_mv= vehicles_gb |> 
  filter(collision_index %in% veh_e$collision_index) |> 
  summarise_vehicle_types("short_name") |> 
  mutate(vehicle_cat = ifelse(short_name == "Other vehicle","Unknown",short_name)) |> 
  mutate(vehicle_cat = ifelse(vehicle_cat %in% c("Pedal cycle", "e-scooter","Mobility scooter"),"Bicycle/E-scooter/Mobility Scooter",vehicle_cat)) |>
  mutate(vehicle_cat = ifelse(!vehicle_cat == "Bicycle/E-scooter/Mobility Scooter" & !vehicle_cat == "Unknown","Motor vehicle",vehicle_cat)) |> 
  mutate(vehicle_cat = ifelse(short_name == "Tram","Tram",vehicle_cat)) |> 
  mutate(vehicle_cat = ifelse(is.na(short_name),"Unknown",vehicle_cat)) |> 
  dplyr::group_by(collision_index, vehicle_cat) %>%
  dplyr::mutate(number_vehicles = 1) %>%
  dplyr::summarise(number_vehicles = sum(number_vehicles), .groups = "drop") |> 
  tidyr::pivot_wider(names_from = "vehicle_cat", values_from = "number_vehicles")

veh_m = filter(veh_e_mv,!is.na(`Motor vehicle`))



cas_ped = casualties_gb |> 
  filter(casualty_type == "Pedestrian") |>
  summarise_casualties_per_collision() |> 
  group_by(collision_year) |> 
  summarise(fatal = sum(Fatal,na.rm = TRUE),
            serious = sum(Serious,na.rm = TRUE),
            ksi = sum(Fatal,Serious,na.rm = TRUE),
            slight = sum(Slight,na.rm = TRUE),
            all = sum(Fatal,Serious,Slight,na.rm = TRUE)) |> 
  mutate(fat_pc = round(fatal/sum(fatal)*100,1),
         ser_pc = round(serious/sum(serious)*100,1),
         ksi_pc = round(ksi/sum(ksi)*100,1),
         sli_pc = round(slight/sum(slight)*100,1),
         all_pc = round(all/sum(all)*100,1))

cas_ped$all[5]-cas_ped$all[4]

# casuatlies catagorised as on footway or verge https://assets.publishing.service.gov.uk/media/6925a35433d088f6d5da2cf0/STATS20_2024_specification.pdf
casualties_pv = casualties_gb |> 
  filter(pedestrian_location == "On footway or verge" & casualty_type == "Pedestrian") |> 
  summarise_casualties_per_collision() # summarise casualties to one line per collision

cas_pv = casualties_pv |> 
  group_by(collision_year) |> 
  summarise(all = sum(Fatal,Serious,Slight))

cas_pv_fatal = casualties_pv |> 
  group_by(collision_year) |> 
  summarise(Fatal = sum(Fatal))

plot_pavement_severity_by_year(casualties_pv = casualties_pv,base_year = base_year,upper_year = upper_year,
                                               theme = c("#484645","#D45C5D","#F0C9CA"),save_path = "outputs/stats19/sev_year.png"
)

# casualties since fatal crash in 2023
casualties_since_2023 = casualties_pv |> 
  inner_join(crashes_gb) |> 
  filter(datetime > "2023-07-06")

casualties_sev = casualties_pv |> 
  select(collision_year,Fatal,Serious,Slight) |> 
  reshape2::melt("collision_year", value.name = "number_of_casualties")

casualties_urb = casualties_pv |> 
  inner_join(crashes_gb) |> 
  ungroup() |> 
  group_by(urban_or_rural_area) |> 
  summarise(fatal = sum(Fatal,na.rm = TRUE),
            serious = sum(Serious,na.rm = TRUE),
            ksi = sum(Fatal,Serious,na.rm = TRUE),
            slight = sum(Slight,na.rm = TRUE),
            all = sum(Fatal,Serious,Slight,na.rm = TRUE)) |> 
  mutate(fat_pc = round(fatal/sum(fatal)*100,1),
         ser_pc = round(serious/sum(serious)*100,1),
         ksi_pc = round(ksi/sum(ksi)*100,1),
         sli_pc = round(slight/sum(slight)*100,1),
         all_pc = round(all/sum(all)*100,1))

# summarise vehicle classes using function and then summarise to a category, large,high powered vehicle (Car/Motorbike/Taxi/Bus/Goods vehicle) or 'micro mobility/bicycle'
vehicles_categorised = vehicles_gb |> 
  filter(collision_index %in% casualties_pv$collision_index) |> 
  summarise_vehicle_types("short_name") |> 
  mutate(vehicle_cat = ifelse(short_name == "Other vehicle","Unknown",short_name)) |> 
  mutate(vehicle_cat = ifelse(vehicle_cat %in% c("Pedal cycle", "e-scooter","Mobility scooter"),"Bicycle/E-scooter/Mobility Scooter",vehicle_cat)) |>
  mutate(vehicle_cat = ifelse(!vehicle_cat == "Bicycle/E-scooter/Mobility Scooter" & !vehicle_cat == "Unknown","Motor vehicle",vehicle_cat)) |> 
  mutate(vehicle_cat = ifelse(short_name == "Tram","Tram",vehicle_cat)) |> 
  mutate(vehicle_cat = ifelse(is.na(short_name),"Unknown",vehicle_cat)) |> 
  select(collision_index,collision_year,vehicle_type, short_name,vehicle_cat) |> 
  mutate(count = 1)

vehicles_cat_sum = vehicles_categorised |> 
  group_by(short_name,vehicle_cat) |> 
  summarise(count = sum(n())) |> 
  mutate(label = paste0(short_name, "\n", count))

veh_pv_fat_cat = vehicles_categorised |>  
  filter(collision_index %in% casualties_pv$collision_index[casualties_pv$Fatal > 0]) |> 
  group_by(vehicle_cat) |> 
  summarise(total = n()) |> 
  mutate(pc = round(total/sum(total)*100))

# colours based on ltc heatmap0 Deuter https://loukesio.github.io/ltc-color-palettes/palette-explorer.html
veh_colz = c("#ABA7C3","#070517", "#CFD52F", "#798316")

# treeplot
p0 = ggplot(vehicles_cat_sum, aes(area = count, fill = vehicle_cat, label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", min.size = 0, reflow = TRUE)+
  #geom_treemap_text(aes(label = count),    colour = "white", min.size = 16, place = "bottomleft")+
  scale_fill_manual(values = veh_colz)+
  labs(title="Vehicle types involved in pavement collisions",
       subtitle = paste0("Sized by value, coloured by category. Between ",base_year," and ", upper_year))+
  bbc_style()+
  theme(legend.position = "bottom")

# write out
finalise_plot(plot_name = p0, source_name = "Source: STATS19",save_filepath = "outputs/stats19/vehicles_pavements.png",width_pixels = 800, height_pixels = 500)

veh_summary =  vehicles_categorised %>%
  dplyr::group_by(collision_index, vehicle_cat) %>%
  dplyr::mutate(number_vehicles = 1) %>%
  dplyr::summarise(number_vehicles = sum(number_vehicles), .groups = "drop") |> 
  tidyr::pivot_wider(names_from = "vehicle_cat", values_from = "number_vehicles")

veh_summary_yr =  vehicles_categorised %>%
  dplyr::group_by(collision_index, vehicle_cat, collision_year) %>%
  dplyr::mutate(number_vehicles = 1) %>%
  dplyr::summarise(number_vehicles = sum(number_vehicles), .groups = "drop") |> 
  tidyr::pivot_wider(names_from = "vehicle_cat", values_from = "number_vehicles") |> 
  group_by(collision_year) |> 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |> 
  rowwise() |> 
  mutate(all = sum(`Motor vehicle`,`Bicycle/E-scooter/Mobility Scooter`,Unknown,Tram))

mv_pc = veh_summary_yr$`Motor vehicle`[veh_summary_yr$collision_year == 2025]

veh_mob = vehicles_gb |> 
  filter(collision_index == "2025401669531")

# summarise vehicle category assumptions
vehicle_cat_table = vehicles_categorised |>
  select(-collision_index,-count) |> 
  distinct(vehicle_type, .keep_all = TRUE)

t1 <- gt(vehicle_cat_table, auto_align = TRUE) |>
  cols_label(vehicle_type = md("**STATS19 name**"),
             short_name = md("**Shortened name**"),
             vehicle_cat = md("**Vehicle category name**")) |>
  tab_footnote(md("**Source: STATS19**")) |>
  opt_table_font(size = 11) |>
  opt_row_striping(row_striping = FALSE) |>
  tab_header(title = md("STATS19 vehicle categories and the summary categories")) |>
  tab_options(heading.align = "left",
              column_labels.border.top.style = "none",
              table.border.top.style = "none",
              column_labels.border.bottom.style = "none",
              column_labels.border.bottom.width = 1,
              column_labels.border.bottom.color = "black",
              table_body.border.top.style = "none",
              table_body.border.bottom.color = "white",
              heading.border.bottom.style = "none",
              table.border.bottom.style = "none")

#gtsave(speed_table, paste0(save_path, ".html"))
gtsave(t1, paste0("outputs/stats19/vehicle_cat_table.png"))


cas_sum = summarise_casualties_per_collision(casualties_gb)

# only single vehicle collisions can be confident of the vehicle that hit the pedestrian
all_vehicle_pavement_TY <- crashes_gb |> 
  filter(collision_year == upper_year) |> 
  inner_join(veh_summary) |> 
  inner_join(casualties_pv) |> 
  filter(Fatal > 0)

# only single vehicle collisions can be confident of the vehicle that hit the pedestrian
all_vehicle_pavement <- crashes_gb |> 
  filter(collision_year >= base_year & collision_year <= upper_year) |> 
  inner_join(veh_summary) |>  
  inner_join(casualties_pv) |> 
  filter(Fatal > 0) 

av_pav_fat_table = all_vehicle_pavement |> 
  st_set_geometry(NULL) |> 
  select(collision_year,`Motor vehicle`,Unknown,`Bicycle/E-scooter/Mobility Scooter`,Tram) |> 
  reshape2::melt("collision_year") |> 
  group_by(variable) |> 
  summarise(n_veh = sum(value,na.rm = TRUE)) |> 
  mutate(pc = n_veh/sum(n_veh)*100)

av_pav_plot = all_vehicle_pavement |> 
  st_set_geometry(NULL) |> 
  select(collision_year,`Motor vehicle`,Unknown,`Bicycle/E-scooter/Mobility Scooter`,Tram) |> 
  reshape2::melt("collision_year") |> 
  group_by(collision_year, variable) |> 
  summarise(n_veh = sum(value,na.rm = TRUE))

#Make plot
p1_5 = ggplot(av_pav_plot, aes(x = collision_year, y = n_veh, fill = variable)) +
  geom_bar(stat="identity", 
           position="stack") +
  scale_fill_manual(values = veh_colz[c(2,4,1,3)]) +
  labs(title="Vehicles involved in pavement fatalities",
       subtitle = paste0("Number of vehicles by category between ",base_year," and ", upper_year))+
  bbc_style()

# write out
finalise_plot(plot_name = p1_5, source_name = "Source: STATS19",save_filepath = "outputs/stats19/av_pavement_fatalities.png")

# only single vehicle collisions can be confident of the vehicle that hit the pedestrian
all_vehicle_pavement_ksi <- crashes_gb |> 
  filter(collision_year >= base_year & collision_year <= upper_year) |> 
  inner_join(veh_summary) |>  
  inner_join(casualties_pv) |> 
  filter(Fatal > 0 | Serious > 0) 

av_pav_plot_ksi = all_vehicle_pavement_ksi |> 
  st_set_geometry(NULL) |> 
  select(collision_year,`Motor vehicle`,Unknown,`Bicycle/E-scooter/Mobility Scooter`,Tram) |> 
  reshape2::melt("collision_year") |> 
  group_by(collision_year, variable) |> 
  summarise(n_veh = sum(value,na.rm = TRUE))

#Make plot
p1_4 = ggplot(av_pav_plot_ksi, aes(x = collision_year, y = n_veh, fill = variable)) +
  geom_bar(stat="identity", 
           position="stack") +
  scale_fill_manual(values = veh_colz[c(2,4,1,3)]) +
  labs(title="Vehicles involved in pavement KSI",
       subtitle = paste0("Number of vehicles by category between ",base_year," and ", upper_year))+
  bbc_style()

# write out
finalise_plot(plot_name = p1_4, source_name = "Source: STATS19", save_filepath = "outputs/stats19/av_pavement_ksi.png")

# only single vehicle collisions can be confident of the vehicle that hit the pedestrian
all_vehicle_pavement_all <- crashes_gb |> 
  filter(collision_year >= base_year & collision_year <= upper_year) |> 
  inner_join(veh_summary) |>  
  inner_join(casualties_pv) 

# av_pav_table_all = all_vehicle_pavement_all |> 
#   st_set_geometry(NULL) |> 
#   select(collision_year,`Motor vehicle`,Unknown,`Bicycle/E-scooter/Mobility Scooter`,Tram) |> 
#   reshape2::melt("collision_year") |> 
#   group_by(collision_year,variable) |> 
#   summarise(n_veh = sum(value,na.rm = TRUE)) |> 
#   group_by(collision_year) |> 
#   mutate(pc = n_veh/sum(n_veh)*100)

av_pav_plot_all = all_vehicle_pavement_all |> 
  st_set_geometry(NULL) |> 
  select(collision_year,`Motor vehicle`,Unknown,`Bicycle/E-scooter/Mobility Scooter`,Tram) |> 
  reshape2::melt("collision_year") |> 
  group_by(collision_year, variable) |> 
  summarise(n_veh = sum(value,na.rm = TRUE))

#Make plot
p1_3 = ggplot(av_pav_plot_all, aes(x = collision_year, y = n_veh, fill = variable)) +
  geom_bar(stat="identity", 
           position="stack") +
  scale_fill_manual(values = veh_colz[c(2,4,1,3)]) +
  labs(title="Vehicles involved in all pavement collisions",
       subtitle = paste0("Number of vehicles by category between ",base_year," and ", upper_year))+
  bbc_style()

# write out
finalise_plot(plot_name = p1_3, source_name = "Source: STATS19",save_filepath = "outputs/stats19/av_pavement_all.png")

summarise_casualty_types <- function(casualties,
                                     summary_type = c("short_name", "in_or_on")) {
  summary_type <- match.arg(summary_type)
  
  cas_type <- casualty_type_lookup %>%
    dplyr::select(casualty_type, !!rlang::sym(summary_type))
  
  cas_out <- dplyr::left_join(casualties, cas_type, by = "casualty_type")
  return(cas_out)
}

cas_type = casualties_gb |> 
  #group_by(casualty_type) |> 
  summarise_casualty_types(summary_type = "short_name") |> 
  group_by(short_name) |> 
  summarise(fatal = sum(fatal_count,na.rm = TRUE),
            serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            ksi = sum(fatal_count,casualty_adjusted_severity_serious,na.rm = TRUE),
            slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE),
            all = sum(fatal,serious,slight,na.rm = TRUE)) |> 
  mutate(fat_pc = round(fatal/sum(fatal)*100,1),
         ser_pc = round(serious/sum(serious)*100,1),
         ksi_pc = round(ksi/sum(ksi)*100,1),
         sli_pc = round(slight/sum(slight)*100,1),
         all_pc = round(all/sum(all)*100,1))



# --- numbers for the report text ---

report_text <- glue("
pedestrian casualties recorded in {upper_year}: {cas_ped$all[cas_ped$collision_year == 2025]}<br>
pedestrian casualties recorded in {upper_year-1}: {cas_ped$all[cas_ped$collision_year == 2024]}<br>
difference = {cas_ped$all[cas_ped$collision_year == 2025]-cas_ped$all[cas_ped$collision_year == 2024]}<br>
pedestrian casualties on pavement in 2025: {cas_pv$all[cas_pv$collision_year == 2025]}<br>
pedestrian casualties on pavement in 2024: {cas_pv$all[cas_pv$collision_year == 2024]}<br>
number of bicycle, e-scooter, and mobility scooter collisions on pavements 2025: {veh_summary_yr$`Bicycle/E-scooter/Mobility Scooter`[veh_summary_yr$collision_year == 2025]}<br>
number of bicycle, e-scooter, and mobility scooter collisions on pavements 2021: {veh_summary_yr$`Bicycle/E-scooter/Mobility Scooter`[veh_summary_yr$collision_year == 2021]}<br>
number of motor vehicle collisions on pavements 2025: {veh_summary_yr$`Motor vehicle`[veh_summary_yr$collision_year == 2025]}<br>
number of motor vehicle collisions on pavements 2021: {veh_summary_yr$`Motor vehicle`[veh_summary_yr$collision_year == 2021]}<br>
% of vehicles involved in all pavement collisions that were motor vehicles in 2025: {round(veh_summary_yr$`Motor vehicle`[veh_summary_yr$collision_year == 2025]/veh_summary_yr$all[veh_summary_yr$collision_year == 2025])}<br>
fatal pedestrian casualties on pavement in 2025: {cas_pv_fatal$Fatal[cas_pv_fatal$collision_year == 2025]}<br>
fatal pedestrian casualties on pavement in 2024: {cas_pv_fatal$Fatal[cas_pv_fatal$collision_year == 2024]}<br>
% of all fatal pavement collisions that involved a motor car: {veh_pv_fat_cat$pc[veh_pv_fat_cat$vehicle_cat == 'Motor vehicle']}<br>
% of all fatal pavement collisions that involved an unknown: {veh_pv_fat_cat$pc[veh_pv_fat_cat$vehicle_cat == 'Unknown']}<br>
% of all fatal pavement collisions that involved a Bicycle, e-scooter or mobility scooter: {veh_pv_fat_cat$pc[veh_pv_fat_cat$vehicle_cat == 'Bicycle/E-scooter/Mobility Scooter']}<br>
% of people KSI who were in motor vehicles between 2021 and 2025: {cas_type$ksi_pc[cas_type$short_name == 'Motor vehicle'][1]}<br>
% of people KSI were were on an escooter between 2021 and 2025: {cas_type$ksi_pc[cas_type$short_name == 'E-scooter rider'][1]}<br>
% of escooter collisions also involving a motor vehicle: {round(NROW(veh_m)/NROW(veh_e_mv)*100)}<br>
Under 16s KSI on escooters between 2021 and 2025: {cas_u16$ksi[cas_u16$casualty_type == 'E-scooter rider'][1]}<br>
Under 16s KSI inside cars between 2021 and 2025: {cas_u16$ksi[cas_u16$casualty_type == 'Car occupant'][1]}<br>
Under 16s KSI as pedestrians between 2021 and 2025: {cas_u16$ksi[cas_u16$casualty_type == 'Pedestrian'][1]}<br>
Pavement fatalities since 09-07-2023: {sum(casualties_since_2023$Fatal)}
")

# --- where to send it ---
cat(report_text)                                  # console
writeLines(report_text, "stats_text.md")     # to a file

#write.csv(all_vehicle_pavement, "pavement_crashes.csv")



# #Make plot
# p1_6 = ggplot(casualties_sev, aes(x = collision_year, y = number_of_casualties, fill = variable)) +
#   geom_bar(stat="identity", 
#            position="stack") +
#   scale_fill_manual(values = c("#484645","#D45C5D","#F0C9CA")) +
#   labs(title="Pedestrian pavement casualties",
#        subtitle = paste0("Between ",base_year," and ", upper_year," by vehicle category"))+
#   bbc_style()
# 
# # write out
# finalise_plot(plot_name = p1_6, source_name = "Source: STATS19",save_filepath = "outputs/stats19/sev_pavement_casualties.png")

