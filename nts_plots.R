library(dplyr)
library(tidyr)
library(ltc)
library(ggplot2)
library(gghighlight)

dir.create("outputs/nts/grouped/", recursive = TRUE)
dir.create("data")

latest_yr = "2025"
base_yr = "2004"

# ownloaded from https://assets.publishing.service.gov.uk/media/6a9eca60f222242033e4642b/nts0601.ods
download.file("https://assets.publishing.service.gov.uk/media/6a9eca60f222242033e4642b/nts0601.ods",destfile = "data/nts0601.ods", mode = "wb")

nts0601_raw = readODS::read_ods("data/nts0601.ods",sheet = "NTS0601c_distance",skip = 5) |> 
  filter(Year >= base_yr)

nts_table = nts0601_raw |> 
  filter(Year == base_yr | Year == latest_yr) |> 
  reshape2::melt(c("Year", "Sex [note 7]", "Age")) |> 
  reshape2::dcast(Year~Age,value.var = "value")

nts_table <- nts0601_raw |>
  filter(Year %in% c(base_yr, latest_yr)) |>
  pivot_longer(
    cols = -c(Year, `Sex [note 7]`, Age),
    names_to = "variable",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from = Year,
    values_from = value
  ) |> 
  mutate(diff = round(.data[[latest_yr]]-.data[[base_yr]]))

walking = filter(nts_table, variable == names(nts0601_raw)[4])
cycling = filter(nts_table, variable == names(nts0601_raw)[6])

sexes = unique(nts0601_raw$`Sex [note 7]`)

modes = data.frame(survey_nam = names(nts0601_raw)[4:17],
                   activity = c("Walking", "Walks of a mile or more", "Cycling", "Driving car or van", "Passenger car or van",
                                "Motorcycle", "Other private transport", "Bus in London", "Other local bus", "Non-local bus", "London underground",
                                "Surface Rail", "Taxi or minicab", "Other public transport"),
                   groups = c("Active travel","Walks","Active travel", "Private transport", "Private transport", "Motorcycling", "Private transport", "Public transport",
                              "Public transport","Public transport","Public transport","Public transport","Taxi","Public transport"))

at = filter(modes,groups == "Active travel")

active_travel = nts_table |> 
  filter(variable %in% names(nts0601_raw)[c(4,6)]) |> 
  filter(!Age == "All ages") |> 
  group_by(`Sex [note 7]`,Age) |> 
  summarise(ybase = sum(.data[[base_yr]]),
            ylast = sum(.data[[latest_yr]])) |> 
  mutate(diff = ylast-ybase)

active_travel_sex = active_travel |> 
  group_by(`Sex [note 7]`) |> 
  summarise(ybase = mean(ybase),
            ylast = mean(ylast)) |> 
  mutate(diff = ylast-ybase)

sexes = sexes[2]
u_modes = unique(modes$groups)[2]

for (s in sexes){

for (m in u_modes){

  mode_df = filter(modes, groups == m)
  
nts0601 = nts0601_raw |> 
  filter(`Sex [note 7]` == s) |> 
  select(Year,mode_df$survey_nam,Age) |> 
  reshape2::melt(c("Year","Age")) |> 
  group_by(Year, Age) |> 
  summarise(miles = sum(value,na.rm = TRUE))

min_yr = min(nts0601$Year)
max_yr = max(nts0601$Year)

  
cols = ltc::ltc("casa_natal",NROW(unique(nts0601$Age)))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, scale_color_manual(values = cols))

mean_yrz = nts0601 |> 
  group_by(Year) |> 
  summarise(mean_dist = mean(miles))

nts0601 = left_join(nts0601, mean_yrz, by = "Year")

mean_distance = mean(nts0601$miles)

if(m == "Walks"){
  gg_title = paste0("Average number of walks over 1 mile for ", s," between ",min_yr," and ", max_yr)
  y_label = "Number of walks"
} else {
  gg_title = paste0("Average distance by ",m," for ", s," between ",min_yr," and ", max_yr)
  y_label = "Distance (miles)"
}

nts0601 %>%
  ggplot(aes(Year, miles,color = Age)) +
  geom_line(size = 1.4, alpha = .8) +
  dft_theme+
  theme(panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  #geom_hline(yintercept=100, linetype='dotted', col = 'black')+
  ggtitle(gg_title) +
  scale_x_continuous(name = NULL,
                     breaks = seq(min_yr, max_yr, by = 1)  # Add more tick marks
  ) +
  ylab(y_label)+
  labs(caption = "Source: Nation Travel Survey England table NTS0601c")+
  theme(panel.border = element_blank())

ggsave(paste0("outputs/nts/grouped/total_",m,"_",s, ".png"))

nts_base = nts0601 |> 
  filter(Year == min_yr) |> 
  ungroup() |> 
  select(base = miles,Age)

nts_index = nts0601 |> 
  left_join(nts_base, by = "Age") |> 
  mutate(
    activity_index = miles/base * 100
  ) |> 
  mutate(activity_index = ifelse(is.nan(activity_index)|is.infinite(activity_index),100,activity_index))

min_ind = round(min(nts_index$activity_index),-1)
max_ind = round(max(nts_index$activity_index)-1)

if(m == "Walks"){
  gg_tit = ggtitle(paste0("Index of number of walks over 1 mile by ",s, " by age group, ", min_yr, " - ", max_yr, " (Index ",min_yr,"=100)"))
} else {
  gg_tit = ggtitle(paste0("Index of distance by ", m, " by ",s, " by age group, ", min_yr, " - ", max_yr, " (Index ",min_yr,"=100)"))
}

p1 = nts_index %>%
  ggplot(aes(Year, activity_index, color = Age)) +
  geom_line(alpha = .8, size = 1.4) +
  dft_theme +
  #scale_linetype_manual(values = c(ksi_mile_index = "dotted", index = "solid")) +
  #scale_linewidth_manual(values = c(ksi_mile_index = 1.2, index = 2)) +
  theme(panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  geom_hline(yintercept = 100, linetype = 'dotted', col = 'black') +
  scale_x_continuous(name = NULL,
                     breaks = seq(min_yr, max_yr, by = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(name = "index relative to 2004",
                     breaks = seq(min_ind, max_ind, by = 10),
                     expand = c(0, 0)) +
  ylab("index") +
  labs(caption = "Source: Nation Travel Survey England table NTS0601c") +
  ggtitle(gg_tit) +
  theme(panel.border = element_blank())

ggsave(paste0("outputs/nts/grouped/index_",m,"_",s, ".png"))

}
  
}

dir.create("outputs/nts/activity/")

u_act = unique(modes$activity)[1]

for (s in sexes){
  
  for (m in u_act){
    
    mode_df = filter(modes, activity == m)
    
    nts0601 = nts0601_raw |> 
      filter(`Sex [note 7]` == s) |> 
      select(Year,mode_df$survey_nam,Age) |> 
      reshape2::melt(c("Year","Age")) |> 
      group_by(Year, Age) |> 
      summarise(miles = sum(value,na.rm = TRUE))
    
    min_yr = min(nts0601$Year)
    max_yr = max(nts0601$Year)
    
    
    cols = ltc::ltc("casa_natal",NROW(unique(nts0601$Age)))
    cust_theme <- theme(panel.grid.major = element_line(size = 2))
    # put the elements in a list
    dft_theme <- list(cust_theme, scale_color_manual(values = cols))
    
    mean_yrz = nts0601 |> 
      group_by(Year) |> 
      summarise(mean_dist = mean(miles))
    
    nts0601 = left_join(nts0601, mean_yrz, by = "Year")
    
    mean_distance = mean(nts0601$miles)
    
    if(m == "Walks"){
      gg_title = paste0("Average number of walks over 1 mile for ", s," between ",min_yr," and ", max_yr)
      y_label = "Number of walks"
    } else {
      gg_title = paste0("Average distance by ",m," for ", s," between ",min_yr," and ", max_yr)
      y_label = "Distance (miles)"
    }
    
    nts0601 %>%
      ggplot(aes(Year, miles,color = Age)) +
      geom_line(size = 1.4, alpha = .8) +
      dft_theme+
      theme(panel.background = element_blank(),
            legend.position = "top",
            legend.title = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
      #geom_hline(yintercept=100, linetype='dotted', col = 'black')+
      ggtitle(gg_title) +
      scale_x_continuous(name = NULL,
                         breaks = seq(min_yr, max_yr, by = 1)  # Add more tick marks
      ) +
      ylab(y_label)+
      labs(caption = "Source: Nation Travel Survey England table NTS0601c")+
      theme(panel.border = element_blank())
    
    ggsave(paste0("outputs/nts/activity/total_",m,"_",s, ".png"), dpi = 500, width = 8)
    
    nts_base = nts0601 |> 
      filter(Year == min_yr) |> 
      ungroup() |> 
      select(base = miles,Age)
    
    nts_index = nts0601 |> 
      left_join(nts_base, by = "Age") |> 
      mutate(
        activity_index = miles/base * 100
      ) |> 
      mutate(activity_index = ifelse(is.nan(activity_index)|is.infinite(activity_index),100,activity_index))
    
    min_ind = round(min(nts_index$activity_index),-1)
    max_ind = round(max(nts_index$activity_index)-1)
    
    if(m == "Walks"){
      gg_tit = ggtitle(paste0("Index of number of walks over 1 mile by ",s, " by age group, ", min_yr, " - ", max_yr, " (Index ",min_yr,"=100)"))
    } else {
      gg_tit = ggtitle(paste0("Index of distance by ", m, " by ",s, " by age group, ", min_yr, " - ", max_yr, " (Index ",min_yr,"=100)"))
    }
    
    nts_index %>%
      ggplot(aes(Year, activity_index, color = Age)) +
      geom_line(alpha = .8, size = 1.4) +
      dft_theme +
      #scale_linetype_manual(values = c(ksi_mile_index = "dotted", index = "solid")) +
      #scale_linewidth_manual(values = c(ksi_mile_index = 1.2, index = 2)) +
      theme(panel.background = element_blank(),
            legend.position = "top",
            legend.title = element_blank()) +
      geom_hline(yintercept = 100, linetype = 'dotted', col = 'black') +
      scale_x_continuous(name = NULL,
                         breaks = seq(min_yr, max_yr+1, by = 1),
                         expand = expansion(mult = c(0, 0.02))) +
      scale_y_continuous(name = "index relative to 2004",
                         breaks = seq(min_ind, max_ind, by = 10),
                         expand = c(0, 0)) +
      ylab("index") +
      labs(caption = "Source: Nation Travel Survey England table NTS0601c") +
      ggtitle(gg_tit) +
      theme(panel.border = element_blank())
    
    ggsave(paste0("outputs/nts/activity/index_",m,"_",s, ".png"), dpi = 500, width = 10)
    
  }
  
}



img1 = magick::image_read("outputs/nts/activity/index_Walking_Female.png")
img2 = magick::image_read("outputs/nts/activity/total_Walking_Female.png")

imgs = magick::image_append(image = c(img1,img2), stack = FALSE)

magick::image_write(imgs,"outputs/nts/activity/female_walking_plots.png")

dir.create("outputs/nts/highlight/")

for (s in sexes){
  
  for (m in u_act){
    
    mode_df = filter(modes, activity == m)
    
    nts0601 = nts0601_raw |> 
      filter(`Sex [note 7]` == s) |> 
      select(Year,mode_df$survey_nam,Age) |> 
      reshape2::melt(c("Year","Age")) |> 
      group_by(Year, Age) |> 
      summarise(miles = sum(value,na.rm = TRUE))
    
    min_yr = min(nts0601$Year)
    max_yr = max(nts0601$Year)
    
    
    cols = ltc::ltc("casa_natal",NROW(unique(nts0601$Age)))[c(1,4)]
    cust_theme <- theme(panel.grid.major = element_line(size = 2))
    # put the elements in a list
    dft_theme <- list(cust_theme, scale_color_manual(values = cols))
    
    mean_yrz = nts0601 |> 
      group_by(Year) |> 
      summarise(mean_dist = mean(miles))
    
    nts0601 = left_join(nts0601, mean_yrz, by = "Year")
    
    mean_distance = mean(nts0601$miles)
    
    if(m == "Walks"){
      gg_title = paste0("Average number of walks over 1 mile for ", s," between ",min_yr," and ", max_yr)
      y_label = "Number of walks"
    } else {
      gg_title = paste0("Average distance walked by age group for ", s," between ",min_yr," and ", max_yr)
      y_label = "Distance (miles)"
    }
    
    nts0601 %>%
      ggplot(aes(Year, miles,color = Age)) +
      geom_line(size = 1.4, alpha = .8) +
      gghighlight(Age %in% c("0 to 16", "70 and over")) +
      dft_theme+
      theme(panel.background = element_blank(),
            legend.position = "top",
            legend.title = element_blank()) +
      scale_x_continuous(expand = c(0, 0)) +
      ggtitle(gg_title) +
      scale_x_continuous(name = NULL,
                         breaks = seq(min_yr, max_yr, by = 1)  # Add more tick marks
      ) +
      ylab(y_label)+
      labs(caption = "Source: Nation Travel Survey England table NTS0601c")+
      theme(panel.border = element_blank())
    
    ggsave(paste0("outputs/nts/highlight/total_",m,"_",s, ".png"), dpi = 500, width = 8)
    
    nts_base = nts0601 |> 
      filter(Year == min_yr) |> 
      ungroup() |> 
      select(base = miles,Age)
    
    nts_index = nts0601 |> 
      left_join(nts_base, by = "Age") |> 
      mutate(
        activity_index = miles/base * 100
      ) |> 
      mutate(activity_index = ifelse(is.nan(activity_index)|is.infinite(activity_index),100,activity_index))
    
    min_ind = round(min(nts_index$activity_index),-1)
    max_ind = round(max(nts_index$activity_index)-1)
    
    if(m == "Walks"){
      gg_tit = ggtitle(paste0("Index of number of walks over 1 mile by ",s, " by age group, ", min_yr, " - ", max_yr, " (Index ",min_yr,"=100)"))
    } else {
      gg_tit = ggtitle(paste0("Index of distance walked by ",s, " by age group, ", min_yr, " - ", max_yr, "\n(Index ",min_yr,"=100)"))
    }
    
    cols = ltc::ltc("casa_natal",NROW(unique(nts0601$Age)))
    cust_theme <- theme(panel.grid.major = element_line(size = 2))
    # put the elements in a list
    dft_theme <- list(cust_theme, scale_color_manual(values = cols))
    
    nts_index %>%
      ggplot(aes(Year, activity_index, color = Age)) +
      geom_line(alpha = .8, size = 1.4) +
      gghighlight(Age %in% c("0 to 16", "17 to 20")) +
      dft_theme +
      theme(panel.background = element_blank(),
            legend.position = "top",
            legend.title = element_blank()) +
      geom_hline(yintercept = 100, linetype = 'dotted', col = 'black') +
      scale_x_continuous(name = NULL,
                         breaks = seq(min_yr, max_yr, by = 1),
                         expand = expansion(mult = c(0, 0.02))) +
      scale_y_continuous(breaks = seq(min_ind, max_ind, by = 10),
                         expand = c(0, 0)) +
      ylab("index") +
      labs(caption = "Source: National Travel Survey England table NTS0601c") +
      ggtitle(gg_tit) +
      theme(panel.border = element_blank())
    
    ggsave(paste0("outputs/nts/highlight/index_",m,"_",s, ".png"),dpi = 500, width = 8)
    
  }
  
}

img1 = magick::image_read("outputs/nts/highlight/index_Walking_Female.png")
img2 = magick::image_read("outputs/nts/highlight/total_Walking_Female.png")

imgs = magick::image_append(image = c(img1,img2), stack = FALSE)

magick::image_write(imgs,"outputs/nts/highlight/female_plots.png")


Since 2004, men walk 40 miles more on average, but the figure is only 20 miles more for women.That is except for the under 20s, notably girls, who in the 0-16 age bracket have gone from being one of the age groups that walked the most in 2004 to the age bracket walking the least, now walking on average 70 miles less, the least of any age group except for the over 70s (chart 5).
