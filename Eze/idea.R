library(baseballr) #install the package beforehand
library(dplyr)
theme_set(theme_minimal())
#scraping pitching data from 2021
data_2021 = baseballr::fg_pitcher_leaders(startseason = 2021, endseason = 2021) |> 
  filter(Pitches >= 250) #only including pitchers who threw more than 250 pitches
#adjusting the position column to classify into starters (SP) and relievers (RP)
for (player in data_2021){
  data_2021$position <- ifelse(data_2021$GS >= (data_2021$G - data_2021$GS), 
                               "SP", "RP")
}
data_2021 |> 
  ggplot(aes(x= sp_s_FF, y = FBv))+
  geom_point()




data_2022 = baseballr::fg_pitcher_leaders(startseason = 2022, endseason = 2022) |> 
  filter(Pitches >= 250)
for (player in data_2022){
  data_2022$position <- ifelse(data_2022$GS >= (data_2022$G - data_2022$GS), 
                               "SP", "RP")
}



data_2023 = baseballr::fg_pitcher_leaders(startseason = 2023, endseason = 2023) |> 
  filter(Pitches >= 250)
for (player in data_2023){
  data_2023$position <- ifelse(data_2023$GS >= (data_2023$G - data_2023$GS), 
                               "SP", "RP")
}


key_vars <- c("team_name", "Throws", "PlayerName", "Age", "W", "L", "ERA", "G", "GS", "SO", "IP", "Pitches", 
              "Balls", "Strikes", "FB_pct", 'FB_pct1', 'SL_pct', "FBv", "SL_pct", "SLv", "CT_pct", "CTv", "CB_pct", "CBv", "CH_pct", "CHv",
              "sp_stuff", "sp_location", 'K_9', "sp_pitching", 'WAR', 'wFB')
cond_data_2021 <- data_2021 |> 
  dplyr::select(all_of(key_vars))
cond_data_2022 <- data_2022 |> 
  dplyr::select(all_of(key_vars))
# summary(cond_data_2021)



# Earned Runs Average
cond_data_2021 |> 
  ggplot(aes(ERA))+
  geom_histogram(binwidth = 0.5, fill = 'blue', color = 'black')+
  labs(title = "Distribution of ERA", x = "ERA", y = "Frequency")

cond_data_2021 |> 
  ggplot(aes(x = FBv, y = K_9))+
  geom_point(alpha = .45, color = 'red', size = .9)+
  geom_smooth(method = 'lm', color = 'blue', se = F)+
  labs(title = "Fastball Velocity vs Strikeouts per 9 innings", x = "Fastball Velocity (mph)", y = "Strikeouts / 9 innings")

# Boxplot of ERA by Pitching Hand (Throws)
cond_data_2021 |> 
  ggplot(aes(Throws, ERA))+
  geom_boxplot(fill = c('lightblue', 'lightgreen'))+
  labs(title = "ERA by Pitching Hand", x = "Pitching Hand", y = "ERA")

cond_data_2021 |> 
  ggplot(aes(x = sp_stuff, y = ERA, color = Throws))+
  geom_smooth(method = 'lm')+
  labs(title = "Stuff+ vs ERA by Throwing Hand", x = "Stuff+", y = "ERA")


# Wins Above Replace
# The formula for WAR can be complex and varies by source, 
# but the aim is to combine these elements to reflect a player's overall value 
# in terms of additional wins contributed to their team.

# Stuff+ vs Performance
cond_data_2021 |> 
  ggplot(aes(sp_stuff, WAR))+
  geom_point(alpha = .45, size = .9)+
  geom_smooth(method = 'lm')+
  labs(title = "WAR vs Stuff+", x = "Stuff+", y = "WAR")

# Pitch Type vs Performance:
cond_data_2021 |> 
  ggplot(aes(wFB, ERA))+
  geom_point(alpha = .45, size = .9)+
  geom_smooth(method = 'lm')+
  labs(title = "ERA vs Fastball Value (wFB)", x = "wFB", y = "ERA")


library(ggcorrplot)
cor_matrix <- cor(cond_data_2021 %>% select_if(is.numeric), use = "complete.obs")
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)



# Plot pitch usage patterns
pitch_type_names <- c(
  "FB_pct1" = "Fastball %", 
  "SL_pct" = "Slider %", 
  "CT_pct" = "Cutter %", 
  "CB_pct" = "Curveball %", 
  "CH_pct" = "Changeup %"
)

cond_data_2021 |> 
  pivot_longer(cols = c(FB_pct1, SL_pct, CT_pct, CB_pct, CH_pct), 
               names_to = 'pitch_type', values_to = 'percentage') |> 
  ggplot(aes(x = pitch_type, y = percentage, fill = pitch_type))+
  geom_boxplot()+
  scale_x_discrete(labels = pitch_type_names)+
  scale_fill_manual(values = c(
    'FB_pct1' = 'blue',
    'SL_pct' = 'green',
    'CT_pct' = 'red',
    'CB_pct' = 'purple',
    'CH_pct' = 'orange'
  ), labels = pitch_type_names)+
  labs(title = "Pitch Usage Patterns", x = "Pitch Type", y = "Percentage", fill = "Pitch Type")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))




library(ggridges)
library(patchwork)


# Combine datasets for seasonal info --------------------------------------


combined_data <- bind_rows(data_2021, data_2022, data_2023)



# Filter relevant columns -------------------------------------------------



# This approach ensures we retain rows that have data for any of the pitch types. 
# If a row has data for a fastball (FB_pct1), but not for other pitches, it will still be included.

filtered_data <- combined_data |> 
  dplyr::select(Season, PlayerName, pfx_FA_pct, pfx_SL_pct, pfx_FC_pct, pfx_CU_pct, pfx_CH_pct, pfx_SI_pct) |> 
  filter(!is.na(pfx_FA_pct) | !is.na(pfx_SL_pct) | !is.na(pfx_FC_pct) | !is.na(pfx_CU_pct) | !is.na(pfx_CH_pct) | !is.na(pfx_SI_pct))



# Creating ridge plots ----------------------------------------------------


create_ridge_plot <- function(data_set, pitch_col, pitch_name) {
  ggplot(data_set, aes_string(x = pitch_col, y = "as.factor(Season)", fill = "as.factor(Season)")) +
    geom_density_ridges(scale = 0.9, rel_min_height = 0.01) +
    labs(
      title = paste("Distribution of", pitch_name, "Across Seasons"),
      x = paste(pitch_name, "Percentage"),
      y = "Season",
      fill = "Season"
    ) +
    theme_ridges() +
    theme(legend.position = "none")
    
}





# Use function to create plots for each pitch -----------------------------


plot_fb <- create_ridge_plot(filtered_data, 'pfx_FA_pct', "Fastball")
plot_sl <- create_ridge_plot(filtered_data, 'pfx_SL_pct', 'Slider')
plot_ct <- create_ridge_plot(filtered_data, 'pfx_FC_pct', 'Cutter')
plot_cb <- create_ridge_plot(filtered_data, 'pfx_CU_pct', 'Curveball')
plot_ch <- create_ridge_plot(filtered_data, 'pfx_CH_pct', 'Changeup')
plot_si <- create_ridge_plot(filtered_data, 'pfx_SI_pct', 'Sinker')



# Print relevant plot onto plots plane  -----------------------------------


print(plot_fb)
print(plot_sl)
print(plot_ct)
print(plot_cb)
print(plot_ch)
print(plot_si)



# pitch_data <- combined_data |> 
#   dplyr::select(Season, FB_pct1, SL_pct, CT_pct, CB_pct, CH_pct) |> 
#   pivot_longer(starts_with('FB_pct1') : starts_with('CH_pct'),
#                names_to = 'Pitch_Type',
#                values_to = 'Usage_percentage')
# 
# pitch_data |> 
#   ggplot(aes(x = Usage_percentage, y = Pitch_Type, fill = factor(Season)))+
#   geom_density_ridges(scale = 3, rel_min_height = .01)+
#   labs(title = "Pitch Usage Distribution Over Seasons",
#        x = "Usage Percentage",
#        y = "Pitch Type",
#        fill = "Season") +
#   theme_ridges() +
#   theme(legend.position = "top")
# 
# 
# # Ridge plot for Fastball percentage over seasons
# filtered_data |> 
#   ggplot(aes(x = FB_pct, y = as.factor(Season), fill = as.factor(Season)))+
#   geom_density_ridges(scale = .9, rel_min_height =  .01)+
#   labs(
#     title = 'Distribution of Fastball Percentage Across Seasons',
#     x = "Fastball Percentage",
#     y = "Season",
#     fill = "Season"
#   ) +
#   theme_ridges() +
#   theme(legend.position = "none")
# 




# Out of the myriads of variables which one is important?
# EDA
# runs above average to stuff+


# ridge plots over time !boxplot
  # a way to tally changes in arsenal

# tree base approach to output potential run value
# plots movements vs avg on each axis(pfx with dash)
  # color by maybe slider for now
# Limitation
  # In creating these metrics for pitch arsenal, stuff plus (sp) doesn't take into account the batter's action