library(baseballr) #install the package beforehand
library(dplyr)
theme_set(theme_light())
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




# identify pitcher who changed pitch usage --------------------------------

library(tidyr)
library(tidyverse)

relevant_cols <- c('PlayerName', 'Season', 'pfx_FA_pct', 'pfx_FC_pct', 'pfx_SL_pct', 'pfx_CH_pct', 'pfx_CU_pct', 'pfx_SI_pct')


# Define custom labels for each pitch type
custom_labels <- c(
  pfx_CH_pct = "Changeup (CH)",
  pfx_CU_pct = "Curveball (CU)",
  pfx_FA_pct = "Fastball (FA)",
  pfx_SI_pct = "Sinker (SI)",
  pfx_SL_pct = "Slider (SL)",
  pfx_FC_pct = 'Cutter (FC)'
)


# Define the custom color palette
custom_colors <- c("#E41A1C", 
                   "#377EB8", 
                   "#4DAF4A", 
                   "#984EA3", 
                   "#FF7F00", 
                   '#FFFF33')



# Ridge plots but faceted -------------------------------------------------



# Filter and gather the data to long format
data_long <- combined_data |> 
  dplyr::select(PlayerName, Season, pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, pfx_FA_pct, pfx_SI_pct, pfx_SL_pct) |> 
  gather(key = 'PitchType', value = 'UsagePct', pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, pfx_FA_pct, pfx_SI_pct, pfx_SL_pct) |> 
  filter(is.finite(UsagePct)) # Remove non-finite values


# Convert 'Season' to a factor for proper grouping
data_long$Season <- as.factor(data_long$Season)


# Create ridge plots
data_long |> 
  ggplot(aes(x = UsagePct, y = Season, fill = Season))+
  geom_density_ridges(alpha = .8, scale = 1.5)+
  facet_wrap(~ PitchType, scales = 'free_y', labeller = labeller(PitchType = custom_labels))+
  labs(title = "Distribution of Pitch Usage Changes by Pitch Type (2021-2023)",
       x = "Usage Percentage",
       y = "Season",
       fill = "Season")+
  theme(legend.position = 'bottom')



# Fluctuation of pitch types for top pitchers -----------------------------


# Calculate the change in pitch usage for each pitcher and each pitch type
usage_change <- combined_data |> 
  dplyr::select(PlayerName, Season, pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, pfx_FA_pct, pfx_SI_pct, pfx_SL_pct) |> 
  gather(key = 'PitchType', value = 'UsagePct', pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, pfx_FA_pct, pfx_SI_pct, pfx_SL_pct) |> 
  spread(key = Season, value = UsagePct) |> 
  mutate(Change = `2023` - `2021`) |> 
  filter(!is.na(Change))


# Get the top 5 pitchers with the most significant changes yearly
top_pitchers <- usage_change |> 
  group_by(PlayerName) |> 
  summarize(TotalChange = sum(abs(Change), na.rm = T)) |> 
  arrange(desc(TotalChange)) |> 
  top_n(5, TotalChange)


# Filter the combined data for the top pitchers
data_top_pitchers <- combined_data |> 
  filter(PlayerName %in% top_pitchers$PlayerName)


# Convert 'Season' to a factor for proper grouping
data_top_pitchers$Season <- as.factor(data_top_pitchers$Season)



# Gather the data to long format for plotting
data_long_top <- data_top_pitchers |> 
  dplyr::select(PlayerName, Season, pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, pfx_FA_pct, pfx_SI_pct, pfx_SL_pct) |> 
  gather(key = "PitchType", value = "UsagePct", pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, pfx_FA_pct, pfx_SI_pct, pfx_SL_pct)



# Create a faceted line plot with custom titles and colors
data_long_top |> 
  ggplot(aes(x = Season, y = UsagePct, group = PlayerName, color = PlayerName))+
  geom_line()+
  facet_wrap(~ PitchType, scales = "free_y", labeller = labeller(PitchType = custom_labels)) +
  scale_color_manual(values = custom_colors)+
  labs(title = "Pitch Usage Changes for Top 5 Pitchers (2021-2023)",
       x = "Season",
       y = "Usage Percentage",
       color = "Player")+
  theme(legend.position = 'bottom')




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