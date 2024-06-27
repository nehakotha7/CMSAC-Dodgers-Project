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
              "sp_stuff", "sp_location", "sp_pitching", 'WAR', 'wFB')
cond_data_2021 <- data_2021 |> 
  dplyr::select(all_of(key_vars))

# summary(cond_data_2021)



# Earned Runs Average
cond_data_2021 |> 
  ggplot(aes(ERA))+
  geom_histogram(binwidth = 0.5, fill = 'blue', color = 'black')+
  labs(title = "Distribution of ERA", x = "ERA", y = "Frequency")

cond_data_2021 |> 
  ggplot(aes(x = FBv, y = SO))+
  geom_point(alpha = .5, color = 'red')+
  geom_smooth(method = 'lm', color = 'blue', se = F)+
  labs(title = "Fastball Velocity vs Strikeouts", x = "Fastball Velocity (mph)", y = "Strikeouts")

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
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm')+
  labs(title = "Stuff+ vs WAR", x = "Stuff+", y = "WAR")

# Pitch Type vs Performance:
cond_data_2021 |> 
  ggplot(aes(wFB, ERA))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm')+
  labs(title = "Fastball Value (wFB) vs ERA", x = "wFB", y = "ERA")


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


# Out of the myriads of variables which one is important?
# EDA

# Limitation
  # In creating these metrics for pitch arsenal, stuff plus (sp) doesn't take into account the batter's action