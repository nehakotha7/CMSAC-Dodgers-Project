library(dplyr)
library(ggplot2)
library(ggridges)
library(patchwork)
library(tidyr)
library(tidyverse)
library(mgcv)
library(caret)
library(mice)
set.seed(123)
theme_set(theme_light())

library(baseballr) # install the package beforehand
# scraping pitching data from 2021
data_2021 = baseballr::fg_pitcher_leaders(startseason = 2021, endseason = 2021)
# only including pitchers who threw more than 250 pitches 
# *Might want to be more selective*
data_2021 <- data_2021[which(data_2021$Pitches >= 250),]
# adjusting the position column to classify into starters (SP) and relievers (RP)
for (player in data_2021){
  data_2021$position <- ifelse(data_2021$GS >= (data_2021$G - data_2021$GS), 
                               "SP", "RP")
}
for (player in data_2021){
  data_2021$pfx_FO_pct = NA
  data_2021$pfx_vFO = NA
  data_2021$`pfx_FO-X` = NA
  data_2021$`pfx_FO-Z` = NA
  data_2021$pfx_wFO_C = NA
  data_2021$sp_s_FO = NA
}

# Process repeated for 2022
data_2022 = baseballr::fg_pitcher_leaders(startseason = 2022, endseason = 2022)
data_2022 <- data_2022[which(data_2022$Pitches >= 250),]
for (player in data_2022){
  data_2022$position <- ifelse(data_2022$GS >= (data_2022$G - data_2022$GS), 
                               "SP", "RP")
}
for (player in data_2022){
  data_2022$pfx_SC_pct = NA
  data_2022$pfx_vSC = NA
  data_2022$`pfx_SC-X` = NA
  data_2022$`pfx_SC-Z` = NA
  data_2022$pfx_wSC_C = NA
  data_2022$pfx_FO_pct = NA
  data_2022$pfx_vFO = NA
  data_2022$`pfx_FO-X` = NA
  data_2022$`pfx_FO-Z` = NA
  data_2022$pfx_wFO_C = NA
  data_2022$sp_s_FO = NA
}


# process repeated for 2023
data_2023 = baseballr::fg_pitcher_leaders(startseason = 2023, endseason = 2023)
data_2023 <- data_2023[which(data_2023$Pitches >= 250),]
for (player in data_2023){
  data_2023$position <- ifelse(data_2023$GS >= (data_2023$G - data_2023$GS), 
                               "SP", "RP")
}

key_vars <- c("Season", 'position', 'IP', 'Throws', 'xMLBAMID', 'PlayerNameRoute',
              "ERA-", "K_9+", "K_BB+", "HR_9+", "WHIP+", "AVG+", "FIP-", 
              "BABIP+", "RAR", "WAR", "RA9-Wins", "xFIP-", "WPA", "RE24", 
              "REW", "pfx_CH_pct", "pfx_CU_pct", "pfx_FA_pct", "pfx_FC_pct", 
              "pfx_SI_pct", "pfx_SL_pct", "pfx_vCH", "pfx_vCU", "pfx_vFA", "pfx_vFC", 
              "pfx_vSI", "pfx_vSL", "pfx_CH-X", "pfx_CU-X", "pfx_FA-X", "pfx_FC-X", 
              "pfx_SI-X", "pfx_SL-X", "pfx_CH-Z", "pfx_CU-Z", "pfx_FA-Z", "pfx_FC-Z", 
              "pfx_SI-Z", "pfx_SL-Z", "pfx_wCH_C", "pfx_wCU_C", "pfx_wFA_C", "pfx_wFC_C", 
              "pfx_wSI_C", "pfx_wSL_C", "pfx_FS_pct", "pfx_vFS", "pfx_FS-X", "pfx_FS-Z", 
              "pfx_wFS_C", "pfx_SC_pct", "pfx_vSC", "pfx_SC-X", "pfx_SC-Z", "pfx_wSC_C", 
              "pfx_KC_pct", "pfx_vKC", "pfx_KC-X", "pfx_KC-Z", "pfx_wKC_C", "pfx_KN_pct", 
              "pfx_vKN", "pfx_KN-X", "pfx_KN-Z", "pfx_wKN_C","pfx_FO_pct", "pfx_vFO", 
              "pfx_FO-X", "pfx_FO-Z", "pfx_wFO_C", "sp_s_CH", "sp_s_CU", "sp_s_FF", 
              "sp_s_SI", "sp_s_SL", "sp_s_FC", "sp_s_FS", "sp_s_KC", "sp_s_FO",
              "sp_stuff"
              
)
# no stuff+ for screwball (only one pitcher throws it)

# creating condensed dataset for 2021
cond_data_2021 <- data_2021 |> 
  select(all_of(key_vars)
  )

# Creating condensed dataset for 2022 
# missing some Pitch Info variables.  Check on pitchFX
cond_data_2022 <- data_2022 |> 
  select(all_of(key_vars)
  )
# Creating condensed dataset for 2023
cond_data_2023 <- data_2023 |> 
  select(all_of(key_vars)
  )


# Reading in the data from statcast for extension and release point
savant <- read.csv("savant.csv")

savant_cond <- savant |> 
  select(pitch_type, game_year, release_pos_x, release_pos_z, player_name, 
         pitcher, release_extension) |> 
  group_by(player_name, game_year) |> 
  mutate(avg_release_extension = mean(release_extension, na.rm = TRUE),
         avg_rp_x = mean(release_pos_x, na.rm = TRUE),
         avg_rp_z = mean(release_pos_z, na.rm = TRUE)) |> 
  select(game_year, player_name, pitcher, avg_release_extension, avg_rp_x, avg_rp_z) |> 
  distinct(player_name, game_year, .keep_all = TRUE) |> 
  rename(xMLBAMID = pitcher, Season = game_year)

savant_cond_2021 <- savant_cond |> 
  filter(Season == 2021)

savant_cond_2022 <- savant_cond |> 
  filter(Season == 2022)

savant_cond_2023 <- savant_cond |> 
  filter(Season == 2023)


# Adding the spin rates for each pitch with a CSV pulled from Statcast
# Link to the 2021 data:https://baseballsavant.mlb.com/pitch-arsenals?year=2021&
# min=250&type=n_&hand=&sort=9&sortDir=desc

spin_2021 <- read.csv("pitch_spin_2021.csv")
spin_2021 <- rename(spin_2021, xMLBAMID = pitcher)
cond_data_2021 <- left_join(cond_data_2021, spin_2021, by="xMLBAMID")
cond_data_2021 <- left_join(cond_data_2021, savant_cond_2021, by = "xMLBAMID")

# 2022
spin_2022 <- read.csv("pitch_spin_2022.csv")
spin_2022 <- rename(spin_2022, xMLBAMID = pitcher)
cond_data_2022 <- left_join(cond_data_2022, spin_2022, by="xMLBAMID")
cond_data_2022 <- left_join(cond_data_2022, savant_cond_2022, by = "xMLBAMID")

# 2023
spin_2023 <- read.csv("pitch_spin_2023.csv")
spin_2023 <- rename(spin_2023, xMLBAMID = pitcher)
cond_data_2023 <- left_join(cond_data_2023, spin_2023, by="xMLBAMID")
cond_data_2023 <- left_join(cond_data_2023, savant_cond_2023, by = "xMLBAMID")


# combining all three condensed datasets
cond_data = rbind(cond_data_2021, cond_data_2022, cond_data_2023)

# Adding indicator variables for each pitch
# Setting the cutoff at 5% usage
cond_data <- cond_data |>
  mutate(ind_fastball = ifelse(is.na(pfx_FA_pct) | pfx_FA_pct <= 0.05, "No", "Yes"),
         ind_slider = ifelse(is.na(pfx_SL_pct) | pfx_SL_pct <= 0.05, "No", "Yes"),
         ind_cutter = ifelse(is.na(pfx_FC_pct) | pfx_FC_pct <= 0.05, "No", "Yes"),
         ind_curve = ifelse(is.na(pfx_CU_pct) | pfx_CU_pct <= 0.05, "No", "Yes"),
         ind_change = ifelse(is.na(pfx_CH_pct) | pfx_CH_pct <= 0.05, "No", "Yes"),
         ind_split = ifelse(is.na(pfx_FS_pct) | pfx_FS_pct <= 0.05, "No", "Yes"),
         ind_sinker = ifelse(is.na(pfx_SI_pct) | pfx_SI_pct <= 0.05, "No", "Yes"),
         ind_screw = ifelse(is.na(pfx_SC_pct) | pfx_SC_pct <= 0.05, "No", "Yes"),
         ind_fork = ifelse(is.na(pfx_FO_pct) | pfx_FO_pct <= 0.05, "No", "Yes"),
         ind_kc = ifelse(is.na(pfx_KC_pct) | pfx_KC_pct <= 0.05, "No", "Yes"),
         ind_knuckle = ifelse(is.na(pfx_KN_pct) | pfx_KN_pct <= 0.05, "No", "Yes")
  )

# Adjusting the horizontal movement variable
cond_data <- cond_data |> 
  mutate(`pfx_FA-X` = ifelse(Throws == "R", `pfx_SL-X` * -1, `pfx_SL-X`),
         `pfx_SL-X` = ifelse(Throws == "L", `pfx_SL-X` * -1, `pfx_SL-X`),
         `pfx_FC-X` = ifelse(Throws == "L", `pfx_FC-X` * -1, `pfx_FC-X`),
         `pfx_CU-X` = ifelse(Throws == "L", `pfx_CU-X` * -1, `pfx_CU-X`),
         `pfx_CH-X` = ifelse(Throws == "R", `pfx_CH-X` * -1, `pfx_CH-X`),
         `pfx_FS-X` = ifelse(Throws == "R", `pfx_FS-X` * -1, `pfx_FS-X`),
         `pfx_SI-X` = ifelse(Throws == "R", `pfx_SI-X` * -1, `pfx_SI-X`),
         `pfx_SC-X` = ifelse(Throws == "R", `pfx_SC-X` * -1, `pfx_SC-X`),
         `pfx_FO-X` = ifelse(Throws == "R", `pfx_FO-X` * -1, `pfx_FO-X`),
         `pfx_KC-X` = ifelse(Throws == "L", `pfx_KC-X` * -1, `pfx_KC-X`),
         `pfx_KN-X` = abs(`pfx_KN-X`)
  )





# early EDA ---------------------------------------------------------------



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





# Combine datasets for seasonal info --------------------------------------

combined_data <- bind_rows(data_2021, data_2022, data_2023)


# identify pitcher who changed pitch usage --------------------------------
relevant_cols <- c('PlayerName', 'Season', 'pfx_FA_pct', 'pfx_FC_pct', 'pfx_SL_pct', 
                   'pfx_CH_pct', 'pfx_CU_pct', 'pfx_SI_pct', 'pfx_FS_pct', 
                   'pfx_SC_pct', 'pfx_FO_pct', 'pfx_KC_pct', 'pfx_EP_pct', 'pfx_KN_pct')
colnames_combined_data <- colnames(combined_data)
pitch_cols <- colnames_combined_data[grep("pfx_", colnames_combined_data)]


# Define custom labels for each pitch type
custom_labels <- c(
  pfx_CH_pct = "Changeup (CH)",
  pfx_CU_pct = "Curveball (CU)",
  pfx_FA_pct = "4-Seam Fastball (FA)",
  pfx_SI_pct = "Sinker (SI)",
  pfx_SL_pct = "Slider (SL)",
  pfx_FC_pct = 'Cutter (FC)',
  pfx_SC_pct = 'Screwball (SC)',
  pfx_FS_pct = 'Split-Finger (FS)',
  pfx_FO_pct = 'Forkball (FO)',
  pfx_KC_pct = 'Knuckle Curve (KC)',
  pfx_EP_pct = 'Eephus (EP)',
  pfx_KN_pct = 'Knuckleball (KN)',
  pfx_PO_pct = 'Pitch Out (PO)',
  pfx_SV_pct = 'Slurve (SV)',
  pfx_CS_pct = 'Slow Curve (CS)',
  pfx_ST_pct = 'Sweeper (ST)',
  
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
library(ggplot2)
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
  usage_change <- combined_data |>
  dplyr::select(PlayerName, Season, pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, 
                pfx_FA_pct, pfx_SI_pct, pfx_SL_pct, pfx_SC_pct, pfx_FS_pct, pfx_FO_pct, pfx_KC_pct, pfx_KN_pct, pfx_EP_pct) |>
  gather(key = 'PitchType', value = 'UsagePct', pfx_CH_pct, pfx_CU_pct, pfx_FC_pct, 
         pfx_FA_pct, pfx_SI_pct, pfx_SL_pct, pfx_SC_pct, pfx_FS_pct, pfx_FO_pct, pfx_KC_pct, pfx_KN_pct, pfx_EP_pct) |>
  pivot_wider(names_from = Season, values_from = UsagePct, names_prefix = "Year_") |>
  mutate(Change = `Year_2023` - `Year_2021`) |>
  filter(!is.na(Change)) |>
  select(-Change)

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





# Exploring savant & condensed datasets ------------------------------------------------




# Rename columns for usability

cond_data <- cond_data |> 
  rename(PlayerName = PlayerNameRoute, 
         Season = Season.x)


# Function to detect changes in pitch arsenal

detect_arsenal_change <- function(current, previous) {
  
  return(ifelse(is.na(previous), 0, (current - previous) / (previous + 1e-10)))
  
}



# Initialize comprehensive pitch arsenal

pitch_arsenal <- cond_data |> 
  group_by(PlayerName) |> 
  arrange(PlayerName, Season) |> 
  mutate(
    Change_FA = if_else(Season == 2021, 0, detect_arsenal_change(pfx_FA_pct, lag(pfx_FA_pct))),
    Change_SL = if_else(Season == 2021, 0, detect_arsenal_change(pfx_SL_pct, lag(pfx_SL_pct))),
    Change_CH = if_else(Season == 2021, 0, detect_arsenal_change(pfx_CH_pct, lag(pfx_CH_pct))),
    Change_CU = if_else(Season == 2021, 0, detect_arsenal_change(pfx_CU_pct, lag(pfx_CU_pct))),
    Change_SI = if_else(Season == 2021, 0, detect_arsenal_change(pfx_SI_pct, lag(pfx_SI_pct))),
    Change_FC = if_else(Season == 2021, 0, detect_arsenal_change(pfx_FC_pct, lag(pfx_FC_pct))),
    Addition_FA = if_else(Season == 2021, 0, if_else(Change_FA > .02, 1, 0)),
    Deletion_FA = if_else(Season == 2021, 0, if_else(Change_FA <= -.02, 1, 0)),
    Addition_SL = if_else(Season == 2021, 0, if_else(Change_SL > .02, 1, 0)),
    Deletion_SL = if_else(Season == 2021, 0, if_else(Change_SL <= -.02, 1, 0)),
    Addition_CH = if_else(Season == 2021, 0, if_else(Change_CH > .02, 1, 0)),
    Deletion_CH = if_else(Season == 2021, 0, if_else(Change_CH <= -.02, 1, 0)),
    Addition_CU = if_else(Season == 2021, 0, if_else(Change_CH > .02, 1, 0)),
    Deletion_CU = if_else(Season == 2021, 0, if_else(Change_CU <= -.02, 1, 0)),
    Addition_SI = if_else(Season == 2021, 0, if_else(Change_SI > .02, 1, 0)),
    Deletion_SI = if_else(Season == 2021, 0, if_else(Change_SI <= -.02, 1, 0)),
    Addition_FC = if_else(Season == 2021, 0, if_else(Change_FC > .02, 1, 0)),
    Deletion_FC = if_else(Season == 2021, 0, if_else(Change_FC <= -.02, 1, 0))
  ) |> 
  ungroup()




# Additional adjustments

# Ensure categorical variables are factors
pitch_arsenal$Throws <- as.factor(pitch_arsenal$Throws)
pitch_arsenal$position <- as.factor(pitch_arsenal$position)

# Edit problematic column names
names(pitch_arsenal)[names(pitch_arsenal) == 'K_9+'] <- 'K_9_plus'
names(pitch_arsenal)[names(pitch_arsenal) == 'ERA-'] <- 'ERA_minus'
names(pitch_arsenal)[names(pitch_arsenal) == 'WHIP+'] <- 'WHIP_plus'
names(pitch_arsenal)[names(pitch_arsenal) == 'BABIP+'] <- 'BABIP_plus'
names(pitch_arsenal)[names(pitch_arsenal) == 'FIP-'] <- 'FIP_minus'
names(pitch_arsenal)[names(pitch_arsenal) == 'xFIP-'] <- 'xFIP_minus'




# Changeup Calc ----------------------------------------------------------





# Select relevant columns

relevant_cols <- c('Season', 'PlayerName', 'sp_stuff', 'RAR', 'pfx_CH_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'pfx_CH-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_CH-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'ch_avg_spin', 'WPA',
                   'REW', 'pfx_vCH', 'sp_s_CH', 'xFIP_minus', "ind_change",
                   'Throws', 'position', 'Addition_CH', 'Deletion_CH')


# Ensure target and primary predictor are not missing

filtered_data <- pitch_arsenal |> 
  select(all_of(relevant_cols)) |> 
  filter(!is.na(sp_stuff), ind_change == 'Yes')


# Rename movement columns in the filtered_data 

names(filtered_data) <- gsub('-', '_', names(filtered_data))





# Handling missing values using mice 


mice_data <- filtered_data |> 
  select(-PlayerName) |> 
  mice(method = 'rf', m = 10, maxit = 100)

data_filled <- complete(mice_data)


# # Split data into training and testing sets
# 






# K-Fold Cross-Validation



data_filled$Throws <- as.factor(data_filled$Throws)
data_filled$position <- as.factor(data_filled$position)


k <- 6
folds <- createFolds(data_filled$sp_stuff, k = k, list = T)

perform_cv <- function(fold, data) {
  
  # Split into training and validation sets
  train_indices <- unlist(folds[-fold])
  val_indices <- unlist(folds[fold])
  
  train_set <- data[train_indices, ]
  val_set <- data[val_indices, ]
  
  # Fit GAM model
  gam_model <- gam(sp_stuff ~ s(sp_s_CH, by = interaction(Throws, position)) +
                     s(avg_release_extension, by = Throws) + Throws + 
                     s(pfx_CH_pct, pfx_vCH, by = position) + position +
                     s(avg_rp_x, avg_rp_z) + s(pfx_CH_X, pfx_CH_Z) +
                     s(pfx_vCH, ch_avg_spin, by = Throws) + 
                     s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                     s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                   data = train_set)
  
  # Get model summary
  gam_summary <- summary(gam_model)
  
  # Predict on validation set
  val_preds <- predict(gam_model, newdata = val_set)
  
  # Extract metrics
  R2 <- gam_summary$r.sq
  Deviance_Explained <- gam_summary$dev.expl
  val_rmse <- sqrt(mean((val_set$sp_stuff - val_preds)^2))
  
  # Return results as a list
  return(data.frame(fold = fold, R2 = R2, Deviance_Explained = Deviance_Explained, rmse = val_rmse))
  
}


# Perform k-fold cross-validation using lapply
cv_results <- lapply(1:k, perform_cv, data = data_filled)

# Combine results into a single data frame
cv_results <- bind_rows(cv_results)

# Average cross-validation results
median_results <- apply(cv_results[, 2:4], 2, median)
print(median_results)







# Fit final GAM calculator

final_gam_model <- gam(sp_stuff ~ s(sp_s_CH, by = interaction(Throws, position))
                       + s(avg_release_extension, by = Throws) + Throws + 
                         s(pfx_CH_pct, pfx_vCH, by = position) + position +
                         s(avg_rp_x, avg_rp_z) + s(pfx_CH_X, pfx_CH_Z) +
                         s(pfx_vCH, ch_avg_spin, by = Throws) + 
                         s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                         s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                       data = data_filled)


summary(final_gam_model)



# Make Predictions

# Function to predict sp_stuff for a given player

predict_sp_stuff <- function(new_data) {
  
  new_data_filled <- new_data
  
  # Handling missing values using mice 

  if (sum(is.na(new_data)) != 0) {
    mice_new_data <- mice(new_data, method = 'rf', m = 5, maxit = 50)
    new_data_filled <- complete(mice_new_data)
  }
  
  # Predict sp_stuff using GAM model
  predict(final_gam_model, newdata = new_data_filled)
  
}


# Example Use

new_player_data <- filtered_data[filtered_data$PlayerName == 'Aaron Nola', ]
predicted_stuff_plus <- predict_sp_stuff(new_player_data)
print(predicted_stuff_plus)







# Curveball Calc ---------------------------------------------------------






# Select relevant columns

relevant_cols <- c('Season', 'PlayerName', 'sp_stuff', 'RAR', 'pfx_CU_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'pfx_CU-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_CU-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'cu_avg_spin', 'WPA',
                   'REW', 'pfx_vCU', 'sp_s_CU', 'xFIP_minus', 'ind_curve',
                   'Throws', 'position', 'Addition_CU', 'Deletion_CU')

# Ensure target and primary predictor are not missing

filtered_data <- pitch_arsenal |> 
  select(all_of(relevant_cols)) |> 
  filter(!is.na(sp_stuff), ind_curve == 'Yes')


# Rename movement columns in the filtered_data 
names(filtered_data) <- gsub('-', '_', names(filtered_data))





# Handling missing values using mice 


mice_data <- filtered_data |> 
  select(-PlayerName) |> 
  mice(method = 'rf', m = 10, maxit = 100)

data_filled <- complete(mice_data)


# Split data into training and testing sets



# K-Fold Cross-Validation



data_filled$Throws <- as.factor(data_filled$Throws)
data_filled$position <- as.factor(data_filled$position)


k <- 6
folds <- createFolds(data_filled$sp_stuff, k = k, list = T)

perform_cv <- function(fold, data) {
  
  # Split into training and validation sets
  train_indices <- unlist(folds[-fold])
  val_indices <- unlist(folds[fold])
  
  train_set <- data[train_indices, ]
  val_set <- data[val_indices, ]
  
  # Fit GAM model
  gam_model <- gam(sp_stuff ~ s(sp_s_CU, by = interaction(Throws, position)) +
                     s(avg_release_extension, by = Throws) + Throws + 
                     s(pfx_CU_pct, pfx_vCU, by = position) + position +
                     s(avg_rp_x, avg_rp_z) + s(pfx_CU_X, pfx_CU_Z) +
                     s(pfx_vCU, cu_avg_spin, by = Throws) + 
                     s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                     s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                   data = train_set)
  
  # Get model summary
  gam_summary <- summary(gam_model)
  
  # Predict on validation set
  val_preds <- predict(gam_model, newdata = val_set)
  
  # Extract metrics
  R2 <- gam_summary$r.sq
  Deviance_Explained <- gam_summary$dev.expl
  val_rmse <- sqrt(mean((val_set$sp_stuff - val_preds)^2))
  
  # Return results as a list
  return(data.frame(fold = fold, R2 = R2, Deviance_Explained = Deviance_Explained, rmse = val_rmse))
  
}


# Perform k-fold cross-validation using lapply
cv_results <- lapply(1:k, perform_cv, data = data_filled)

# Combine results into a single data frame
cv_results <- bind_rows(cv_results)

# Average cross-validation results
median_results <- apply(cv_results[, 2:4], 2, median)
print(median_results)




# Fit final GAM calculator

final_gam_model <- gam(sp_stuff ~ s(sp_s_CU, by = interaction(Throws, position))
                       + s(avg_release_extension, by = Throws) + Throws + 
                         s(pfx_CU_pct, pfx_vCU, by = position) + position +
                         s(avg_rp_x, avg_rp_z) + s(pfx_CU_X, pfx_CU_Z) +
                         s(pfx_vCU, cu_avg_spin, by = Throws) + 
                         s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                         s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                       data = data_filled)


summary(final_gam_model)



# Make Predictions

# Function to predict sp_stuff for a given player

predict_sp_stuff <- function(new_data) {
  
  new_data_filled <- new_data
  
  # Handling missing values using mice 
  
  if (sum(is.na(new_data)) != 0) {
    mice_new_data <- mice(new_data, method = 'rf', m = 5, maxit = 50)
    new_data_filled <- complete(mice_new_data)
  }
  
  # Predict sp_stuff using GAM model
  predict(final_gam_model, newdata = new_data_filled)
  
}


# Example Use

new_player_data <- filtered_data[filtered_data$PlayerName == 'Aaron Civale', ]
predicted_stuff_plus <- predict_sp_stuff(new_player_data)
print(predicted_stuff_plus)








# Cutter Calc -------------------------------------------------------------






# Select relevant columns

relevant_cols <- c('Season', 'PlayerName', 'sp_stuff', 'RAR', 'pfx_FC_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'pfx_FC-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_FC-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'fc_avg_spin', 'WPA',
                   'REW', 'pfx_vFC', 'sp_s_FC', 'xFIP_minus', 'ind_cutter',
                   'Throws', 'position', 'Addition_FC', 'Deletion_FC')


# Ensure target and primary predictor are not missing

filtered_data <- pitch_arsenal |> 
  select(all_of(relevant_cols)) |> 
  filter(!is.na(sp_stuff), ind_cutter == 'Yes')


# Rename movement columns in the filtered_data 
names(filtered_data) <- gsub('-', '_', names(filtered_data))





# Handling missing values using mice 


mice_data <- filtered_data |> 
  select(-PlayerName) |> 
  mice(method = 'rf', m = 10, maxit = 100)

data_filled <- complete(mice_data)


# Split data into training and testing sets





# K-Fold Cross-Validation




data_filled$Throws <- as.factor(data_filled$Throws)
data_filled$position <- as.factor(data_filled$position)


k <- 6
folds <- createFolds(data_filled$sp_stuff, k = k, list = T)

perform_cv <- function(fold, data) {
  
  # Split into training and validation sets
  train_indices <- unlist(folds[-fold])
  val_indices <- unlist(folds[fold])
  
  train_set <- data[train_indices, ]
  val_set <- data[val_indices, ]
  
  # Fit GAM model
  gam_model <- gam(sp_stuff ~ s(sp_s_FC, by = interaction(Throws, position))
                   + s(avg_release_extension, by = Throws) + Throws + 
                     s(pfx_FC_pct, pfx_vFC, by = position) + position +
                     s(avg_rp_x, avg_rp_z) + s(pfx_FC_X, pfx_FC_Z) +
                     s(pfx_vFC, fc_avg_spin, by = Throws) + 
                     s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                     s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                   data = train_set)
  
  # Get model summary
  gam_summary <- summary(gam_model)
  
  # Predict on validation set
  val_preds <- predict(gam_model, newdata = val_set)
  
  # Extract metrics
  R2 <- gam_summary$r.sq
  Deviance_Explained <- gam_summary$dev.expl
  val_rmse <- sqrt(mean((val_set$sp_stuff - val_preds)^2))
  
  # Return results as a list
  return(data.frame(fold = fold, R2 = R2, Deviance_Explained = Deviance_Explained, rmse = val_rmse))
  
}


# Perform k-fold cross-validation using lapply
cv_results <- lapply(1:k, perform_cv, data = data_filled)

# Combine results into a single data frame
cv_results <- bind_rows(cv_results)

# Average cross-validation results
median_results <- apply(cv_results[, 2:4], 2, median)
print(median_results)




# Fit final GAM calculator

final_gam_model <- gam(sp_stuff ~ s(sp_s_FC, by = interaction(Throws, position))
                       + s(avg_release_extension, by = Throws) + Throws + 
                         s(pfx_FC_pct, pfx_vFC, by = position) + position +
                         s(avg_rp_x, avg_rp_z) + s(pfx_FC_X, pfx_FC_Z) +
                         s(pfx_vFC, fc_avg_spin, by = Throws) + 
                         s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                         s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                       data = data_filled)


summary(final_gam_model)



# Make Predictions

# Function to predict sp_stuff for a given player

predict_sp_stuff <- function(new_data) {
  
  new_data_filled <- new_data
  
  # Handling missing values using mice 
  
  if (sum(is.na(new_data)) != 0) {
    mice_new_data <- mice(new_data, method = 'rf', m = 5, maxit = 50)
    new_data_filled <- complete(mice_new_data)
  }
  
  # Predict sp_stuff using GAM model
  predict(final_gam_model, newdata = new_data_filled)
  
}


# Example Use

new_player_data <- filtered_data[filtered_data$PlayerName == 'Brooks Raley', ]
predicted_stuff_plus <- predict_sp_stuff(new_player_data)
print(predicted_stuff_plus)








# Fastball Calc -----------------------------------------------------------






# Select relevant columns

relevant_cols <- c('Season', 'PlayerName', 'sp_stuff', 'RAR', 'pfx_FA_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'pfx_FA-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_FA-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'ff_avg_spin', 'WPA',
                   'REW', 'pfx_vFA', 'sp_s_FF', 'xFIP_minus', 'ind_fastball',
                   'Throws', 'position', 'Addition_FA', 'Deletion_FA')


# Ensure target and primary predictor are not missing

filtered_data <- pitch_arsenal |> 
  select(all_of(relevant_cols)) |> 
  filter(!is.na(sp_stuff), ind_fastball == 'Yes')


# Rename movement columns in the filtered_data 
names(filtered_data) <- gsub('-', '_', names(filtered_data))





# Handling missing values using mice 


mice_data <- filtered_data |> 
  select(-PlayerName) |> 
  mice(method = 'rf', m = 10, maxit = 100)

data_filled <- complete(mice_data)

# Split data into training and testing sets







# K-Fold Cross-Validation




data_filled$Throws <- as.factor(data_filled$Throws)
data_filled$position <- as.factor(data_filled$position)


k <- 6
folds <- createFolds(data_filled$sp_stuff, k = k, list = T)

perform_cv <- function(fold, data) {
  
  # Split into training and validation sets
  train_indices <- unlist(folds[-fold])
  val_indices <- unlist(folds[fold])
  
  train_set <- data[train_indices, ]
  val_set <- data[val_indices, ]
  
  # Fit GAM model
  gam_model <- gam(sp_stuff ~ s(sp_s_FF, by = interaction(Throws, position))
                   + s(avg_release_extension, by = Throws) + Throws + 
                     s(pfx_FA_pct, pfx_vFA, by = position) + position +
                     s(avg_rp_x, avg_rp_z) + s(pfx_FA_X, pfx_FA_Z) +
                     s(pfx_vFA, ff_avg_spin, by = Throws) + 
                     s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                     s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                   data = train_set)
  
  # Get model summary
  gam_summary <- summary(gam_model)
  
  # Predict on validation set
  val_preds <- predict(gam_model, newdata = val_set)
  
  # Extract metrics
  R2 <- gam_summary$r.sq
  Deviance_Explained <- gam_summary$dev.expl
  val_rmse <- sqrt(mean((val_set$sp_stuff - val_preds)^2))
  
  # Return results as a list
  return(data.frame(fold = fold, R2 = R2, Deviance_Explained = Deviance_Explained, rmse = val_rmse))
  
}


# Perform k-fold cross-validation using lapply
cv_results <- lapply(1:k, perform_cv, data = data_filled)

# Combine results into a single data frame
cv_results <- bind_rows(cv_results)

# Average cross-validation results
median_results <- apply(cv_results[, 2:4], 2, median)
print(median_results)




# Fit final GAM calculator

final_gam_model <- gam(sp_stuff ~ s(sp_s_FF, by = interaction(Throws, position))
                       + s(avg_release_extension, by = Throws) + Throws + 
                         s(pfx_FA_pct, pfx_vFA, by = position) + position +
                         s(avg_rp_x, avg_rp_z) + s(pfx_FA_X, pfx_FA_Z) +
                         s(pfx_vFA, ff_avg_spin, by = Throws) + 
                         s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                         s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                       data = data_filled)


summary(final_gam_model)



# Make Predictions

# Function to predict sp_stuff for a given player

predict_sp_stuff <- function(new_data) {
  
  new_data_filled <- new_data
  
  # Handling missing values using mice 
  
  if (sum(is.na(new_data)) != 0) {
    mice_new_data <- mice(new_data, method = 'rf', m = 5, maxit = 50)
    new_data_filled <- complete(mice_new_data)
  }
  
  # Predict sp_stuff using GAM model
  predict(final_gam_model, newdata = new_data_filled)
  
}


# Example Use

new_player_data <- filtered_data[filtered_data$PlayerName == 'Anthony Bass', ]
predicted_stuff_plus <- predict_sp_stuff(new_player_data)
print(predicted_stuff_plus)







# Sinker Calc -------------------------------------------------------------





# Select relevant columns

relevant_cols <- c('Season', 'PlayerName', 'sp_stuff', 'RAR', 'pfx_SI_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'pfx_SI-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_SI-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'si_avg_spin', 'WPA',
                   'REW', 'pfx_vSI', 'sp_s_SI', 'xFIP_minus', 'ind_sinker',
                   'Throws', 'position', 'Addition_SI', 'Deletion_SI')


# Ensure target and primary predictor are not missing

filtered_data <- pitch_arsenal |> 
  select(all_of(relevant_cols)) |> 
  filter(!is.na(sp_stuff), ind_sinker == 'Yes')


# Rename movement columns in the filtered_data 
names(filtered_data) <- gsub('-', '_', names(filtered_data))





# Handling missing values using mice 


mice_data <- filtered_data |> 
  select(-PlayerName) |> 
  mice(method = 'rf', m = 10, maxit = 100)

data_filled <- complete(mice_data)

# Split data into training and testing sets







# K-Fold Cross-Validation




data_filled$Throws <- as.factor(data_filled$Throws)
data_filled$position <- as.factor(data_filled$position)


k <- 6
folds <- createFolds(data_filled$sp_stuff, k = k, list = T)

perform_cv <- function(fold, data) {
  
  # Split into training and validation sets
  train_indices <- unlist(folds[-fold])
  val_indices <- unlist(folds[fold])
  
  train_set <- data[train_indices, ]
  val_set <- data[val_indices, ]
  
  # Fit GAM model
  gam_model <- gam(sp_stuff ~ s(sp_s_SI, by = interaction(Throws, position))
                   + s(avg_release_extension, by = Throws) + Throws + 
                     s(pfx_SI_pct, pfx_vSI, by = position) + position +
                     s(avg_rp_x, avg_rp_z) + s(pfx_SI_X, pfx_SI_Z) +
                     s(pfx_vSI, si_avg_spin, by = Throws) + 
                     s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                     s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                   data = train_set)
  
  # Get model summary
  gam_summary <- summary(gam_model)
  
  # Predict on validation set
  val_preds <- predict(gam_model, newdata = val_set)
  
  # Extract metrics
  R2 <- gam_summary$r.sq
  Deviance_Explained <- gam_summary$dev.expl
  val_rmse <- sqrt(mean((val_set$sp_stuff - val_preds)^2))
  
  # Return results as a list
  return(data.frame(fold = fold, R2 = R2, Deviance_Explained = Deviance_Explained, rmse = val_rmse))
  
}


# Perform k-fold cross-validation using lapply
cv_results <- lapply(1:k, perform_cv, data = data_filled)

# Combine results into a single data frame
cv_results <- bind_rows(cv_results)

# Average cross-validation results
median_results <- apply(cv_results[, 2:4], 2, median)
print(median_results)




# Fit final GAM calculator

final_gam_model <- gam(sp_stuff ~ s(sp_s_SI, by = interaction(Throws, position))
                       + s(avg_release_extension, by = Throws) + Throws + 
                         s(pfx_SI_pct, pfx_vSI, by = position) + position +
                         s(avg_rp_x, avg_rp_z) + s(pfx_SI_X, pfx_SI_Z) +
                         s(pfx_vSI, si_avg_spin, by = Throws) + 
                         s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                         s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                       data = data_filled)


summary(final_gam_model)



# Make Predictions

# Function to predict sp_stuff for a given player

predict_sp_stuff <- function(new_data) {
  
  new_data_filled <- new_data
  
  # Handling missing values using mice 
  
  if (sum(is.na(new_data)) != 0) {
    mice_new_data <- mice(new_data, method = 'rf', m = 5, maxit = 50)
    new_data_filled <- complete(mice_new_data)
  }
  
  # Predict sp_stuff using GAM model
  predict(final_gam_model, newdata = new_data_filled)
  
}


# Example Use

new_player_data <- filtered_data[filtered_data$PlayerName == 'Alek Manoah', ]
predicted_stuff_plus <- predict_sp_stuff(new_player_data)
print(predicted_stuff_plus)






# Slider Calc -------------------------------------------------------------






# Select relevant columns

relevant_cols <- c('Season', 'PlayerName', 'sp_stuff', 'RAR', 'pfx_SL_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'pfx_SL-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_SL-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'sl_avg_spin', 'WPA',
                   'REW', 'pfx_vSL', 'sp_s_SL', 'xFIP_minus', 'ind_slider',
                   'Throws', 'position', 'Addition_SL', 'Deletion_SL')


# Ensure target and primary predictor are not missing

filtered_data <- pitch_arsenal |> 
  select(all_of(relevant_cols)) |> 
  filter(!is.na(sp_stuff), ind_slider == 'Yes')


# Rename movement columns in the filtered_data 
names(filtered_data) <- gsub('-', '_', names(filtered_data))





# Handling missing values using mice 


mice_data <- filtered_data |> 
  select(-PlayerName) |> 
  mice(method = 'rf', m = 10, maxit = 100)

data_filled <- complete(mice_data)


# Split data into training and testing sets







# K-Fold Cross-Validation




data_filled$Throws <- as.factor(data_filled$Throws)
data_filled$position <- as.factor(data_filled$position)


k <- 6
folds <- createFolds(data_filled$sp_stuff, k = k, list = T)

perform_cv <- function(fold, data) {
  
  # Split into training and validation sets
  train_indices <- unlist(folds[-fold])
  val_indices <- unlist(folds[fold])
  
  train_set <- data[train_indices, ]
  val_set <- data[val_indices, ]
  
  # Fit GAM model
  gam_model <- gam(sp_stuff ~ s(sp_s_SL, by = interaction(Throws, position))
                   + s(avg_release_extension, by = Throws) + Throws + 
                     s(pfx_SL_pct, pfx_vSL, by = position) + position +
                     s(avg_rp_x, avg_rp_z) + s(pfx_SL_X, pfx_SL_Z) +
                     s(pfx_vSL, sl_avg_spin, by = Throws) + 
                     s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                     s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                   data = train_set)
  
  # Get model summary
  gam_summary <- summary(gam_model)
  
  # Predict on validation set
  val_preds <- predict(gam_model, newdata = val_set)
  
  # Extract metrics
  R2 <- gam_summary$r.sq
  Deviance_Explained <- gam_summary$dev.expl
  val_rmse <- sqrt(mean((val_set$sp_stuff - val_preds)^2))
  
  # Return results as a list
  return(data.frame(fold = fold, R2 = R2, Deviance_Explained = Deviance_Explained, rmse = val_rmse))
  
}


# Perform k-fold cross-validation using lapply
cv_results <- lapply(1:k, perform_cv, data = data_filled)

# Combine results into a single data frame
cv_results <- bind_rows(cv_results)

# Average cross-validation results
median_results <- apply(cv_results[, 2:4], 2, median)
print(median_results)




# Fit final GAM calculator

final_gam_model <- gam(sp_stuff ~ s(sp_s_SL, by = interaction(Throws, position))
                       + s(avg_release_extension, by = Throws) + Throws + 
                         s(pfx_SL_pct, pfx_vSL, by = position) + position +
                         s(avg_rp_x, avg_rp_z) + s(pfx_SL_X, pfx_SL_Z) +
                         s(pfx_vSL, sl_avg_spin, by = Throws) + 
                         s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                         s(RAR, WAR) + s(REW, BABIP_plus) + s(xFIP_minus, WPA),
                       data = data_filled)



summary(final_gam_model)



# Make Predictions

# Function to predict sp_stuff for a given player

predict_sp_stuff <- function(new_data) {
  
  new_data_filled <- new_data
  
  # Handling missing values using mice 
  
  if (sum(is.na(new_data)) != 0) {
    mice_new_data <- mice(new_data, method = 'rf', m = 5, maxit = 50)
    new_data_filled <- complete(mice_new_data)
  }
  
  # Predict sp_stuff using GAM model
  predict(final_gam_model, newdata = new_data_filled)
  
}


# Example Use

new_player_data <- filtered_data[filtered_data$PlayerName == 'Adrian Houser', ]
predicted_stuff_plus <- predict_sp_stuff(new_player_data)
print(predicted_stuff_plus)






# note to self ------------------------------------------------------------

# Out of the myriads of variables which one is important?
# EDA
# runs above average to stuff+

# ridge plots over time !boxplot
# a way to tally changes in arsenal

# binary column variable as factor checker to detect a change between consecutive years
# just keep conditioning - already have the motherboard
# Stick with it but remain gracious to flexibility

# generalize line plots to no of pitchers per year instead of usage percentage
# frequency of added and subtracted pitchers

# tree base approach to output potential run value

# plots movements vs avg on each axis(pfx with dash)
# color by maybe slider for now


# Limitation

# In creating these metrics for pitch arsenal, stuff plus (sp) doesn't take into account the batter's action
# model stuff+ based on attributes using gam and mgcv
# compare to pfx_stuff+ given
# get a run value for pitchers

# ridge and lasso regression for initially modeling for stuff+
# cross validation
# toss it in see what sticks
# Individual gam based on 6 relevant pitch types
# predicting aggregate characteristics


# Does changing arsenal influence metrics in the dataset
# Decision trees to create holdout sets to better test

# Make a row unique to pitcher & year
  # 

# package catboost for tree based approach

# with cond_data & pitch_arsenal you've got to create a calculator based function model
  # to estimate stuff+ of a new pitch to be added to arsenal in relation to other metric. We're restricting input prompt to test for only the response of adding not subtracting to the arsenal, for now

# Look to integrate decision trees using catboost to terminate the NA's to a group on its own to implement a more accurate model