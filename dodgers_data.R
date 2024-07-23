
# Data Wrangling ----------------------------------------------------------
library(baseballr)
library(ggplot2)
library(tidyr)
library(glmnet)
library(broom)
library(caret)
library(tidyverse)
library(dplyr)
library(Metrics)
library(ggridges)
library(patchwork)
library(catboost)
library(mgcv)
library(rsample)
library(ranger) #suarez

#scraping pitching data from 2021
data_2021 = baseballr::fg_pitcher_leaders(startseason = 2021, endseason = 2021)
#only including pitchers who threw more than 250 pitches 
#*Might want to be more selective*
data_2021 <- data_2021[which(data_2021$Pitches >= 250),]
#adjusting the position column to classify into starters (SP) and relievers (RP)
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

#Process repeated for 2022
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

#process repeated for 2023
data_2023 = baseballr::fg_pitcher_leaders(startseason = 2023, endseason = 2023)
data_2023 <- data_2023[which(data_2023$Pitches >= 250),]
for (player in data_2023){
  data_2023$position <- ifelse(data_2023$GS >= (data_2023$G - data_2023$GS), 
                               "SP", "RP")
}

#selecting variables we care about
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
#no stuff+ for screwball or knuckleball (only one pitcher throws it)

#creating condensed dataset for 2021
cond_data_2021 <- data_2021 |> 
  select(all_of(key_vars))

#Creating condensed dataset for 2022 
#missing some Pitch Info variables.  Check on pitchFX
cond_data_2022 <- data_2022 |> 
  select(all_of(key_vars))

#Creating condensed dataset for 2023
cond_data_2023 <- data_2023 |> 
  select(all_of(key_vars))

# Merging with Other Data -------------------------------------------------

#Reading in the data from statcast for extension and release point
savant <- read.csv("savant.csv")
savant_cond <- savant |> 
  select(pitch_type, game_year, release_pos_x, release_pos_z, player_name, 
         pitcher, release_extension) |> 
  mutate(season = game_year) |> 
  group_by(player_name, season) |> 
  mutate(avg_release_extension = mean(release_extension, na.rm = TRUE),
         avg_rp_x = mean(release_pos_x, na.rm = TRUE),
         avg_rp_z = mean(release_pos_z, na.rm = TRUE)) |> 
  select(season, player_name, pitcher, avg_release_extension, avg_rp_x, avg_rp_z) |> 
  distinct(player_name, season, .keep_all = TRUE) |> 
  rename(xMLBAMID = pitcher)

savant_cond_2021 <- savant_cond |> 
  filter(season == 2021)

savant_cond_2022 <- savant_cond |> 
  filter(season == 2022)

savant_cond_2023 <- savant_cond |> 
  filter(season == 2023)

#Adding the spin rates for each pitch with a CSV pulled from Statcast
#Link to the 2021 data:https://baseballsavant.mlb.com/pitch-arsenals?year=2021&
#min=250&type=n_&hand=&sort=9&sortDir=desc
#Issue here: we aren't recognizing sweepers and slurves, Statcast isn't recognizing
#knuckle curves and possibly forkballs.
spin_2021 <- read.csv("pitch_spin_2021.csv")
spin_2021 <- rename(spin_2021, xMLBAMID = pitcher)
cond_data_2021 <- left_join(cond_data_2021, spin_2021, by="xMLBAMID")
cond_data_2021 <- left_join(cond_data_2021, savant_cond_2021, by = "xMLBAMID")

#2022
spin_2022 <- read.csv("pitch_spin_2022.csv")
spin_2022 <- rename(spin_2022, xMLBAMID = pitcher)
cond_data_2022 <- left_join(cond_data_2022, spin_2022, by="xMLBAMID")
cond_data_2022 <- left_join(cond_data_2022, savant_cond_2022, by = "xMLBAMID")

#2023
spin_2023 <- read.csv("pitch_spin_2023.csv")
spin_2023 <- rename(spin_2023, xMLBAMID = pitcher)
cond_data_2023 <- left_join(cond_data_2023, spin_2023, by="xMLBAMID")
cond_data_2023 <- left_join(cond_data_2023, savant_cond_2023, by = "xMLBAMID")

#combining all three condensed datasets
cond_data = rbind(cond_data_2021, cond_data_2022, cond_data_2023)

# Indicator Variable and Horizontal Movement Changes ----------------------

#Adding indicator variables for each pitch
#Setting the cutoff at 5% usage
cond_data <- cond_data |>
  mutate(ind_fastball = ifelse(is.na(pfx_FA_pct) | pfx_FA_pct < 0.05, "No", "Yes"),
         ind_slider = ifelse(is.na(pfx_SL_pct) | pfx_SL_pct < 0.05, "No", "Yes"),
         ind_cutter = ifelse(is.na(pfx_FC_pct) | pfx_FC_pct < 0.05, "No", "Yes"),
         ind_curve = ifelse(is.na(pfx_CU_pct) | pfx_CU_pct < 0.05, "No", "Yes"),
         ind_change = ifelse(is.na(pfx_CH_pct) | pfx_CH_pct < 0.05, "No", "Yes"),
         ind_split = ifelse(is.na(pfx_FS_pct) | pfx_FS_pct < 0.05, "No", "Yes"),
         ind_sinker = ifelse(is.na(pfx_SI_pct) | pfx_SI_pct < 0.05, "No", "Yes"),
         ind_screw = ifelse(is.na(pfx_SC_pct) | pfx_SC_pct < 0.05, "No", "Yes"),
         ind_fork = ifelse(is.na(pfx_FO_pct) | pfx_FO_pct < 0.05, "No", "Yes"),
         ind_kc = ifelse(is.na(pfx_KC_pct) | pfx_KC_pct < 0.05, "No", "Yes"),
         ind_knuckle = ifelse(is.na(pfx_KN_pct) | pfx_KN_pct < 0.05, "No", "Yes")
  )

#Adjusting the horizontal movement variable so that it is (almost) always positive
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


# Fully excluding the pitches that were thrown <5% of the time ------------

#Before, we had created indicator variables but left the data in
cond_data <- cond_data |>
  mutate(pfx_vFA = ifelse(ind_fastball == "No", NA, pfx_vFA), #Fastball
         `pfx_FA-X` = ifelse(ind_fastball == "No", NA, `pfx_FA-X`),
         `pfx_FA-Z` = ifelse(ind_fastball == "No", NA, `pfx_FA-Z`),
         ff_avg_spin = ifelse(ind_fastball == "No", NA, ff_avg_spin),
         sp_s_FF = ifelse(ind_fastball == "No", NA, sp_s_FF),
         pfx_vSI = ifelse(ind_sinker == "No", NA, pfx_vSI), #sinker
         `pfx_SI-X` = ifelse(ind_sinker == "No", NA, `pfx_SI-X`),
         `pfx_SI-Z` = ifelse(ind_sinker == "No", NA, `pfx_SI-Z`),
         si_avg_spin = ifelse(ind_sinker == "No", NA, si_avg_spin),
         sp_s_SI = ifelse(ind_sinker == "No", NA, sp_s_SI),
         pfx_vFC = ifelse(ind_cutter == "No", NA, pfx_vFC), #cutter
         `pfx_FC-X` = ifelse(ind_cutter == "No", NA, `pfx_FC-X`),
         `pfx_FC-Z` = ifelse(ind_cutter == "No", NA, `pfx_FC-Z`),
         fc_avg_spin = ifelse(ind_cutter == "No", NA, fc_avg_spin),
         sp_s_FC = ifelse(ind_cutter == "No", NA, sp_s_FC),
         pfx_vSL = ifelse(ind_slider == "No", NA, pfx_vSL), #slider
         `pfx_SL-X` = ifelse(ind_slider == "No", NA, `pfx_SL-X`),
         `pfx_SL-Z` = ifelse(ind_slider == "No", NA, `pfx_SL-Z`),
         sl_avg_spin = ifelse(ind_slider == "No", NA, sl_avg_spin),
         sp_s_SL = ifelse(ind_slider == "No", NA, sp_s_SL),
         pfx_vCH = ifelse(ind_change == "No", NA, pfx_vCH), #changeup
         `pfx_CH-X` = ifelse(ind_change == "No", NA, `pfx_CH-X`),
         `pfx_CH-Z` = ifelse(ind_change == "No", NA, `pfx_CH-Z`),
         ch_avg_spin = ifelse(ind_change == "No", NA, ch_avg_spin),
         sp_s_CH = ifelse(ind_change == "No", NA, sp_s_CH),
         pfx_vCU = ifelse(ind_curve == "No", NA, pfx_vCU), #curveball
         `pfx_CU-X` = ifelse(ind_curve == "No", NA, `pfx_CU-X`),
         `pfx_CU-Z` = ifelse(ind_curve == "No", NA, `pfx_CU-Z`),
         cu_avg_spin = ifelse(ind_curve == "No", NA, cu_avg_spin),
         sp_s_CU = ifelse(ind_curve == "No", NA, sp_s_CU),
         #no knuckle curve avg spin
         pfx_vKC = ifelse(ind_kc == "No", NA, pfx_vKC), #knuckle curve
         `pfx_KC-X` = ifelse(ind_kc == "No", NA, `pfx_KC-X`),
         `pfx_KC-Z` = ifelse(ind_kc == "No", NA, `pfx_KC-Z`),
         sp_s_KC = ifelse(ind_kc == "No", NA, sp_s_KC),
         #no knuckle curve stuff+
         pfx_vKN = ifelse(ind_knuckle == "No", NA, pfx_vKN), #knuckleball
         `pfx_KN-X` = ifelse(ind_knuckle == "No", NA, `pfx_KN-X`),
         `pfx_KN-Z` = ifelse(ind_knuckle == "No", NA, `pfx_KN-Z`),
         kn_avg_spin = ifelse(ind_knuckle == "No", NA, kn_avg_spin),
         #no forkball avg spin
         pfx_vFO = ifelse(ind_fork == "No", NA, pfx_vFO), #forkball
         `pfx_FO-X` = ifelse(ind_fork == "No", NA, `pfx_FO-X`),
         `pfx_FO-Z` = ifelse(ind_fork == "No", NA, `pfx_FO-Z`),
         sp_s_FO = ifelse(ind_fork == "No", NA, sp_s_FO),
         pfx_vFS = ifelse(ind_split == "No", NA, pfx_vFS), #splitter
         `pfx_FS-X` = ifelse(ind_split == "No", NA, `pfx_FS-X`),
         `pfx_FS-Z` = ifelse(ind_split == "No", NA, `pfx_FS-Z`),
         fs_avg_spin = ifelse(ind_split == "No", NA, fs_avg_spin),
         sp_s_FS = ifelse(ind_split == "No", NA, sp_s_FS),
         #no screwball avg spin, stuff+
         pfx_vSC = ifelse(ind_screw == "No", NA, pfx_vSC), #screwball
         `pfx_SC-X` = ifelse(ind_screw == "No", NA, `pfx_SC-X`),
         `pfx_SC-Z` = ifelse(ind_screw == "No", NA, `pfx_SC-Z`),)
#only avg spin for sweeper
#only avg spin for slurve


# EDA ---------------------------------------------------------------------

cond_data |> 
  filter(cond_data$ind_fastball == "Yes") |> 
  ggplot(aes(x=pfx_vFA, y=sp_s_FF, color = `K_BB+`))+
  geom_point()

cond_data |> 
  ggplot(aes(x=sp_s_SL))+
  geom_histogram()  

ggplot(cond_data, aes(x=`xFIP-`, colour = ind_curve))+
  geom_density()+
  facet_wrap(vars(ind_curve), nrow=2)

cond_data |>
  filter(ind_slider == "Yes") |>
  ggplot(aes(x = `pfx_SL-X`, y = sp_s_SL)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm") +
  labs(x = "pfx_SL-X", y = "sp_s_SL") +
  ggtitle("Scatterplot of pfx_SL-X vs sp_s_SL (Slider Indicator = Yes)")
#facet_wrap(~ Throws)

cond_data |> 
  ggplot(aes(x=sp_stuff, y=`K_9+`, color = `xFIP-`))+
  geom_point(alpha = 0.7)+
  scale_color_viridis_c()+
  theme_light()+
  xlab("Overall Stuff+")+
  ylab("K/9+")

# Pitchers Who Added or Subtracted a Fastball -----------------------------

# Function to check if fastball indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_fastball <- function(data) {
  fastball <- as.integer(factor(data$ind_fastball, levels = c("No", "Yes")))
  diff_fastball <- diff(fastball)
  
  # Identify rows with changes
  change_indices <- which(diff_fastball != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their fastball indicator
changed_fastball_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_fastball(.)) |>
  select(1:12, ind_fastball)

changed_fastball_pitchers <- changed_fastball_pitchers |> 
  mutate(change_type = case_when(
    ind_fastball == "Yes" & lag(ind_fastball) == "No" ~ "Added",
    ind_fastball == "No" & lag(ind_fastball) == "Yes" ~ "Subtracted"
  ))


# Calculate the difference in 'FIP-' between consecutive years for each pitcher
fip_diff <- changed_fastball_pitchers |>
  filter(IP >= 25) |> 
  arrange(xMLBAMID, Season) |>
  group_by(xMLBAMID, PlayerNameRoute) |>
  mutate(FIP_diff = `FIP-` - lag(`FIP-`)) |>
  filter(!is.na(FIP_diff)) |>
  mutate(PlayerSeason = paste(PlayerNameRoute, ", '", substr(Season, 3, 4), 
                              sep = "")) |> 
  ungroup()

# Create a bar chart to visualize the change in fastball indicator with color coding
ggplot(fip_diff, aes(x = FIP_diff, y = reorder(PlayerSeason, -FIP_diff), 
                     fill = change_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Added" = "blue", 
                               "Subtracted" = "red"),
                    guide = guide_legend(title = "Change")) +
  labs(title = "FIP- Change for Pitchers who Added or Subtracted a Fastball",
       x = "Change in FIP-",
       y = "Player Name, Season") +
  facet_wrap(~ change_type, scales = "free_y")+
  scale_x_reverse(limits = c(70, -80))


#MORE CODE NEEDS TO BE ADDED BEFORE GRAPHING ANYTHING OTHER THAN A FASTBALL

# ------------ Pitchers Who Added or Removed a sinker ----------------------------------
# Function to check if sinker indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_sinker <- function(data) {
  sinker <- as.integer(factor(data$ind_sinker, levels = c("No", "Yes")))
  diff_sinker <- diff(sinker)
  
  # Identify rows with changes
  change_indices <- which(diff_sinker != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their sinker indicator
changed_sinker_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_sinker(.)) |>
  select(1:12, ind_sinker)

changed_sinker_pitchers <- changed_sinker_pitchers |> 
  mutate(change_type = case_when(
    ind_sinker == "Yes" & lag(ind_sinker) == "No" ~ "Added",
    ind_sinker == "No" & lag(ind_sinker) == "Yes" ~ "Subtracted"
  ))


# -------------- Pitchers Who Added or Removed a slider ----------------------------------
# Function to check if slider indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_slider <- function(data) {
  slider <- as.integer(factor(data$ind_slider, levels = c("No", "Yes")))
  diff_slider <- diff(slider)
  
  # Identify rows with changes
  change_indices <- which(diff_slider != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their slider indicator
changed_slider_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_slider(.)) |>
  select(1:12, ind_slider)

changed_slider_pitchers <- changed_slider_pitchers |> 
  mutate(change_type = case_when(
    ind_slider == "Yes" & lag(ind_slider) == "No" ~ "Added",
    ind_slider == "No" & lag(ind_slider) == "Yes" ~ "Subtracted"
  ))


# ------------ Pitchers Who Added or Removed a cutter ----------------------------------
# Function to check if cutter indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_cutter <- function(data) {
  cutter <- as.integer(factor(data$ind_cutter, levels = c("No", "Yes")))
  diff_cutter <- diff(cutter)
  
  # Identify rows with changes
  change_indices <- which(diff_cutter != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their cutter indicator
changed_cutter_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_cutter(.)) |>
  select(1:12, ind_cutter)

changed_cutter_pitchers <- changed_cutter_pitchers |> 
  mutate(change_type = case_when(
    ind_cutter == "Yes" & lag(ind_cutter) == "No" ~ "Added",
    ind_cutter == "No" & lag(ind_cutter) == "Yes" ~ "Subtracted"
  ))


# --------------Pitchers Who Added or Removed a curve ----------------------------------
# Function to check if curve indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_curve <- function(data) {
  curve <- as.integer(factor(data$ind_curve, levels = c("No", "Yes")))
  diff_curve <- diff(curve)
  
  # Identify rows with changes
  change_indices <- which(diff_curve != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their curve indicator
changed_curve_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_curve(.)) |>
  select(1:12, ind_curve)

changed_curve_pitchers <- changed_curve_pitchers |> 
  mutate(change_type = case_when(
    ind_curve == "Yes" & lag(ind_curve) == "No" ~ "Added",
    ind_curve == "No" & lag(ind_curve) == "Yes" ~ "Subtracted"
  ))



# --------------- Pitchers Who Added or Removed a change ----------------------------------
# Function to check if change indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_change <- function(data) {
  change <- as.integer(factor(data$ind_change, levels = c("No", "Yes")))
  diff_change <- diff(change)
  
  # Identify rows with changes
  change_indices <- which(diff_change != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their change indicator
changed_change_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_change(.)) |>
  select(1:12, ind_change)

changed_change_pitchers <- changed_change_pitchers |> 
  mutate(change_type = case_when(
    ind_change == "Yes" & lag(ind_change) == "No" ~ "Added",
    ind_change == "No" & lag(ind_change) == "Yes" ~ "Subtracted"
  ))



# ----------------- Pitchers Who Added or Removed a split ----------------------------------
# Function to check if split indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_split <- function(data) {
  split <- as.integer(factor(data$ind_split, levels = c("No", "Yes")))
  diff_split <- diff(split)
  
  # Identify rows with changes
  change_indices <- which(diff_split != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their split indicator
changed_split_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_split(.)) |>
  select(1:12, ind_split)

changed_split_pitchers <- changed_split_pitchers |> 
  mutate(change_type = case_when(
    ind_split == "Yes" & lag(ind_split) == "No" ~ "Added",
    ind_split == "No" & lag(ind_split) == "Yes" ~ "Subtracted"
  ))


# -------------- Pitchers Who Added or Removed a Knuckle Curve ----------------------------------
# Function to check if kc indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_kc <- function(data) {
  kc <- as.integer(factor(data$ind_kc, levels = c("No", "Yes")))
  diff_kc <- diff(kc)
  
  # Identify rows with changes
  change_indices <- which(diff_kc != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their kc indicator
changed_kc_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_kc(.)) |>
  select(1:12, ind_kc)

changed_kc_pitchers <- changed_kc_pitchers |> 
  mutate(change_type = case_when(
    ind_kc == "Yes" & lag(ind_kc) == "No" ~ "Added",
    ind_kc == "No" & lag(ind_kc) == "Yes" ~ "Subtracted"
  ))


# -------------- Pitchers Who Added or Removed a Screwball ----------------------------------
# Function to check if screw indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_screw <- function(data) {
  screw <- as.integer(factor(data$ind_screw, levels = c("No", "Yes")))
  diff_screw <- diff(screw)
  
  # Identify rows with changes
  change_indices <- which(diff_screw != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their screw indicator
changed_screw_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_screw(.)) |>
  select(1:12, ind_screw)

changed_screw_pitchers <- changed_screw_pitchers |> 
  mutate(change_type = case_when(
    ind_screw == "Yes" & lag(ind_screw) == "No" ~ "Added",
    ind_screw == "No" & lag(ind_screw) == "Yes" ~ "Subtracted"
  ))


# ------------ Pitchers Who Added or Removed a Forkball ----------------------------------
# Function to check if fork indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_fork <- function(data) {
  fork <- as.integer(factor(data$ind_fork, levels = c("No", "Yes")))
  diff_fork <- diff(fork)
  
  # Identify rows with changes
  change_indices <- which(diff_fork != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their fork indicator
changed_fork_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_fork(.)) |>
  select(1:12, ind_fork)

changed_fork_pitchers <- changed_fork_pitchers |> 
  mutate(change_type = case_when(
    ind_fork == "Yes" & lag(ind_fork) == "No" ~ "Added",
    ind_fork == "No" & lag(ind_fork) == "Yes" ~ "Subtracted"
  ))


# -------------- Pitchers Who Added or Removed a Knuckleball ----------------------------------
# Function to check if knuckle indicator has changed from Yes to No
# and exclude consecutive years with the same indicator
has_changed_knuckle <- function(data) {
  knuckle <- as.integer(factor(data$ind_knuckle, levels = c("No", "Yes")))
  diff_knuckle <- diff(knuckle)
  
  # Identify rows with changes
  change_indices <- which(diff_knuckle != 0)
  
  # Include only rows with changes and the year following a change
  change_rows <- sort(unique(c(change_indices, change_indices + 1)))
  
  # Ensure we don't go out of bounds
  change_rows <- change_rows[change_rows <= nrow(data)]
  
  return(data[change_rows, ])
}

# Group data by pitcher and filter for those who have changed their knuckle indicator
changed_knuckle_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_knuckle(.)) |>
  select(1:12, 107)

changed_knuckle_pitchers <- changed_knuckle_pitchers |> 
  mutate(change_type = case_when(
    ind_knuckle == "Yes" & lag(ind_knuckle) == "No" ~ "Added",
    ind_knuckle == "No" & lag(ind_knuckle) == "Yes" ~ "Subtracted"
  ))

# Line Graphs -------------------------------------------------------------

#Attempting to Build a Line Graph: This ISN'T for ADDING and SUBTRACTING
pitch_freq <- cond_data |> 
  group_by(Season) |> 
  summarize(fb_yes = sum(ind_fastball == "Yes", na.rm = TRUE),
            si_yes = sum(ind_sinker == "Yes", na.rm = TRUE),
            fc_yes = sum(ind_cutter == "Yes", na.rm = TRUE),
            sl_yes = sum(ind_slider == "Yes", na.rm = TRUE),
            cb_yes = sum(ind_curve == "Yes", na.rm = TRUE),
            ch_yes = sum(ind_change == "Yes", na.rm = TRUE),
            kc_yes = sum(ind_kc == "Yes", na.rm = TRUE),
            sc_yes = sum(ind_screw == "Yes", na.rm = TRUE),
            fs_yes = sum(ind_split == "Yes", na.rm = TRUE),
            fo_yes = sum(ind_fork == "Yes", na.rm = TRUE),
            kn_yes = sum(ind_knuckle == "Yes", na.rm = TRUE))

# Reshape data to long format
library(tidyr)
pitch_freq_long <- pitch_freq |> 
  pivot_longer(
    cols = -Season,
    names_to = "Pitch_Type",
    values_to = "Count"
  )

# Plot the data
ggplot(pitch_freq_long, aes(x = Season, y = Count, color = Pitch_Type)) +
  geom_point()+
  geom_line(linewidth = 1) +
  labs(
    title = "Number of Pitchers Throwing Each Pitch Type by Season",
    x = "Season",
    y = "Number of Pitchers",
    color = "Pitch Type"
  ) +
  theme_minimal()

#This IS for Adding and Subtracting
fb_change <- changed_fastball_pitchers |> 
  group_by(Season) |> 
  summarize(fb_added = sum(change_type == "Added", na.rm = TRUE),
            fb_sub = sum(change_type == "Subtracted", na.rm = TRUE))
si_change <- changed_sinker_pitchers |> 
  group_by(Season) |> 
  summarize(si_added = sum(change_type == "Added", na.rm = TRUE),
            si_sub = sum(change_type == "Subtracted", na.rm = TRUE))
fc_change <- changed_cutter_pitchers |> 
  group_by(Season) |> 
  summarize(fc_added = sum(change_type == "Added", na.rm = TRUE),
            fc_sub = sum(change_type == "Subtracted", na.rm = TRUE))
sl_change <- changed_slider_pitchers |> 
  group_by(Season) |> 
  summarize(sl_added = sum(change_type == "Added", na.rm = TRUE),
            sl_sub = sum(change_type == "Subtracted", na.rm = TRUE))
ch_change <- changed_change_pitchers |> 
  group_by(Season) |> 
  summarize(ch_added = sum(change_type == "Added", na.rm = TRUE),
            ch_sub = sum(change_type == "Subtracted", na.rm = TRUE))
cu_change <- changed_curve_pitchers |> 
  group_by(Season) |> 
  summarize(cu_added = sum(change_type == "Added", na.rm = TRUE),
            cu_sub = sum(change_type == "Subtracted", na.rm = TRUE))
fs_change <- changed_split_pitchers |> 
  group_by(Season) |> 
  summarize(fs_added = sum(change_type == "Added", na.rm = TRUE),
            fs_sub = sum(change_type == "Subtracted", na.rm = TRUE))
kc_change <- changed_kc_pitchers |> 
  group_by(Season) |> 
  summarize(kc_added = sum(change_type == "Added", na.rm = TRUE),
            kc_sub = sum(change_type == "Subtracted", na.rm = TRUE))
#NO DATA HERE: No one added or removed any of these
fk_change <- changed_fork_pitchers |> 
  group_by(Season) |> 
  summarize(fk_added = sum(change_type == "Added", na.rm = TRUE),
            fk_sub = sum(change_type == "Subtracted", na.rm = TRUE))
kn_change <- changed_knuckle_pitchers |> 
  group_by(Season) |> 
  summarize(kn_added = sum(change_type == "Added", na.rm = TRUE),
            kn_sub = sum(change_type == "Subtracted", na.rm = TRUE))
sc_change <- changed_screw_pitchers |> 
  group_by(Season) |> 
  summarize(sc_added = sum(change_type == "Added", na.rm = TRUE),
            sc_sub = sum(change_type == "Subtracted", na.rm = TRUE))
ovr_change <- bind_cols(fb_change ,si_change, fc_change, sl_change, ch_change,
                        cu_change, fs_change, kc_change) |> 
  select(!c(4, 7, 10, 13, 16, 19, 22)) |> 
  slice(-1) |> 
  rename(Season = Season...1)
#rename columns here
ovr_change <- ovr_change |>
  pivot_longer(
    cols = -Season,
    names_to = "Pitch Change",
    values_to = "Count"
  )
ovr_change <- ovr_change |> 
  mutate(Pitch_Class = case_when(
    substr(`Pitch Change`, 1, 2) == "fb" ~ "Fastball",
    substr(`Pitch Change`, 1, 2) == "si" ~ "Sinker",
    substr(`Pitch Change`, 1, 2) == "fc" ~ "Cutter",
    substr(`Pitch Change`, 1, 2) == "sl" ~ "Slider",
    substr(`Pitch Change`, 1, 2) == "ch" ~ "Changeup",
    substr(`Pitch Change`, 1, 2) == "cu" ~ "Curveball",
    substr(`Pitch Change`, 1, 2) == "fs" ~ "Splitter",
    substr(`Pitch Change`, 1, 2) == "kc" ~ "Knuckle Curve",
    TRUE ~ "Other"
  ))

pitch_order <- c("Fastball", "Sinker", "Cutter", "Slider", "Changeup", 
                 "Curveball", "Splitter", "Knuckle Curve")

ovr_change <- ovr_change |>
  mutate(Pitch_Class = factor(Pitch_Class, levels = pitch_order))

# Plot the data
ggplot(ovr_change, aes(x = Season, y = Count, color = `Pitch Change`)) +
  geom_point(shape= 19)+
  geom_line(linewidth = 1) +
  labs(
    title = "Number of Pitchers Throwing Each Pitch Type by Season",
    x = "Season",
    y = "Number of Pitchers",
    color = "Pitch Type"
  ) +
  scale_x_continuous(breaks = c(2022, 2023))+
  scale_color_manual(
    values = c("fb_added" = "black", "fb_sub" = "gray70",
               "si_added" = "blue", "si_sub" = "skyblue",
               "fc_added" = "chartreuse4", "fc_sub" = "chartreuse",
               "sl_added" = "darkorange3", "sl_sub" = "orange",
               "ch_added" = "goldenrod4", "ch_sub" = "lightgoldenrod",
               "cu_added" = "red", "cu_sub" = "pink",
               "fs_added" = "mediumpurple3", "fs_sub" = "plum3",
               "kc_added" = "tan4", "kc_sub" = "tan"),
    labels = c("fb_added" = "Fastball Added", "fb_sub" = "Fastball Subtracted",
               "si_added" = "Sinker Added","si_sub" = "Sinker Subtracted",
               "fc_added" = "Cutter Added", "fc_sub" = "Cutter Subtracted",
               "sl_added" = "Slider Added", "sl_sub" = "Slider Subtracted",
               "ch_added" = "Changeup Added","ch_sub" = "Changeup Subtracted",
               "cu_added" = "Curveball Added", "cu_sub" = "Curveball Subtracted",
               "fs_added" = "Splitter Added", "fs_sub" = "Splitter Subtracted",
               "kc_added" = "Knuckle Curve Added", "kc_sub" = "Knuckle Curve Subtracted")
  )+
  facet_wrap(~ Pitch_Class)+
  theme_minimal()


# Decision Tree -----------------------------------------------------------

set.seed(4)
train <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_change == "Yes") |> 
  select(pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, sp_s_CH, sp_s_FF) |> 
  drop_na() |> 
  slice_sample(prop = 0.5) 

test <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_change == "Yes") |> 
  select(pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, sp_s_CH, sp_s_FF) |> 
  anti_join(train, by = c("pfx_vCH", "pfx_CH-X", "pfx_CH-Z", "sp_s_CH", "sp_s_FF")) |> 
  drop_na()

if(anyNA(train) | anyNA(test)) {
  stop("There are missing values in the training or testing data.")
}

hr_tree <- train(sp_s_FF ~ ., 
                 data = train, 
                 method = "rpart", 
                 tuneLength = 20,
                 trControl = trainControl(method = "cv", number = 10))


# Load required packages
library(dplyr)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Prepare the data
train <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_change == "Yes") |> 
  select(pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, sp_s_CH, sp_s_FF) |> 
  slice_sample(prop = 0.5) |> 
  drop_na()

# Ensure no duplicate columns or issues with anti_join by creating a unique identifier
train <- train |> 
  mutate(id = row_number())

# Prepare testing data
test <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_change == "Yes") |>
  select(pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, sp_s_CH, sp_s_FF) |> 
  drop_na() |> 
  mutate(id = row_number())

# Exclude training data from testing data
test <- test |> 
  anti_join(train, by = "id") |> 
  select(-id)

# Check dimensions to ensure there is no mismatch
print(dim(train))
print(dim(test))

# Ensure no missing values in training and testing data
if(anyNA(train) | anyNA(test)) {
  stop("There are missing values in the training or testing data.")
}

# Train decision tree model
set.seed(1)
hr_tree <- train(sp_s_FF ~ ., 
                 data = train, 
                 method = "rpart", 
                 tuneLength = 20,
                 trControl = trainControl(method = "cv", number = 10))

# Print model summary
print(hr_tree)


# Simple Linear Regression ------------------------------------------------
#Going step-by-step like the slides
#Interested in modeling a pitcher's Fastball Stuff+
cond_data |>
  filter(ind_fastball == "Yes") |> 
  ggplot(aes(x = sp_s_FF)) +
  geom_histogram(color = "black", fill = "gray")

#Relationship Between Sinker Velocity and Fastball Stuff
si_data <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_sinker == "Yes") |> 
  select(season, PlayerNameRoute, pfx_vSI, sp_s_FF, si_avg_spin, `pfx_SI-X`, 
         `pfx_SI-Z`, sp_s_SI, avg_release_extension, avg_rp_x, avg_rp_z) |> 
  drop_na()

plot_si <- si_data |>
  ggplot(aes(x = pfx_vSI, y = sp_s_FF)) +
  geom_point(size = 3, alpha = 0.5)
plot_si

simple_lm <- lm(sp_s_FF ~ pfx_vSI, 
                data = si_data) 
summary(simple_lm) #or use tidy() or glance()

train_preds <- predict(simple_lm)
head(train_preds)

si_data <- si_data |>
  mutate(pred_vals = train_preds) 

si_data |>
  mutate(pred_vals = predict(simple_lm)) |> 
  ggplot(aes(x = pred_vals, y = sp_s_FF)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed",
              color = "red",
              linewidth = 2)
si_data <- simple_lm |> 
  augment(si_data)

si_data |>
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.5, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "red", linewidth = 2) +
  # plot the residual mean
  geom_smooth(se = FALSE)
#Multiple Linear Regression with Sinker Characteristics (plus release data)
si_data <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, sp_s_FF, si_avg_spin, 
         `pfx_SI-X`, `pfx_SI-Z`, sp_s_SI, avg_release_extension, avg_rp_x, avg_rp_z)

si_lm <- lm(sp_s_FF ~ pfx_vSI + si_avg_spin + `pfx_SI-X` + `pfx_SI-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = si_data)
summary(si_lm)

si_data <- si_data |> 
  mutate(pred_vals = predict(si_lm, newdata = si_data))

si_data |>
  ggplot(aes(x = pred_vals, y = sp_s_FF)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed",
              color = "red",
              linewidth = 2)

si_filtered <- si_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(si_filtered$sp_s_FF, si_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with SIGNIFICANT Sinker Characteristics (plus release data)
si_lm <- lm(sp_s_FF ~ pfx_vSI + si_avg_spin + `pfx_SI-Z` + 
              avg_release_extension + avg_rp_z, data = si_data)
summary(si_lm)

si_data <- si_data |> 
  mutate(pred_vals = predict(si_lm, newdata = si_data))

si_filtered <- si_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(si_filtered$sp_s_FF, si_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

# #Attempting Multiple Linear Regression with Sinker Velo and Slider Velo
# cond_data_b <- cond_data |> 
#   filter(ind_fastball == "Yes") |> 
#   filter(ind_sinker == "Yes" | ind_slider == "Yes") |> 
#   select(pfx_vSI, sp_s_FF, pfx_vSL)
# 
# multiple_lm2 <- lm(sp_s_FF ~ pfx_vSI + pfx_vSL, data=cond_data_b)
# summary(multiple_lm2)
# train_preds2 <- predict(multiple_lm2)
# head(train_preds2)
# cond_data_b <- cond_data_b |>
#   mutate(pred_vals = train_preds2)
# 
# #Multiple Linear Regression with the speed of every pitch: NOT WORKING
# cond_data_c <- cond_data |> 
#   filter(ind_fastball == "Yes") |> 
#   filter(ind_sinker == "Yes" | ind_slider == "Yes" | ind_change == "Yes" | 
#            ind_curve == "Yes" | ind_cutter == "Yes" | ind_split == "Yes" |
#            ind_screw == "Yes" | ind_kc == "Yes" | ind_knuckle =="Yes" | 
#            ind_fork == "Yes") |> 
#   select(sp_s_FF, pfx_vSL, pfx_vSI, pfx_vCH, pfx_vCU, pfx_vFC, pfx_vFS, pfx_vSC, 
#          pfx_vKC, pfx_vKN, pfx_vFO)
# multiple_lm3 <- lm(sp_s_FF ~ pfx_vSL + pfx_vSI + pfx_vCH + pfx_vCU + pfx_vFC + 
#                    pfx_vFS + pfx_vKC,
#                    data = cond_data_c)
# #Error comes from: screwball, knuckleball, forkball
# summary(multiple_lm3)

#Since combining pitches isn't working, temporarily pivoting to looking at every
#trait for each pitch (Sinker is above)

#Multiple Linear Regression with Cutter Characteristics (plus release data)
fc_data <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, sp_s_FF, fc_avg_spin, 
         `pfx_FC-X`, `pfx_FC-Z`, sp_s_FC, avg_release_extension, avg_rp_x, avg_rp_z)

fc_lm <- lm(sp_s_FF ~ pfx_vFC + fc_avg_spin + `pfx_FC-X` + `pfx_FC-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = fc_data)
summary(fc_lm)

fc_data <- fc_data |> 
  mutate(pred_vals = predict(fc_lm, newdata = fc_data))

fc_filtered <- fc_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(fc_filtered$sp_s_FF, fc_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with SIGNIFICANT Cutter Characteristics (plus release data)
fc_lm <- lm(sp_s_FF ~ pfx_vFC + fc_avg_spin + `pfx_FC-Z` + 
              avg_release_extension + avg_rp_z, data = fc_data)
summary(fc_lm)

fc_data <- fc_data |> 
  mutate(pred_vals = predict(fc_lm, newdata = fc_data))

fc_filtered <- fc_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(fc_filtered$sp_s_FF, fc_filtered$pred_vals)
print(paste("RMSE:", rmse_value))


#Multiple Linear Regression with Slider Characteristics (plus release data)
#Don't forget spin problem
sl_data <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  select(pfx_vSL, sp_s_FF, sl_avg_spin, `pfx_SL-X`, `pfx_SL-Z`, sp_s_SL, 
         avg_release_extension, avg_rp_x, avg_rp_z)

sl_lm <- lm(sp_s_FF ~ pfx_vSL + sl_avg_spin + `pfx_SL-X` + `pfx_SL-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = sl_data)
summary(sl_lm)

sl_data <- sl_data |> 
  mutate(pred_vals = predict(sl_lm, newdata = sl_data))

sl_filtered <- sl_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(sl_filtered$sp_s_FF, sl_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with SIGNIFICANT Slider Characteristics (plus release data)
#Don't forget spin problem
sl_lm <- lm(sp_s_FF ~ pfx_vSL + sl_avg_spin + `pfx_SL-X` + `pfx_SL-Z` + 
              avg_release_extension + avg_rp_z, data = sl_data)
summary(sl_lm)

sl_data <- sl_data |> 
  mutate(pred_vals = predict(sl_lm, newdata = sl_data))

sl_filtered <- sl_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(sl_filtered$sp_s_FF, sl_filtered$pred_vals)
print(paste("RMSE:", rmse_value))


#Multiple Linear Regression with Curveball Characteristics (plus release data)
cu_data <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  select(pfx_vCU, sp_s_FF, cu_avg_spin, `pfx_CU-X`, `pfx_CU-Z`, sp_s_CU, 
         avg_release_extension, avg_rp_x, avg_rp_z)
cu_lm <- lm(sp_s_FF ~ pfx_vCU + cu_avg_spin + `pfx_CU-X` + `pfx_CU-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = cu_data)
summary(cu_lm)

cu_data <- cu_data |> 
  mutate(pred_vals = predict(cu_lm, newdata = cu_data))

cu_filtered <- cu_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(cu_filtered$sp_s_FF, cu_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with SIGNIFICANT Curveball Characteristics (plus release data)
cu_lm <- lm(sp_s_FF ~ pfx_vCU + cu_avg_spin + `pfx_CU-Z` + avg_release_extension, 
            data = cu_data)
summary(cu_lm)

cu_data <- cu_data |> 
  mutate(pred_vals = predict(cu_lm, newdata = cu_data))

cu_filtered <- cu_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(cu_filtered$sp_s_FF, cu_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with Changeup Characteristics (plus release data)
ch_data <- cond_data |> 
  filter(ind_change == "Yes") |> 
  select(pfx_vCH, sp_s_FF, ch_avg_spin, `pfx_CH-X`, `pfx_CH-Z`, sp_s_CH, 
         avg_release_extension, avg_rp_x, avg_rp_z)
ch_lm <- lm(sp_s_FF ~ pfx_vCH + ch_avg_spin + `pfx_CH-X` + `pfx_CH-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = ch_data)
summary(ch_lm)

ch_data <- ch_data |> 
  mutate(pred_vals = predict(ch_lm, newdata = ch_data))

ch_filtered <- ch_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(ch_filtered$sp_s_FF, ch_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with SIGNIFICANT Changeup Characteristics (plus release data)
ch_lm <- lm(sp_s_FF ~ pfx_vCH + ch_avg_spin + `pfx_CH-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = ch_data)
summary(ch_lm)

ch_data <- ch_data |> 
  mutate(pred_vals = predict(ch_lm, newdata = ch_data))

ch_filtered <- ch_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(ch_filtered$sp_s_FF, ch_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with Knuckle Curve Characteristics (plus release data)
#Error with the spin: Statcast doesn't give knuckle curve spin and instead includes 
# it with curveballs
kc_data <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  select(pfx_vKC, sp_s_FF, `pfx_KC-X`, `pfx_KC-Z`, sp_s_KC, 
         avg_release_extension, avg_rp_x, avg_rp_z)
kc_lm <- lm(sp_s_FF ~ pfx_vKC + `pfx_KC-X` + `pfx_KC-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = kc_data)
summary(kc_lm)

kc_data <- kc_data |> 
  mutate(pred_vals = predict(kc_lm, newdata = kc_data))

kc_filtered <- kc_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(kc_filtered$sp_s_FF, kc_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with SIGNIFICANT Knuckle Curve Characteristics 
#(plus release data)
#Error with the spin: Statcast doesn't give knuckle curve spin and instead includes 
# it with curveballs
kc_lm <- lm(sp_s_FF ~ pfx_vKC + `pfx_KC-Z`, data = kc_data)
summary(kc_lm)

kc_data <- kc_data |> 
  mutate(pred_vals = predict(kc_lm, newdata = kc_data))

kc_filtered <- kc_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(kc_filtered$sp_s_FF, kc_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with Splitter Characteristics (plus release data)
fs_data <- cond_data |> 
  filter(ind_split == "Yes") |> 
  select(pfx_vFS, sp_s_FF, fs_avg_spin, `pfx_FS-X`, `pfx_FS-Z`, sp_s_FS, 
         avg_release_extension, avg_rp_x, avg_rp_z)
fs_lm <- lm(sp_s_FF ~ pfx_vFS + fs_avg_spin + `pfx_FS-X` + `pfx_FS-Z` + 
              avg_release_extension + avg_rp_x + avg_rp_z, data = fs_data)
summary(fs_lm)

fs_data <- fs_data |> 
  mutate(pred_vals = predict(fs_lm, newdata = fs_data))

fs_filtered <- fs_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(fs_filtered$sp_s_FF, fs_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Multiple Linear Regression with SIGNIFICANT Splitter Characteristics (plus release data)
fs_lm <- lm(sp_s_FF ~ pfx_vFS + `pfx_FS-Z` + avg_release_extension, data = fs_data)
summary(fs_lm)

fs_data <- fs_data |> 
  mutate(pred_vals = predict(fs_lm, newdata = fs_data))

fs_filtered <- fs_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(fs_filtered$sp_s_FF, fs_filtered$pred_vals)
print(paste("RMSE:", rmse_value))

#Sinker Stuff+ experiment
si_lm <- lm(sp_s_FF ~ sp_s_SI, data = si_data)
summary(si_lm)

si_data <- si_data |> 
  mutate(pred_vals = predict(si_lm, newdata = si_data))

si_filtered <- si_data |>
  filter(!is.na(sp_s_FF) & !is.na(pred_vals))

rmse_value <- rmse(si_filtered$sp_s_FF, si_filtered$pred_vals)
print(paste("RMSE:", rmse_value))
#Adjusted R^2 and RMSE are not better than the other sinker models

#Experiment for going across pitches

#Before, we had created indicator variables but left the data in
cond_data_exp <- cond_data |>
  mutate(ind_fastball = ifelse(is.na(pfx_FA_pct) | pfx_FA_pct < 0.05, 0, 1),
         ind_slider = ifelse(is.na(pfx_SL_pct) | pfx_SL_pct < 0.05, 0, 1),
         ind_cutter = ifelse(is.na(pfx_FC_pct) | pfx_FC_pct < 0.05, 0, 1),
         ind_curve = ifelse(is.na(pfx_CU_pct) | pfx_CU_pct < 0.05, 0, 1),
         ind_change = ifelse(is.na(pfx_CH_pct) | pfx_CH_pct < 0.05, 0, 1),
         ind_split = ifelse(is.na(pfx_FS_pct) | pfx_FS_pct < 0.05, 0, 1),
         ind_sinker = ifelse(is.na(pfx_SI_pct) | pfx_SI_pct < 0.05, 0, 1),
         ind_screw = ifelse(is.na(pfx_SC_pct) | pfx_SC_pct < 0.05, 0, 1),
         ind_fork = ifelse(is.na(pfx_FO_pct) | pfx_FO_pct < 0.05, 0, 1),
         ind_kc = ifelse(is.na(pfx_KC_pct) | pfx_KC_pct < 0.05, 0, 1),
         ind_knuckle = ifelse(is.na(pfx_KN_pct) | pfx_KN_pct < 0.05, 0, 1)
  )
cond_data_exp <- cond_data_exp |>
  mutate(pfx_vFA = ifelse(ind_fastball == 0, 0, pfx_vFA), #Fastball
         `pfx_FA-X` = ifelse(ind_fastball == 0, 0, `pfx_FA-X`),
         `pfx_FA-Z` = ifelse(ind_fastball == 0, 0, `pfx_FA-Z`),
         ff_avg_spin = ifelse(ind_fastball == 0, 0, ff_avg_spin),
         sp_s_FF = ifelse(ind_fastball == 0, 0, sp_s_FF),
         pfx_vSI = ifelse(ind_sinker == 0, 0, pfx_vSI), #sinker
         `pfx_SI-X` = ifelse(ind_sinker == 0, 0, `pfx_SI-X`),
         `pfx_SI-Z` = ifelse(ind_sinker == 0, 0, `pfx_SI-Z`),
         si_avg_spin = ifelse(ind_sinker == 0, 0, si_avg_spin),
         sp_s_SI = ifelse(ind_sinker == 0, 0, sp_s_SI),
         pfx_vFC = ifelse(ind_cutter == 0, 0, pfx_vFC), #cutter
         `pfx_FC-X` = ifelse(ind_cutter == 0, 0, `pfx_FC-X`),
         `pfx_FC-Z` = ifelse(ind_cutter == 0, 0, `pfx_FC-Z`),
         fc_avg_spin = ifelse(ind_cutter == 0, 0, fc_avg_spin),
         sp_s_FC = ifelse(ind_cutter == 0, 0, sp_s_FC),
         pfx_vSL = ifelse(ind_slider == 0, 0, pfx_vSL), #slider
         `pfx_SL-X` = ifelse(ind_slider == 0, 0, `pfx_SL-X`),
         `pfx_SL-Z` = ifelse(ind_slider == 0, 0, `pfx_SL-Z`),
         sl_avg_spin = ifelse(ind_slider == 0, 0, sl_avg_spin),
         sp_s_SL = ifelse(ind_slider == 0, 0, sp_s_SL),
         pfx_vCH = ifelse(ind_change == 0, 0, pfx_vCH), #changeup
         `pfx_CH-X` = ifelse(ind_change == 0, 0, `pfx_CH-X`),
         `pfx_CH-Z` = ifelse(ind_change == 0, 0, `pfx_CH-Z`),
         ch_avg_spin = ifelse(ind_change == 0, 0, ch_avg_spin),
         sp_s_CH = ifelse(ind_change == 0, 0, sp_s_CH),
         pfx_vCU = ifelse(ind_curve == 0, 0, pfx_vCU), #curveball
         `pfx_CU-X` = ifelse(ind_curve == 0, 0, `pfx_CU-X`),
         `pfx_CU-Z` = ifelse(ind_curve == 0, 0, `pfx_CU-Z`),
         cu_avg_spin = ifelse(ind_curve == 0, 0, cu_avg_spin),
         sp_s_CU = ifelse(ind_curve == 0, 0, sp_s_CU),
         #no knuckle curve avg spin
         pfx_vKC = ifelse(ind_kc == 0, 0, pfx_vKC), #knuckle curve
         `pfx_KC-X` = ifelse(ind_kc == 0, 0, `pfx_KC-X`),
         `pfx_KC-Z` = ifelse(ind_kc == 0, 0, `pfx_KC-Z`),
         sp_s_KC = ifelse(ind_kc == 0, 0, sp_s_KC),
         #no knuckle curve stuff+
         pfx_vKN = ifelse(ind_knuckle == 0, 0, pfx_vKN), #knuckleball
         `pfx_KN-X` = ifelse(ind_knuckle == 0, 0, `pfx_KN-X`),
         `pfx_KN-Z` = ifelse(ind_knuckle == 0, 0, `pfx_KN-Z`),
         kn_avg_spin = ifelse(ind_knuckle == 0, 0, kn_avg_spin),
         #no forkball avg spin
         pfx_vFO = ifelse(ind_fork == 0, 0, pfx_vFO), #forkball
         `pfx_FO-X` = ifelse(ind_fork == 0, 0, `pfx_FO-X`),
         `pfx_FO-Z` = ifelse(ind_fork == 0, 0, `pfx_FO-Z`),
         sp_s_FO = ifelse(ind_fork == 0, 0, sp_s_FO),
         pfx_vFS = ifelse(ind_split == 0, 0, pfx_vFS), #splitter
         `pfx_FS-X` = ifelse(ind_split == 0, 0, `pfx_FS-X`),
         `pfx_FS-Z` = ifelse(ind_split == 0, 0, `pfx_FS-Z`),
         fs_avg_spin = ifelse(ind_split == 0, 0, fs_avg_spin),
         sp_s_FS = ifelse(ind_split == 0, 0, sp_s_FS),
         #no screwball avg spin, stuff+
         pfx_vSC = ifelse(ind_screw == 0, 0, pfx_vSC), #screwball
         `pfx_SC-X` = ifelse(ind_screw == 0, 0, `pfx_SC-X`),
         `pfx_SC-Z` = ifelse(ind_screw == 0, 0, `pfx_SC-Z`),)
#only avg spin for sweeper
#only avg spin for slurve

#Attempting Multiple Linear Regression with Sinker Velo and Slider Velo
cond_data_exp <- cond_data_exp |>
  filter(ind_fastball == 1) |> 
  mutate(si_interaction = pfx_vSI * ind_sinker)

exp_lm <- lm(sp_s_FF ~ (pfx_vSI * ind_sinker) + (pfx_vSL * ind_slider), 
             data=cond_data_exp)
summary(exp_lm)
train_preds2 <- predict(multiple_lm2)
head(train_preds2)
cond_data_b <- cond_data_b |>
  mutate(pred_vals = train_preds2)



#Linear Regression with Cond_Data_Longer - Sinker Velo and Fastball Stuff+
# Filter for sinker velocity
sinker_velo <- cond_data_velo |>
  filter(Pitch_Type == "Sinker") |>
  select(Season, PlayerNameRoute, xMLBAMID, Velocity) |>
  rename(Sinker_Velocity = Velocity)

# Filter for fastball stuff plus
fastball_stuff_plus <- cond_data_stuff_plus |>
  filter(Pitch_Type == "Fastball") |>
  select(Season, PlayerNameRoute, xMLBAMID, Stuff_Plus) |>
  rename(Fastball_Stuff_Plus = Stuff_Plus)

# Merge the tables on Season, PlayerNameRoute, and xMLBAMID
merged_data <- sinker_velo |>
  left_join(fastball_stuff_plus, by = c("Season", "PlayerNameRoute", "xMLBAMID"))

# Run the linear regression
simple_lm <- lm(Fastball_Stuff_Plus ~ Sinker_Velocity, data = merged_data)

# Summary of the linear model
summary(simple_lm)

#Multiple Linear Regression with all Velocities and Fastball Stuff+
all_velo <- cond_data_velo |> 
  left_join(fastball_stuff_plus, by = c("Season", "PlayerNameRoute", "xMLBAMID"))
all_velo_lm <- lm(Fastball_Stuff_Plus ~ Velocity, data = all_velo)
summary(all_velo_lm)


# Lasso Modeling: Fastball Stuff+ ----------------------------------------------------------
# Sinker ---> Fastball Stuff+
si_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF) |> 
  drop_na()
# predictors
model_x <- si_to_ff |> 
  select(pfx_vSI:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_ff |> 
  pull(sp_s_FF)
#Lasso Model
si_to_ff_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(si_to_ff_lasso_cv)
tidy_lasso_coef <- tidy(si_to_ff_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = si_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(si_to_ff_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = si_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_ff_lasso_cv$lambda.1se,
)

# Predict on the training data
predictions <- predict(lasso_final, newx = model_x)

# Calculate RMSE
rmse <- sqrt(mean((model_y - predictions)^2))
rmse

observed_vs_predicted <- tibble(
  Observed = model_y,
  Predicted = as.vector(predictions)
)

observed_vs_predicted |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Predicted vs. Observed Values",
       x = "Predicted Values",
       y = "Observed Values")

lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Cutter ---> Fastball Stuff+
fc_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`, 
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF) |> 
  drop_na()
# predictors
model_x <- fc_to_ff |> 
  select(pfx_vFC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fc_to_ff |> 
  pull(sp_s_FF)
#Lasso Model
fc_to_ff_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fc_to_ff_lasso_cv)
tidy_lasso_coef <- tidy(fc_to_ff_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fc_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fc_to_ff_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fc_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fc_to_ff_lasso_cv$lambda.1se,
)
# Predict on the training data
predictions <- predict(lasso_final, newx = model_x)

# Calculate RMSE
rmse <- sqrt(mean((model_y - predictions)^2))
rmse

observed_vs_predicted <- tibble(
  Observed = model_y,
  Predicted = as.vector(predictions)
)

observed_vs_predicted |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Predicted vs. Observed Values",
       x = "Predicted Values",
       y = "Observed Values")
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Slider ---> Fastball Stuff+
sl_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`, 
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF) |> 
  drop_na()
# predictors
model_x <- sl_to_ff |> 
  select(pfx_vSL:avg_rp_z) |> 
  as.matrix()
# response
model_y <- sl_to_ff |> 
  pull(sp_s_FF)
#Lasso Model
sl_to_ff_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(sl_to_ff_lasso_cv)
tidy_lasso_coef <- tidy(sl_to_ff_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = sl_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(sl_to_ff_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = sl_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = sl_to_ff_lasso_cv$lambda.1se,
)
# Predict on the training data
predictions <- predict(lasso_final, newx = model_x)

# Calculate RMSE
rmse <- sqrt(mean((model_y - predictions)^2))
rmse

observed_vs_predicted <- tibble(
  Observed = model_y,
  Predicted = as.vector(predictions)
)

observed_vs_predicted |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Predicted vs. Observed Values",
       x = "Predicted Values",
       y = "Observed Values")

lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Curveball ---> Fastball Stuff+
cu_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`, 
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF) |> 
  drop_na()
# predictors
model_x <- cu_to_ff |> 
  select(pfx_vCU:avg_rp_z) |> 
  as.matrix()
# response
model_y <- cu_to_ff |> 
  pull(sp_s_FF)
#Lasso Model
cu_to_ff_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(cu_to_ff_lasso_cv)
tidy_lasso_coef <- tidy(cu_to_ff_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = cu_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(cu_to_ff_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cu_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = cu_to_ff_lasso_cv$lambda.1se,
)
# Predict on the training data
predictions <- predict(lasso_final, newx = model_x)

# Calculate RMSE
rmse <- sqrt(mean((model_y - predictions)^2))
rmse

observed_vs_predicted <- tibble(
  Observed = model_y,
  Predicted = as.vector(predictions)
)

observed_vs_predicted |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Predicted vs. Observed Values",
       x = "Predicted Values",
       y = "Observed Values")
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Changeup ---> Fastball Stuff+
ch_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_change == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, 
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF) |> 
  drop_na()
# predictors
model_x <- ch_to_ff |> 
  select(pfx_vCH:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ch_to_ff |> 
  pull(sp_s_FF)
#Lasso Model
ch_to_ff_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ch_to_ff_lasso_cv)
tidy_lasso_coef <- tidy(ch_to_ff_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ch_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ch_to_ff_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ch_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ch_to_ff_lasso_cv$lambda.1se,
)
# Predict on the training data
predictions <- predict(lasso_final, newx = model_x)

# Calculate RMSE
rmse <- sqrt(mean((model_y - predictions)^2))
rmse

observed_vs_predicted <- tibble(
  Observed = model_y,
  Predicted = as.vector(predictions)
)

observed_vs_predicted |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Predicted vs. Observed Values",
       x = "Predicted Values",
       y = "Observed Values")
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Splitter ---> Fastball Stuff+
fs_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`, 
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF) |> 
  drop_na()
# predictors
model_x <- fs_to_ff |> 
  select(pfx_vFS:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fs_to_ff |> 
  pull(sp_s_FF)
#Lasso Model
fs_to_ff_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fs_to_ff_lasso_cv)
tidy_lasso_coef <- tidy(fs_to_ff_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fs_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fs_to_ff_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fs_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fs_to_ff_lasso_cv$lambda.1se,
)
# Predict on the training data
predictions <- predict(lasso_final, newx = model_x)

# Calculate RMSE
rmse <- sqrt(mean((model_y - predictions)^2))
rmse

observed_vs_predicted <- tibble(
  Observed = model_y,
  Predicted = as.vector(predictions)
)

observed_vs_predicted |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Predicted vs. Observed Values",
       x = "Predicted Values",
       y = "Observed Values")
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Knuckle Curve ---> Fastball Stuff+
kc_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF) |> 
  drop_na()
# predictors
model_x <- kc_to_ff |> 
  select(pfx_vKC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- kc_to_ff |> 
  pull(sp_s_FF)
#Lasso Model
kc_to_ff_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(kc_to_ff_lasso_cv)
tidy_lasso_coef <- tidy(kc_to_ff_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = kc_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(kc_to_ff_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = kc_to_ff_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_ff_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = kc_to_ff_lasso_cv$lambda.1se,
)
# Predict on the training data
predictions <- predict(lasso_final, newx = model_x)

# Calculate RMSE
rmse <- sqrt(mean((model_y - predictions)^2))
rmse

observed_vs_predicted <- tibble(
  Observed = model_y,
  Predicted = as.vector(predictions)
)

observed_vs_predicted |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Predicted vs. Observed Values",
       x = "Predicted Values",
       y = "Observed Values")
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)


# Lasso Modeling: Sinker Stuff+ ----------------------------------------------------------
# Fastball ---> Sinker Stuff+
ff_to_si <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`, 
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI) |> 
  drop_na()
# predictors
model_x <- ff_to_si |> 
  select(pfx_vFA:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ff_to_si |> 
  pull(sp_s_SI)

k_folds <- 5
# Create the folds
set.seed(4)
folds <- createFolds(model_y, k = k_folds, list = TRUE, returnTrain = TRUE)

# Initialize a vector to store the RMSE for each fold
rmse_values <- numeric(k_folds)

# Perform nested cross-validation
for (i in seq_along(folds)) {
  # Split the data into training and test sets
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(model_x)), train_indices)
  
  x_train <- model_x[train_indices, ]
  y_train <- model_y[train_indices]
  x_test <- model_x[test_indices, ]
  y_test <- model_y[test_indices]
  
  # Perform cross-validation on the training set to find the optimal lambda
  lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
  
  # Fit the lasso model on the training set using the optimal lambda
  lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_cv$lambda.1se)
  
  # Predict the response for the test set
  predictions <- predict(lasso_model, newx = x_test)
  
  # Calculate the RMSE for the test set
  rmse_values[i] <- sqrt(mean((y_test - predictions)^2))
}
rmse_table <- tibble(
  Lasso = rmse_values
)
# Calculate the average RMSE across all folds
average_rmse <- mean(rmse_values)
average_rmse

# Cutter ---> Sinker Stuff+
fc_to_si <- cond_data |> 
  filter(ind_cutter == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`, 
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI) |> 
  drop_na()
# predictors
model_x <- fc_to_si |> 
  select(pfx_vFC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fc_to_si |> 
  pull(sp_s_SI)
#Lasso Model
fc_to_si_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fc_to_si_lasso_cv)
tidy_lasso_coef <- tidy(fc_to_si_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fc_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fc_to_si_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fc_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fc_to_si_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Slider ---> Sinker Stuff+
sl_to_si <- cond_data |> 
  filter(ind_slider == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`, 
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI) |> 
  drop_na()
# predictors
model_x <- sl_to_si |> 
  select(pfx_vSL:avg_rp_z) |> 
  as.matrix()
# response
model_y <- sl_to_si |> 
  pull(sp_s_SI)
#Lasso Model
sl_to_si_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(sl_to_si_lasso_cv)
tidy_lasso_coef <- tidy(sl_to_si_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = sl_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(sl_to_si_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = sl_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coeslicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = sl_to_si_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Curveball ---> Sinker Stuff+
cu_to_si <- cond_data |> 
  filter(ind_curve == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`, 
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI) |> 
  drop_na()
# predictors
model_x <- cu_to_si |> 
  select(pfx_vCU:avg_rp_z) |> 
  as.matrix()
# response
model_y <- cu_to_si |> 
  pull(sp_s_CU)
#Lasso Model
cu_to_si_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(cu_to_si_lasso_cv)
tidy_lasso_coef <- tidy(cu_to_si_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = cu_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(cu_to_si_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cu_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = cu_to_si_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)


# Changeup ---> Sinker Stuff+
ch_to_si <- cond_data |> 
  filter(ind_change == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, 
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI) |> 
  drop_na()
# predictors
model_x <- ch_to_si |> 
  select(pfx_vCH:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ch_to_si |> 
  pull(sp_s_SI)
#Lasso Model
ch_to_si_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ch_to_si_lasso_cv)
tidy_lasso_coef <- tidy(ch_to_si_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ch_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ch_to_si_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ch_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coechicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ch_to_si_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Splitter ---> Sinker Stuff+
fs_to_si <- cond_data |> 
  filter(ind_split == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`, 
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI) |> 
  drop_na()
# predictors
model_x <- fs_to_si |> 
  select(pfx_vFS:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fs_to_si |> 
  pull(sp_s_SI)
#Lasso Model
fs_to_si_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fs_to_si_lasso_cv)
tidy_lasso_coef <- tidy(fs_to_si_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fs_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fs_to_si_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fs_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefsicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fs_to_si_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Knuckle Curve ---> Sinker Stuff+
kc_to_si <- cond_data |> 
  filter(ind_kc == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI) |> 
  drop_na()
# predictors
model_x <- kc_to_si |> 
  select(pfx_vKC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- kc_to_si |> 
  pull(sp_s_SI)
#Lasso Model
kc_to_si_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(kc_to_si_lasso_cv)
tidy_lasso_coef <- tidy(kc_to_si_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = kc_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(kc_to_si_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = kc_to_si_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_si_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coekcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = kc_to_si_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Lasso Modeling: Cutter Stuff+ ----------------------------------------------------------
# Sinker ---> Cutter Stuff+
si_to_fc <- cond_data |> 
  filter(ind_sinker == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC) |> 
  drop_na()
# predictors
model_x <- si_to_fc |> 
  select(pfx_vSI:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_fc |> 
  pull(sp_s_FC)
#Lasso Model
si_to_fc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(si_to_fc_lasso_cv)
tidy_lasso_coef <- tidy(si_to_fc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = si_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(si_to_fc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = si_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coesiicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_fc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Fastball ---> Cutter Stuff+
ff_to_fc <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`, 
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC) |> 
  drop_na()
# predictors
model_x <- ff_to_fc |> 
  select(pfx_vFA:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ff_to_fc |> 
  pull(sp_s_FC)
#Lasso Model
ff_to_fc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ff_to_fc_lasso_cv)
tidy_lasso_coef <- tidy(ff_to_fc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ff_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ff_to_fc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ff_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# Slider ---> Cutter Stuff+
sl_to_fc <- cond_data |> 
  filter(ind_slider == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`, 
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC) |> 
  drop_na()
# predictors
model_x <- sl_to_fc |> 
  select(pfx_vSL:avg_rp_z) |> 
  as.matrix()
# response
model_y <- sl_to_fc |> 
  pull(sp_s_FC)
#Lasso Model
sl_to_fc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(sl_to_fc_lasso_cv)
tidy_lasso_coef <- tidy(sl_to_fc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = sl_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(sl_to_fc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = sl_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# Curveball ---> Cutter Stuff+
cu_to_fc <- cond_data |> 
  filter(ind_curve == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`, 
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC) |> 
  drop_na()
# predictors
model_x <- cu_to_fc |> 
  select(pfx_vCU:avg_rp_z) |> 
  as.matrix()
# response
model_y <- cu_to_fc |> 
  pull(sp_s_FC)
#Lasso Model
cu_to_fc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(cu_to_fc_lasso_cv)
tidy_lasso_coef <- tidy(cu_to_fc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = cu_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(cu_to_fc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cu_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# Changeup ---> Cutter Stuff+
ch_to_fc <- cond_data |> 
  filter(ind_change == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, 
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC) |> 
  drop_na()
# predictors
model_x <- ch_to_fc |> 
  select(pfx_vCH:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ch_to_fc |> 
  pull(sp_s_FC)
#Lasso Model
ch_to_fc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ch_to_fc_lasso_cv)
tidy_lasso_coef <- tidy(ch_to_fc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ch_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ch_to_fc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ch_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# Splitter ---> Cutter Stuff+
fs_to_fc <- cond_data |> 
  filter(ind_split == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`, 
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC) |> 
  drop_na()
# predictors
model_x <- fs_to_fc |> 
  select(pfx_vFS:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fs_to_fc |> 
  pull(sp_s_FC)
#Lasso Model
fs_to_fc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fs_to_fc_lasso_cv)
tidy_lasso_coef <- tidy(fs_to_fc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fs_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fs_to_fc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fs_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# Knuckle Curve ---> Cutter Stuff+
kc_to_fc <- cond_data |> 
  filter(ind_kc == "Yes" & ind_cutter == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC) |> 
  drop_na()
# predictors
model_x <- kc_to_fc |> 
  select(pfx_vKC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- kc_to_fc |> 
  pull(sp_s_FC)
#Lasso Model
kc_to_fc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(kc_to_fc_lasso_cv)
tidy_lasso_coef <- tidy(kc_to_fc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = kc_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(kc_to_fc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = kc_to_fc_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_fc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# Lasso Modeling: Slider Stuff+ ----------------------------------------------------------
# Fastball ---> Slider Stuff+
ff_to_sl <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`, 
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL) |> 
  drop_na()
# predictors
model_x <- ff_to_sl |> 
  select(pfx_vFA:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ff_to_sl |> 
  pull(sp_s_SL)
#Lasso Model
ff_to_sl_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ff_to_sl_lasso_cv)
tidy_lasso_coef <- tidy(ff_to_sl_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ff_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ff_to_sl_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ff_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ff_to_sl_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Sinker ---> Slider Stuff+
si_to_sl <- cond_data |> 
  filter(ind_sinker == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL) |> 
  drop_na()
# predictors
model_x <- si_to_sl |> 
  select(pfx_vSI:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_sl |> 
  pull(sp_s_SL)
#Lasso Model
si_to_sl_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(si_to_sl_lasso_cv)
tidy_lasso_coef <- tidy(si_to_sl_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = si_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(si_to_sl_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = si_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coesiicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_sl_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Cutter ---> Slider Stuff+
fc_to_sl <- cond_data |> 
  filter(ind_cutter == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`, 
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL) |> 
  drop_na()
# predictors
model_x <- fc_to_sl |> 
  select(pfx_vFC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fc_to_sl |> 
  pull(sp_s_SL)
#Lasso Model
fc_to_sl_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fc_to_sl_lasso_cv)
tidy_lasso_coef <- tidy(fc_to_sl_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fc_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fc_to_sl_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fc_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fc_to_sl_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Curveball ---> Slider Stuff+
cu_to_sl <- cond_data |> 
  filter(ind_curve == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`, 
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL) |> 
  drop_na()
# predictors
model_x <- cu_to_sl |> 
  select(pfx_vCU:avg_rp_z) |> 
  as.matrix()
# response
model_y <- cu_to_sl |> 
  pull(sp_s_SL)
#Lasso Model
cu_to_sl_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(cu_to_sl_lasso_cv)
tidy_lasso_coef <- tidy(cu_to_sl_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = cu_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(cu_to_sl_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cu_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coecuicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = cu_to_sl_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Changeup ---> Slider Stuff+
ch_to_sl <- cond_data |> 
  filter(ind_change == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, 
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL) |> 
  drop_na()
# predictors
model_x <- ch_to_sl |> 
  select(pfx_vCH:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ch_to_sl |> 
  pull(sp_s_SL)
#Lasso Model
ch_to_sl_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ch_to_sl_lasso_cv)
tidy_lasso_coef <- tidy(ch_to_sl_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ch_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ch_to_sl_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ch_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coechicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ch_to_sl_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Splitter ---> Slider Stuff+
fs_to_sl <- cond_data |> 
  filter(ind_split == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`, 
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL) |> 
  drop_na()
# predictors
model_x <- fs_to_sl |> 
  select(pfx_vFS:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fs_to_sl |> 
  pull(sp_s_SL)
#Lasso Model
fs_to_sl_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fs_to_sl_lasso_cv)
tidy_lasso_coef <- tidy(fs_to_sl_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fs_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fs_to_sl_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fs_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefsicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fs_to_sl_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Knuckle Curve ---> Slider Stuff+
kc_to_sl <- cond_data |> 
  filter(ind_kc == "Yes" & ind_slider == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL) |> 
  drop_na()
# predictors
model_x <- kc_to_sl |> 
  select(pfx_vKC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- kc_to_sl |> 
  pull(sp_s_SL)
#Lasso Model
kc_to_sl_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(kc_to_sl_lasso_cv)
tidy_lasso_coef <- tidy(kc_to_sl_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = kc_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(kc_to_sl_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = kc_to_sl_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_sl_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coekcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = kc_to_sl_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Lasso Modeling: Curveball Stuff+ ----------------------------------------------------------
# Sinker ---> Curveball Stuff+
si_to_cu <- cond_data |> 
  filter(ind_sinker == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU) |> 
  drop_na()
# predictors
model_x <- si_to_cu |> 
  select(pfx_vSI:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_cu |> 
  pull(sp_s_CU)
#Lasso Model
si_to_cu_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(si_to_cu_lasso_cv)
tidy_lasso_coef <- tidy(si_to_cu_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = si_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(si_to_cu_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = si_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coesiicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_cu_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Fastball ---> Curveball Stuff+
ff_to_cu <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`, 
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU) |> 
  drop_na()
# predictors
model_x <- ff_to_cu |> 
  select(pfx_vFA:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ff_to_cu |> 
  pull(sp_s_CU)
#Lasso Model
ff_to_cu_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ff_to_cu_lasso_cv)
tidy_lasso_coef <- tidy(ff_to_cu_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ff_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ff_to_cu_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ff_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ff_to_cu_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Cutter ---> Curveball Stuff+
fc_to_cu <- cond_data |> 
  filter(ind_cutter == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`, 
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU) |> 
  drop_na()
# predictors
model_x <- fc_to_cu |> 
  select(pfx_vFC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fc_to_cu |> 
  pull(sp_s_CU)
#Lasso Model
fc_to_cu_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fc_to_cu_lasso_cv)
tidy_lasso_coef <- tidy(fc_to_cu_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fc_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fc_to_cu_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fc_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fc_to_cu_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Slider ---> Curveball Stuff+
sl_to_cu <- cond_data |> 
  filter(ind_slider == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`, 
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU) |> 
  drop_na()
# predictors
model_x <- sl_to_cu |> 
  select(pfx_vSL:avg_rp_z) |> 
  as.matrix()
# response
model_y <- sl_to_cu |> 
  pull(sp_s_CU)
#Lasso Model
sl_to_cu_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(sl_to_cu_lasso_cv)
tidy_lasso_coef <- tidy(sl_to_cu_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = sl_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(sl_to_cu_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = sl_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coeslicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = sl_to_cu_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Changeup ---> Curveball Stuff+
ch_to_cu <- cond_data |> 
  filter(ind_change == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, 
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU) |> 
  drop_na()
# predictors
model_x <- ch_to_cu |> 
  select(pfx_vCH:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ch_to_cu |> 
  pull(sp_s_CU)
#Lasso Model
ch_to_cu_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ch_to_cu_lasso_cv)
tidy_lasso_coef <- tidy(ch_to_cu_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ch_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ch_to_cu_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ch_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coechicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ch_to_cu_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Splitter ---> Curveball Stuff+
fs_to_cu <- cond_data |> 
  filter(ind_split == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`, 
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU) |> 
  drop_na()
# predictors
model_x <- fs_to_cu |> 
  select(pfx_vFS:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fs_to_cu |> 
  pull(sp_s_CU)
#Lasso Model
fs_to_cu_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fs_to_cu_lasso_cv)
tidy_lasso_coef <- tidy(fs_to_cu_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fs_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fs_to_cu_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fs_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefsicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fs_to_cu_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Knuckle Curve ---> Curveball Stuff+
kc_to_cu <- cond_data |> 
  filter(ind_kc == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU) |> 
  drop_na()
# predictors
model_x <- kc_to_cu |> 
  select(pfx_vKC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- kc_to_cu |> 
  pull(sp_s_CU)
#Lasso Model
kc_to_cu_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(kc_to_cu_lasso_cv)
tidy_lasso_coef <- tidy(kc_to_cu_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = kc_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(kc_to_cu_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = kc_to_cu_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_cu_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coekcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = kc_to_cu_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Lasso Modeling: Changeup Stuff+ ----------------------------------------------------------
# Sinker ---> Changeup Stuff+
si_to_ch <- cond_data |> 
  filter(ind_sinker == "Yes" & ind_change == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH) |> 
  drop_na()
# predictors
model_x <- si_to_ch |> 
  select(pfx_vSI:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_ch |> 
  pull(sp_s_CH)
#Lasso Model
si_to_ch_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(si_to_ch_lasso_cv)
tidy_lasso_coef <- tidy(si_to_ch_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = si_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(si_to_ch_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = si_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coesiicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_ch_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Fastball ---> Changeup Stuff+
ff_to_ch <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_change == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`, 
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH) |> 
  drop_na()
# predictors
model_x <- ff_to_ch |> 
  select(pfx_vFA:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ff_to_ch |> 
  pull(sp_s_CH)
#Lasso Model
ff_to_ch_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ff_to_ch_lasso_cv)
tidy_lasso_coef <- tidy(ff_to_ch_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ff_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ff_to_ch_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ff_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ff_to_ch_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Cutter ---> Change Stuff+
fc_to_ch <- cond_data |> 
  filter(ind_cutter == "Yes" & ind_change == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`, 
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH) |> 
  drop_na()
# predictors
model_x <- fc_to_ch |> 
  select(pfx_vFC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fc_to_ch |> 
  pull(sp_s_CH)
#Lasso Model
fc_to_ch_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fc_to_ch_lasso_cv)
tidy_lasso_coef <- tidy(fc_to_ch_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fc_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fc_to_ch_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fc_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fc_to_ch_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Slider ---> Changeup Stuff+
sl_to_ch <- cond_data |> 
  filter(ind_slider == "Yes" & ind_change == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`, 
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH) |> 
  drop_na()
# predictors
model_x <- sl_to_ch |> 
  select(pfx_vSL:avg_rp_z) |> 
  as.matrix()
# response
model_y <- sl_to_ch |> 
  pull(sp_s_CH)
#Lasso Model
sl_to_ch_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(sl_to_ch_lasso_cv)
tidy_lasso_coef <- tidy(sl_to_ch_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = sl_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(sl_to_ch_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = sl_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coeslicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = sl_to_ch_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Curveball ---> Changeup Stuff+
cu_to_ch <- cond_data |> 
  filter(ind_change == "Yes" & ind_curve == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`, 
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH) |> 
  drop_na()
# predictors
model_x <- cu_to_ch |> 
  select(pfx_vCU:avg_rp_z) |> 
  as.matrix()
# response
model_y <- cu_to_ch |> 
  pull(sp_s_CH)
#Lasso Model
cu_to_ch_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(cu_to_ch_lasso_cv)
tidy_lasso_coef <- tidy(cu_to_ch_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = cu_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(cu_to_ch_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cu_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coechicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = cu_to_ch_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Splitter ---> Changeup Stuff+
fs_to_ch <- cond_data |> 
  filter(ind_split == "Yes" & ind_change == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`, 
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH) |> 
  drop_na()
# predictors
model_x <- fs_to_ch |> 
  select(pfx_vFS:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fs_to_ch |> 
  pull(sp_s_CH)
#Lasso Model
fs_to_ch_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fs_to_ch_lasso_cv)
tidy_lasso_coef <- tidy(fs_to_ch_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fs_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fs_to_ch_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fs_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefsicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fs_to_ch_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Knuckle Curve ---> Changeup Stuff+
kc_to_ch <- cond_data |> 
  filter(ind_kc == "Yes" & ind_change == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH) |> 
  drop_na()
# predictors
model_x <- kc_to_ch |> 
  select(pfx_vKC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_ch |> 
  pull(sp_s_CH)
#Lasso Model
kc_to_ch_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(kc_to_ch_lasso_cv)
tidy_lasso_coef <- tidy(kc_to_ch_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = kc_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(kc_to_ch_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = kc_to_ch_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_ch_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coekcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = kc_to_ch_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Lasso Modeling: Splitter Stuff+ ----------------------------------------------------------
# Fastball ---> Splitter Stuff+
ff_to_fs <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`, 
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS) |> 
  drop_na()
# predictors
model_x <- ff_to_fs |> 
  select(pfx_vFA:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ff_to_fs |> 
  pull(sp_s_FS)
#Lasso Model
ff_to_fs_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ff_to_fs_lasso_cv)
tidy_lasso_coef <- tidy(ff_to_fs_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ff_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ff_to_fs_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ff_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ff_to_fs_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Sinker ---> Splitter Stuff+
si_to_fs <- cond_data |> 
  filter(ind_sinker == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS) |> 
  drop_na()
# predictors
model_x <- si_to_fs |> 
  select(pfx_vSI:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_fs |> 
  pull(sp_s_FS)
#Lasso Model
si_to_fs_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(si_to_fs_lasso_cv)
tidy_lasso_coef <- tidy(si_to_fs_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = si_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(si_to_fs_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = si_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coesiicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_fs_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Cutter ---> Splitter Stuff+
fc_to_fs <- cond_data |> 
  filter(ind_cutter == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`, 
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS) |> 
  drop_na()
# predictors
model_x <- fc_to_fs |> 
  select(pfx_vFC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fc_to_fs |> 
  pull(sp_s_FS)
#Lasso Model
fc_to_fs_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fc_to_fs_lasso_cv)
tidy_lasso_coef <- tidy(fc_to_fs_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fc_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fc_to_fs_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fc_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fc_to_fs_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Slider ---> Splitter Stuff+
sl_to_fs <- cond_data |> 
  filter(ind_slider == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`, 
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS) |> 
  drop_na()
# predictors
model_x <- sl_to_fs |> 
  select(pfx_vSL:avg_rp_z) |> 
  as.matrix()
# response
model_y <- sl_to_fs |> 
  pull(sp_s_FS)
#Lasso Model
sl_to_fs_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(sl_to_fs_lasso_cv)
tidy_lasso_coef <- tidy(sl_to_fs_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = sl_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(sl_to_fs_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = sl_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coeslicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = sl_to_fs_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Curveball ---> Splitter Stuff+
cu_to_fs <- cond_data |> 
  filter(ind_curve == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`, 
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS) |> 
  drop_na()
# predictors
model_x <- cu_to_fs |> 
  select(pfx_vCU:avg_rp_z) |> 
  as.matrix()
# response
model_y <- cu_to_fs |> 
  pull(sp_s_FS)
#Lasso Model
cu_to_fs_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(cu_to_fs_lasso_cv)
tidy_lasso_coef <- tidy(cu_to_fs_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = cu_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(cu_to_fs_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cu_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coecuicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = cu_to_fs_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Changeup ---> Splitter Stuff+
ch_to_fs <- cond_data |> 
  filter(ind_change == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, 
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS) |> 
  drop_na()
# predictors
model_x <- ch_to_fs |> 
  select(pfx_vCH:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ch_to_fs |> 
  pull(sp_s_FS)
#Lasso Model
ch_to_fs_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ch_to_fs_lasso_cv)
tidy_lasso_coef <- tidy(ch_to_fs_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ch_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ch_to_fs_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ch_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coechicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ch_to_fs_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Knuckle Curve ---> Splitter Stuff+
kc_to_fs <- cond_data |> 
  filter(ind_kc == "Yes" & ind_split == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS) |> 
  drop_na()
# predictors
model_x <- kc_to_fs |> 
  select(pfx_vKC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- kc_to_fs |> 
  pull(sp_s_FS)
#Lasso Model
kc_to_fs_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(kc_to_fs_lasso_cv)
tidy_lasso_coef <- tidy(kc_to_fs_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = kc_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(kc_to_fs_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = kc_to_fs_lasso_cv$lambda.min) +
  geom_vline(xintercept = kc_to_fs_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coekcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = kc_to_fs_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Lasso Modeling: Knuckle Curve Stuff+ ----------------------------------------------------------
# Fastball ---> Knuckle Curve Stuff+
ff_to_kc <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`, 
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC) |> 
  drop_na()
# predictors
model_x <- ff_to_kc |> 
  select(pfx_vFA:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ff_to_kc |> 
  pull(sp_s_KC)
#Lasso Model
ff_to_kc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ff_to_kc_lasso_cv)
tidy_lasso_coef <- tidy(ff_to_kc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ff_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ff_to_kc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ff_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ff_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ff_to_kc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Sinker ---> Knuckle Curve Stuff+
si_to_kc <- cond_data |> 
  filter(ind_sinker == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC) |> 
  drop_na()
# predictors
model_x <- si_to_kc |> 
  select(pfx_vSI:avg_rp_z) |> 
  as.matrix()
# response
model_y <- si_to_kc |> 
  pull(sp_s_KC)
#Lasso Model
si_to_kc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(si_to_kc_lasso_cv)
tidy_lasso_coef <- tidy(si_to_kc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = si_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(si_to_kc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = si_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = si_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coesiicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_kc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Cutter ---> Knuckle Curve Stuff+
fc_to_kc <- cond_data |> 
  filter(ind_cutter == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`, 
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC) |> 
  drop_na()
# predictors
model_x <- fc_to_kc |> 
  select(pfx_vFC:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fc_to_kc |> 
  pull(sp_s_KC)
#Lasso Model
fc_to_kc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fc_to_kc_lasso_cv)
tidy_lasso_coef <- tidy(fc_to_kc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fc_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fc_to_kc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fc_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = fc_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefcicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fc_to_kc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Slider ---> Knuckle Curve Stuff+
sl_to_kc <- cond_data |> 
  filter(ind_slider == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`, 
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC) |> 
  drop_na()
# predictors
model_x <- sl_to_kc |> 
  select(pfx_vSL:avg_rp_z) |> 
  as.matrix()
# response
model_y <- sl_to_kc |> 
  pull(sp_s_KC)
#Lasso Model
sl_to_kc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(sl_to_kc_lasso_cv)
tidy_lasso_coef <- tidy(sl_to_kc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = sl_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(sl_to_kc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = sl_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = sl_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coeslicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = sl_to_kc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Curveball ---> Knuckle Curve Stuff+
cu_to_kc <- cond_data |> 
  filter(ind_curve == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`, 
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC) |> 
  drop_na()
# predictors
model_x <- cu_to_kc |> 
  select(pfx_vCU:avg_rp_z) |> 
  as.matrix()
# response
model_y <- cu_to_kc |> 
  pull(sp_s_KC)
#Lasso Model
cu_to_kc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(cu_to_kc_lasso_cv)
tidy_lasso_coef <- tidy(cu_to_kc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = cu_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(cu_to_kc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cu_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = cu_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coecuicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = cu_to_kc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Changeup ---> Knuckle Curve Stuff+
ch_to_kc <- cond_data |> 
  filter(ind_change == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, 
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC) |> 
  drop_na()
# predictors
model_x <- ch_to_kc |> 
  select(pfx_vCH:avg_rp_z) |> 
  as.matrix()
# response
model_y <- ch_to_kc |> 
  pull(sp_s_KC)
#Lasso Model
ch_to_kc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(ch_to_kc_lasso_cv)
tidy_lasso_coef <- tidy(ch_to_kc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = ch_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(ch_to_kc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = ch_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = ch_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coechicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = ch_to_kc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Splitter ---> Knuckle Curve Stuff+
fs_to_kc <- cond_data |> 
  filter(ind_split == "Yes" & ind_kc == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`, 
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC) |> 
  drop_na()
# predictors
model_x <- fs_to_kc |> 
  select(pfx_vFS:avg_rp_z) |> 
  as.matrix()
# response
model_y <- fs_to_kc |> 
  pull(sp_s_KC)
#Lasso Model
fs_to_kc_lasso_cv <- cv.glmnet(model_x, model_y, 
                               alpha = 1)
plot(fs_to_kc_lasso_cv)
tidy_lasso_coef <- tidy(fs_to_kc_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fs_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fs_to_kc_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fs_to_kc_lasso_cv$lambda.min) +
  geom_vline(xintercept = fs_to_kc_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefsicient estimates
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fs_to_kc_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)



# Lasso RMSE Calculations -------------------------------------------------
# Define a function to perform nested cross-validation
nested_cv_lasso <- function(predictor_pitch, response_pitch, data, lassoRMSE) {
  # Prepare the data
  data <- data |> 
    rename_with(~ make.names(.))
  
  # Prepare the predictors and response
  data <- data |> 
    select(4:11)
  
  model_x <- data |> 
    select(1:7) |> 
    as.matrix()
  model_y <- data |> 
    pull(8)
  
  # Create folds for cross-validation
  set.seed(4)
  k_folds <- 5
  data <- data |> mutate(fold_id = sample(rep(1:k_folds, length.out = n())))
  
  rmse_list <- vector("numeric", length = k_folds)
  
  for (i in 1:k_folds) {
    # Split the data into training and test sets
    train_data <- data |> filter(fold_id != i)
    test_data <- data |> filter(fold_id == i)
    
    # Prepare training and test data
    x_train <- train_data |> select(1:7) |> as.matrix()
    y_train <- train_data |> pull(8)
    x_test <- test_data |> select(1:7) |> as.matrix()
    y_test <- test_data |> pull(8)
    
    # Perform Lasso regression with cross-validation on training data
    lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
    
    # Fit the final Lasso model with the best lambda
    lasso_final <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_cv$lambda.1se)
    
    # Predict on the test data
    predictions <- predict(lasso_final, newx = x_test)
    
    # Calculate RMSE for the test data
    rmse <- sqrt(mean((y_test - predictions)^2))
    rmse_list[i] <- rmse
  }
  
  # Calculate the average RMSE
  avg_rmse <- mean(rmse_list)
  
  # Append the result to the lassoRMSE
  lassoRMSE <- lassoRMSE |> 
    add_row(
      `Predictor Pitch` = predictor_pitch,
      `Response Pitch` = response_pitch,
      `Average RMSE` = avg_rmse
    )
  
  return(lassoRMSE)
}

# Initialize an empty results data frame
lassoRMSE <- tibble(
  `Predictor Pitch` = character(),
  `Response Pitch` = character(),
  `Average RMSE` = numeric()
)

lassoRMSE <- nested_cv_lasso("Sinker", "Fastball", si_to_ff, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Cutter", "Fastball", fc_to_ff, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Slider", "Fastball", sl_to_ff, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Curveball", "Fastball", cu_to_ff, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Changeup", "Fastball", ch_to_ff, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Splitter", "Fastball", fs_to_ff, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Fastball", "Sinker", ff_to_si, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Cutter", "Sinker", fc_to_si, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Slider", "Sinker", sl_to_si, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Curveball", "Sinker", cu_to_si, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Changeup", "Sinker", ch_to_si, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Splitter", "Sinker", fs_to_si, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Fastball", "Cutter", ff_to_fc, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Sinker", "Cutter", si_to_fc, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Slider", "Cutter", sl_to_fc, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Curveball", "Cutter", cu_to_fc, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Changeup", "Cutter", ch_to_fc, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Splitter", "Cutter", fs_to_fc, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Fastball", "Slider", ff_to_sl, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Sinker", "Slider", si_to_sl, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Cutter", "Slider", fc_to_sl, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Curveball", "Slider", cu_to_sl, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Changeup", "Slider", ch_to_sl, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Splitter", "Slider", fs_to_sl, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Fastball", "Curveball", ff_to_cu, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Sinker", "Curveball", si_to_cu, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Cutter", "Curveball", fc_to_cu, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Slider", "Curveball", sl_to_cu, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Changeup", "Curveball", ch_to_cu, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Splitter", "Curveball", fs_to_cu, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Fastball", "Changeup", ff_to_ch, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Sinker", "Changeup", si_to_ch, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Cutter", "Changeup", fc_to_ch, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Slider", "Changeup", sl_to_ch, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Curveball", "Changeup", cu_to_ch, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Splitter", "Changeup", fs_to_ch, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Fastball", "Splitter", ff_to_fs, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Sinker", "Splitter", si_to_fs, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Cutter", "Splitter", fc_to_fs, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Slider", "Splitter", sl_to_fs, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Curveball", "Splitter", cu_to_fs, lassoRMSE)
lassoRMSE <- nested_cv_lasso("Changeup", "Splitter", ch_to_fs, lassoRMSE)
#for simplicity's sake, not including knuckle curve (no spin rate)

# Display the result
print(lassoRMSE)
write.table(lassoRMSE, "lassoRSME")


# Random Forest RMSE Calculations -----------------------------------------------------------
# Function to perform cross-validation with Random Forest for any pitch type
rf_cv <- function(predictor_pitch, response_pitch, response_var, data, rf_RMSE) {
  # Rename columns to make them valid for formula interface
  data <- data |> 
    rename_with(~ make.names(.)) |> 
    select(4:11)
  
  # Prepare the response variable
  model_y <- data[[response_var]]
  
  # Create folds for cross-validation
  set.seed(4)
  k_folds <- 5
  data <- data |> mutate(fold_id = sample(rep(1:k_folds, length.out = n())))
  
  rmse_list <- vector("numeric", length = k_folds)
  
  for (i in 1:k_folds) {
    # Split data into training and test sets
    train_data <- data |> filter(fold_id != i)
    test_data <- data |> filter(fold_id == i)
    
    # Prepare training and test data
    x_train <- train_data |> select(-fold_id, -all_of(response_var))
    y_train <- train_data[[response_var]]
    x_test <- test_data |> select(-fold_id, -all_of(response_var))
    y_test <- test_data[[response_var]]
    
    # Train Random Forest model
    rf_model <- ranger(y_train ~ .,
                       data = cbind(x_train, y_train), 
                       num.trees = 500, 
                       importance = "impurity",
                       mtry = 2)
    
    # Predict on the test data
    predictions <- predict(rf_model, data = x_test)$predictions
    
    # Calculate RMSE for the test data
    rmse <- sqrt(mean((y_test - predictions)^2))
    rmse_list[i] <- rmse
  }
  print(rmse_list)
  # Calculate the average RMSE
  avg_rmse <- mean(rmse_list)
  
  # Append the result to the rf_RMSE
  rf_RMSE <- rf_RMSE |> 
    add_row(
      `Predictor Pitch` = predictor_pitch,
      `Response Pitch` = response_pitch,
      `Average RMSE` = avg_rmse
    )
  
  return(rf_RMSE)
}

# Initialize an empty results data frame
rf_RMSE <- tibble(
  `Predictor Pitch` = character(),
  `Response Pitch` = character(),
  `Average RMSE` = numeric()
)

# Example usage for different predictor pitches
rf_RMSE <- rf_cv("Sinker", "Fastball", "sp_s_FF", si_to_ff, rf_RMSE)
rf_RMSE <- rf_cv("Cutter", "Fastball", "sp_s_FF", fc_to_ff, rf_RMSE)
rf_RMSE <- rf_cv("Slider", "Fastball", "sp_s_FF", sl_to_ff, rf_RMSE)
rf_RMSE <- rf_cv("Curveball", "Fastball", "sp_s_FF", cu_to_ff, rf_RMSE)
rf_RMSE <- rf_cv("Changeup", "Fastball", "sp_s_FF", ch_to_ff, rf_RMSE)
rf_RMSE <- rf_cv("Splitter", "Fastball", "sp_s_FF", fs_to_ff, rf_RMSE)

rf_RMSE <- rf_cv("Fastball", "Sinker", "sp_s_SI", ff_to_si, rf_RMSE)
rf_RMSE <- rf_cv("Cutter", "Sinker", "sp_s_SI", fc_to_si, rf_RMSE)
rf_RMSE <- rf_cv("Slider", "Sinker", "sp_s_SI", sl_to_si, rf_RMSE)
rf_RMSE <- rf_cv("Curveball", "Sinker", "sp_s_SI", cu_to_si, rf_RMSE)
rf_RMSE <- rf_cv("Changeup", "Sinker", "sp_s_SI", ch_to_si, rf_RMSE)
rf_RMSE <- rf_cv("Splitter", "Sinker", "sp_s_SI", fs_to_si, rf_RMSE)

rf_RMSE <- rf_cv("Fastball", "Cutter", "sp_s_FC", ff_to_fc, rf_RMSE)
rf_RMSE <- rf_cv("Sinker", "Cutter", "sp_s_FC", si_to_fc, rf_RMSE)
rf_RMSE <- rf_cv("Slider", "Cutter", "sp_s_FC", sl_to_fc, rf_RMSE)
rf_RMSE <- rf_cv("Curveball", "Cutter", "sp_s_FC", cu_to_fc, rf_RMSE)
rf_RMSE <- rf_cv("Changeup", "Cutter", "sp_s_FC", ch_to_fc, rf_RMSE)
rf_RMSE <- rf_cv("Splitter", "Cutter", "sp_s_FC", fs_to_fc, rf_RMSE)

rf_RMSE <- rf_cv("Fastball", "Slider", "sp_s_SL", ff_to_sl, rf_RMSE)
rf_RMSE <- rf_cv("Sinker", "Slider", "sp_s_SL", si_to_sl, rf_RMSE)
rf_RMSE <- rf_cv("Cutter", "Slider", "sp_s_SL", fc_to_sl, rf_RMSE)
rf_RMSE <- rf_cv("Curveball", "Slider", "sp_s_SL", cu_to_sl, rf_RMSE)
rf_RMSE <- rf_cv("Changeup", "Slider", "sp_s_SL", ch_to_sl, rf_RMSE)
rf_RMSE <- rf_cv("Splitter", "Slider", "sp_s_SL", fs_to_sl, rf_RMSE)

rf_RMSE <- rf_cv("Fastball", "Curveball", "sp_s_CU", ff_to_cu, rf_RMSE)
rf_RMSE <- rf_cv("Sinker", "Curveball", "sp_s_CU", si_to_cu, rf_RMSE)
rf_RMSE <- rf_cv("Cutter", "Curveball", "sp_s_CU", fc_to_cu, rf_RMSE)
rf_RMSE <- rf_cv("Slider", "Curveball", "sp_s_CU", sl_to_cu, rf_RMSE)
rf_RMSE <- rf_cv("Changeup", "Curveball", "sp_s_CU", ch_to_cu, rf_RMSE)
rf_RMSE <- rf_cv("Splitter", "Curveball", "sp_s_CU", fs_to_cu, rf_RMSE)

rf_RMSE <- rf_cv("Fastball", "Changeup", "sp_s_CH", ff_to_ch, rf_RMSE)
rf_RMSE <- rf_cv("Sinker", "Changeup", "sp_s_CH", si_to_ch, rf_RMSE)
rf_RMSE <- rf_cv("Cutter", "Changeup", "sp_s_CH", fc_to_ch, rf_RMSE)
rf_RMSE <- rf_cv("Slider", "Changeup", "sp_s_CH", sl_to_ch, rf_RMSE)
rf_RMSE <- rf_cv("Curveball", "Changeup", "sp_s_CH", cu_to_ch, rf_RMSE)
rf_RMSE <- rf_cv("Splitter", "Changeup", "sp_s_CH", fs_to_ch, rf_RMSE)

rf_RMSE <- rf_cv("Fastball", "Splitter", "sp_s_FS", ff_to_fs, rf_RMSE)
rf_RMSE <- rf_cv("Sinker", "Splitter", "sp_s_FS", si_to_fs, rf_RMSE)
rf_RMSE <- rf_cv("Cutter", "Splitter", "sp_s_FS", fc_to_fs, rf_RMSE)
rf_RMSE <- rf_cv("Slider", "Splitter", "sp_s_FS", sl_to_fs, rf_RMSE)
rf_RMSE <- rf_cv("Curveball", "Splitter", "sp_s_FS", cu_to_fs, rf_RMSE)
rf_RMSE <- rf_cv("Changeup", "Splitter", "sp_s_FS", ch_to_fs, rf_RMSE)

# Display the result
print(rf_RMSE)
write.table(rf_RMSE, "rfRMSE")



# Intercept Only Model ----------------------------------------------------
#Fastball
set.seed(4)
k <- 5
ff_int_only <- cond_data |>
  filter(!is.na(sp_s_FF)) |> 
  select(sp_s_FF) |> 
  mutate(test_fold = sample(rep(1:k, length.out = n())))

# Function to get RMSE for a single fold
ff_int_fn <- function(fold) {
  test_data <- ff_int_only |> filter(test_fold == fold)
  train_data <- ff_int_only |> filter(test_fold != fold)
  
  lm_fit <- lm(sp_s_FF ~ 1, data = train_data) 
  
  # Predict on the test data
  predictions <- rep(coef(lm_fit)[1], nrow(test_data))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$sp_s_FF - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each fold
rmse_list <- sapply(1:k, ff_int_fn)

# Calculate the average RMSE
avg_rmse <- mean(rmse_list)

# Output the results
ffRMSE <- tibble(
  `Pitch Type` = "Fastball",
  `Average RMSE` = avg_rmse
)

print(ffRMSE)

#Sinker
set.seed(4)
k <- 5
si_int_only <- cond_data |>
  filter(!is.na(sp_s_SI)) |> 
  select(sp_s_SI) |> 
  mutate(test_fold = sample(rep(1:k, length.out = n())))

# Function to get RMSE for a single fold
si_int_fn <- function(fold) {
  test_data <- si_int_only |> filter(test_fold == fold)
  train_data <- si_int_only |> filter(test_fold != fold)
  
  lm_fit <- lm(sp_s_SI ~ 1, data = train_data) 
  
  # Predict on the test data
  predictions <- rep(coef(lm_fit)[1], nrow(test_data))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$sp_s_SI - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each fold
rmse_list <- sapply(1:k, si_int_fn)

# Calculate the average RMSE
avg_rmse <- mean(rmse_list)

# Output the results
siRMSE <- tibble(
  `Pitch Type` = "Sinker",
  `Average RMSE` = avg_rmse
)

print(siRMSE)

#Cutter
set.seed(4)
k <- 5
fc_int_only <- cond_data |>
  filter(!is.na(sp_s_FC)) |> 
  select(sp_s_FC) |> 
  mutate(test_fold = sample(rep(1:k, length.out = n())))

# Function to get RMSE for a single fold
fc_int_fn <- function(fold) {
  test_data <- fc_int_only |> filter(test_fold == fold)
  train_data <- fc_int_only |> filter(test_fold != fold)
  
  lm_fit <- lm(sp_s_FC ~ 1, data = train_data) 
  
  # Predict on the test data
  predictions <- rep(coef(lm_fit)[1], nrow(test_data))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$sp_s_FC - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each fold
rmse_list <- sapply(1:k, fc_int_fn)

# Calculate the average RMSE
avg_rmse <- mean(rmse_list)

# Output the results
fcRMSE <- tibble(
  `Pitch Type` = "Cutter",
  `Average RMSE` = avg_rmse
)

print(fcRMSE)

#Slider
set.seed(4)
k <- 5
sl_int_only <- cond_data |>
  filter(!is.na(sp_s_SL)) |> 
  select(sp_s_SL) |> 
  mutate(test_fold = sample(rep(1:k, length.out = n())))

# Function to get RMSE for a single fold
sl_int_fn <- function(fold) {
  test_data <- sl_int_only |> filter(test_fold == fold)
  train_data <- sl_int_only |> filter(test_fold != fold)
  
  lm_fit <- lm(sp_s_SL ~ 1, data = train_data) 
  
  # Predict on the test data
  predictions <- rep(coef(lm_fit)[1], nrow(test_data))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$sp_s_SL - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each fold
rmse_list <- sapply(1:k, sl_int_fn)

# Calculate the average RMSE
avg_rmse <- mean(rmse_list)

# Output the results
slRMSE <- tibble(
  `Pitch Type` = "Slider",
  `Average RMSE` = avg_rmse
)
print(slRMSE)

#Curveball
set.seed(4)
k <- 5
cu_int_only <- cond_data |>
  filter(!is.na(sp_s_CU)) |> 
  select(sp_s_CU) |> 
  mutate(test_fold = sample(rep(1:k, length.out = n())))

# Function to get RMSE for a single fold
cu_int_fn <- function(fold) {
  test_data <- cu_int_only |> filter(test_fold == fold)
  train_data <- cu_int_only |> filter(test_fold != fold)
  
  lm_fit <- lm(sp_s_CU ~ 1, data = train_data) 
  
  # Predict on the test data
  predictions <- rep(coef(lm_fit)[1], nrow(test_data))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$sp_s_CU - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each fold
rmse_list <- sapply(1:k, cu_int_fn)

# Calculate the average RMSE
avg_rmse <- mean(rmse_list)

# Output the results
cuRMSE <- tibble(
  `Pitch Type` = "Curveball",
  `Average RMSE` = avg_rmse
)

print(cuRMSE)

#Changeup
set.seed(4)
k <- 5
ch_int_only <- cond_data |>
  filter(!is.na(sp_s_CH)) |> 
  select(sp_s_CH) |> 
  mutate(test_fold = sample(rep(1:k, length.out = n())))

# Function to get RMSE for a single fold
ch_int_fn <- function(fold) {
  test_data <- ch_int_only |> filter(test_fold == fold)
  train_data <- ch_int_only |> filter(test_fold != fold)
  
  lm_fit <- lm(sp_s_CH ~ 1, data = train_data) 
  
  # Predict on the test data
  predictions <- rep(coef(lm_fit)[1], nrow(test_data))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$sp_s_CH - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each fold
rmse_list <- sapply(1:k, ch_int_fn)

# Calculate the average RMSE
avg_rmse <- mean(rmse_list)

# Output the results
chRMSE <- tibble(
  `Pitch Type` = "Changeup",
  `Average RMSE` = avg_rmse
)

print(chRMSE)

#Splitter
set.seed(4)
k <- 5
fs_int_only <- cond_data |>
  filter(!is.na(sp_s_FS)) |> 
  select(sp_s_FS) |> 
  mutate(test_fold = sample(rep(1:k, length.out = n())))

# Function to get RMSE for a single fold
fs_int_fn <- function(fold) {
  test_data <- fs_int_only |> filter(test_fold == fold)
  train_data <- fs_int_only |> filter(test_fold != fold)
  
  lm_fit <- lm(sp_s_FS ~ 1, data = train_data) 
  
  # Predict on the test data
  predictions <- rep(coef(lm_fit)[1], nrow(test_data))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$sp_s_FS - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each fold
rmse_list <- sapply(1:k, fs_int_fn)

# Calculate the average RMSE
avg_rmse <- mean(rmse_list)

# Output the results
fsRMSE <- tibble(
  `Pitch Type` = "Splitter",
  `Average RMSE` = avg_rmse
)

print(fsRMSE)

#Combining all pitches intercept-only RMSE into a table
int_only_RMSE = bind_rows(ffRMSE, siRMSE, fcRMSE, slRMSE, cuRMSE, chRMSE, fsRMSE)
write.table(int_only_RMSE, "IntOnlyRMSE")


# Model Comparison --------------------------------------------------------

lassoRMSE <- lassoRMSE |> 
  mutate(Method = "Lasso")
rf_RMSE <- rf_RMSE |> 
  mutate(Method = "Random Forest")
int_only_RMSE <- int_only_RMSE |> 
  mutate(Method = "Intercept Only") |> 
  rename("Response Pitch" = `Pitch Type`)
RMSEcomp = bind_rows(int_only_RMSE, lassoRMSE, rf_RMSE)

RMSEcomp |> 
  mutate(`Response Pitch` = factor(`Response Pitch`, 
                                   levels = c("Fastball", "Sinker", "Cutter",
                                              "Slider", "Curveball", 
                                              "Changeup", "Splitter"))) |> 
  ggplot(aes(`Response Pitch`, `Average RMSE`, color = Method))+
  geom_point(alpha = 0.8, size = 2)+
  scale_color_manual(values = c("black","red2", "dodgerblue2"))+
  theme_bw()

#Referencing tibble created earlier: comparing the RMSEs of lasso and RF for
# fastball predicting sinker
rmse_table <- rmse_table |> 
  mutate(`Random Forest` = c(16.58268, 13.25207, 10.68425, 12.0298, 12.74035)) |> 
#wee bit of hard coding here
  pivot_longer(cols = c(Lasso, `Random Forest`), 
               names_to = "Method", values_to = "RMSE")
  
ggplot(rmse_table, aes(x = Method, y = RMSE)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2)+
  theme_bw()+
  geom_hline(aes(yintercept = 17.2472, color = "Intercept Only RMSE"), 
             linetype = "dashed", linewidth = 1.5) +
  scale_y_continuous(breaks = c(10, 12, 14, 16, 18), limits = c(10, 18)) +
  labs(y = "RMSE, Sinker Prediction from Fastball", 
       color = "",
       title = "Modeling Comparison: Lasso Regression vs. Random Forest") + 
  scale_color_manual(values = c("Intercept Only RMSE" = "dodgerblue4"))

#Predict Fastball Stuff+ from Sinker Traits: Random Forest ------------------------------
#Sinker has the smallest RMSE for random forests with Fastball as response
rf_si_to_ff <- si_to_ff |> 
  rename_with(~ make.names(.)) |> 
  select(4:11)
set.seed(4)
ff_model <- ranger(sp_s_FF ~ ., num.trees = 500, importance = "impurity", 
                   data = rf_si_to_ff, mtry = 2)

si_only <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  rename_with(~ make.names(.)) |> 
  select(Season, PlayerNameRoute, xMLBAMID, names(rf_si_to_ff)[1:7]) |>  
  drop_na()
# Generate predictions using the trained model
set.seed(4)
ff_preds <- predict(ff_model, data = si_only)$predictions

# Add the predictions to the new dataset
si_only <- si_only |> 
  mutate(ff_preds = ff_preds) 

# Merge the predictions with the original data
merged_data <- cond_data |> 
  left_join(si_only, by = c('xMLBAMID', 'Season')) |>
  select(Season, PlayerNameRoute.x, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL, 
         sp_s_CU, sp_s_CH, sp_s_FS, ff_preds) |> 
  rename(PlayerNameRoute = PlayerNameRoute.x)

#Fastball has the smallest RMSE for random forests with Sinker as response
rf_ff_to_si <- ff_to_si |> 
  rename_with(~ make.names(.)) |> 
  select(4:11)
set.seed(4)
si_model <- ranger(sp_s_SI ~ ., num.trees = 500, importance = "impurity", 
                   data = rf_ff_to_si, mtry = 2)

ff_only <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  rename_with(~ make.names(.)) |> 
  select(Season, PlayerNameRoute, xMLBAMID, names(rf_ff_to_si)[1:7]) |>  
  drop_na()
# Generate predictions using the trained model
set.seed(4)
si_preds <- predict(si_model, data = ff_only)$predictions

# Add the predictions to the new dataset
ff_only <- ff_only |> 
  mutate(si_preds = si_preds) 

# Merge the predictions with the original data
merged_data <- merged_data |> 
  left_join(ff_only, by = c('xMLBAMID', 'Season')) |> 
  select(Season, PlayerNameRoute.x, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL,
         sp_s_CU, sp_s_CH, sp_s_FS, ff_preds, si_preds) |>
  rename(PlayerNameRoute = PlayerNameRoute.x)

#Splitter has the smallest RMSE for random forests with Cutter as response
rf_fs_to_fc <- fs_to_fc |> 
  rename_with(~ make.names(.)) |> 
  select(4:11)
set.seed(4)
fc_model <- ranger(sp_s_FC ~ ., num.trees = 500, importance = "impurity", 
                   data = rf_fs_to_fc, mtry = 2)

fs_only <- cond_data |> 
  filter(ind_split == "Yes") |> 
  rename_with(~ make.names(.)) |> 
  select(Season, PlayerNameRoute, xMLBAMID, names(rf_fs_to_fc)[1:7]) |>  
  drop_na()
# Generate predictions using the trained model
set.seed(4)
fc_preds <- predict(fc_model, data = fs_only)$predictions

# Add the predictions to the new dataset
fs_only <- fs_only |> 
  mutate(fc_preds = fc_preds) 

# Merge the predictions with the original data
merged_data <- merged_data |> 
  left_join(fs_only, by = c('xMLBAMID', 'Season')) |> 
  select(Season, PlayerNameRoute.x, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL,
         sp_s_CU, sp_s_CH, sp_s_FS, ff_preds, si_preds, fc_preds) |>
  rename(PlayerNameRoute = PlayerNameRoute.x)

#Fastball has the smallest RMSE for random forests with Slider as response
rf_ff_to_sl <- ff_to_sl |> 
  rename_with(~ make.names(.)) |> 
  select(4:11)
set.seed(4)
sl_model <- ranger(sp_s_SL ~ ., num.trees = 500, importance = "impurity", 
                   data = rf_ff_to_sl, mtry = 2)

ff_only2 <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  rename_with(~ make.names(.)) |> 
  select(Season, PlayerNameRoute, xMLBAMID, names(rf_ff_to_sl)[1:7]) |>  
  drop_na()
# Generate predictions using the trained model
set.seed(4)
sl_preds <- predict(sl_model, data = ff_only2)$predictions

# Add the predictions to the new dataset
ff_only2 <- ff_only2 |> 
  mutate(sl_preds = sl_preds) 

# Merge the predictions with the original data
merged_data <- merged_data |> 
  left_join(ff_only2, by = c('xMLBAMID', 'Season'))
merged_data <- merged_data |> 
  select(Season, PlayerNameRoute.x, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL,
        sp_s_CU, sp_s_CH, sp_s_FS, ff_preds, si_preds, fc_preds, sl_preds) |>
  rename(PlayerNameRoute = PlayerNameRoute.x)

#Changeup has the smallest RMSE for random forests with Curveball as response
rf_ch_to_cu <- ch_to_cu |> 
  rename_with(~ make.names(.)) |> 
  select(4:11)
set.seed(4)
cu_model <- ranger(sp_s_CU ~ ., num.trees = 500, importance = "impurity", 
                   data = rf_ch_to_cu, mtry = 2)

ch_only <- cond_data |> 
  filter(ind_change == "Yes") |> 
  rename_with(~ make.names(.)) |> 
  select(Season, PlayerNameRoute, xMLBAMID, names(rf_ch_to_cu)[1:7]) |>  
  drop_na()
# Generate predictions using the trained model
set.seed(4)
cu_preds <- predict(cu_model, data = ch_only)$predictions

# Add the predictions to the new dataset
ch_only <- ch_only |> 
  mutate(cu_preds = cu_preds) 

# Merge the predictions with the original data
merged_data <- merged_data |> 
  left_join(ch_only, by = c('xMLBAMID', 'Season'))
merged_data <- merged_data |> 
  select(Season, PlayerNameRoute.x, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL,
         sp_s_CU, sp_s_CH, sp_s_FS, ff_preds, si_preds, fc_preds, sl_preds, 
         cu_preds) |>
  rename(PlayerNameRoute = PlayerNameRoute.x)

#Curveball has the smallest RMSE for random forests with Changeup as response
rf_cu_to_ch <- cu_to_ch |> 
  rename_with(~ make.names(.)) |> 
  select(4:11)
set.seed(4)
ch_model <- ranger(sp_s_CH ~ ., num.trees = 500, importance = "impurity", 
                   data = rf_cu_to_ch, mtry = 2)

cu_only <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  rename_with(~ make.names(.)) |> 
  select(Season, PlayerNameRoute, xMLBAMID, names(rf_cu_to_ch)[1:7]) |>  
  drop_na()
# Generate predictions using the trained model
set.seed(4)
ch_preds <- predict(ch_model, data = cu_only)$predictions

# Add the predictions to the new dataset
cu_only <- cu_only |> 
  mutate(ch_preds = ch_preds) 

# Merge the predictions with the original data
merged_data <- merged_data |> 
  left_join(cu_only, by = c('xMLBAMID', 'Season'))
merged_data <- merged_data |> 
  select(Season, PlayerNameRoute.x, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL,
         sp_s_CU, sp_s_CH, sp_s_FS, ff_preds, si_preds, fc_preds, sl_preds, 
         cu_preds, ch_preds) |>
  rename(PlayerNameRoute = PlayerNameRoute.x)

#Fastball has the smallest RMSE for random forests with Splitter as response
rf_ff_to_fs <- ff_to_fs |> 
  rename_with(~ make.names(.)) |> 
  select(4:11)
set.seed(4)
fs_model <- ranger(sp_s_FS ~ ., num.trees = 500, importance = "impurity", 
                   data = rf_ff_to_fs, mtry = 2)

ff_only3 <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  rename_with(~ make.names(.)) |> 
  select(Season, PlayerNameRoute, xMLBAMID, names(rf_ff_to_fs)[1:7]) |>  
  drop_na()
# Generate predictions using the trained model
set.seed(4)
fs_preds <- predict(fs_model, data = ff_only3)$predictions

# Add the predictions to the new dataset
ff_only3 <- ff_only3 |> 
  mutate(fs_preds = fs_preds) 

# Merge the predictions with the original data
merged_data <- merged_data |> 
  left_join(ff_only3, by = c('xMLBAMID', 'Season'))
merged_data <- merged_data |> 
  select(Season, PlayerNameRoute.x, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL,
         sp_s_CU, sp_s_CH, sp_s_FS, ff_preds, si_preds, fc_preds, sl_preds, 
         cu_preds, ch_preds, fs_preds) |>
  rename(PlayerNameRoute = PlayerNameRoute.x)



# Attempt at a GAM --------------------------------------------------------
# Sinker Calc
# Select relevant columns
relevant_cols <- c('Season', 'PlayerNameRoute', 'sp_s_FF', 'RAR', 'pfx_SI_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'pfx_SI-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_SI-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'si_avg_spin', 'WPA',
                   'REW', 'pfx_vSI', 'sp_s_SI', 'xFIP_minus', "ind_sinker",
                   'Throws', 'position', 'Addition_SI', 'Deletion_SI')

# Ensure target and primary predictor are not missing
filtered_data <- pitch_arsenal |> 
  select(all_of(relevant_cols)) |> 
  filter(!is.na(sp_s_FF), ind_sinker == 'Yes') 



# Rename movement columns in the filtered_data 
names(filtered_data) <- gsub('-', '_', names(filtered_data))


# Perform k-fold cross-validation
set.seed(4)
train <- filtered_data |> 
  slice_sample(prop = 0.7) |> 
  drop_na()
test <- filtered_data |> 
  anti_join(train) |> 
  drop_na()

library(mgcv)
ch_gam <- gam(sp_s_FF ~ s(pfx_vSI, si_avg_spin) + s(pfx_SI_X, pfx_SI_Z) + s(si_avg_spin) + 
                s(avg_rp_x, avg_rp_z, avg_release_extension),
              method = "REML", # more stable solution than default
              data = train)
tidy(ch_gam)
gam.check(ch_gam)
ch_gam |> 
  augment(type.predict = "response") |> 
  mutate(newdata = train, pred_class = round(.fitted)) |> 
  summarize(RMSE = sqrt(mean((sp_s_FF - pred_class)^2)))

# HistGradientBoostingRegressor -------------------------------------------


hgbc <- cond_data
names(hgbc) <- gsub('-', '_', names(hgbc))  
hgbc <- hgbc |> 
  filter(ind_fastball == "Yes", !is.na(sp_s_FF)) |> 
  select(xMLBAMID, pfx_vCH, pfx_vCU, pfx_vFA, pfx_vFC, 
         pfx_vSI, pfx_vSL, pfx_CH_X, pfx_CU_X, pfx_FA_X, pfx_FC_X, 
         pfx_SI_X, pfx_SL_X, pfx_CH_Z, pfx_CU_Z, pfx_FA_Z, pfx_FC_Z, 
         pfx_SI_Z, pfx_SL_Z, pfx_vFS, pfx_FS_X, pfx_FS_Z, 
         pfx_vKC, pfx_KC_X, pfx_KC_Z, ff_avg_spin, si_avg_spin,
         fc_avg_spin, sl_avg_spin, ch_avg_spin, cu_avg_spin, fs_avg_spin, sp_s_FF)
write.csv(hgbc, "hgbc")

ggplot(cond_data, aes(x=sp_s_CU, y=sp_s_FF))+
  geom_point()
