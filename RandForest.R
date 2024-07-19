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
library(mgcv)

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
library(dplyr)
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



# Random Forest -----------------------------------------------------------
library(tidyverse)
library(vip)
library(ranger)
theme_set(theme_light())
library(stacks)
library(Metrics)
library(caret)


#PREDICTING FASTBALL STUFF+ --------------------

#Changeup
rf_changeup <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_changeup) <- gsub('-', '_', names(rf_changeup))

change_rf <- ranger(sp_s_FF ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_changeup, mtry = 2)
change_rf
vip_plot_changeup <- vip(change_rf)

vip_data_changeup <- vip_plot_changeup$data

vip_data_changeup$Variable <- recode(vip_data_changeup$Variable, 
                                     `pfx_vCH` = "Velocity",
                                     `ch_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_CH_X` = "Horizontal Movement",
                                     `pfx_CH_Z` = "Vertical Movement")

vip_plot_modified_changeup <- ggplot(vip_data_changeup, aes(x = Importance, y = reorder(Variables, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Changeup",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_changeup

ch_predictions <- change_rf$predictions

rf_results_changeup <- rf_changeup |> 
  mutate(pred = ch_predictions)

rf_changeup |>
  mutate(pred = ch_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_FF, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

# Create an empty data frame to store the RMSE values
rmse_table <- data.frame(
  Predicted_Pitch = character(),
  Predictor_Pitch = character(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

# Predicting Fastball Stuff+ with Changeup as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Fastball",
  Predictor_Pitch = "Changeup",
  RMSE = rmse_value_changeup
))


# Sinker
rf_sinker <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_sinker) <- gsub('-', '_', names(rf_sinker))

sinker_rf <- ranger(sp_s_FF ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_sinker, mtry = 3)
sinker_rf
vip_plot_sinker <- vip(sinker_rf)

vip_data_sinker <- vip_plot_sinker$data

vip_data_sinker$Variable <- recode(vip_data_sinker$Variable, 
                                   `pfx_vSI` = "Velocity",
                                   `si_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SI_X` = "Horizontal Movement",
                                   `pfx_SI_Z` = "Vertical Movement")

vip_plot_modified_sinker <- ggplot(vip_data_sinker, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Sinker",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_sinker

si_predictions <- sinker_rf$predictions

rf_results_sinker <- rf_sinker |> 
  mutate(pred = si_predictions)

rf_sinker |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_sinker <- rmse(rf_results_sinker$sp_s_FF, rf_results_sinker$pred)
print(paste("RMSE for Sinker:", rmse_value_sinker))

# Predicting Fastball Stuff+ with Sinker as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Fastball",
  Predictor_Pitch = "Sinker",
  RMSE = rmse_value_sinker
))


# Cutter
rf_cutter <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_cutter) <- gsub('-', '_', names(rf_cutter))

cutter_rf <- ranger(sp_s_FF ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_cutter, mtry = 2)
cutter_rf
vip_plot_cutter <- vip(cutter_rf)

vip_data_cutter <- vip_plot_cutter$data

vip_data_cutter$Variable <- recode(vip_data_cutter$Variable, 
                                   `pfx_vFC` = "Velocity",
                                   `fc_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FC_X` = "Horizontal Movement",
                                   `pfx_FC_Z` = "Vertical Movement")

vip_plot_modified_cutter <- ggplot(vip_data_cutter, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Cutter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_cutter

fc_predictions <- cutter_rf$predictions

rf_results_cutter <- rf_cutter |> 
  mutate(pred = fc_predictions)

rf_cutter |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_cutter <- rmse(rf_results_cutter$sp_s_FF, rf_results_cutter$pred)
print(paste("RMSE for Cutter:", rmse_value_cutter))

# Predicting Fastball Stuff+ with Cutter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Fastball",
  Predictor_Pitch = "Cutter",
  RMSE = rmse_value_cutter
))


# Slider
rf_slider <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_slider) <- gsub('-', '_', names(rf_slider))

slider_rf <- ranger(sp_s_FF ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_slider, mtry = 2)
slider_rf
vip_plot_slider <- vip(slider_rf)

vip_data_slider <- vip_plot_slider$data

vip_data_slider$Variable <- recode(vip_data_slider$Variable, 
                                   `pfx_vSL` = "Velocity",
                                   `sl_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SL_X` = "Horizontal Movement",
                                   `pfx_SL_Z` = "Vertical Movement")

vip_plot_modified_slider <- ggplot(vip_data_slider, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Slider",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_slider

sl_predictions <- slider_rf$predictions

rf_results_slider <- rf_slider |> 
  mutate(pred = sl_predictions)

rf_slider |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_slider <- rmse(rf_results_slider$sp_s_FF, rf_results_slider$pred)
print(paste("RMSE for Slider:", rmse_value_slider))

# Predicting Fastball Stuff+ with Slider as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Fastball",
  Predictor_Pitch = "Slider",
  RMSE = rmse_value_slider
))


# Curveball
rf_curveball <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_curveball) <- gsub('-', '_', names(rf_curveball))

curve_rf <- ranger(sp_s_FF ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_curveball, mtry = 2)
curve_rf
vip_plot_curveball <- vip(curve_rf)

vip_data_curveball <- vip_plot_curveball$data

vip_data_curveball$Variable <- recode(vip_data_curveball$Variable, 
                                      `pfx_vCU` = "Velocity",
                                      `cu_avg_spin` = "Spin Rate",
                                      `avg_release_extension` = "Avg Release Extension",
                                      `avg_rp_x` = "Avg Release Position X",
                                      `avg_rp_z` = "Avg Release Position Z", 
                                      `pfx_CU_X` = "Horizontal Movement",
                                      `pfx_CU_Z` = "Vertical Movement")

vip_plot_modified_curveball <- ggplot(vip_data_curveball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Curveball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_curveball

cu_predictions <- curve_rf$predictions

rf_results_curveball <- rf_curveball |> 
  mutate(pred = cu_predictions)

rf_curveball |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_curveball <- rmse(rf_results_curveball$sp_s_FF, rf_results_curveball$pred)
print(paste("RMSE for Curveball:", rmse_value_curveball))

# Predicting Fastball Stuff+ with Curveball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Fastball",
  Predictor_Pitch = "Curveball",
  RMSE = rmse_value_curveball
))


# Splitter
rf_splitter <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_splitter) <- gsub('-', '_', names(rf_splitter))

split_rf <- ranger(sp_s_FF ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_splitter, mtry = 2)
split_rf

vip_plot <- vip(split_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vFS` = "Velocity",
                            `fs_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_FS_X` = "Horizontal Movement",
                            `pfx_FS_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Splitter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_splitter <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_splitter <- rmse(rf_results_splitter$sp_s_FF, rf_results_splitter$pred)
print(paste("RMSE for Splitter:", rmse_value_splitter))

# Predicting Fastball Stuff+ with Splitter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Fastball",
  Predictor_Pitch = "Splitter",
  RMSE = rmse_value_splitter
))


#Knuckle Curve: no spin
rf_knucklecurve <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_knucklecurve) <- gsub('-', '_', names(rf_knucklecurve))

kc_rf <- ranger(sp_s_FF ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                + avg_release_extension + avg_rp_x + avg_rp_z, 
                num.trees = 500, importance = "impurity", data = rf_knucklecurve, mtry = 2)
kc_rf

vip_plot <- vip(kc_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vKC` = "Velocity",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_KC_X` = "Horizontal Movement",
                            `pfx_KC_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Knuckle Curve",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_knucklecurve <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)


rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_knucklecurve <- rmse(rf_results_knucklecurve$sp_s_FF, rf_results_knucklecurve$pred)
print(paste("RMSE for Knuckle Curve:", rmse_value_knucklecurve))

# Predicting Fastball Stuff+ with Knuckle Curve as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Fastball",
  Predictor_Pitch = "Knuckle Curve",
  RMSE = rmse_value_knucklecurve
))

print(rmse_table)

#PREDICTING SINKER STUFF+ --------------------

#Changeup
rf_changeup <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_SI)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI)
names(rf_changeup) <- gsub('-', '_', names(rf_changeup))

change_rf <- ranger(sp_s_SI ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_changeup, mtry=2)
change_rf
vip_plot_changeup <- vip(change_rf)

vip_data_changeup <- vip_plot_changeup$data

vip_data_changeup$Variable <- recode(vip_data_changeup$Variable, 
                                     `pfx_vCH` = "Velocity",
                                     `ch_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_CH_X` = "Horizontal Movement",
                                     `pfx_CH_Z` = "Vertical Movement")

vip_plot_modified_changeup <- ggplot(vip_data_changeup, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Changeup",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_changeup

ch_predictions <- change_rf$predictions

rf_results_changeup <- rf_changeup |> 
  mutate(pred = ch_predictions)

rf_changeup |>
  mutate(pred = ch_predictions) |>
  ggplot(aes(sp_s_SI, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Changeup RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_SI, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

# Predicting Sinker Stuff+ with Changeup as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Sinker",
  Predictor_Pitch = "Changeup",
  RMSE = rmse_value_changeup
))


# Fastball
rf_fastball <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  filter(!is.na(ff_avg_spin), !is.na(`pfx_FA-X`), !is.na(sp_s_SI)) |> 
  select(Season, PlayerNameRoute, pfx_FA_pct, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`,
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI)
names(rf_fastball) <- gsub('-', '_', names(rf_fastball))

fastball_rf <- ranger(sp_s_SI ~ pfx_vFA + ff_avg_spin + pfx_FA_X + pfx_FA_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_fastball, mtry = 3)
fastball_rf
vip_plot_fastball <- vip(fastball_rf)

vip_data_fastball <- vip_plot_fastball$data

vip_data_fastball$Variable <- recode(vip_data_fastball$Variable, 
                                   `pfx_vFA` = "Velocity",
                                   `ff_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FA_X` = "Horizontal Movement",
                                   `pfx_FA_Z` = "Vertical Movement")

vip_plot_modified_fastball <- ggplot(vip_data_fastball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Fastball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_fastball

ff_predictions <- fastball_rf$predictions

rf_results_fastball <- rf_fastball |> 
  mutate(pred = ff_predictions)

rf_fastball |>
  mutate(pred = ff_predictions) |>
  ggplot(aes(sp_s_SI, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Fastball RMSE
rmse_value_fastball <- rmse(rf_results_fastball$sp_s_SI, rf_results_fastball$pred)
print(paste("RMSE for Fastball:", rmse_value_fastball))

# Predicting Sinker Stuff+ with Fastball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Sinker",
  Predictor_Pitch = "Fastball",
  RMSE = rmse_value_fastball
))


# Cutter
rf_cutter <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_SI)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI)
names(rf_cutter) <- gsub('-', '_', names(rf_cutter))

cutter_rf <- ranger(sp_s_SI ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_cutter, mtry = 2)
cutter_rf
vip_plot_cutter <- vip(cutter_rf)

vip_data_cutter <- vip_plot_cutter$data

vip_data_cutter$Variable <- recode(vip_data_cutter$Variable, 
                                   `pfx_vFC` = "Velocity",
                                   `fc_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FC_X` = "Horizontal Movement",
                                   `pfx_FC_Z` = "Vertical Movement")

vip_plot_modified_cutter <- ggplot(vip_data_cutter, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Cutter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_cutter

fc_predictions <- cutter_rf$predictions

rf_results_cutter <- rf_cutter |> 
  mutate(pred = fc_predictions)

rf_cutter |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_SI, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Cutter RMSE
rmse_value_cutter <- rmse(rf_results_cutter$sp_s_SI, rf_results_cutter$pred)
print(paste("RMSE for Cutter:", rmse_value_cutter))

# Predicting Sinker Stuff+ with Cutter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Sinker",
  Predictor_Pitch = "Cutter",
  RMSE = rmse_value_cutter
))


# Slider
rf_slider <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_SI)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI)
names(rf_slider) <- gsub('-', '_', names(rf_slider))

slider_rf <- ranger(sp_s_SI ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_slider, mtry = 2)
slider_rf
vip_plot_slider <- vip(slider_rf)

vip_data_slider <- vip_plot_slider$data

vip_data_slider$Variable <- recode(vip_data_slider$Variable, 
                                   `pfx_vSL` = "Velocity",
                                   `sl_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SL_X` = "Horizontal Movement",
                                   `pfx_SL_Z` = "Vertical Movement")

vip_plot_modified_slider <- ggplot(vip_data_slider, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Slider",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_slider

sl_predictions <- slider_rf$predictions

rf_results_slider <- rf_slider |> 
  mutate(pred = sl_predictions)

rf_slider |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_SI, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Slider RMSE
rmse_value_slider <- rmse(rf_results_slider$sp_s_SI, rf_results_slider$pred)
print(paste("RMSE for Slider:", rmse_value_slider))

# Predicting Sinker Stuff+ with Slider as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Sinker",
  Predictor_Pitch = "Slider",
  RMSE = rmse_value_slider
))

# Curveball
rf_curveball <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_SI)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI)
names(rf_curveball) <- gsub('-', '_', names(rf_curveball))

curve_rf <- ranger(sp_s_SI ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_curveball, mtry = 2)
curve_rf
vip_plot_curveball <- vip(curve_rf)

vip_data_curveball <- vip_plot_curveball$data

vip_data_curveball$Variable <- recode(vip_data_curveball$Variable, 
                                      `pfx_vCU` = "Velocity",
                                      `cu_avg_spin` = "Spin Rate",
                                      `avg_release_extension` = "Avg Release Extension",
                                      `avg_rp_x` = "Avg Release Position X",
                                      `avg_rp_z` = "Avg Release Position Z", 
                                      `pfx_CU_X` = "Horizontal Movement",
                                      `pfx_CU_Z` = "Vertical Movement")

vip_plot_modified_curveball <- ggplot(vip_data_curveball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Curveball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_curveball

cu_predictions <- curve_rf$predictions

rf_results_curveball <- rf_curveball |> 
  mutate(pred = cu_predictions)

rf_curveball |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_SI, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Curveball RMSE
rmse_value_curveball <- rmse(rf_results_curveball$sp_s_SI, rf_results_curveball$pred)
print(paste("RMSE for Curveball:", rmse_value_curveball))

# Predicting Sinker Stuff+ with Curveball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Sinker",
  Predictor_Pitch = "Curveball",
  RMSE = rmse_value_curveball
))


# Splitter
rf_splitter <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_SI)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI)
names(rf_splitter) <- gsub('-', '_', names(rf_splitter))

split_rf <- ranger(sp_s_SI ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_splitter, mtry = 2)
split_rf

vip_plot <- vip(split_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vFS` = "Velocity",
                            `fs_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_FS_X` = "Horizontal Movement",
                            `pfx_FS_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Splitter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_splitter <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_SI, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Splitter RMSE
rmse_value_splitter <- rmse(rf_results_splitter$sp_s_SI, rf_results_splitter$pred)
print(paste("RMSE for Splitter:", rmse_value_splitter))

# Predicting Sinker Stuff+ with Splitter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Sinker",
  Predictor_Pitch = "Splitter",
  RMSE = rmse_value_splitter
))


#Knuckle Curve: no spin
rf_knucklecurve <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_SI)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI)
names(rf_knucklecurve) <- gsub('-', '_', names(rf_knucklecurve))

kc_rf <- ranger(sp_s_SI ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                + avg_release_extension + avg_rp_x + avg_rp_z, 
                num.trees = 500, importance = "impurity", data = rf_knucklecurve, mtry = 2)
kc_rf

vip_plot <- vip(kc_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vKC` = "Velocity",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_KC_X` = "Horizontal Movement",
                            `pfx_KC_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Knuckle Curve",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_knucklecurve <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)

rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_SI, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Knuckle Curve RMSE
rmse_value_knucklecurve <- rmse(rf_results_knucklecurve$sp_s_SI, rf_results_knucklecurve$pred)
print(paste("RMSE for Knuckle Curve:", rmse_value_knucklecurve))

# Predicting Sinker Stuff+ with Knuckle Curve as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Sinker",
  Predictor_Pitch = "Knuckle Curve",
  RMSE = rmse_value_knucklecurve
))



#PREDICTING CHANGEUP STUFF+ -----------------

# Fastball
rf_fastball <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  filter(!is.na(ff_avg_spin), !is.na(`pfx_FA-X`), !is.na(sp_s_CH)) |> 
  select(Season, PlayerNameRoute, pfx_FA_pct, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`,
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH)
names(rf_fastball) <- gsub('-', '_', names(rf_fastball))

fastball_rf <- ranger(sp_s_CH ~ pfx_vFA + ff_avg_spin + pfx_FA_X + pfx_FA_Z
                      + avg_release_extension + avg_rp_x + avg_rp_z, 
                      num.trees = 500, importance = "impurity", data = rf_fastball, mtry = 3)
fastball_rf
vip_plot_fastball <- vip(fastball_rf)

vip_data_fastball <- vip_plot_fastball$data

vip_data_fastball$Variable <- recode(vip_data_fastball$Variable, 
                                     `pfx_vFA` = "Velocity",
                                     `ff_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_FA_X` = "Horizontal Movement",
                                     `pfx_FA_Z` = "Vertical Movement")

vip_plot_modified_fastball <- ggplot(vip_data_fastball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Fastball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_fastball

ff_predictions <- fastball_rf$predictions

rf_results_fastball <- rf_fastball |> 
  mutate(pred = ff_predictions)

rf_fastball |>
  mutate(pred = ff_predictions) |>
  ggplot(aes(sp_s_CH, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Fastball RMSE
rmse_value_fastball <- rmse(rf_results_fastball$sp_s_CH, rf_results_fastball$pred)
print(paste("RMSE for Fastball:", rmse_value_fastball))

# Predicting Changeup Stuff+ with Fastball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Changeup",
  Predictor_Pitch = "Fastball",
  RMSE = rmse_value_fastball
))


#Sinker
rf_sinker <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_CH)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH)
names(rf_sinker) <- gsub('-', '_', names(rf_sinker))

sinker_rf <- ranger(sp_s_CH ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_sinker, mtry = 2)
sinker_rf
vip_plot_sinker <- vip(sinker_rf)

vip_data_sinker <- vip_plot_sinker$data

vip_data_sinker$Variable <- recode(vip_data_sinker$Variable, 
                                     `pfx_vSI` = "Velocity",
                                     `si_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_SI_X` = "Horizontal Movement",
                                     `pfx_SI_Z` = "Vertical Movement")

vip_plot_modified_sinker <- ggplot(vip_data_sinker, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Sinker",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_sinker

si_predictions <- sinker_rf$predictions

rf_results_sinker <- rf_sinker |> 
  mutate(pred = si_predictions)

rf_sinker |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_CH, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Sinker RMSE
rmse_value_sinker <- rmse(rf_results_sinker$sp_s_CH, rf_results_sinker$pred)
print(paste("RMSE for Sinker:", rmse_value_sinker))

# Predicting Changeup Stuff+ with Sinker as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Changeup",
  Predictor_Pitch = "Sinker",
  RMSE = rmse_value_sinker
))


# Cutter
rf_cutter <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_CH)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH)
names(rf_cutter) <- gsub('-', '_', names(rf_cutter))

cutter_rf <- ranger(sp_s_CH ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_cutter, mtry = 2)
cutter_rf
vip_plot_cutter <- vip(cutter_rf)

vip_data_cutter <- vip_plot_cutter$data

vip_data_cutter$Variable <- recode(vip_data_cutter$Variable, 
                                   `pfx_vFC` = "Velocity",
                                   `fc_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FC_X` = "Horizontal Movement",
                                   `pfx_FC_Z` = "Vertical Movement")

vip_plot_modified_cutter <- ggplot(vip_data_cutter, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Cutter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_cutter

fc_predictions <- cutter_rf$predictions

rf_results_cutter <- rf_cutter |> 
  mutate(pred = fc_predictions)

rf_cutter |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_CH, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Cutter RMSE
rmse_value_cutter <- rmse(rf_results_cutter$sp_s_CH, rf_results_cutter$pred)
print(paste("RMSE for Cutter:", rmse_value_cutter))

# Predicting Changeup Stuff+ with Cutter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Changeup",
  Predictor_Pitch = "Cutter",
  RMSE = rmse_value_cutter
))


# Slider
rf_slider <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_CH)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH)
names(rf_slider) <- gsub('-', '_', names(rf_slider))

slider_rf <- ranger(sp_s_CH ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_slider, mtry = 2)
slider_rf
vip_plot_slider <- vip(slider_rf)

vip_data_slider <- vip_plot_slider$data

vip_data_slider$Variable <- recode(vip_data_slider$Variable, 
                                   `pfx_vSL` = "Velocity",
                                   `sl_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SL_X` = "Horizontal Movement",
                                   `pfx_SL_Z` = "Vertical Movement")

vip_plot_modified_slider <- ggplot(vip_data_slider, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Slider",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_slider

sl_predictions <- slider_rf$predictions

rf_results_slider <- rf_slider |> 
  mutate(pred = sl_predictions)

rf_slider |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_CH, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Slider RMSE
rmse_value_slider <- rmse(rf_results_slider$sp_s_CH, rf_results_slider$pred)
print(paste("RMSE for Slider:", rmse_value_slider))

# Predicting Changeup Stuff+ with Slider as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Changeup",
  Predictor_Pitch = "Slider",
  RMSE = rmse_value_slider
))


# Curveball
rf_curveball <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_CH)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH)
names(rf_curveball) <- gsub('-', '_', names(rf_curveball))

curve_rf <- ranger(sp_s_CH ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_curveball, mtry = 2)
curve_rf
vip_plot_curveball <- vip(curve_rf)

vip_data_curveball <- vip_plot_curveball$data

vip_data_curveball$Variable <- recode(vip_data_curveball$Variable, 
                                      `pfx_vCU` = "Velocity",
                                      `cu_avg_spin` = "Spin Rate",
                                      `avg_release_extension` = "Avg Release Extension",
                                      `avg_rp_x` = "Avg Release Position X",
                                      `avg_rp_z` = "Avg Release Position Z", 
                                      `pfx_CU_X` = "Horizontal Movement",
                                      `pfx_CU_Z` = "Vertical Movement")

vip_plot_modified_curveball <- ggplot(vip_data_curveball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Curveball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_curveball

cu_predictions <- curve_rf$predictions

rf_results_curveball <- rf_curveball |> 
  mutate(pred = cu_predictions)

rf_curveball |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_CH, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Curveball RMSE
rmse_value_curveball <- rmse(rf_results_curveball$sp_s_CH, rf_results_curveball$pred)
print(paste("RMSE for Curveball:", rmse_value_curveball))

# Predicting Changeup Stuff+ with Curveball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Changeup",
  Predictor_Pitch = "Curveball",
  RMSE = rmse_value_curveball
))


# Splitter
rf_splitter <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_CH)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH)
names(rf_splitter) <- gsub('-', '_', names(rf_splitter))

split_rf <- ranger(sp_s_CH ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_splitter, mtry = 2)
split_rf

vip_plot <- vip(split_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vFS` = "Velocity",
                            `fs_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_FS_X` = "Horizontal Movement",
                            `pfx_FS_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Splitter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_splitter <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_CH, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Splitter RMSE
rmse_value_splitter <- rmse(rf_results_splitter$sp_s_CH, rf_results_splitter$pred)
print(paste("RMSE for Splitter:", rmse_value_splitter))

# Predicting Changeup Stuff+ with Splitter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Changeup",
  Predictor_Pitch = "Splitter",
  RMSE = rmse_value_splitter
))


#Knuckle Curve: no spin
rf_knucklecurve <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_CH)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CH)
names(rf_knucklecurve) <- gsub('-', '_', names(rf_knucklecurve))

kc_rf <- ranger(sp_s_CH ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                + avg_release_extension + avg_rp_x + avg_rp_z, 
                num.trees = 500, importance = "impurity", data = rf_knucklecurve, mtry = 2)
kc_rf

vip_plot <- vip(kc_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vKC` = "Velocity",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_KC_X` = "Horizontal Movement",
                            `pfx_KC_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Knuckle Curve",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_knucklecurve <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)

rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_CH, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Knuckle Curve RMSE
rmse_value_knucklecurve <- rmse(rf_results_knucklecurve$sp_s_CH, rf_results_knucklecurve$pred)
print(paste("RMSE for Knuckle Curve:", rmse_value_knucklecurve))

# Predicting Changeup Stuff+ with Knuckle Curve as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Changeup",
  Predictor_Pitch = "Knuckle Curve",
  RMSE = rmse_value_knucklecurve
))



#PREDICTING CUTTER STUFF+ --------------------

#Changeup
rf_changeup <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_FC)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC)
names(rf_changeup) <- gsub('-', '_', names(rf_changeup))

change_rf <- ranger(sp_s_FC ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_changeup, mtry=2)
change_rf
vip_plot_changeup <- vip(change_rf)

vip_data_changeup <- vip_plot_changeup$data

vip_data_changeup$Variable <- recode(vip_data_changeup$Variable, 
                                     `pfx_vCH` = "Velocity",
                                     `ch_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_CH_X` = "Horizontal Movement",
                                     `pfx_CH_Z` = "Vertical Movement")

vip_plot_modified_changeup <- ggplot(vip_data_changeup, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Changeup",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_changeup

ch_predictions <- change_rf$predictions

rf_results_changeup <- rf_changeup |> 
  mutate(pred = ch_predictions)

rf_changeup |>
  mutate(pred = ch_predictions) |>
  ggplot(aes(sp_s_FC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Changeup RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_FC, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

# Predicting Cutter Stuff+ with Changeup as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Cutter",
  Predictor_Pitch = "Changeup",
  RMSE = rmse_value_changeup
))


# Fastball
rf_fastball <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  filter(!is.na(ff_avg_spin), !is.na(`pfx_FA-X`), !is.na(sp_s_FC)) |> 
  select(Season, PlayerNameRoute, pfx_FA_pct, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`,
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC)
names(rf_fastball) <- gsub('-', '_', names(rf_fastball))

fastball_rf <- ranger(sp_s_FC ~ pfx_vFA + ff_avg_spin + pfx_FA_X + pfx_FA_Z
                      + avg_release_extension + avg_rp_x + avg_rp_z, 
                      num.trees = 500, importance = "impurity", data = rf_fastball, mtry = 3)
fastball_rf
vip_plot_fastball <- vip(fastball_rf)

vip_data_fastball <- vip_plot_fastball$data

vip_data_fastball$Variable <- recode(vip_data_fastball$Variable, 
                                     `pfx_vFA` = "Velocity",
                                     `ff_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_FA_X` = "Horizontal Movement",
                                     `pfx_FA_Z` = "Vertical Movement")

vip_plot_modified_fastball <- ggplot(vip_data_fastball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Fastball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_fastball

ff_predictions <- fastball_rf$predictions

rf_results_fastball <- rf_fastball |> 
  mutate(pred = ff_predictions)

rf_fastball |>
  mutate(pred = ff_predictions) |>
  ggplot(aes(sp_s_FC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Fastball RMSE
rmse_value_fastball <- rmse(rf_results_fastball$sp_s_FC, rf_results_fastball$pred)
print(paste("RMSE for Fastball:", rmse_value_fastball))

# Predicting Cutter Stuff+ with Fastball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Cutter",
  Predictor_Pitch = "Fastball",
  RMSE = rmse_value_fastball
))


#Sinker
rf_sinker <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_FC)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC)
names(rf_sinker) <- gsub('-', '_', names(rf_sinker))

sinker_rf <- ranger(sp_s_FC ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_sinker, mtry = 2)
sinker_rf
vip_plot_sinker <- vip(sinker_rf)

vip_data_sinker <- vip_plot_sinker$data

vip_data_sinker$Variable <- recode(vip_data_sinker$Variable, 
                                   `pfx_vSI` = "Velocity",
                                   `si_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SI_X` = "Horizontal Movement",
                                   `pfx_SI_Z` = "Vertical Movement")

vip_plot_modified_sinker <- ggplot(vip_data_sinker, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Sinker",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_sinker

si_predictions <- sinker_rf$predictions

rf_results_sinker <- rf_sinker |> 
  mutate(pred = si_predictions)

rf_sinker |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_FC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Sinker RMSE
rmse_value_sinker <- rmse(rf_results_sinker$sp_s_FC, rf_results_sinker$pred)
print(paste("RMSE for Sinker:", rmse_value_sinker))

# Predicting Cutter Stuff+ with Sinker as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Cutter",
  Predictor_Pitch = "Sinker",
  RMSE = rmse_value_sinker
))


# Slider
rf_slider <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_FC)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC)
names(rf_slider) <- gsub('-', '_', names(rf_slider))

slider_rf <- ranger(sp_s_FC ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_slider, mtry = 2)
slider_rf
vip_plot_slider <- vip(slider_rf)

vip_data_slider <- vip_plot_slider$data

vip_data_slider$Variable <- recode(vip_data_slider$Variable, 
                                   `pfx_vSL` = "Velocity",
                                   `sl_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SL_X` = "Horizontal Movement",
                                   `pfx_SL_Z` = "Vertical Movement")

vip_plot_modified_slider <- ggplot(vip_data_slider, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Slider",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_slider

sl_predictions <- slider_rf$predictions

rf_results_slider <- rf_slider |> 
  mutate(pred = sl_predictions)

rf_slider |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_FC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Slider RMSE
rmse_value_slider <- rmse(rf_results_slider$sp_s_FC, rf_results_slider$pred)
print(paste("RMSE for Slider:", rmse_value_slider))

# Predicting Cutter Stuff+ with Slider as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Cutter",
  Predictor_Pitch = "Slider",
  RMSE = rmse_value_slider
))


# Curveball
rf_curveball <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_FC)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC)
names(rf_curveball) <- gsub('-', '_', names(rf_curveball))

curve_rf <- ranger(sp_s_FC ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_curveball, mtry = 2)
curve_rf
vip_plot_curveball <- vip(curve_rf)

vip_data_curveball <- vip_plot_curveball$data

vip_data_curveball$Variable <- recode(vip_data_curveball$Variable, 
                                      `pfx_vCU` = "Velocity",
                                      `cu_avg_spin` = "Spin Rate",
                                      `avg_release_extension` = "Avg Release Extension",
                                      `avg_rp_x` = "Avg Release Position X",
                                      `avg_rp_z` = "Avg Release Position Z", 
                                      `pfx_CU_X` = "Horizontal Movement",
                                      `pfx_CU_Z` = "Vertical Movement")

vip_plot_modified_curveball <- ggplot(vip_data_curveball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Curveball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_curveball

cu_predictions <- curve_rf$predictions

rf_results_curveball <- rf_curveball |> 
  mutate(pred = cu_predictions)

rf_curveball |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_FC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Curveball RMSE
rmse_value_curveball <- rmse(rf_results_curveball$sp_s_FC, rf_results_curveball$pred)
print(paste("RMSE for Curveball:", rmse_value_curveball))

# Predicting Cutter Stuff+ with Curveball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Cutter",
  Predictor_Pitch = "Curveball",
  RMSE = rmse_value_curveball
))


# Splitter
rf_splitter <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_FC)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC)
names(rf_splitter) <- gsub('-', '_', names(rf_splitter))

split_rf <- ranger(sp_s_FC ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_splitter, mtry = 2)
split_rf

vip_plot <- vip(split_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vFS` = "Velocity",
                            `fs_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_FS_X` = "Horizontal Movement",
                            `pfx_FS_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Splitter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_splitter <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_FC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Splitter RMSE
rmse_value_splitter <- rmse(rf_results_splitter$sp_s_FC, rf_results_splitter$pred)
print(paste("RMSE for Splitter:", rmse_value_splitter))

# Predicting Cutter Stuff+ with Splitter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Cutter",
  Predictor_Pitch = "Splitter",
  RMSE = rmse_value_splitter
))


#Knuckle Curve: no spin
rf_knucklecurve <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_FC)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FC)
names(rf_knucklecurve) <- gsub('-', '_', names(rf_knucklecurve))

kc_rf <- ranger(sp_s_FC ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                + avg_release_extension + avg_rp_x + avg_rp_z, 
                num.trees = 500, importance = "impurity", data = rf_knucklecurve, mtry = 2)
kc_rf

vip_plot <- vip(kc_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vKC` = "Velocity",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_KC_X` = "Horizontal Movement",
                            `pfx_KC_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Knuckle Curve",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_knucklecurve <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)

rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_FC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Knuckle Curve RMSE
rmse_value_knucklecurve <- rmse(rf_results_knucklecurve$sp_s_FC, rf_results_knucklecurve$pred)
print(paste("RMSE for Knuckle Curve:", rmse_value_knucklecurve))

# Predicting Cutter Stuff+ with Knuckle Curve as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Cutter",
  Predictor_Pitch = "Knuckle Curve",
  RMSE = rmse_value_knucklecurve
))



#PREDICTING SLIDER STUFF+ --------------------

#Changeup
rf_changeup <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_SL)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL)
names(rf_changeup) <- gsub('-', '_', names(rf_changeup))

change_rf <- ranger(sp_s_SL ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_changeup, mtry=2)
change_rf
vip_plot_changeup <- vip(change_rf)

vip_data_changeup <- vip_plot_changeup$data

vip_data_changeup$Variable <- recode(vip_data_changeup$Variable, 
                                     `pfx_vCH` = "Velocity",
                                     `ch_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_CH_X` = "Horizontal Movement",
                                     `pfx_CH_Z` = "Vertical Movement")

vip_plot_modified_changeup <- ggplot(vip_data_changeup, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Changeup",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_changeup

ch_predictions <- change_rf$predictions

rf_results_changeup <- rf_changeup |> 
  mutate(pred = ch_predictions)

rf_changeup |>
  mutate(pred = ch_predictions) |>
  ggplot(aes(sp_s_SL, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Changeup RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_SL, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

# Predicting Slider Stuff+ with Changeup as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Slider",
  Predictor_Pitch = "Changeup",
  RMSE = rmse_value_changeup
))


# Fastball
rf_fastball <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  filter(!is.na(ff_avg_spin), !is.na(`pfx_FA-X`), !is.na(sp_s_SL)) |> 
  select(Season, PlayerNameRoute, pfx_FA_pct, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`,
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL)
names(rf_fastball) <- gsub('-', '_', names(rf_fastball))

fastball_rf <- ranger(sp_s_SL ~ pfx_vFA + ff_avg_spin + pfx_FA_X + pfx_FA_Z
                      + avg_release_extension + avg_rp_x + avg_rp_z, 
                      num.trees = 500, importance = "impurity", data = rf_fastball, mtry = 3)
fastball_rf
vip_plot_fastball <- vip(fastball_rf)

vip_data_fastball <- vip_plot_fastball$data

vip_data_fastball$Variable <- recode(vip_data_fastball$Variable, 
                                     `pfx_vFA` = "Velocity",
                                     `ff_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_FA_X` = "Horizontal Movement",
                                     `pfx_FA_Z` = "Vertical Movement")

vip_plot_modified_fastball <- ggplot(vip_data_fastball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Fastball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_fastball

ff_predictions <- fastball_rf$predictions

rf_results_fastball <- rf_fastball |> 
  mutate(pred = ff_predictions)

rf_fastball |>
  mutate(pred = ff_predictions) |>
  ggplot(aes(sp_s_SL, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Fastball RMSE
rmse_value_fastball <- rmse(rf_results_fastball$sp_s_SL, rf_results_fastball$pred)
print(paste("RMSE for Fastball:", rmse_value_fastball))

# Predicting Slider Stuff+ with Fastball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Slider",
  Predictor_Pitch = "Fastball",
  RMSE = rmse_value_fastball
))


#Sinker
rf_sinker <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_SL)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL)
names(rf_sinker) <- gsub('-', '_', names(rf_sinker))

sinker_rf <- ranger(sp_s_SL ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_sinker, mtry = 2)
sinker_rf
vip_plot_sinker <- vip(sinker_rf)

vip_data_sinker <- vip_plot_sinker$data

vip_data_sinker$Variable <- recode(vip_data_sinker$Variable, 
                                   `pfx_vSI` = "Velocity",
                                   `si_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SI_X` = "Horizontal Movement",
                                   `pfx_SI_Z` = "Vertical Movement")

vip_plot_modified_sinker <- ggplot(vip_data_sinker, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Sinker",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_sinker

si_predictions <- sinker_rf$predictions

rf_results_sinker <- rf_sinker |> 
  mutate(pred = si_predictions)

rf_sinker |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_SL, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Sinker RMSE
rmse_value_sinker <- rmse(rf_results_sinker$sp_s_SL, rf_results_sinker$pred)
print(paste("RMSE for Sinker:", rmse_value_sinker))

# Predicting Slider Stuff+ with Sinker as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Slider",
  Predictor_Pitch = "Sinker",
  RMSE = rmse_value_sinker
))


# Cutter
rf_cutter <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_SL)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL)
names(rf_cutter) <- gsub('-', '_', names(rf_cutter))

cutter_rf <- ranger(sp_s_SL ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_cutter, mtry = 2)
cutter_rf
vip_plot_cutter <- vip(cutter_rf)

vip_data_cutter <- vip_plot_cutter$data

vip_data_cutter$Variable <- recode(vip_data_cutter$Variable, 
                                   `pfx_vFC` = "Velocity",
                                   `fc_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FC_X` = "Horizontal Movement",
                                   `pfx_FC_Z` = "Vertical Movement")

vip_plot_modified_cutter <- ggplot(vip_data_cutter, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Cutter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_cutter

fc_predictions <- cutter_rf$predictions

rf_results_cutter <- rf_cutter |> 
  mutate(pred = fc_predictions)

rf_cutter |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_SL, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Cutter RMSE
rmse_value_cutter <- rmse(rf_results_cutter$sp_s_SL, rf_results_cutter$pred)
print(paste("RMSE for Cutter:", rmse_value_cutter))

# Predicting Slider Stuff+ with Cutter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Slider",
  Predictor_Pitch = "Cutter",
  RMSE = rmse_value_cutter
))


# Curveball
rf_curveball <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_SL)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL)
names(rf_curveball) <- gsub('-', '_', names(rf_curveball))

curve_rf <- ranger(sp_s_SL ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_curveball, mtry = 2)
curve_rf
vip_plot_curveball <- vip(curve_rf)

vip_data_curveball <- vip_plot_curveball$data

vip_data_curveball$Variable <- recode(vip_data_curveball$Variable, 
                                      `pfx_vCU` = "Velocity",
                                      `cu_avg_spin` = "Spin Rate",
                                      `avg_release_extension` = "Avg Release Extension",
                                      `avg_rp_x` = "Avg Release Position X",
                                      `avg_rp_z` = "Avg Release Position Z", 
                                      `pfx_CU_X` = "Horizontal Movement",
                                      `pfx_CU_Z` = "Vertical Movement")

vip_plot_modified_curveball <- ggplot(vip_data_curveball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Curveball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_curveball

cu_predictions <- curve_rf$predictions

rf_results_curveball <- rf_curveball |> 
  mutate(pred = cu_predictions)

rf_curveball |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_SL, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Curveball RMSE
rmse_value_curveball <- rmse(rf_results_curveball$sp_s_SL, rf_results_curveball$pred)
print(paste("RMSE for Curveball:", rmse_value_curveball))

# Predicting Slider Stuff+ with Curveball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Slider",
  Predictor_Pitch = "Curveball",
  RMSE = rmse_value_curveball
))


# Splitter
rf_splitter <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_SL)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL)
names(rf_splitter) <- gsub('-', '_', names(rf_splitter))

split_rf <- ranger(sp_s_SL ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_splitter, mtry = 2)
split_rf

vip_plot <- vip(split_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vFS` = "Velocity",
                            `fs_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_FS_X` = "Horizontal Movement",
                            `pfx_FS_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Splitter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_splitter <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_SL, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Splitter RMSE
rmse_value_splitter <- rmse(rf_results_splitter$sp_s_SL, rf_results_splitter$pred)
print(paste("RMSE for Splitter:", rmse_value_splitter))

# Predicting Slider Stuff+ with Splitter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Slider",
  Predictor_Pitch = "Splitter",
  RMSE = rmse_value_splitter
))


#Knuckle Curve: no spin
rf_knucklecurve <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_SL)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SL)
names(rf_knucklecurve) <- gsub('-', '_', names(rf_knucklecurve))

kc_rf <- ranger(sp_s_SL ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                + avg_release_extension + avg_rp_x + avg_rp_z, 
                num.trees = 500, importance = "impurity", data = rf_knucklecurve, mtry = 2)
kc_rf

vip_plot <- vip(kc_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vKC` = "Velocity",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_KC_X` = "Horizontal Movement",
                            `pfx_KC_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Knuckle Curve",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_knucklecurve <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)

rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_SL, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Knuckle Curve RMSE
rmse_value_knucklecurve <- rmse(rf_results_knucklecurve$sp_s_SL, rf_results_knucklecurve$pred)
print(paste("RMSE for Knuckle Curve:", rmse_value_knucklecurve))

# Predicting Slider Stuff+ with Knuckle Curve as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Slider",
  Predictor_Pitch = "Knuckle Curve",
  RMSE = rmse_value_knucklecurve
))



#PREDICTING CURVEBALL STUFF+ --------------------

#Changeup
rf_changeup <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_CU)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU)
names(rf_changeup) <- gsub('-', '_', names(rf_changeup))

change_rf <- ranger(sp_s_CU ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_changeup, mtry=2)
change_rf
vip_plot_changeup <- vip(change_rf)

vip_data_changeup <- vip_plot_changeup$data

vip_data_changeup$Variable <- recode(vip_data_changeup$Variable, 
                                     `pfx_vCH` = "Velocity",
                                     `ch_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_CH_X` = "Horizontal Movement",
                                     `pfx_CH_Z` = "Vertical Movement")

vip_plot_modified_changeup <- ggplot(vip_data_changeup, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Changeup",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_changeup

ch_predictions <- change_rf$predictions

rf_results_changeup <- rf_changeup |> 
  mutate(pred = ch_predictions)

rf_changeup |>
  mutate(pred = ch_predictions) |>
  ggplot(aes(sp_s_CU, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Changeup RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_CU, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

# Predicting Curveball Stuff+ with Changeup as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Curveball",
  Predictor_Pitch = "Changeup",
  RMSE = rmse_value_changeup
))


# Fastball
rf_fastball <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  filter(!is.na(ff_avg_spin), !is.na(`pfx_FA-X`), !is.na(sp_s_CU)) |> 
  select(Season, PlayerNameRoute, pfx_FA_pct, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`,
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU)
names(rf_fastball) <- gsub('-', '_', names(rf_fastball))

fastball_rf <- ranger(sp_s_CU ~ pfx_vFA + ff_avg_spin + pfx_FA_X + pfx_FA_Z
                      + avg_release_extension + avg_rp_x + avg_rp_z, 
                      num.trees = 500, importance = "impurity", data = rf_fastball, mtry = 3)
fastball_rf
vip_plot_fastball <- vip(fastball_rf)

vip_data_fastball <- vip_plot_fastball$data

vip_data_fastball$Variable <- recode(vip_data_fastball$Variable, 
                                     `pfx_vFA` = "Velocity",
                                     `ff_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_FA_X` = "Horizontal Movement",
                                     `pfx_FA_Z` = "Vertical Movement")

vip_plot_modified_fastball <- ggplot(vip_data_fastball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Fastball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_fastball

ff_predictions <- fastball_rf$predictions

rf_results_fastball <- rf_fastball |> 
  mutate(pred = ff_predictions)

rf_fastball |>
  mutate(pred = ff_predictions) |>
  ggplot(aes(sp_s_CU, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Fastball RMSE
rmse_value_fastball <- rmse(rf_results_fastball$sp_s_CU, rf_results_fastball$pred)
print(paste("RMSE for Fastball:", rmse_value_fastball))

# Predicting Curveball Stuff+ with Fastball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Curveball",
  Predictor_Pitch = "Fastball",
  RMSE = rmse_value_fastball
))


#Sinker
rf_sinker <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_CU)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU)
names(rf_sinker) <- gsub('-', '_', names(rf_sinker))

sinker_rf <- ranger(sp_s_CU ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_sinker, mtry = 2)
sinker_rf
vip_plot_sinker <- vip(sinker_rf)

vip_data_sinker <- vip_plot_sinker$data

vip_data_sinker$Variable <- recode(vip_data_sinker$Variable, 
                                   `pfx_vSI` = "Velocity",
                                   `si_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SI_X` = "Horizontal Movement",
                                   `pfx_SI_Z` = "Vertical Movement")

vip_plot_modified_sinker <- ggplot(vip_data_sinker, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Sinker",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_sinker

si_predictions <- sinker_rf$predictions

rf_results_sinker <- rf_sinker |> 
  mutate(pred = si_predictions)

rf_sinker |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_CU, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Sinker RMSE
rmse_value_sinker <- rmse(rf_results_sinker$sp_s_CU, rf_results_sinker$pred)
print(paste("RMSE for Sinker:", rmse_value_sinker))

# Predicting Curveball Stuff+ with Sinker as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Curveball",
  Predictor_Pitch = "Sinker",
  RMSE = rmse_value_sinker
))


# Cutter
rf_cutter <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_CU)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU)
names(rf_cutter) <- gsub('-', '_', names(rf_cutter))

cutter_rf <- ranger(sp_s_CU ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_cutter, mtry = 2)
cutter_rf
vip_plot_cutter <- vip(cutter_rf)

vip_data_cutter <- vip_plot_cutter$data

vip_data_cutter$Variable <- recode(vip_data_cutter$Variable, 
                                   `pfx_vFC` = "Velocity",
                                   `fc_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FC_X` = "Horizontal Movement",
                                   `pfx_FC_Z` = "Vertical Movement")

vip_plot_modified_cutter <- ggplot(vip_data_cutter, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Cutter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_cutter

fc_predictions <- cutter_rf$predictions

rf_results_cutter <- rf_cutter |> 
  mutate(pred = fc_predictions)

rf_cutter |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_CU, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Cutter RMSE
rmse_value_cutter <- rmse(rf_results_cutter$sp_s_CU, rf_results_cutter$pred)
print(paste("RMSE for Cutter:", rmse_value_cutter))

# Predicting Curveball Stuff+ with Cutter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Curveball",
  Predictor_Pitch = "Cutter",
  RMSE = rmse_value_cutter
))


# Slider
rf_slider <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_CU)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU)
names(rf_slider) <- gsub('-', '_', names(rf_slider))

slider_rf <- ranger(sp_s_CU ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_slider, mtry = 2)
slider_rf
vip_plot_slider <- vip(slider_rf)

vip_data_slider <- vip_plot_slider$data

vip_data_slider$Variable <- recode(vip_data_slider$Variable, 
                                   `pfx_vSL` = "Velocity",
                                   `sl_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SL_X` = "Horizontal Movement",
                                   `pfx_SL_Z` = "Vertical Movement")

vip_plot_modified_slider <- ggplot(vip_data_slider, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Slider",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_slider

sl_predictions <- slider_rf$predictions

rf_results_slider <- rf_slider |> 
  mutate(pred = sl_predictions)

rf_slider |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_CU, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Slider RMSE
rmse_value_slider <- rmse(rf_results_slider$sp_s_CU, rf_results_slider$pred)
print(paste("RMSE for Slider:", rmse_value_slider))
# Predicting Curveball Stuff+ with Slider as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Curveball",
  Predictor_Pitch = "Slider",
  RMSE = rmse_value_slider
))


# Splitter
rf_splitter <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_CU)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU)
names(rf_splitter) <- gsub('-', '_', names(rf_splitter))

split_rf <- ranger(sp_s_CU ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_splitter, mtry = 2)
split_rf

vip_plot <- vip(split_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vFS` = "Velocity",
                            `fs_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_FS_X` = "Horizontal Movement",
                            `pfx_FS_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Splitter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_splitter <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_CU, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Splitter RMSE
rmse_value_splitter <- rmse(rf_results_splitter$sp_s_CU, rf_results_splitter$pred)
print(paste("RMSE for Splitter:", rmse_value_splitter))

# Predicting Curveball Stuff+ with Splitter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Curveball",
  Predictor_Pitch = "Splitter",
  RMSE = rmse_value_splitter
))


#Knuckle Curve: no spin
rf_knucklecurve <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_CU)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_CU)
names(rf_knucklecurve) <- gsub('-', '_', names(rf_knucklecurve))

kc_rf <- ranger(sp_s_CU ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                + avg_release_extension + avg_rp_x + avg_rp_z, 
                num.trees = 500, importance = "impurity", data = rf_knucklecurve, mtry = 2)
kc_rf

vip_plot <- vip(kc_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vKC` = "Velocity",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_KC_X` = "Horizontal Movement",
                            `pfx_KC_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Knuckle Curve",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_knucklecurve <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)

rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_CU, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Knuckle Curve RMSE
rmse_value_knucklecurve <- rmse(rf_results_knucklecurve$sp_s_CU, rf_results_knucklecurve$pred)
print(paste("RMSE for Knuckle Curve:", rmse_value_knucklecurve))

# Predicting Curveball Stuff+ with Knuckle Curve as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Curveball",
  Predictor_Pitch = "Knuckle Curve",
  RMSE = rmse_value_knucklecurve
))


#PREDICTING SPLITTER STUFF+ --------------------

#Changeup
rf_changeup <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_FS)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS)
names(rf_changeup) <- gsub('-', '_', names(rf_changeup))

change_rf <- ranger(sp_s_FS ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_changeup, mtry=2)
change_rf
vip_plot_changeup <- vip(change_rf)

vip_data_changeup <- vip_plot_changeup$data

vip_data_changeup$Variable <- recode(vip_data_changeup$Variable, 
                                     `pfx_vCH` = "Velocity",
                                     `ch_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_CH_X` = "Horizontal Movement",
                                     `pfx_CH_Z` = "Vertical Movement")

vip_plot_modified_changeup <- ggplot(vip_data_changeup, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Changeup",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_changeup

ch_predictions <- change_rf$predictions

rf_results_changeup <- rf_changeup |> 
  mutate(pred = ch_predictions)

rf_changeup |>
  mutate(pred = ch_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Changeup RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_FS, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

# Predicting Splitter Stuff+ with Changeup as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Splitter",
  Predictor_Pitch = "Changeup",
  RMSE = rmse_value_changeup
))


# Fastball
rf_fastball <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  filter(!is.na(ff_avg_spin), !is.na(`pfx_FA-X`), !is.na(sp_s_FS)) |> 
  select(Season, PlayerNameRoute, pfx_FA_pct, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`,
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS)
names(rf_fastball) <- gsub('-', '_', names(rf_fastball))

fastball_rf <- ranger(sp_s_FS ~ pfx_vFA + ff_avg_spin + pfx_FA_X + pfx_FA_Z
                      + avg_release_extension + avg_rp_x + avg_rp_z, 
                      num.trees = 500, importance = "impurity", data = rf_fastball, mtry = 3)
fastball_rf
vip_plot_fastball <- vip(fastball_rf)

vip_data_fastball <- vip_plot_fastball$data

vip_data_fastball$Variable <- recode(vip_data_fastball$Variable, 
                                     `pfx_vFA` = "Velocity",
                                     `ff_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_FA_X` = "Horizontal Movement",
                                     `pfx_FA_Z` = "Vertical Movement")

vip_plot_modified_fastball <- ggplot(vip_data_fastball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Fastball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_fastball

ff_predictions <- fastball_rf$predictions

rf_results_fastball <- rf_fastball |> 
  mutate(pred = ff_predictions)

rf_fastball |>
  mutate(pred = ff_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Fastball RMSE
rmse_value_fastball <- rmse(rf_results_fastball$sp_s_FS, rf_results_fastball$pred)
print(paste("RMSE for Fastball:", rmse_value_fastball))

# Predicting Splitter Stuff+ with Fastball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Splitter",
  Predictor_Pitch = "Fastball",
  RMSE = rmse_value_fastball
))


#Sinker
rf_sinker <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_FS)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS)
names(rf_sinker) <- gsub('-', '_', names(rf_sinker))

sinker_rf <- ranger(sp_s_FS ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_sinker, mtry = 2)
sinker_rf
vip_plot_sinker <- vip(sinker_rf)

vip_data_sinker <- vip_plot_sinker$data

vip_data_sinker$Variable <- recode(vip_data_sinker$Variable, 
                                   `pfx_vSI` = "Velocity",
                                   `si_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SI_X` = "Horizontal Movement",
                                   `pfx_SI_Z` = "Vertical Movement")

vip_plot_modified_sinker <- ggplot(vip_data_sinker, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Sinker",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_sinker

si_predictions <- sinker_rf$predictions

rf_results_sinker <- rf_sinker |> 
  mutate(pred = si_predictions)

rf_sinker |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Sinker RMSE
rmse_value_sinker <- rmse(rf_results_sinker$sp_s_FS, rf_results_sinker$pred)
print(paste("RMSE for Sinker:", rmse_value_sinker))

# Predicting Splitter Stuff+ with Sinker as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Splitter",
  Predictor_Pitch = "Sinker",
  RMSE = rmse_value_sinker
))


# Cutter
rf_cutter <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_FS)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS)
names(rf_cutter) <- gsub('-', '_', names(rf_cutter))

cutter_rf <- ranger(sp_s_FS ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_cutter, mtry = 2)
cutter_rf
vip_plot_cutter <- vip(cutter_rf)

vip_data_cutter <- vip_plot_cutter$data

vip_data_cutter$Variable <- recode(vip_data_cutter$Variable, 
                                   `pfx_vFC` = "Velocity",
                                   `fc_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FC_X` = "Horizontal Movement",
                                   `pfx_FC_Z` = "Vertical Movement")

vip_plot_modified_cutter <- ggplot(vip_data_cutter, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Cutter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_cutter

fc_predictions <- cutter_rf$predictions

rf_results_cutter <- rf_cutter |> 
  mutate(pred = fc_predictions)

rf_cutter |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Cutter RMSE
rmse_value_cutter <- rmse(rf_results_cutter$sp_s_FS, rf_results_cutter$pred)
print(paste("RMSE for Cutter:", rmse_value_cutter))

# Predicting Splitter Stuff+ with Cutter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Splitter",
  Predictor_Pitch = "Cutter",
  RMSE = rmse_value_cutter
))


# Slider
rf_slider <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_FS)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS)
names(rf_slider) <- gsub('-', '_', names(rf_slider))

slider_rf <- ranger(sp_s_FS ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_slider, mtry = 2)
slider_rf
vip_plot_slider <- vip(slider_rf)

vip_data_slider <- vip_plot_slider$data

vip_data_slider$Variable <- recode(vip_data_slider$Variable, 
                                   `pfx_vSL` = "Velocity",
                                   `sl_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SL_X` = "Horizontal Movement",
                                   `pfx_SL_Z` = "Vertical Movement")

vip_plot_modified_slider <- ggplot(vip_data_slider, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Slider",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_slider

sl_predictions <- slider_rf$predictions

rf_results_slider <- rf_slider |> 
  mutate(pred = sl_predictions)

rf_slider |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Slider RMSE
rmse_value_slider <- rmse(rf_results_slider$sp_s_FS, rf_results_slider$pred)
print(paste("RMSE for Slider:", rmse_value_slider))

# Predicting Splitter Stuff+ with Slider as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Splitter",
  Predictor_Pitch = "Slider",
  RMSE = rmse_value_slider
))


# Curveball
rf_curveball <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_FS)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS)
names(rf_curveball) <- gsub('-', '_', names(rf_curveball))

curve_rf <- ranger(sp_s_FS ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_curveball, mtry = 2)
curve_rf
vip_plot_curveball <- vip(curve_rf)

vip_data_curveball <- vip_plot_curveball$data

vip_data_curveball$Variable <- recode(vip_data_curveball$Variable, 
                                      `pfx_vCU` = "Velocity",
                                      `cu_avg_spin` = "Spin Rate",
                                      `avg_release_extension` = "Avg Release Extension",
                                      `avg_rp_x` = "Avg Release Position X",
                                      `avg_rp_z` = "Avg Release Position Z", 
                                      `pfx_CU_X` = "Horizontal Movement",
                                      `pfx_CU_Z` = "Vertical Movement")

vip_plot_modified_curveball <- ggplot(vip_data_curveball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Curveball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_curveball

cu_predictions <- curve_rf$predictions

rf_results_curveball <- rf_curveball |> 
  mutate(pred = cu_predictions)

rf_curveball |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Curveball RMSE
rmse_value_curveball <- rmse(rf_results_curveball$sp_s_FS, rf_results_curveball$pred)
print(paste("RMSE for Curveball:", rmse_value_curveball))

# Predicting Splitter Stuff+ with Curveball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Splitter",
  Predictor_Pitch = "Curveball",
  RMSE = rmse_value_curveball
))


#Knuckle Curve: no spin
rf_knucklecurve <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_FS)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FS)
names(rf_knucklecurve) <- gsub('-', '_', names(rf_knucklecurve))

kc_rf <- ranger(sp_s_FS ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                + avg_release_extension + avg_rp_x + avg_rp_z, 
                num.trees = 500, importance = "impurity", data = rf_knucklecurve, mtry = 2)
kc_rf

vip_plot <- vip(kc_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vKC` = "Velocity",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_KC_X` = "Horizontal Movement",
                            `pfx_KC_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Knuckle Curve",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_knucklecurve <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)

rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Knuckle Curve RMSE
rmse_value_knucklecurve <- rmse(rf_results_knucklecurve$sp_s_FS, rf_results_knucklecurve$pred)
print(paste("RMSE for Knuckle Curve:", rmse_value_knucklecurve))

# Predicting Splitter Stuff+ with Knuckle Curve as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Splitter",
  Predictor_Pitch = "Knuckle Curve",
  RMSE = rmse_value_knucklecurve
))


#PREDICTING KNUCKLE CURVE STUFF+ -------------------

#Changeup
rf_changeup <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_KC)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC)
names(rf_changeup) <- gsub('-', '_', names(rf_changeup))

change_rf <- ranger(sp_s_KC ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_changeup, mtry=2)
change_rf
vip_plot_changeup <- vip(change_rf)

vip_data_changeup <- vip_plot_changeup$data

vip_data_changeup$Variable <- recode(vip_data_changeup$Variable, 
                                     `pfx_vCH` = "Velocity",
                                     `ch_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_CH_X` = "Horizontal Movement",
                                     `pfx_CH_Z` = "Vertical Movement")

vip_plot_modified_changeup <- ggplot(vip_data_changeup, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Changeup",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_changeup

ch_predictions <- change_rf$predictions

rf_results_changeup <- rf_changeup |> 
  mutate(pred = ch_predictions)

rf_changeup |>
  mutate(pred = ch_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Changeup RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_KC, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

# Predicting Knuckle Curve Stuff+ with Changeup as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Knuckle Curve",
  Predictor_Pitch = "Changeup",
  RMSE = rmse_value_changeup
))


# Fastball
rf_fastball <- cond_data |> 
  filter(ind_fastball == "Yes") |> 
  filter(!is.na(ff_avg_spin), !is.na(`pfx_FA-X`), !is.na(sp_s_KC)) |> 
  select(Season, PlayerNameRoute, pfx_FA_pct, pfx_vFA, `pfx_FA-X`, `pfx_FA-Z`,
         ff_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC)
names(rf_fastball) <- gsub('-', '_', names(rf_fastball))

fastball_rf <- ranger(sp_s_KC ~ pfx_vFA + ff_avg_spin + pfx_FA_X + pfx_FA_Z
                      + avg_release_extension + avg_rp_x + avg_rp_z, 
                      num.trees = 500, importance = "impurity", data = rf_fastball, mtry = 3)
fastball_rf
vip_plot_fastball <- vip(fastball_rf)

vip_data_fastball <- vip_plot_fastball$data

vip_data_fastball$Variable <- recode(vip_data_fastball$Variable, 
                                     `pfx_vFA` = "Velocity",
                                     `ff_avg_spin` = "Spin Rate",
                                     `avg_release_extension` = "Avg Release Extension",
                                     `avg_rp_x` = "Avg Release Position X",
                                     `avg_rp_z` = "Avg Release Position Z", 
                                     `pfx_FA_X` = "Horizontal Movement",
                                     `pfx_FA_Z` = "Vertical Movement")

vip_plot_modified_fastball <- ggplot(vip_data_fastball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Fastball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_fastball

ff_predictions <- fastball_rf$predictions

rf_results_fastball <- rf_fastball |> 
  mutate(pred = ff_predictions)

rf_fastball |>
  mutate(pred = ff_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Fastball RMSE
rmse_value_fastball <- rmse(rf_results_fastball$sp_s_KC, rf_results_fastball$pred)
print(paste("RMSE for Fastball:", rmse_value_fastball))

# Predicting Knuckle Curve Stuff+ with Fastball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Knuckle Curve",
  Predictor_Pitch = "Fastball",
  RMSE = rmse_value_fastball
))


#Sinker
rf_sinker <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_KC)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC)
names(rf_sinker) <- gsub('-', '_', names(rf_sinker))

sinker_rf <- ranger(sp_s_KC ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_sinker, mtry = 2)
sinker_rf
vip_plot_sinker <- vip(sinker_rf)

vip_data_sinker <- vip_plot_sinker$data

vip_data_sinker$Variable <- recode(vip_data_sinker$Variable, 
                                   `pfx_vSI` = "Velocity",
                                   `si_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SI_X` = "Horizontal Movement",
                                   `pfx_SI_Z` = "Vertical Movement")

vip_plot_modified_sinker <- ggplot(vip_data_sinker, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Sinker",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_sinker

si_predictions <- sinker_rf$predictions

rf_results_sinker <- rf_sinker |> 
  mutate(pred = si_predictions)

rf_sinker |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Sinker RMSE
rmse_value_sinker <- rmse(rf_results_sinker$sp_s_KC, rf_results_sinker$pred)
print(paste("RMSE for Sinker:", rmse_value_sinker))

# Predicting Knuckle Curve Stuff+ with Sinker as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Knuckle Curve",
  Predictor_Pitch = "Sinker",
  RMSE = rmse_value_sinker
))


# Cutter
rf_cutter <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_KC)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC)
names(rf_cutter) <- gsub('-', '_', names(rf_cutter))

cutter_rf <- ranger(sp_s_KC ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_cutter, mtry = 2)
cutter_rf
vip_plot_cutter <- vip(cutter_rf)

vip_data_cutter <- vip_plot_cutter$data

vip_data_cutter$Variable <- recode(vip_data_cutter$Variable, 
                                   `pfx_vFC` = "Velocity",
                                   `fc_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_FC_X` = "Horizontal Movement",
                                   `pfx_FC_Z` = "Vertical Movement")

vip_plot_modified_cutter <- ggplot(vip_data_cutter, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Cutter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_cutter

fc_predictions <- cutter_rf$predictions

rf_results_cutter <- rf_cutter |> 
  mutate(pred = fc_predictions)

rf_cutter |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Cutter RMSE
rmse_value_cutter <- rmse(rf_results_cutter$sp_s_KC, rf_results_cutter$pred)
print(paste("RMSE for Cutter:", rmse_value_cutter))

# Predicting Knuckle Curve Stuff+ with Cutter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Knuckle Curve",
  Predictor_Pitch = "Cutter",
  RMSE = rmse_value_cutter
))


# Slider
rf_slider <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_KC)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC)
names(rf_slider) <- gsub('-', '_', names(rf_slider))

slider_rf <- ranger(sp_s_KC ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_slider, mtry = 2)
slider_rf
vip_plot_slider <- vip(slider_rf)

vip_data_slider <- vip_plot_slider$data

vip_data_slider$Variable <- recode(vip_data_slider$Variable, 
                                   `pfx_vSL` = "Velocity",
                                   `sl_avg_spin` = "Spin Rate",
                                   `avg_release_extension` = "Avg Release Extension",
                                   `avg_rp_x` = "Avg Release Position X",
                                   `avg_rp_z` = "Avg Release Position Z", 
                                   `pfx_SL_X` = "Horizontal Movement",
                                   `pfx_SL_Z` = "Vertical Movement")

vip_plot_modified_slider <- ggplot(vip_data_slider, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Slider",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_slider

sl_predictions <- slider_rf$predictions

rf_results_slider <- rf_slider |> 
  mutate(pred = sl_predictions)

rf_slider |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Slider RMSE
rmse_value_slider <- rmse(rf_results_slider$sp_s_KC, rf_results_slider$pred)
print(paste("RMSE for Slider:", rmse_value_slider))

# Predicting Knuckle Curve Stuff+ with Slider as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Knuckle Curve",
  Predictor_Pitch = "Slider",
  RMSE = rmse_value_slider
))


# Curveball
rf_curveball <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_KC)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC)
names(rf_curveball) <- gsub('-', '_', names(rf_curveball))

curve_rf <- ranger(sp_s_KC ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_curveball, mtry = 2)
curve_rf
vip_plot_curveball <- vip(curve_rf)

vip_data_curveball <- vip_plot_curveball$data

vip_data_curveball$Variable <- recode(vip_data_curveball$Variable, 
                                      `pfx_vCU` = "Velocity",
                                      `cu_avg_spin` = "Spin Rate",
                                      `avg_release_extension` = "Avg Release Extension",
                                      `avg_rp_x` = "Avg Release Position X",
                                      `avg_rp_z` = "Avg Release Position Z", 
                                      `pfx_CU_X` = "Horizontal Movement",
                                      `pfx_CU_Z` = "Vertical Movement")

vip_plot_modified_curveball <- ggplot(vip_data_curveball, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Curveball",
    subtitle = "Random Forest Model"
  )

vip_plot_modified_curveball

cu_predictions <- curve_rf$predictions

rf_results_curveball <- rf_curveball |> 
  mutate(pred = cu_predictions)

rf_curveball |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Curveball RMSE
rmse_value_curveball <- rmse(rf_results_curveball$sp_s_KC, rf_results_curveball$pred)
print(paste("RMSE for Curveball:", rmse_value_curveball))

# Predicting Knuckle Curve Stuff+ with Curveball as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Knuckle Curve",
  Predictor_Pitch = "Curveball",
  RMSE = rmse_value_curveball
))


# Splitter
rf_splitter <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_KC)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_KC)
names(rf_splitter) <- gsub('-', '_', names(rf_splitter))

split_rf <- ranger(sp_s_KC ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                   + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_splitter, mtry = 2)
split_rf

vip_plot <- vip(split_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vFS` = "Velocity",
                            `fs_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_FS_X` = "Horizontal Movement",
                            `pfx_FS_Z` = "Vertical Movement",
)

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot: Splitter",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results_splitter <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate Splitter RMSE
rmse_value_splitter <- rmse(rf_results_splitter$sp_s_KC, rf_results_splitter$pred)
print(paste("RMSE for Splitter:", rmse_value_splitter))

# Predicting Knuckle Curve Stuff+ with Splitter as predictor
# Add the RMSE value to the table
rmse_table <- rbind(rmse_table, data.frame(
  Predicted_Pitch = "Knuckle Curve",
  Predictor_Pitch = "Splitter",
  RMSE = rmse_value_splitter
))



#Dodged Bar Plot for RMSE
rmse_data = data.frame(Linear_Regression = c(14.19, 15.7, 17.76, 17.2, 16.39, 
                                             15.79, 17.28),
                       Random_Forest = c(13.14, 15.52, 16.69, 16.27, 15.51, 
                                         15.09, 15.84),
                       row.names = c("Sinker", "Cutter", "Slider", "Curveball",
                                     "Changeup", "Splitter", "Knuckle Curve"))
# Sample Data
rmse_data <- data.frame(
  Pitch_Type = rep(c("Sinker", "Cutter", "Slider", "Curveball", "Changeup",
                     "Splitter", "Knuckle Curve"), times = 2),
  RMSE = c(14.19, 15.7, 17.76, 17.2, 16.39, 15.79, 17.28, 13.14, 15.52, 16.69, 
           16.27, 15.51, 15.09, 15.84),
  Method = rep(c("Linear Regression", "Random Forest"), each = 7)
)

# Customize the order of the factors
rmse_data <- rmse_data |>
  mutate(Pitch_Type = factor(Pitch_Type, levels = c("Sinker", "Splitter", "Changeup",
                                                    "Cutter", "Knuckle Curve", 
                                                    "Curveball", "Slider")))

ggplot(rmse_data, aes(x=Pitch_Type, y=RMSE, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), alpha = 0.7) +
  labs(title = "RMSE by Pitch Type and Modeling Method",
       x = "Pitch Type",
       y = "RMSE") +
  theme_bw() +
  scale_fill_manual(values = c("Random Forest" = "blue", "Linear Regression" = "red")) # Customize colors



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