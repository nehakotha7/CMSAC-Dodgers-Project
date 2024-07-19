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

#Why certain NAs here?
cond_data_longer <- list(cond_data_freq, cond_data_velo, cond_data_horizontal, 
                         cond_data_vertical, cond_data_spin, cond_data_stuff_plus) |> 
  reduce(left_join, by = c("Season", "xMLBAMID", "PlayerNameRoute", "Pitch_Type"))

#Experimenting with visualizations
library(ggplot2)
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
         
         #no knuckleball stuff+
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



# Random Forest -----------------------------------------------------------
library(tidyverse)
library(vip)
library(ranger)
theme_set(theme_light())
library(stacks)
library(Metrics)

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
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

# Calculate RMSE
rmse_value_changeup <- rmse(rf_results_changeup$sp_s_FF, rf_results_changeup$pred)
print(paste("RMSE for Changeup:", rmse_value_changeup))

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
rf_results <- rf_knucklecurve |> 
  mutate(pred = kc_predictions)

rf_knucklecurve |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_FS, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)


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
rf_results <- rf_splitter |> 
  mutate(pred = fs_predictions)

rf_splitter |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_KC, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)




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