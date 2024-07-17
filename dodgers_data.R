
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

#Pivot Tables
cond_data_freq <- cond_data |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_FA_pct, pfx_SI_pct, pfx_FC_pct, 
         pfx_SL_pct, pfx_CH_pct, pfx_CU_pct, pfx_FS_pct, pfx_KC_pct, pfx_KN_pct, 
         pfx_SC_pct, pfx_FO_pct) |> 
  pivot_longer(cols = contains("FA") | contains("SI") | contains("FC") | 
                 contains("SL") | contains("CH") | contains("CU") | contains("FS") |
                 contains("KC") | contains("KN") | contains("SC") | contains("FO"),
               names_to = "Pitch_Type",
               values_to = "Percentage") |>
  mutate(Pitch_Type = case_when(
    str_detect(Pitch_Type, "FA") ~ "Fastball",
    str_detect(Pitch_Type, "SI") ~ "Sinker",
    str_detect(Pitch_Type, "FC") ~ "Cutter",
    str_detect(Pitch_Type, "SL") ~ "Slider",
    str_detect(Pitch_Type, "CH") ~ "Changeup",
    str_detect(Pitch_Type, "CU") ~ "Curveball",
    str_detect(Pitch_Type, "FS") ~ "Splitter",
    str_detect(Pitch_Type, "KC") ~ "Knuckle Curve",
    str_detect(Pitch_Type, "KN") ~ "Knuckleball",
    str_detect(Pitch_Type, "SC") ~ "Screwball",
    str_detect(Pitch_Type, "FO") ~ "Forkball",
    TRUE ~ Pitch_Type
  )) |> 
  drop_na()

cond_data_velo <- cond_data |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vFA, pfx_vSI, pfx_vFC, pfx_vSL, 
         pfx_vCH, pfx_vCU, pfx_vFS, pfx_vKC, pfx_vKN, pfx_vSC, pfx_vFO) |> 
  pivot_longer(cols = ends_with("FA") | ends_with("SI") | ends_with("FC") | 
                 ends_with("SL") | ends_with("CH") | ends_with("CU") | ends_with("FS") |
                 ends_with("KC") | ends_with("KN") | ends_with("SC") | ends_with("FO"),
               names_to = "Pitch_Type",
               values_to = "Velocity") |>
  mutate(Pitch_Type = case_when(
    str_detect(Pitch_Type, "FA") ~ "Fastball",
    str_detect(Pitch_Type, "SI") ~ "Sinker",
    str_detect(Pitch_Type, "FC") ~ "Cutter",
    str_detect(Pitch_Type, "SL") ~ "Slider",
    str_detect(Pitch_Type, "CH") ~ "Changeup",
    str_detect(Pitch_Type, "CU") ~ "Curveball",
    str_detect(Pitch_Type, "FS") ~ "Splitter",
    str_detect(Pitch_Type, "KC") ~ "Knuckle Curve",
    str_detect(Pitch_Type, "KN") ~ "Knuckleball",
    str_detect(Pitch_Type, "SC") ~ "Screwball",
    str_detect(Pitch_Type, "FO") ~ "Forkball",
    TRUE ~ Pitch_Type
  )) |> 
  drop_na()

cond_data_horizontal <- cond_data |> 
  select(Season, PlayerNameRoute, xMLBAMID, `pfx_FA-X`, `pfx_SI-X`, `pfx_FC-X`, 
         `pfx_SL-X`, `pfx_CH-X`, `pfx_CU-X`, `pfx_FS-X`, `pfx_KC-X`, `pfx_KN-X`, 
         `pfx_SC-X`, `pfx_FO-X`) |> 
  pivot_longer(cols = contains("FA") | contains("SI") | contains("FC") | 
                 contains("SL") | contains("CH") | contains("CU") | contains("FS") |
                 contains("KC") | contains("KN") | contains("SC") | contains("FO"),
               names_to = "Pitch_Type",
               values_to = "Horizontal_Movement") |>
  mutate(Pitch_Type = case_when(
    str_detect(Pitch_Type, "FA") ~ "Fastball",
    str_detect(Pitch_Type, "SI") ~ "Sinker",
    str_detect(Pitch_Type, "FC") ~ "Cutter",
    str_detect(Pitch_Type, "SL") ~ "Slider",
    str_detect(Pitch_Type, "CH") ~ "Changeup",
    str_detect(Pitch_Type, "CU") ~ "Curveball",
    str_detect(Pitch_Type, "FS") ~ "Splitter",
    str_detect(Pitch_Type, "KC") ~ "Knuckle Curve",
    str_detect(Pitch_Type, "KN") ~ "Knuckleball",
    str_detect(Pitch_Type, "SC") ~ "Screwball",
    str_detect(Pitch_Type, "FO") ~ "Forkball",
    TRUE ~ Pitch_Type
  )) |> 
  drop_na()

cond_data_vertical <- cond_data |> 
  select(Season, PlayerNameRoute, xMLBAMID, `pfx_FA-Z`, `pfx_SI-Z`, `pfx_FC-Z`, 
         `pfx_SL-Z`, `pfx_CH-Z`, `pfx_CU-Z`, `pfx_FS-Z`, `pfx_KC-Z`, `pfx_KN-Z`, 
         `pfx_SC-Z`, `pfx_FO-Z`) |> 
  pivot_longer(cols = contains("FA") | contains("SI") | contains("FC") | 
                 contains("SL") | contains("CH") | contains("CU") | contains("FS") |
                 contains("KC") | contains("KN") | contains("SC") | contains("FO"),
               names_to = "Pitch_Type",
               values_to = "Vertical_Movement") |>
  mutate(Pitch_Type = case_when(
    str_detect(Pitch_Type, "FA") ~ "Fastball",
    str_detect(Pitch_Type, "SI") ~ "Sinker",
    str_detect(Pitch_Type, "FC") ~ "Cutter",
    str_detect(Pitch_Type, "SL") ~ "Slider",
    str_detect(Pitch_Type, "CH") ~ "Changeup",
    str_detect(Pitch_Type, "CU") ~ "Curveball",
    str_detect(Pitch_Type, "FS") ~ "Splitter",
    str_detect(Pitch_Type, "KC") ~ "Knuckle Curve",
    str_detect(Pitch_Type, "KN") ~ "Knuckleball",
    str_detect(Pitch_Type, "SC") ~ "Screwball",
    str_detect(Pitch_Type, "FO") ~ "Forkball",
    TRUE ~ Pitch_Type
  )) |> 
  drop_na()

#Spin Data Problem
cond_data_spin <- cond_data |> 
  select(Season, PlayerNameRoute, xMLBAMID, ff_avg_spin, si_avg_spin, fc_avg_spin, 
         sl_avg_spin, ch_avg_spin, cu_avg_spin, fs_avg_spin, kn_avg_spin) |> 
  pivot_longer(cols = contains("ff") | contains("si") | contains("fc") | 
                 contains("sl") | contains("ch") | contains("cu") | contains("fs") |
                 contains("kn"),
               names_to = "Pitch_Type",
               values_to = "Spin") |>
  mutate(Pitch_Type = case_when(
    str_detect(Pitch_Type, "ff") ~ "Fastball",
    str_detect(Pitch_Type, "si") ~ "Sinker",
    str_detect(Pitch_Type, "fc") ~ "Cutter",
    str_detect(Pitch_Type, "sl") ~ "Slider",
    str_detect(Pitch_Type, "ch") ~ "Changeup",
    str_detect(Pitch_Type, "cu") ~ "Curveball",
    str_detect(Pitch_Type, "fs") ~ "Splitter",
    str_detect(Pitch_Type, "kn") ~ "Knuckleball",
    TRUE ~ Pitch_Type
  )) |> 
  drop_na()

cond_data_stuff_plus <- cond_data |> 
  select(Season, PlayerNameRoute, xMLBAMID, sp_s_FF, sp_s_SI, sp_s_FC, sp_s_SL, 
         sp_s_CH, sp_s_CU, sp_s_FS, sp_s_KC, sp_s_FO) |> 
  pivot_longer(cols = contains("FF") | contains("SI") | contains("FC") | 
                 contains("SL") | contains("CH") | contains("CU") | contains("FS") |
                 contains("KC") | contains("FO"),
               names_to = "Pitch_Type",
               values_to = "Stuff_Plus") |>
  mutate(Pitch_Type = case_when(
    str_detect(Pitch_Type, "FF") ~ "Fastball",
    str_detect(Pitch_Type, "SI") ~ "Sinker",
    str_detect(Pitch_Type, "FC") ~ "Cutter",
    str_detect(Pitch_Type, "SL") ~ "Slider",
    str_detect(Pitch_Type, "CH") ~ "Changeup",
    str_detect(Pitch_Type, "CU") ~ "Curveball",
    str_detect(Pitch_Type, "FS") ~ "Splitter",
    str_detect(Pitch_Type, "KC") ~ "Knuckle Curve",
    str_detect(Pitch_Type, "FO") ~ "Forkball",
    TRUE ~ Pitch_Type
  )) |> 
  drop_na()

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


# EDA ---------------------------------------------------------------------

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

set.seed(123)
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

library(caret)
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


# Lasso Modeling ----------------------------------------------------------
#Modeling Fastball Stuff+ from Sinker Characteristics
si_to_ff <- cond_data |> 
  filter(ind_fastball == "Yes" & ind_sinker == "Yes") |> 
  select(Season, PlayerNameRoute, xMLBAMID, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`, 
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_SI, sp_s_FF) |> 
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
# tidy_lasso_coef |>
#   filter(lambda == prostate_lasso_cv$lambda.1se)
lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = si_to_ff_lasso_cv$lambda.1se,
)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

# Changeup Predicting Fastball Stuff+ Calc ------------------------------------
# From Gabe's Code and Switched Around
detect_arsenal_change <- function(current, previous) {
  
  return(ifelse(is.na(previous), 0, (current - previous) / (previous + 1e-10)))
  
}



# Initialize comprehensive pitch arsenal

pitch_arsenal <- cond_data |> 
  group_by(PlayerNameRoute) |> 
  arrange(PlayerNameRoute, Season) |> 
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



# Select relevant columns

relevant_cols <- c('Season', 'PlayerNameRoute', 'sp_stuff', 'RAR', 'pfx_CH_pct', 
                   'ERA_minus', 'WHIP_plus', 'BABIP_plus', 'sp_s_FF', 'pfx_CH-X',
                   'FIP_minus', 'K_9_plus', 'avg_rp_x', 'pfx_CH-Z', 'WAR', 'WPA'
                   ,'avg_rp_z', 'avg_release_extension', 'ch_avg_spin', 'WPA',
                   'REW', 'pfx_vCH', 'sp_s_CH', 'xFIP_minus',
                   'Throws', 'position', 'Addition_CH', 'Deletion_CH')


# Ensure target and primary predictor are not missing

filtered_data <- cond_data |> 
  filter(ind_change == "Yes") |> 
  select(pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`, ch_avg_spin, 
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)

# Rename columns to avoid issues with special characters
names(filtered_data) <- gsub('-', '_', names(filtered_data))

# Impute missing values using mice
filtered_data <- filtered_data |> 
  mutate(across(everything(), as.numeric))

# Handle missing values using mice
predictorMatrix <- quickpred(filtered_data)

# Impute missing values using mice with custom predictor matrix
mice_data <- mice(filtered_data, method = 'rf', predictorMatrix = predictorMatrix, m = 10, maxit = 50, seed = 123)

# Complete the dataset with the imputed values
data_filled <- complete(mice_data)
# Split data into training and testing sets



# K-Fold Cross-Validation



#data_filled <- rbind(train_data_filled, test_data_filled)

data_filled$Throws <- as.factor(data_filled$Throws)
data_filled$position <- as.factor(data_filled$position)


# Define k-fold cross-validation
k <- 3
folds <- createFolds(data_filled$sp_stuff, k = k, list = T)


# Initialize results storage
cv_results <- data.frame(fold = integer(), R2 = double(), Deviance_Explained = double())


# Perform k-fold cross-validation
for (i in 1:k) {
  
  # Split into training and validation sets
  train_indices <- unlist(folds[-i])
  val_indices <- unlist(folds[i])
  
  train_set <- data_filled[train_indices, ]
  val_set <- data_filled[val_indices, ]
  
  # Fit GAM model
  gam_model <- gam(sp_stuff ~ s(sp_s_CH, by = interaction(Throws, position))
                   + s(avg_release_extension, by = Throws) + Throws + 
                     s(pfx_CH_pct, pfx_vCH, by = position) + position +
                     s(avg_rp_x, avg_rp_z) + s(pfx_CH_X, pfx_CH_Z) +
                     s(pfx_vCH, ch_avg_spin, by = Throws) + 
                     s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                     s(RAR, REW) + s(BABIP_plus, WAR) + s(xFIP_minus, WPA),
                   data = train_set)
  
  
  
  # Get model summary
  gam_summary <- summary(gam_model)
  
  # Predict on validation set
  #val_preds <- predict(gam_model, newdata = val_set)
  
  # Extract metrics
  R2 <- gam_summary$r.sq
  Deviance_Explained <- gam_summary$dev.expl
  
  # Store results
  cv_results <- rbind(cv_results, data.frame(fold = i, R2 = R2, Deviance_Explained = Deviance_Explained))
  
  
  
}

# Average cross-validation results
avg_results <- colMeans(cv_results[, -1])
print(avg_results)





# Fit final GAM calculator

final_gam_model <- gam(sp_stuff ~ s(sp_s_CH, by = interaction(Throws, position))
                       + s(avg_release_extension, by = Throws) + Throws + 
                         s(pfx_CH_pct, pfx_vCH, by = position) + position +
                         s(avg_rp_x, avg_rp_z) + s(pfx_CH_X, pfx_CH_Z) +
                         s(pfx_vCH, ch_avg_spin, by = Throws) + 
                         s(ERA_minus, FIP_minus) +  s(K_9_plus, WHIP_plus) + 
                         s(RAR, REW) + s(BABIP_plus, WAR) + s(xFIP_minus, WPA),
                       data = data_filled)


summary(final_gam_model)



# Make Predictions

# Function to predict sp_stuff for a given player

predict_sp_stuff <- function(player_name, new_data) {
  
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
predicted_stuff_plus <- predict_sp_stuff('Aaron Nola', new_player_data)
print(predicted_stuff_plus)

#Could we track change in Stuff+?


# Random Forest -----------------------------------------------------------
#Changeup
#install.packages("stacks")
library(tidyverse)
theme_set(theme_light())
library(stacks)
rf_data <- cond_data |> 
  filter(ind_change == "Yes") |> 
  filter(!is.na(ch_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_CH_pct, pfx_vCH, `pfx_CH-X`, `pfx_CH-Z`,
         ch_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
glimpse(rf_data)
names(rf_data) <- gsub('-', '_', names(rf_data))

library(ranger) #suarez
change_rf <- ranger(sp_s_FF ~ pfx_vCH + ch_avg_spin + pfx_CH_X + pfx_CH_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                   num.trees = 500, importance = "impurity", data = rf_data, mtry=2)
change_rf
library(vip)
vip(change_rf)

predictions <- change_rf$predictions

# Create a new data frame with actual and predicted values
rf_results <- rf_data |> 
  mutate(pred = predictions)

rf_data |>
  mutate(pred = predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

#Sinker
rf_2 <- cond_data |> 
  filter(ind_sinker == "Yes") |> 
  filter(!is.na(si_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_SI_pct, pfx_vSI, `pfx_SI-X`, `pfx_SI-Z`,
         si_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_2) <- gsub('-', '_', names(rf_2))

sinker_rf <- ranger(sp_s_FF ~ pfx_vSI + si_avg_spin + pfx_SI_X + pfx_SI_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_2, mtry = 3)
sinker_rf

vip(sinker_rf)

si_predictions <- sinker_rf$predictions

# Create a new data frame with actual and predicted values
rf_results <- rf_2 |> 
  mutate(pred = si_predictions)

rf_2 |>
  mutate(pred = si_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

#Cutter
rf_3 <- cond_data |> 
  filter(ind_cutter == "Yes") |> 
  filter(!is.na(fc_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_FC_pct, pfx_vFC, `pfx_FC-X`, `pfx_FC-Z`,
         fc_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_3) <- gsub('-', '_', names(rf_3))

cutter_rf <- ranger(sp_s_FF ~ pfx_vFC + fc_avg_spin + pfx_FC_X + pfx_FC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_3, mtry = 2)
cutter_rf

vip(cutter_rf)

fc_predictions <- cutter_rf$predictions

# Create a new data frame with actual and predicted values
rf_results <- rf_3 |> 
  mutate(pred = fc_predictions)

rf_3 |>
  mutate(pred = fc_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

#Slider
rf_4 <- cond_data |> 
  filter(ind_slider == "Yes") |> 
  filter(!is.na(sl_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_SL_pct, pfx_vSL, `pfx_SL-X`, `pfx_SL-Z`,
         sl_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_4) <- gsub('-', '_', names(rf_4))

slider_rf <- ranger(sp_s_FF ~ pfx_vSL + sl_avg_spin + pfx_SL_X + pfx_SL_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_4, mtry = 2)
slider_rf

vip_plot <- vip(slider_rf)

# Extract the data frame from the vip plot object
vip_data <- vip_plot$data

# Rename the variables (example of changing names)
vip_data$Variable <- recode(vip_data$Variable, 
                            `pfx_vSL` = "Velocity",
                            `sl_avg_spin` = "Spin Rate",
                            `avg_release_extension` = "Avg Release Extension",
                            `avg_rp_x` = "Avg Release Position X",
                            `avg_rp_z` = "Avg Release Position Z", 
                            `pfx_SL_X` = "Horizontal Movement",
                            `pfx_SL_Z` = 'Vertical Movement',
                            )

# Create a new vip plot using the modified data frame
vip_plot_modified <- ggplot(vip_data, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(aes(fill = Importance), color = "white", show.legend = FALSE) +
  labs(
    x = "Variable Importance", 
    y = "Predictor Variables",
    title = "Variable Importance Plot",
    subtitle = "Random Forest Model"
  )

vip_plot_modified

sl_predictions <- slider_rf$predictions

# Create a new data frame with actual and predicted values
rf_results <- rf_4 |> 
  mutate(pred = sl_predictions)

rf_4 |>
  mutate(pred = sl_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

#Curveball
rf_5 <- cond_data |> 
  filter(ind_curve == "Yes") |> 
  filter(!is.na(cu_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_CU_pct, pfx_vCU, `pfx_CU-X`, `pfx_CU-Z`,
         cu_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_5) <- gsub('-', '_', names(rf_5))

curve_rf <- ranger(sp_s_FF ~ pfx_vCU + cu_avg_spin + pfx_CU_X + pfx_CU_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_5, mtry = 2)
curve_rf

vip(curve_rf)

cu_predictions <- curve_rf$predictions

# Create a new data frame with actual and predicted values
rf_results <- rf_5 |> 
  mutate(pred = cu_predictions)

rf_5 |>
  mutate(pred = cu_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

#Splitter
rf_6 <- cond_data |> 
  filter(ind_split == "Yes") |> 
  filter(!is.na(fs_avg_spin), !is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_FS_pct, pfx_vFS, `pfx_FS-X`, `pfx_FS-Z`,
         fs_avg_spin, avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_6) <- gsub('-', '_', names(rf_6))

split_rf <- ranger(sp_s_FF ~ pfx_vFS + fs_avg_spin + pfx_FS_X + pfx_FS_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_6, mtry = 2)
split_rf

vip(split_rf)

fs_predictions <- split_rf$predictions

# Create a new data frame with actual and predicted values
rf_results <- rf_6 |> 
  mutate(pred = fs_predictions)

rf_6 |>
  mutate(pred = fs_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", linewidth = 2)

#Knuckle Curve: no spin
rf_7 <- cond_data |> 
  filter(ind_kc == "Yes") |> 
  filter(!is.na(sp_s_FF)) |> 
  select(Season, PlayerNameRoute, pfx_KC_pct, pfx_vKC, `pfx_KC-X`, `pfx_KC-Z`,
         avg_release_extension, avg_rp_x, avg_rp_z, sp_s_FF)
names(rf_7) <- gsub('-', '_', names(rf_7))

kc_rf <- ranger(sp_s_FF ~ pfx_vKC + pfx_KC_X + pfx_KC_Z
                    + avg_release_extension + avg_rp_x + avg_rp_z, 
                    num.trees = 500, importance = "impurity", data = rf_7, mtry = 2)
kc_rf

vip(kc_rf)

kc_predictions <- kc_rf$predictions

# Create a new data frame with actual and predicted values
rf_results <- rf_7 |> 
  mutate(pred = kc_predictions)

rf_7 |>
  mutate(pred = kc_predictions) |>
  ggplot(aes(sp_s_FF, pred)) +
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
