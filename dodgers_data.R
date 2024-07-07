library(baseballr) #install the package beforehand
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
#no stuff+ for screwball (only one pitcher throws it)

#creating condensed dataset for 2021
library(dplyr)
cond_data_2021 <- data_2021 |> 
  select(all_of(key_vars)
         )

#Creating condensed dataset for 2022 
#missing some Pitch Info variables.  Check on pitchFX
cond_data_2022 <- data_2022 |> 
  select(all_of(key_vars)
  )
#Creating condensed dataset for 2023
cond_data_2023 <- data_2023 |> 
  select(all_of(key_vars)
  )

#Adding the spin rates for each pitch with a CSV pulled from Statcast
#Link to the 2021 data:https://baseballsavant.mlb.com/pitch-arsenals?year=2021&
#min=250&type=n_&hand=&sort=9&sortDir=desc

spin_2021 <- read.csv("pitch_spin_2021.csv")
spin_2021 <- rename(spin_2021, xMLBAMID = pitcher)
cond_data_2021 <- left_join(cond_data_2021, spin_2021, by="xMLBAMID")

#2022
spin_2022 <- read.csv("pitch_spin_2022.csv")
spin_2022 <- rename(spin_2022, xMLBAMID = pitcher)
cond_data_2022 <- left_join(cond_data_2022, spin_2022, by="xMLBAMID")

#2023
spin_2023 <- read.csv("pitch_spin_2023.csv")
spin_2023 <- rename(spin_2023, xMLBAMID = pitcher)
cond_data_2023 <- left_join(cond_data_2023, spin_2023, by="xMLBAMID")

#combining all three condensed datasets
cond_data = rbind(cond_data_2021, cond_data_2022, cond_data_2023)

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

#Adjusting the horizontal movement variable
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





#Experimenting with visualizations
library(ggplot2)
cond_data |> 
  filter(cond_data$ind_fastball == "Yes") |> 
  ggplot(aes(x=pfx_vFA, y=sp_s_FF, color = `K_BB+`))+
  geom_point()

cond_data |> 
  ggplot(aes(x=sp_s_FF))+
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
  select(1:12, 97)

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
  select(1:12, 103)

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
  select(1:12, 98)

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
  select(1:12, 99)

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
  select(1:12, 100)

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
  select(1:12, 101)

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
  select(1:12, 102)

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
  select(1:12, 106)

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
  select(1:12, 104)

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
  select(1:12, 105)

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
