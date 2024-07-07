library(baseballr) #install the package beforehand
library(dplyr)
library(ggplot2)
library(ggridges)
library(patchwork)
library(tidyr)
library(tidyverse)
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




# Early EDA ---------------------------------------------------------------


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





# Initialize pitch arsenal variation function -----------------------------


# Function to detect changes in pitch arsenal
detect_arsenal_change <- function(data) {

  
  # Group by PlayerName and PitchType, calculate changes
  data_changes <- data |>
      group_by(PlayerName, PitchType) |>
      summarize(
        Change_2021_2022 = `Year_2022` - `Year_2021`,
        Change_2022_2023 = `Year_2023` - `Year_2022`,
        .groups = 'drop'
      ) |>

  # Threshold of 2%
  mutate(
    Added_2021_2022 = ifelse(Change_2021_2022 > .02, 1, 0),
    Dropped_2021_2022 = ifelse(Change_2021_2022 <= -.02, 1, 0),
    Added_2022_2023 = ifelse(Change_2022_2023 > .02, 1, 0),
    Dropped_2022_2023 = ifelse(Change_2022_2023 <= -.02, 1, 0)
  )

# Update original dataset with new columns
result <- data |>
  left_join(data_changes, by = c("PlayerName", "PitchType"))

# Only drop NA's at the end of arsenal creation
result <- result |> 
  drop_na()

return(result)
}


# Overwrite with new dataset`
usage_change <- detect_arsenal_change(usage_change)



# Exploring savant dataset ------------------------------------------------



# Different assignments in savant as 'FA' is assigned other
  # 'FF' is assigned 4-Seam fastball all under pitch_name variable
# Already modeled 4-Seam as pfx_FA_pct with baseballr so we'll stick with this format
  # merge with 'FA' other count (2-seam fastball ?) to prevent data loss

clean_savant <- savant |>
  mutate(pitch_type = ifelse(pitch_type == 'FF', 'FA', pitch_type))


# Flip Player Name Format for consistent formatting
reformat_name <- function(name) {
  
  parts <- strsplit(name, ', ')[[1]]
  
  return(paste(parts[2], parts[1]))
}


# Apply the function to the player_name column
clean_savant <- clean_savant |> 
  mutate(player_name = sapply(player_name, reformat_name))





# Creating arsenal --------------------------------------------------------


# Define the pitch type mappings
pitch_type_mappings <- c(
  CH = 'pfx_CH_pct',
  CU = 'pfx_CU_pct',
  FA = 'pfx_FA_pct',
  SI = 'pfx_SI_pct',
  SL = 'pfx_SL_pct',
  FC = 'pfx_FC_pct',
  SC = 'pfx_SC_pct',
  FS = 'pfx_FS_pct',
  FO = 'pfx_FO_pct',
  KC = 'pfx_KC_pct',
  EP = 'pfx_EP_pct',
  KN = 'pfx_KN_pct',
  PO = 'pfx_PO_pct',
  SV = 'pfx_SV_pct',
  CS = 'pfx_CS_pct',
  ST = 'pfx_ST_pct'
)


# Filter and transform all pitch types, retaining necessary columns**
filtered_savant <- clean_savant |> 
  mutate(PitchType = recode(pitch_type, !!!pitch_type_mappings))|> 
  select(PlayerName = player_name, PitchType, game_year, 
         release_speed, effective_speed, release_spin_rate, 
         release_pos_x, release_pos_y, release_pos_z) 



# Streamline focus to arsenal metrics only and compute relative weights
pitch_metric <- filtered_savant |> 
  group_by(PlayerName, PitchType, game_year) |> 
  summarize(PitchCount = n(), .groups = 'drop') |> 
  group_by(PlayerName, game_year) |> 
  mutate(TotalPitchCount = sum(PitchCount)) |> 
  ungroup() |> 
  mutate(UsagePct = round(PitchCount / TotalPitchCount, 3)) |> 
  select(PlayerName, PitchType, game_year, UsagePct) |> 
  pivot_wider(names_from = game_year, values_from = UsagePct, names_prefix = 'Year_') |> 
  replace_na(list(Year_2021 = 0, Year_2022 = 0, Year_2023 = 0))


# Apply function
pitch_arsenal <- detect_arsenal_change(pitch_metric)






# From King Danny ---------------------------------------------------------

#Setting the cutoff at 5% usage
cond_data <- combined_data |> 
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
cond_data <- combined_data |> 
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
ggplot(cond_data, aes(x=`xFIP-`, colour = ind_curve))+
  geom_density()+
  facet_wrap(vars(ind_curve), nrow=2)

cond_data |>
  filter(ind_slider == "Yes") |>
  cond_data |>
  filter(ind_slider == "Yes") |>
  ggplot(aes(x = `pfx_SL-X`, y = sp_s_SL)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm") +
  labs(x = "pfx_SL-X", y = "sp_s_SL") +
  ggtitle("Scatterplot of pfx_SL-X vs sp_s_SL (Slider Indicator = Yes)")
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
  changed_fastball_pitchers <- cond_data |>
  group_by(xMLBAMID) |>
  do(has_changed_fastball(.)) |>
  ungroup()

# Example Visualization for Fastball Indicator Change
ggplot(changed_fastball_pitchers, aes(x = factor(Season), y = ind_fastball, group = xMLBAMID, color = PlayerNameRoute)) +
  geom_line() +
  geom_point() +
  labs(title = "Changes in Fastball Indicator Over Years",
       x = "Season",
       y = "Fastball Indicator (Yes/No)") +
  scale_y_discrete(labels = c("No", "Yes")) +
  theme_minimal()

# Calculate the difference in 'FIP-' between consecutive years for each pitcher
fip_diff <- changed_fastball_pitchers |>
  arrange(xMLBAMID, Season) |>
  group_by(xMLBAMID, PlayerNameRoute) |>
  mutate(FIP_diff = `FIP-` - lag(`FIP-`)) |>
  filter(!is.na(FIP_diff)) |>
  fip_diff <- changed_fastball_pitchers |>
  arrange(xMLBAMID, Season) |>
  group_by(xMLBAMID, PlayerNameRoute) |>
  mutate(FIP_diff = `FIP-` - lag(`FIP-`)) |>
  filter(!is.na(FIP_diff)) |>
  ungroup()

# Determine if fastball was added or subtracted
fip_diff <- fip_diff |>
  fip_diff <- fip_diff |>
  mutate(change_type = case_when(
    ind_fastball == "Yes" & lag(ind_fastball) == "No" ~ "Added",
    ind_fastball == "No" & lag(ind_fastball) == "Yes" ~ "Subtracted"
  )) |>
  )) |>
  filter(!is.na(change_type))  # Exclude rows where change_type is NA (no change)


# Create a bar chart to visualize the change in fastball indicator with color coding
ggplot(fip_diff, aes(x = FIP_diff, y = reorder(PlayerNameRoute, FIP_diff), fill = change_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Added" = "blue", "Subtracted" = "red"),
                    guide = guide_legend(title = "Change in Fastball Indicator")) +
  labs(title = "Change in Fastball Indicator from Year to Year",
       x = "Change in FIP-",
       y = "Player Name") +
  theme_minimal()
library(dplyr)
library(ggplot2)


# Function to handle splitting pitchers with alternating fastball indicators
split_pitchers <- function(data) {
  fastball_changes <- data |>
    arrange(xMLBAMID, Season) |>
    group_by(xMLBAMID) |>
    fastball_changes <- data |>
      arrange(xMLBAMID, Season) |>
      group_by(xMLBAMID) |>
      mutate(change = ifelse(ind_fastball != lag(ind_fastball), 1, 0),
             change_group = cumsum(change)) |>
      filter(change_group <= 1) |>
      change_group = cumsum(change)) |>
  filter(change_group <= 1) |>
  ungroup()

return(fastball_changes)
}


# Calculate the difference in 'FIP-' between consecutive years for each pitcher
fip_diff <- changed_fastball_pitchers |>
  arrange(xMLBAMID, Season) |>
  group_by(xMLBAMID, PlayerNameRoute) |>
  mutate(FIP_diff = `FIP-` - lag(`FIP-`)) |>
  ungroup() |>
  filter(!is.na(FIP_diff)) |>
  fip_diff <- changed_fastball_pitchers |>
  arrange(xMLBAMID, Season) |>
  group_by(xMLBAMID, PlayerNameRoute) |>
  mutate(FIP_diff = `FIP-` - lag(`FIP-`)) |>
  ungroup() |>
  filter(!is.na(FIP_diff)) |>
  mutate(change_type = case_when(
    ind_fastball == "Yes" & lead(ind_fastball) == "No" ~ "Added",
    ind_fastball == "No" & lead(ind_fastball) == "Yes" ~ "Subtracted"
  )) |>
  )) |>
  filter(!is.na(change_type))  # Exclude rows where change_type is NA


# Create a bar chart to visualize the change in fastball indicator with color coding
ggplot(fip_diff, aes(x = FIP_diff, y = reorder(PlayerNameRoute, FIP_diff), fill = change_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Added" = "blue", "Subtracted" = "red"),
                    guide = guide_legend(title = "Change in Fastball Indicator")) +
  labs(title = "Change in Fastball Indicator from Year to Year",
       x = "Change in FIP-",
       y = "Player Name") +
  theme_minimal()







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
# model stuff+ based on attributes using gam
# compare to pfx_stuff+ given
# get a run value for pitchers
# ridge and lasso regression for initially modeling for stuff+
# cross validation
# toss it in see what sticks
# Individual gam based on pitch types
# predicting aggregate characteristics