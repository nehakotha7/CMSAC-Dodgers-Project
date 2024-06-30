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
cond_data <- cond_data %>%
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
  
cond_data %>%
  filter(ind_slider == "Yes") %>%
  ggplot(aes(x = `pfx_SL-X`, y = sp_s_SL)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm") +
  labs(x = "pfx_SL-X", y = "sp_s_SL") +
  ggtitle("Scatterplot of pfx_SL-X vs sp_s_SL (Slider Indicator = Yes)")
  #facet_wrap(~ Throws)



library(dplyr)
library(ggplot2)


# -------------------------------------------------------------------------
#Work in Progress

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
changed_fastball_pitchers <- cond_data %>%
  group_by(xMLBAMID) %>%
  do(has_changed_fastball(.)) %>%
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
fip_diff <- changed_fastball_pitchers %>%
  arrange(xMLBAMID, Season) %>%
  group_by(xMLBAMID, PlayerNameRoute) %>%
  mutate(FIP_diff = `FIP-` - lag(`FIP-`)) %>%
  filter(!is.na(FIP_diff)) %>%
  ungroup()

# Determine if fastball was added or subtracted
fip_diff <- fip_diff %>%
  mutate(change_type = case_when(
    ind_fastball == "Yes" & lag(ind_fastball) == "No" ~ "Added",
    ind_fastball == "No" & lag(ind_fastball) == "Yes" ~ "Subtracted"
  )) %>%
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


# -------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Function to handle splitting pitchers with alternating fastball indicators
split_pitchers <- function(data) {
  fastball_changes <- data %>%
    arrange(xMLBAMID, Season) %>%
    group_by(xMLBAMID) %>%
    mutate(change = ifelse(ind_fastball != lag(ind_fastball), 1, 0),
           change_group = cumsum(change)) %>%
    filter(change_group <= 1) %>%
    ungroup()
  
  return(fastball_changes)
}

# Calculate the difference in 'FIP-' between consecutive years for each pitcher
fip_diff <- changed_fastball_pitchers %>%
  arrange(xMLBAMID, Season) %>%
  group_by(xMLBAMID, PlayerNameRoute) %>%
  mutate(FIP_diff = `FIP-` - lag(`FIP-`)) %>%
  ungroup() %>%
  filter(!is.na(FIP_diff)) %>%
  mutate(change_type = case_when(
    ind_fastball == "Yes" & lead(ind_fastball) == "No" ~ "Added",
    ind_fastball == "No" & lead(ind_fastball) == "Yes" ~ "Subtracted"
  )) %>%
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



#Gabe's Code: Won't run here because I changed "key_vars"

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
  
  
  
  
  
  
  
  
  
