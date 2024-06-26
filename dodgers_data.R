library(baseballr) #install the package beforehand
#scraping pitching data from 2021
data_2021 = baseballr::fg_pitcher_leaders(startseason = 2021, endseason = 2021)
#only including pitchers who threw more than 250 pitches
data_2021 <- data_2021[which(data_2021$Pitches >= 250),]
#adjusting the position column to classify into starters (SP) and relievers (RP)
for (player in data_2021){
  data_2021$position <- ifelse(data_2021$GS >= (data_2021$G - data_2021$GS), 
                               "SP", "RP")
}
library(ggplot2)
data_2021 |> 
  ggplot(aes(x= sp_s_FF, y = FBv))+
  geom_point()



data_2022 = baseballr::fg_pitcher_leaders(startseason = 2022, endseason = 2022)
data_2022 <- data_2022[which(data_2022$Pitches >= 250),]
for (player in data_2022){
  data_2022$position <- ifelse(data_2022$GS >= (data_2022$G - data_2022$GS), 
                               "SP", "RP")
}



data_2023 = baseballr::fg_pitcher_leaders(startseason = 2023, endseason = 2023)
data_2023 <- data_2023[which(data_2023$Pitches >= 250),]
for (player in data_2023){
  data_2023$position <- ifelse(data_2023$GS >= (data_2023$G - data_2023$GS), 
                               "SP", "RP")
}
#creating condensed dataseet for 2021
library(dplyr)
cond_data_2021 <- data_2021 |> 
  select(Season, Position, IP, Throws, xMLBAMID, PlayerNameRoute, `ERA-`, `K_BB+`, `HR_9+`, `WHIP+`,
         `AVG+`, `FIP-`, `BABIP+`, RAR, WAR, `RA9-Wins`, `xFIP-`, WPA, RE24, REW,
         FB_pct1, FBv, SL_pct,  SLv, CT_pct, CTv, CB_pct, CBv, CH_pct, CHv,
         sp_s_FF, sp_s_SI, sp_s_SL, sp_s_FC, sp_s_CU, sp_s_CH, sp_s_FS, sp_s_KC,
         sp_stuff, SIERA, KNv, KN_pct,`Soft_pct+`, `Med_pct+`, `Hard_pct+`, xERA, 
         )

#Creating condensed dataset for 2022
cond_data_2022 <- data_2022 |> 
  select(Season, Position, IP, Throws, xMLBAMID, PlayerNameRoute, `ERA-`, `K_BB+`, `HR_9+`, `WHIP+`,
         `AVG+`, `FIP-`, `BABIP+`, RAR, WAR, `RA9-Wins`, `xFIP-`, WPA, RE24, REW,
         FB_pct1, FBv, SL_pct,  SLv, CT_pct, CTv, CB_pct, CBv, CH_pct, CHv,
         sp_s_FF, sp_s_SI, sp_s_SL, sp_s_FC, sp_s_CU, sp_s_CH, sp_s_FS, sp_s_KC,
         sp_stuff, SIERA, KNv, KN_pct,`Soft_pct+`, `Med_pct+`, `Hard_pct+`, xERA, 
  )
#Creating condensed dataset for 2023
cond_data_2023 <- data_2023 |> 
  select(Season, Position, IP, Throws, xMLBAMID, PlayerNameRoute, `ERA-`, `K_BB+`, `HR_9+`, `WHIP+`,
         `AVG+`, `FIP-`, `BABIP+`, RAR, WAR, `RA9-Wins`, `xFIP-`, WPA, RE24, REW,
         FB_pct1, FBv, SL_pct,  SLv, CT_pct, CTv, CB_pct, CBv, CH_pct, CHv,
         sp_s_FF, sp_s_SI, sp_s_SL, sp_s_FC, sp_s_CU, sp_s_CH, sp_s_FS, sp_s_KC,
         sp_stuff, SIERA, KNv, KN_pct,`Soft_pct+`, `Med_pct+`, `Hard_pct+`, xERA, 
  )
#combining all three condensed datasets
cond_data = rbind(cond_data_2021, cond_data_2022, cond_data_2023)
  
#Experimenting with visualizations
cond_data |> 
  ggplot(aes(x=sp_s_FF, y=sp_s_CH))+
  geom_point()
cond_data |> 
  ggplot(aes(x=sp_s_FF))+
  geom_histogram()  
  
  
  
  
  
  
  
  
  
  
  
  
