library(tidyverse)
library(Lahman)

Batting <- as_tibble(Batting)
dim(Batting)
class(Batting)
head(Batting)
glimpse(Batting)

summary(Batting$AB)
table(Batting$lgID)
table(Batting$lgID, useNA = "always")

filter(Batting, lgID=="AL" | lgID=="NL") # Extracting rows
filter(Batting, lgID %in% c("AL","NL"))
filter(Batting, teamID=="NYA" & yearID==2016)

select(Batting, playerID, yearID, H, SO) # Extracting columns

mutate(Batting, batting_avg=H/AB, so_walk_ratio=SO/BB) # Creating new variables

arrange(Batting, HR) # Sorting rows by columns
arrange(Batting, desc(HR))
arrange(Batting, -HR) # Alternate way of descending order
arrange(Batting, desc(AB), HR)

# to get the assignment operator, use alt + dash
# to get the pipe operator below, use ctrl + shift + M
  # allows you to perform multiple tasks
# to comment multiple rows, use ctrl + shift + C

Batting |>
  filter(teamID=="PIT" & yearID==2022 & AB >= 50) |>
  mutate(batting_avg=H/AB) |>
  arrange(desc(batting_avg)) |>
  select(playerID, AB, batting_avg)

Batting |> 
  summarize(median_so=median(SO, na.rm=T),
            cor_ab_hr=cor(AB,HR))
Batting |> 
  filter(yearID %in% 2015:2019) |> 
  group_by(teamID) |> 
  summarize(total_hr = sum(HR), total_so = sum(SO), total_bb = sum(BB))

Batting |> 
  slice(c(1, 99, 101, 500)) # Slice rows using row index