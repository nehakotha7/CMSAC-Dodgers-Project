# Estimating a mixture model: EM algorithm
# Expectation–maximization (EM) algorithm is a method for (approximately) maximizing the (marginal) likelihood in the presence of missing data
# 
# For a GMM:
#   
#   First, initialize the model parameters randomly
# 
# Then, alternate between the following two steps (keep repeating until nothing changes)
# 
# E-step: compute the cluster memberships for each point
# 
# M-step: recompute/update the parameters


library(tidyverse)
theme_set(theme_light())
nba_players <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nba_players.csv")
head(nba_players)

library(mclust)
# x3pa: 3pt attempts per 100 possessions
# trb: total rebounds per 100 possessions
nba_mclust <- nba_players |> 
  dplyr::select(x3pa, trb) |> 
  Mclust()
summary(nba_mclust)

library(broom)
nba_mclust |> 
  tidy()  

nba_mclust |> 
  augment() |> 
  ggplot(aes(x = x3pa, y = trb, color = .class, size = .uncertainty)) +
  geom_point(alpha = 0.4) +
  ggthemes::scale_color_colorblind()

nba_mclust |> 
  plot(what = "BIC", 
       legendArgs = list(x = "bottomright", ncol = 4))

nba_mclust |> 
  plot(what = 'classification')


table(nba_mclust$classification, nba_players$pos)
# you can visualize a 2 by 2 table with a mosaic plot


# cluster probabilities
nba_mclust$z |> 
  View()
nba_player_probs <- nba_mclust$z
colnames(nba_player_probs) <- c('cluster1', 'cluster2', 'cluster3')

nba_player_probs |> 
  as_tibble() |> 
  mutate(player = nba_players$player) |> 
  pivot_longer(!player) |> 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~ name)

nba_mclust |> 
  augment() |> 
  mutate(player = nba_players$player) |> 
  group_by(.class) |> 
  slice_max(.uncertainty, n = 5)
