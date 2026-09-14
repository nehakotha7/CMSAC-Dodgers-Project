library(tidyverse)
theme_set(theme_light())
starbucks <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv"
) |>
  # convert columns to numeric that were saved as character
  mutate(trans_fat_g = as.numeric(trans_fat_g), fiber_g = as.numeric(fiber_g))
glimpse(starbucks)


feat <- starbucks |> 
  dplyr::select(5:15)

starbucks_pca <- prcomp(feat, center = T, scale. = T)
summary(starbucks_pca)
View(starbucks_pca$x)

var(starbucks_pca[,1]) # all rows, col 1
var(starbucks_pca[,2]) # all rows, col 2

starbucks <- starbucks |> 
  mutate(
    pc1 = starbucks_pca$x[,1],
    pc2 = starbucks_pca$x[,2]
  )

starbucks |> 
  ggplot(aes(pc1, pc2))+
  geom_point(alpha = .45, size = .9)

# PCA biplot
library(factoextra)
starbucks_pca |> 
  fviz_pca_biplot(label = 'var',
                  alpha.ind = .25,
                  alpha.var = .8,
                  col.var = 'purple',
                  repel = T)

# On a biplot, angle means correlation. Perfectly orthogonal component would be uncorrelated
 # Bigger than 90 means negatively correlated


# Elbow plot
starbucks_pca |> 
  fviz_eig() +
  geom_hline(yintercept = 100 * 1 / ncol(starbucks_pca$x),
             linetype = 'dashed')

library(broom)
starbucks_pca |> 
  tidy(matrix = 'eigenvalues') |> 
  ggplot(aes(PC, cumulative))+
  #geom_col(fill = 'skyblue')+
  geom_line()+
  geom_point()+
  #geom_hline(yintercept = 1 / ncol(starbucks_pca$x), linetype = 'dashed')+
  scale_x_continuous(breaks = 1:11)
