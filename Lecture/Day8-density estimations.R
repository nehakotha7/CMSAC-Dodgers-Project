library(factoextra)

theme_set(theme_light())

ionescu_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/ionescu_shots.csv")
glimpse(ionescu_shots)

fd_bw <- 2 * IQR(ionescu_shots$shot_distance) / (nrow(ionescu_shots) ^(1/3))

ionescu_shots |> 
  ggplot(aes(x = shot_distance))+
  geom_histogram(binwidth = 1, center = .5, closed = 'left')


ionescu_shots |> 
  ggplot(aes(x = shot_distance)) +
  geom_density(adjust = .5)+ # for smootheness
  geom_rug(alpha = .3)

gaussian_bw <- 1.06 * sd(ionescu_shots$shot_distance) * nrow(ionescu_shots) ^ (-1/5)

ionescu_shots |> 
  ggplot(aes(x = shot_distance))+
  geom_histogram(aes(y = after_stat(density)))+
  geom_density()


ionescu_kde <- ionescu_shots |> 
  ggplot(aes(x = shot_distance,
             color = scoring_play))+
  geom_density()+
  geom_rug(alpha = .3)

ionescu_ecdf <- ionescu_shots |> 
  ggplot(aes(x = shot_distance,
             color = scoring_play))+
  stat_ecdf()+
  geom_rug(alpha = .3)

library(cowplot)
plot_grid(ionescu_kde, ionescu_ecdf)

library(patchwork)

ionescu_kde +
  ionescu_ecdf+
  plot_layout(guides = 'collect')

library(ggridges) # Showing data for different groups

ionescu_shots |> 
  ggplot(aes(x = shot_distance, y = shot_type))+
  geom_density_ridges()


ionescu_shots |> 
  filter(shot_y < 35) |> 
  ggplot(aes(x = shot_x, y = shot_y))+
  geom_point(alpha = .4)+
  geom_density2d(adjust = .1)+
  coord_fixed()


ionescu_shots |> 
  filter(shot_y < 35) |> 
  ggplot(aes(x = shot_x, y = shot_y))+
  stat_density2d(aes(fill = after_stat(density)),
                 geom = 'raster',
                 contour = F,
                 bins = 50,
                 h = .7)+
  scale_fill_gradient(
    low = 'white',
    high = 'darkblue'
  )

library(hexbin)
ionescu_shots |> 
  filter(shot_y < 35) |> 
  ggplot(aes(x = shot_x, y = shot_y))+
  geom_hex(binwidth = c(1,1))+
  scale_fill_gradient(
    low = 'midnightblue',
    high = 'yellow'
  )


ionescu_shots |> 
  filter(shot_y < 35) |> 
  ggplot(aes(x = shot_x, y = shot_y, z = score_value))+
  stat_summary_hex(binwidth = c(2,2),
                   fun = mean)+
  scale_fill_gradient(
    low = 'midnightblue',
    high = 'yellow'
  )
