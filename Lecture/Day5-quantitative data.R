library(tidyverse)
theme_set(theme_light())

names(taylor_all_songs)

taylor_all_songs <- taylor_all_songs |> 
  mutate(duration=duration_ms/60000)

range(taylor_all_songs$duration, na.rm=T)


taylor_all_songs |> 
  ggplot(aes(x=duration))+
  geom_boxplot()+
  theme(axis.text.y = element_blank())

taylor_all_songs |> 
  ggplot(aes(x=duration))+
  geom_histogram(fill='gray', color='white')

taylor_all_songs |> 
  ggplot(aes(x=duration, y=''))+
  geom_beeswarm(cex = 1.7) # adjust scaling

taylor_all_songs |> 
  ggplot(aes(x=duration))+
  geom_dotplot(binwidth = .1)


taylor_all_songs |> 
  ggplot(aes(x=duration, y=''))+
  geom_violin()+
  geom_boxplot(width=.4)


taylor_all_songs |> 
  ggplot(aes(x=duration))+
  stat_ecdf()+
  geom_rug()

taylor_all_songs |> 
  ggplot(aes(x=duration))+
  geom_histogram()+ # supplement a rug plot with an histogram
  geom_rug(alpha=.5)


# 2D quant data

taylor_all_songs |> 
  ggplot(aes(loudness, energy))+
  geom_point(color='red', alpha=.5)+ # adjust transparency to visualize overlap
  geom_smooth(method = 'lm')+
  geom_rug(alpha=.2)

cor(taylor_all_songs$loudness,
    taylor_all_songs$energy,
    use='complete.obs') # There's missing data


x <- 1:100
y <- x^5
plot(x,y)
cor(x,y, method = 'spearman')

taylor_all_songs |> 
  select(danceability, energy, loudness, tempo) |> 
  ggpairs()

taylor_all_songs |> 
  filter(album_name %in% c('reputation', 'Midnights', 'folklore', 'Red')) |> 
  ggplot(aes(y=album_name, x=duration, color=album_name))+
  geom_violin()+
  scale_color_manual(values = c('purple', 'orange', 'green', 'blue'))
  #scale_color_albums() alternate way to add color automatically
  #geom_boxplot(width=.2)
  

taylor_all_songs |> 
  filter(album_name %in% c('reputation', 'Midnights', 'folklore', 'Red')) |> 
  ggplot(aes(y=album_name, x=duration, color=album_name))+
  stat_ecdf(linewidth=1)
