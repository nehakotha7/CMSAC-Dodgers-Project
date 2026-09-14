# cluster analysis

# Clustering refers to a very broad set of techniques for finding subgroups, or clusters, in a data set — ISLR

# Goals: partition of the observations into distinct clusters so that

  # observations within clusters are more similar to each other

  # observations in different clusters are more different from each other


# Distance between observations
# What does it means for two or more observations to be similar or different?
  # This require characterizing the distance between observations

# Clusters: groups of observations that are “close” together


# Lloyd's Algorithm

#Choose k random centers, aka centroids

# Assign each observation closest center (using Euclidean distance)

# Repeat until cluster assignment stop changing:
  
  
  # Compute new centroids as the averages of the updated groups

  # Reassign each observations to closest center

# Converges to a local optimum, not the global

# Results will change from run to run (set the seed!)

theme_set(theme_light())
glimpse(gapminder)

gapminder |> 
  ggplot(aes(gdp))+
  geom_histogram()

clean_gapminder <- gapminder |> 
  filter(year==2011, !is.na(gdp)) |> 
  mutate(log_gdp=log(gdp))

glimpse(clean_gapminder)

clean_gapminder <- clean_gapminder |> 
  mutate(
    std_log_gdp = as.numeric(scale(log_gdp)),
    std_life_expectancy = as.numeric(scale(life_expectancy)))


std_kmeans <- clean_gapminder |> 
  select(std_log_gdp, std_life_expectancy) |> 
  kmeans(centers = 4, nstart = 1, algorithm = 'Lloyd')


clean_gapminder |> 
  mutate(country_cluster = factor(std_kmeans$cluster)) |> 
  ggplot(aes(x=std_log_gdp, y=std_life_expectancy,
             color = country_cluster))+
  geom_point(size = 4)+
  ggthemes::scale_color_colorblind()+
  theme(legend.position = 'bottom')
  coord_fixed()

  
  
init_kmpp <- clean_gapminder |> 
  select(std_log_gdp, std_life_expectancy) |> 
  kcca(k = 4, control = list(initcent = 'kmeanspp'))

clean_gapminder |> 
  mutate(country_clusters = factor(init_kmpp@cluster)) |> 
  ggplot(aes(x = std_log_gdp, y = std_life_expectancy,
             color = country_clusters))+
  geom_point()+
  ggthemes::scale_color_colorblind()


kmeans_elbow <- function(k_val){
  kmeans_results <- clean_gapminder |> 
    select(std_log_gdp, std_life_expectancy) |> 
    kmeans(centers = k_val, nstart = 30)
  
  kmeans_out <- tibble(
    clusters = k_val,
    total_wss = kmeans_results$tot.withinss
  )
  
  return(kmeans_out)
}

n_clusters <- 2:12

n_clusters |> 
  map(kmeans_elbow) |> 
  list_rbind()
kmeans_results$tot.withnss