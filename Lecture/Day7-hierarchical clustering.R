# Hierarchical clustering: does not require commitment to a particular choice of clusters

  # In fact, we end up with a tree-like visual representation of the observations, called a dendrogram

  # This allows us to view at once the clusterings obtained for each possible number of clusters

  # Common approach: agglomerative (bottom-up) hierarchical clustering: build a dendrogram starting from the leaves and combining clusters up to the trunk

  # There’s also divisive (top-down) hierarchical clustering: start with one large cluster and then break the cluster recursively into smaller and smaller pieces


theme_set(theme_light())
utah_health <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/utah_health.csv")
glimpse(utah_health)

utah_health |> 
  ggplot(aes(x = PercentOver65, y = DiabeticRate))+
  geom_point()


utah_health <- utah_health |> 
  mutate(
    std_pct_over65 = as.numeric(scale(PercentOver65)),
    std_diabetic_rate = as.numeric(scale(DiabeticRate))# scale method normally distrubutes this standardization
  )

utah_health |> 
  ggplot(aes(x = std_pct_over65, y = std_diabetic_rate))+
  geom_point(alpha = .7)+
  coord_fixed()


county_dist <- utah_health |> 
  select(std_pct_over65, std_diabetic_rate) |> 
  dist()

county_dist_matrix <- county_dist |> 
  as.matrix()

colnames(county_dist_matrix) <- utah_health$County
rownames(county_dist_matrix) <- utah_health$County

long_dist_matrix <- county_dist_matrix |> 
  as_tibble() |> 
  mutate(county1 = utah_health$County) |> 
  pivot_longer(!county1,
               names_to = 'county2',
               values_to = 'distance')

# Alternate way using widyr package
utah_health |> 
  pairwise_dist(County, std_diabetic_rate, std_pct_over65)


long_dist_matrix |> 
  ggplot(aes(x = county1, y = county2, fill = distance))+
  geom_tile()+
  scale_fill_gradient(low = "darkorange", 
                      high = "darkblue") +
  coord_fixed() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())+
  labs(
    x = NULL,
    y = NULL
  )

library(seriation)

county_dist_seriate <- seriate(county_dist)
county_order <- get_order(county_dist_seriate)
county_names_order <- utah_health$County[county_order]

long_dist_matrix |>
  mutate(
    county1 = fct_relevel(county1, county_names_order),
    county2 = fct_relevel(county2, county_names_order)
  ) |> 
  ggplot(aes(x = county1, y = county2, fill = distance))+
  geom_tile()+
  scale_fill_gradient(low = "darkorange", 
                      high = "darkblue") +
  coord_fixed() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())+
  labs(
    x = NULL,
    y = NULL
  )



# (Agglomerative) Hierarchical clustering
# Let’s pretend all observations are in their own cluster

# Step 1: Compute the pairwise dissimilarities between each cluster
  # e.g., distance matrix on previous slides

# Step 2: Identify the pair of clusters that are least dissimilar

# Step 3: Fuse these two clusters into a new cluster!
  # Repeat Steps 1 to 3 until all observations are in the same cluster

# “Bottom-up”, agglomerative clustering that forms a tree/hierarchy of merging

# No mention of any randomness. And no mention of the number of clusters k


# Example using complete linkage
utah_complete <- county_dist |> 
  hclust(method = 'complete')

utah_health |> 
  mutate(cluster = factor(cutree(utah_complete, k = 3))) |> 
  ggplot(aes(x = std_pct_over65, y = std_diabetic_rate,
             color = cluster))+
  geom_point()+
  ggthemes::scale_color_colorblind()


# Plottign a dendrogram
library(ggdendro)

utah_complete |> 
  ggdendrogram(theme_dendro = F)+
  labs(y = 'Dissimilarities between clusters')


# We still can solve for perfect k,
 # either plot a dendrogram then cut at suitable height, or specify k yourself
utah_health |> 
  mutate(cluster = factor(cutree(utah_complete, h = 3))) |> 
  ggplot(aes(x = std_pct_over65, y = std_diabetic_rate,
             color = cluster))+
  geom_point()+
  ggthemes::scale_color_colorblind()


# Minimax Linkage
library(protoclust)

utah_minimax <- protoclust(county_dist_matrix)

utah_minimax |> 
  as.hclust() |> 
  as.dendrogram() |> 
  ggdendrogram()


minimax_county_clusters <- protocut(utah_minimax, k = 3)

utah_health |> 
  mutate(cluster = factor(minimax_county_clusters$cl)) |> 
  ggplot(aes(x = std_pct_over65, y = std_diabetic_rate,
             color = cluster))+
  geom_point()+
  ggthemes::scale_color_colorblind()



utah_health |> 
  slice(minimax_county_clusters$protos)

utah_health |> 
  mutate(cluster = factor(minimax_county_clusters$cl)) |> 
  ggplot(aes(x = log(Population), fill = cluster))+
  geom_density(alpha = .4)
