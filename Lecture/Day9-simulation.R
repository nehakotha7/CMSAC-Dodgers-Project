sample(letters, 2)

sample(c(0,1), size = 100, replace = T)

sample(1:100, 1) # sample a random integer between 1 and 100

theme_set(theme_light())

tibble(x = c(-4,4)) |> 
  ggplot(aes(x))+
  stat_function(fun = dnorm, color = 'blue')+
  stat_function(fun = dnorm, color = 'red',
                args = list(mean = -1, sd = .5))

curve(dnorm, from = -4, to = 4)

rnorm(100, mean = 2, sd = .1)

z <- rnorm(1000, mean = 0, sd = 1)
z_ecdf <- ecdf(z)
  class(z_ecdf)
  
tibble(z = sort(z)) |> 
  mutate(empirical = z_ecdf(z),
         true = pnorm(z)) |> 
  ggplot(aes(x = z))+
  geom_line(aes(y = empirical), color = 'red')+
  geom_line(aes(y = true),
            color = 'blue')



x <- runif(10000, 0, 1)
smaller <- ifelse(x < .5, x, 1 - x)

hist(smaller)
mean(smaller/ (1 - smaller))


# Monte Carlo for estimating pi
n_points <- 10^6
x <- runif(n_points, -1, 1)
y <- runif(n_points, -1, 1)

inside <- (x^2 + y^2 <= 1)
4*sum(inside) / n_points

# Plotting
tibble(x, y, inside) |> 
  ggplot(aes(x, y, color = factor(inside)))+
  geom_point()


# All pseudorandom number generators depend on what is called a seed value

# This puts the random number generator in a well-defined state, so that the numbers it generates, from then on, will be reproducible

# The seed is just an integer, and can be set with set.seed()

# The reason we set it: so that when someone else runs our simulation code, they can see the same—albeit, still random—results that we do

# Note: set.seed() will be helpful later on for things like cross-validation, 

# k-means clustering, etc. — basically anything that involves randomly sampling of the data


set.seed(2)
n_subjects <- 50
mean_drug <- 2
mean_nodrug <- runif(n_subjects, 0, 1)
x_drug <- 100 * rexp(n_subjects, 1 / mean_nodrug)
x_nodrug <- 100 * rexp(n_subjects, 1 / mean_nodrug)

tibble(x_drug, x_nodrug) |> 
  pivot_longer(x_drug:x_nodrug) |> 
  ggplot(aes(value))+
  geom_histogram(aes(fill = name),
                 alpha = .5)

# Revisiting k means clusterring
library(dslabs)
clean_gapminder <- gapminder |>
  filter(year == 2011, !is.na(gdp)) |>
  mutate(std_log_gdp = as.numeric(scale(log(gdp), center = TRUE, scale = TRUE)),
         std_life_exp = as.numeric(scale(life_expectancy, center = TRUE, scale = TRUE)))

gapminder_kmeans <- function(k){
  kmeans_obj <- clean_gapminder |> 
    select(std_log_gdp, std_life_exp) |> 
    kmeans(centers = k, nstart = 30)
  return(kmeans_obj$tot.withinss)
}

n_clusters <- 2:10

n_clusters |> 
  map(gapminder_kmeans) # Use mapping instead of for loops

kmeans_res <- rep(NA, length(n_clusters))
for(i in 1:length(n_clusters)){
  kmeans_res[i] <- gapminder_kmeans(n_clusters[i])
}


# Pre-allocation
# Example: When 100 coins are tossed, what is the probability that exactly 50 are heads?

library(tictoc)
n_runs <- 500000
a <- c()
tic()
for (i in 1:n_runs) {
  tosses <- sample(0:1, size = 100, replace = TRUE)
  a[i] <- sum(tosses)
}
toc()

b <- rep(NA, n_runs)
tic()
for (i in 1:n_runs) {
  tosses <- sample(0:1, size = 100, replace = TRUE)
  b[i] <- sum(tosses)
}
toc()

# exact: (factorial(100) / (factorial(50) * factorial(50))) * (1 / 2) ^ 100
mean(b == 50)


# Day 10
library(tidyverse)
dumb <- tibble(x = 1:4, y = 4:1)

write_csv(dumb, 'dumb.csv')

df <- read_csv('dumb.csv')

x <- rnorm(100)
x

write_rds()