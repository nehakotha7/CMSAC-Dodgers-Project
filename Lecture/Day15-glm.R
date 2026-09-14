library(tidyverse)
theme_set(theme_light())
goals <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/goals.csv")
glimpse(goals)

goals |> 
  count(n_goals) |> 
  ggplot(aes(n_goals, n))+
  geom_col()+
  scale_x_continuous(breaks = 0:8)

goals |> 
  count(n_goals, league) |> 
  ggplot(aes(n_goals, n))+
  geom_col()+
  scale_x_continuous(breaks = 0:8)+
  facet_wrap(~ league)

goals |> 
  group_by(league) |> 
  summarize(avg_goals = mean(n_goals))

goals |> 
  group_by(is_home) |> 
  summarize(avg_goals = mean(n_goals))


goals |> 
  count(n_goals, is_home) |> 
  mutate(is_home = factor(is_home)) |> 
  ggplot(aes(n_goals, y = n, fill = is_home, group = is_home))+
  geom_col(position = 'dodge')+
  scale_x_continuous(breaks = 0:8)



# fitting a poisson regression model
goals_poisson <- glm(n_goals~league+is_home,
    family = poisson, 
    data = goals)
# get summary
library(broom)
tidy(goals_poisson, exponentiate = T)

# correcting overdispersion when variance >= mean
goals_qp <- glm(n_goals~league+is_home,
                     family = quasipoisson, 
                     data = goals)


sum(residuals(goals_poisson, type = 'pearson'))/df.residual(goals_poisson)



# negative binomial regression
goals_nb <- MASS::glm.nb(n_goals ~ league + is_home,
                        data = goals)
summary(goals_nb)

library(pscl)
# Poisson breaks with the prevalence of zeros in the dataset


# zero inflated poisson
goals_zip <- zeroinfl(n_goals ~ league + is_home,
                      data = goals)
summary(goals_zip)


n_folds <- 5

goals_folds <- goals |> 
  distinct(match_id) |> 
  mutate(match_fold = sample(rep(1:n_folds, length.out = n())))

goals <- goals |> 
  left_join(goals_folds, by = 'match_id')



poisson_cv <- function(k) {
  train <- goals |> 
    filter(match_fold != 3)
  test <- goals |> 
    filter(match_fold == 3)
  
  fit <- glm(n_goals ~ league +is_home, family = poisson,
             data = train)
  
  
  preds <- predict(fit, newdata = test, type = 'response')
  out <- tibble(rmse = sqrt(mean(test$n_goals - preds) ^ 2),
                fold_id = k)
  return(out)
  
}



map(1:n_folds, poisson_cv) |> 
  bind_rows() |> 
  summarize(mean_rmse = mean(rmse))