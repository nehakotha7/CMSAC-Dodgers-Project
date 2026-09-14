library(tidyverse)
theme_set(theme_light())
library(stacks)
frogs <- tree_frogs |> 
  filter(!is.na(latency)) |> 
  select(-clutch, -hatched)
glimpse(frogs)


# implement random foresting
library(ranger)
frogs_rf <- ranger(latency ~ treatment + reflex + t_o_d + age, 
                   num.trees = 500, importance = "impurity", data = frogs)
frogs_rf

# variable importance
library(vip)
vip(frogs_rf)


# partial dependence plot
library(pdp)
frogs_rf |> 
  partial(pred.var = "age") |> 
  autoplot()


# model evaluation
frogs |> 
  mutate(pred = frogs_rf$predictions) |> 
  summarize(rmse = sqrt(mean((latency - pred) ^ 2)))

# Plot predictions versus original observed values
frogs |>
  mutate(pred = frogs_rf$predictions) |>
  ggplot(aes(latency, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")




# splitting into train and test -------------------------------------------


batted_balls <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/batted_balls.csv")
set.seed(123)

train <- batted_balls |> 
  slice_sample(prop = 0.5)
test <- batted_balls |> 
  anti_join(train)

x_train <- train |> 
  select(-is_hr) |> 
  as.matrix()
x_test <- test |> 
  select(-is_hr) |> 
  as.matrix()



library(xgboost)
library(caret)

xg_grid <- crossing(
  nrounds = seq(20, 150, 10), # specifiy # of trees for boosting. you don't need too much
  eta = c(.01, .05, .1),
  max_depth = 2:4,
  
)

# create hyperparameter grid
xg_grid <- crossing(nrounds = seq(20, 150, 10),
                    eta = c(0.01, 0.05, 0.1), gamma = 0,
                    max_depth = c(2, 3, 4), colsample_bytree = 1,
                    min_child_weight = 1, subsample = 1)


# tuning time
xg_tune <- train(x = x_train,
      y = train$is_hr,
      tuneGrid = xg_grid,
      trControl = trainControl(method = 'cv', number = 5),
      objective = 'binary:logistic',
      method = 'xgbTree')


# fit final model to training data
xg_fit <- xgboost(data = x_train,
                  label = train$is_hr,
                  objective = "binary:logistic",
                  nrounds = xg_tune$bestTune$nrounds,
                  params = as.list(select(xg_tune$bestTune, -nrounds)),
                  verbose = 0)
xg_fit |> 
  vip()
