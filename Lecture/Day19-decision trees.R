# Classiffication Trees

# Predict that each observation belongs to the most commonly occurring class in the region to which it belongs
# Just like regression trees, use recursive binary splitting to grow a classification tree
# Instead of RSS, use the Gini index

library(tidyverse)
theme_set(theme_light())
batted_balls <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/batted_balls.csv")
glimpse(batted_balls)

set.seed(123)
train <- batted_balls |> 
  slice_sample(prop = 0.5)
test <- batted_balls |> 
  anti_join(train)

library(caret)
library(rpart)

hr_tree <- train(as.factor(is_hr) ~ .,
                 method = 'rpart',
                 tuneLength = 20,
                 trControl = trainControl(method = 'cv', number = 10), data = train)

str(hr_tree)

plot(hr_tree)

hr_tree$bestTune$cp # parameter from cross validation
ggplot(hr_tree) +
  geom_vline(xintercept = hr_tree$bestTune$cp,
             linetype = 'dashed')

library(rpart.plot)

rpart.plot(hr_tree$finalModel)

train |> 
  mutate(pred = predict(hr_tree, newdata = train)) |> 
  summarize(correct = mean(is_hr == pred))

rpart.rules(hr_tree$finalModel)


test |> 
  mutate(pred = predict(hr_tree, newdata = test)) |> 
  summarize(correct = mean(is_hr == pred))



# check for variable importance
library(vip)
hr_tree |> 
  vip()


library(pdp) # plotting partial dependecies between predictor variables

hr_tree |> 
  partial(pred.var = 'launch_speed',
      which.class = 2,
      prob = T) |> 
  autoplot()


hr_tree |> 
  partial(pred.var = c('launch_speed', 'launch_angle'),
          which.class = 2,
          prob = T,
          progress = T) |> 
  autoplot()


batted_balls |> 
  ggplot(aes(launch_speed, launch_angle)) +
  geom_point(aes(color = factor(is_hr)))

