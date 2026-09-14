# plotting a receiver operating characteristic (ROC) curve
  # we always want to maximize area under the curve (AUC)

library(tidyverse)
theme_set(theme_light())
batted_balls <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/batted_balls.csv")

hr_logit <- glm(is_hr ~ launch_speed + launch_angle,
    family = binomial,
    data = batted_balls)

library(pROC)
library(dplyr)
hr_roc <- batted_balls |> 
  mutate(pred_hr = predict(hr_logit, type = 'response')) |> 
  roc(is_hr, pred_hr)

str(hr_roc)

hr_roc$auc # get area under curve

library(ggplot2)
tibble(
  specificity = hr_roc$specificities,
  sensitivity = hr_roc$sensitivities
) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = 'dashed')


library(readr)
nfl_pbp <- read_csv("https://github.com/36-SURE/36-SURE.github.io/raw/main/data/nfl_pbp.csv.gz")
glimpse(nfl_pbp)


library(forcats)
# additional data prep
nfl_pbp <- nfl_pbp |> 
  # make No_Score the reference level
  mutate(next_score_half = fct_relevel(next_score_half, "No_Score"),
         log_ydstogo = log(ydstogo),
         down = factor(down))


library(nnet)

ep_model <- multinom(next_score_half ~ half_seconds_remaining + 
           yardline_100 + down + log_ydstogo + log_ydstogo * down + yardline_100 * down,
         data = nfl_pbp,
         maxit = 300)

summary(ep_model)

event_prob <- ep_model |>
  predict(newdata = nfl_pbp, type = 'probs') |> 
  as_tibble() |> 
  mutate(ep = Touchdown * 7 + Field_Goal * 3 + Safety * 2 + 
           Opp_Touchdown * -7 + Opp_Field_Goal * -3 + Opp_Safety * -2)

event_prob