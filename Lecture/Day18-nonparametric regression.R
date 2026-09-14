library(tidyverse)
theme(theme_bw())
savant <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/savant.csv")
batted_balls <- savant |> 
  filter(type == "X") |> 
  mutate(is_hr = as.numeric(events == "home_run")) |> 
  filter(!is.na(launch_angle), !is.na(launch_speed), !is.na(is_hr))
head(batted_balls)


batted_balls |> 
  ggplot(aes(x = launch_speed, y = launch_angle,
             color = factor(is_hr)))+
  geom_point(alpha = .2)


set.seed(1999)

# Simple 50-50 split
train <- batted_balls |> 
  slice_sample(prop = .5)
test <- batted_balls |> 
  anti_join(train)


library(mgcv)
tic()
hr_gam <- gam(is_hr ~ s(launch_speed) + s(launch_angle),
    family = binomial,
    method = 'REML',  # Restricted maximum likelihood
    data = train)
toc()
summary(hr_gam)

library(broom)
tidy(hr_gam)
glance(hr_gam)

library(gratia)
draw(hr_gam, fun = plogis, constant = coef(hr_gam)[1])

appraise(hr_gam) # for more diagnostics
gam.check(hr_gam)

library(dplyr)
# IN SAMPLE
hr_gam |> 
  augment(newdata = train, type.predict = 'response') |> 
  dplyr::mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(is_hr == pred_class))

#OUT OF SAMPLE
hr_gam |> 
  augment(newdata = test, type.predict = 'response') |> 
  dplyr::mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(is_hr == pred_class))


hr_logit <- glm(
  is_hr ~ launch_speed + launch_angle,
  family = binomial,
  data = train
)


# IN SAMPLE
hr_logit |> 
  augment(newdata = train, type.predict = 'response') |> 
  dplyr::mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(is_hr == pred_class))

#OUT OF SAMPLE
hr_logit |> 
  augment(newdata = test, type.predict = 'response') |> 
  dplyr::mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(is_hr == pred_class))



# CONTINUOUS INTERACTIONS

hr_gam_mult <- gam(is_hr ~ s(launch_speed, launch_angle),
                   family = binomial,
                   method = 'REML',
                   data = train)

draw(hr_gam_mult)

hr_gam_mult |> 
  smooth_estimates() |> 
  mutate(prob = plogis(.estimate))
  ggplot(aes(launch_speed, launch_angle, z = prob))+
  geom_contour_filled()


# IN SAMPLE
hr_gam_mult |> 
  augment(newdata = train, type.predict = 'response') |> 
  dplyr::mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(is_hr == pred_class))

#OUT OF SAMPLE
hr_gam_mult |> 
  augment(newdata = test, type.predict = 'response') |> 
  dplyr::mutate(pred_class = round(.fitted)) |> 
  summarize(correct = mean(is_hr == pred_class))
