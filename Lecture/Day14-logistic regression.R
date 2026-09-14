# odds = p/(1-p)
# p = odds/(1+odds)

library(tidyverse)
theme_set(theme_minimal())

library(MASS)
pima <- as_tibble(MASS::Pima.tr)

pima <- pima |> 
  mutate(pregnancy = ifelse(npreg > 0, 'Yes', 'No'))


pima_logit <- glm(type ~ pregnancy + bp,
                  family = binomial, 
                  data = pima)

library(broom)
tidy(pima_logit)

tidy(pima_logit, exponentiate = T) # Interpret on an odds scale

# For categorical examples
# pregnancy changes the log-odds of diabetes, relative to the baseline value, by -0.468. 
# The odds ratio is exp(-0.47) = 0.626

# For quantitative example
# each unit of increase in blood pressure (in mmHg) is associated with 
  # an increase in the log-odds of diabetes of 0.0403, 
  # or an increase in the odds of diabetes by a multiplicative factor of exp(0.0403) = 1.04


pima_logit |> 
  augment()

pima |> 
  mutate(pred_prob = fitted(pima_logit),
         i_type = as.numeric(type == "Yes")) |> 
  ggplot(aes(bp)) +
  geom_line(aes(y = pred_prob, color = pregnancy), linewidth = 2) +
  geom_point(aes(y = i_type), alpha = 0.3, color = "darkorange", size = 4)


pred_prob <- predict(pima_logit, type = 'response') # initial predicted values in log odds scale  

pred_class <- ifelse(pred_prob > 0.5, 'Yes', 'No')
pred_binary <- ifelse(pred_prob > .5, 1, 0)

tibble(table(pred_class, pima$type)) # Confusion matrix

mean(pred_class != pima$type) # Mean misclassidication rate. How many were misclassified
