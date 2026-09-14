# Elastic net - combination of lasso and ridge regression
library(tidyverse)
theme_set(theme_light())
# more info: https://rdrr.io/cran/ElemStatLearn/man/prostate.html
prostate <- read_tsv("https://hastie.su.domains/ElemStatLearn/datasets/prostate.data")
glimpse(prostate)

library(glmnet)

model_x <- prostate |> 
  select(lcavol:lpsa) |> 
  as.matrix()
model_y <- prostate$lpsa

prostate_lm <- lm(
  lpsa~lcavol+lweight+age+svi+lcp+gleason+pgg45,
  data = prostate
)

library(broom)

prostate_lm |> 
  tidy() |> 
  mutate(term = fct_reorder(term, estimate)) |> 
  ggplot(aes(x = estimate, y = term))+
  geom_col(aes(fill = estimate > 0),
           show.legend = F)



# ridge regression

prostate_ridge <- glmnet(
  x = model_x,
  y = model_y,
  alpha = 0
)

plot(prostate_ridge, xvar = 'lambda')

# By default, cv.glmnet spits out a 10 fold cross validation
prostate_ridge_cv <- cv.glmnet(
  x = model_x,
  y = model_y,
  alpha = 0
)

plot(prostate_ridge_cv)

str(prostate_ridge_cv)

tidy_ridge_coef <- prostate_ridge_cv$glmnet.fit

tidy_ridge_coef |> 
  tidy() |> 
  ggplot(aes(x = lambda, y = estimate, group = term))+
  geom_line()+
  scale_x_log10()+
  geom_vline(xintercept = prostate_ridge_cv$lambda.min)+
  geom_vline(xintercept = prostate_ridge_cv$lambda.1se,
             linetype = 'dashed',
             color = 'red')


tidy_ridge_coef |> 
  tidy() |> 
  filter(lambda == prostate_ridge_cv$lambda.1se) |> 
  ggplot(aes(estimate, reorder(term, estimate)))+
  geom_col(aes(fill = estimate > 0),
           show.legend = F)


prostate_lasso_cv <- cv.glmnet(
  x = model_x,
  y = model_y,
  alpha = 1
)

tidy_lasso_coef <- prostate_lasso_cv$glmnet.fit

tidy_lasso_coef |> 
  tidy() |> 
  ggplot(aes(x = lambda, y = estimate, group = term))+
  geom_line(alpha = .75)+
  scale_x_log10()+
  geom_vline(xintercept = prostate_lasso_cv$lambda.min)+
  geom_vline(xintercept = prostate_lasso_cv$lambda.1se,
             linetype = 'dashed',
             color = 'red')

lasso_final <- glmnet(
  x = model_x, y = model_y, 
  alpha = 1,
  lambda = prostate_lasso_cv$lambda.1se
)

library(vip)

lasso_final |> 
  vip()


prostate_lasso_cv |> 
  tidy() |> 
  ggplot(aes(x = lambda, y = nzero))+
  geom_line()+
  geom_vline(xintercept = prostate_lasso_cv$lambda.min)+
  geom_vline(xintercept = prostate_lasso_cv$lambda.1se)+
  scale_x_log10()


set.seed(2024)
fold_id <- sample(rep(1:10, length.out = nrow(prostate)))

cv_ridge <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 1)
cv_enet50 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0.5)
cv_enet25 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0.25)

min(cv_ridge$cvm)
min(cv_lasso$cvm)
min(cv_enet50$cvm)
min(cv_enet25$cvm)

cv_enet50 |> 
  tidy() |> 
  ggplot(aes(x = lambda, y = nzero))+
  geom_line()+
  geom_vline(xintercept = prostate_lasso_cv$lambda.min)+
  geom_vline(xintercept = prostate_lasso_cv$lambda.1se)+
  scale_x_log10()