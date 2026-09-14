library(tidyverse)
theme_set(theme_light())
movies <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/movies.csv")
glimpse(movies)

library(janitor)
movies <- movies |> 
  clean_names() # Does magic to variable names

glimpse(movies)


movies |> 
  select(rotten_tomatoes, audience_score, theaters_open_week,
         opening_weekend, budget, domestic_gross, foreign_gross) |> 
  drop_na() # Just for illustration purposes. Never form a habit


movies |> 
  ggplot(aes(audience_score))+
  geom_histogram()

library(ggcorrplot)
movies |> 
  cor() |> 
  ggcorrplot(type = 'lower', lab = T)


library(corrr)
movies |> 
  correlate(diagonal = 1) |> 
  stretch() |> 
  ggplot(aes(x, y, fill = r))+
  geom_tile()


movies_feat <- movies |> 
  select(-audience_score)

feat_cor <- movies_feat |> 
  cor()
View(feat_cor)

library(GGally)
movies |> 
  ggpairs()


set.seed(100)
k <- 10
movies <- movies |> 
  mutate(test_fold = sample(rep(1:10, length.out = n()))) 


# train and test sets -----------------------------------------------------

# Writing a function for k-fold cross-validation
get_cv_pred <- function(model_formula){
  get_test_pred <- function(k) {
    train_data <- movies |> 
      filter(test_fold!=3)
    
    test_data <- movies |> 
      filter(test_fold==3)
    
    # movies |> 
    #   anti_join(train_data)
    
    train_fit <- lm(as.formula(model_formula),
                    data = train_data)
    
    test_pred <- predict(train_fit, newdata = test_data)
    test_actual <- test_data$audience_score
    
    train_out <- tibble(test_pred, test_actual, k)
  }
  
  out <- map(1:k, get_test_pred) |> 
    bind_rows()
  return(out)
}





all_fit <- lm(
  audience_score ~ rotten_tomatoes + theaters_open_week + opening_weekend + budget + domestic_gross + foreign_gross, 
  data = movies
)
# summary(all_fit)
library(broom)
all_fit |> 
  tidy(conf.int = T) |> 
  filter(term != '(Intercept)') |> 
  ggplot(aes(x = estimate, y = term))+
  geom_point()+
  geom_errorbarh(aes(xmin = conf.low, 
                     xmax = conf.high),
                 width = .2)+
  geom_vline(xintercept = 0, linetype = 'dashed')