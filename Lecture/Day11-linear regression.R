library(tidyverse)
theme_set(theme_light())
library(dslabs)
clean_gapminder <- gapminder |>
  filter(year == 2011, !is.na(gdp)) |>
  mutate(log_gdp = log(gdp))
glimpse(clean_gapminder)

simple_lm <- lm(life_expectancy ~ log_gdp, 
                data = clean_gapminder)

library(broom)
simple_lm |> 
  tidy()
glance(simple_lm)
summary(simple_lm)

fitted()
predict()

simple_lm |> 
  pluck('fitted.values')

augment(simple_lm)