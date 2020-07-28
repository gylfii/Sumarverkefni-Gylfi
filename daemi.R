library(tidyverse)
library(knitr)
library(kableExtra)
library(car)

d <- mtcars

fit1 <- lm(mpg ~ factor(cyl) + hp, data = d)
fit2 <- lm(mpg ~ hp, data = d)

Anova(fit1) %>%
  broom::tidy() %>%
  mutate(Model = 1) -> t1

Anova(fit2) %>%
  broom::tidy() %>%
  mutate(Model = 2) -> t2

rbind(t1, t2) %>%
  select(6, everything()) %>%
  mutate(pv = ifelse(p.value < 0.0001, '< 0.0001', as.character(p.value))) -> table

write_csv(table, 'anovatable.csv')


strsplit('0.0001234')
