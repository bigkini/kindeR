rm(list=ls())

pacman::p_load(tidyverse, tidymodels)
set.seed(1234)

tibble(
  trial = 1:10000,
  car = sample(1:3, 10000, replace = TRUE),
  initial_pick = sample(1:3, 10000, replace = TRUE)
) %>% 
  rowwise() %>%
  mutate(
    monty_hall = ifelse(car == initial_pick,
                        sample((1:3)[-car], 1),
                        (1:3)[-c(car, initial_pick)]
    ),
    the_other = (1:3)[-c(monty_hall, initial_pick)]
  ) %>% 
  ungroup() %>% 
  mutate(result = if_else(car == the_other, 1, 0), 
         success_rate = cumsum(result) / trial) %>%
  ggplot(aes(x = trial, y = success_rate)) +
  geom_line(lwd = 1.25) +
  geom_hline(yintercept = 2/3, linetype='dashed', color='#147893', lwd=1) +
  scale_y_continuous(limits = c(0, 1)) 

'kbo_batting_bayesian.csv' %>% 
  read.csv() %>% 
  as_tibble() -> kbo_batting

kbo_batting %>% 
  glimpse()

kbo_batting %>% 
  filter(tpa >= 250) %>% 
  mutate(avg = h / ab) -> kbo_batting_250

kbo_batting_250 %>%
  ggplot(aes(x = avg)) +
  geom_histogram(aes(y = ..density..),
                 fill = 'gray80',
                 color = 'white',
                 bins = 30)

pacman::p_load(MASS)

kbo_batting_250 %>% 
  pull(avg) %>% 
  fitdistr(dbeta, start = list(shape1 = 1, shape2 = 1))

kbo_batting_250 %>% 
  ggplot(aes(x = avg)) +
  geom_histogram(aes(y = ..density..),
                 fill = 'gray80',
                 color = 'white',
                 bins = 30) +
  geom_function(
    fun = dbeta,
    args = list(shape1 = 47, shape2 = 123),
    color = '#53bfd4',
    lwd = 1.25
  )

47 / (47 + 123)

kbo_batting_250 %>% 
  pull(avg) %>% 
  mean()

(45 + 47) / (109 + 47 + 123)