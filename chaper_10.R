pacman::p_load(tidyverse, tidymodels)

set.seed(1234)

'cheonan_attendance.csv' %>% 
  read.csv() %>% 
  as_tibble() -> cheonan_attendance

cheonan_attendance %>% 
  glimpse()

cheonan_attendance %>%
  summarise(여성비율 = mean(성별 == '여'))

cheonan_attendance %>%
  rep_sample_n(reps = 10,
             size = 20,
             replace = TRUE) %>% 
  group_by(replicate) %>% 
  summarise(여성비율 = mean(성별 == '여'),
                .groups='drop') -> cheonan_sample

cheonan_sample %>% 
  summarise(결과 = 여성비율 %>% mean())

cheonan_sample %>% 
  select('여성비율') %>% 
  rep_sample_n(reps = 1000,
             size = 20,
             replace = TRUE) -> cheonan_sample_bootstrap

cheonan_sample_bootstrap %>% 
  summarise(여성비율_평균 = mean(여성비율)) %>% 
  summarise(결과 = 여성비율_평균 %>% mean())
  
cheonan_sample %>%
  ggplot(aes(x = 여성비율)) +
  geom_histogram(binwidth = .05, fill = 'gray75', color = 'white')

cheonan_sample_bootstrap %>% 
  summarise(여성비율_평균 = mean(여성비율)) %>% 
  ggplot(aes(x = 여성비율_평균)) + 
  geom_histogram(binwidth = .01, fill = 'gray75', color = 'white')

cheonan_sample_bootstrap %>%
  summarise(여성비율_평균 = mean(여성비율)) %>%
  summarise(low = quantile(여성비율_평균, .025),
            high = quantile(여성비율_평균, .975))

cheonan_sample %>% 
  specify(response = 여성비율) 

cheonan_sample %>% 
  specify(response = 여성비율) %>% 
  class()

cheonan_sample %>%
  specify(response =  여성비율) %>%
  generate(reps = 1000, type = 'bootstrap')

cheonan_sample %>%
  specify(response =  여성비율) %>%
  generate(reps = 1000, type = 'bootstrap') %>%
  calculate(stat = 'mean')

cheonan_sample %>%
  specify(response =  여성비율) %>%
  generate(reps = 1000, type = 'bootstrap') %>%
  calculate(stat = 'mean') -> cheonan_bootstrap

cheonan_bootstrap %>% 
  visualize()

cheonan_bootstrap %>% 
  get_confidence_interval()

cheonan_bootstrap %>% 
  get_confidence_interval() -> ci_endpoints

cheonan_bootstrap %>% 
  get_confidence_interval(level = .9)

cheonan_bootstrap %>% 
  visualize() +
  shade_confidence_interval(
    endpoints = ci_endpoints
  )

cheonan_bootstrap %>% 
  visualize() +
  shade_confidence_interval(
    endpoints = ci_endpoints,
    color = 'gray75', fill = 'gray75', size=0
  )

cheonan_bootstrap %>% 
  get_confidence_interval(type = 'se', point_estimate = .570)

cheonan_bootstrap %>%
  visualize() +
  shade_confidence_interval(
    endpoints = ci_endpoints,
    color = 'gray75', fill = 'gray75', size = 0)+
  geom_vline(
    xintercept = c(.522, .618),
    color = 'darkorange',
    linetype = 'dashed',
    lwd = 1
  )