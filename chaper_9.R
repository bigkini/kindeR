pacman::p_load(tidyverse, tidymodels)

set.seed(1234)

'gocheock_attendance.csv' %>% 
  read.csv() %>% 
  as_tibble() -> gocheock_attendance

gocheock_attendance %>% 
  glimpse()

gocheock_attendance %>%
  rep_sample_n(reps = 15,
               size = 30,
               replace = TRUE) -> gocheock_sample

gocheock_sample

gocheock_sample %>%
  summarise(여성합계 = sum(성별 == '여'))

gocheock_sample %>%
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30)

gocheock_sample %>%
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30) %>%
  summarise(여성비율_평균 = mean(여성비율))

gocheock_sample %>%
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30) %>%
  ggplot(aes(x = 여성비율)) +
  geom_histogram(binwidth = .05, fill = 'gray75', color = 'white') 

gocheock_attendance %>%
  rep_sample_n(reps = 15,
               size = 30,
               replace = TRUE) -> gocheock_sample2

gocheock_sample %>% 
  mutate(trial = 1) %>% 
  bind_rows(
    gocheock_sample2 %>% 
      mutate(trial = 2)
  ) -> gocheock_sample

gocheock_sample

gocheock_sample %>%
  group_by(trial, replicate) %>% 
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30,
                .groups = 'drop') %>%
  summarise(여성비율_평균 = mean(여성비율))

gocheock_sample %>%
  group_by(trial, replicate) %>% 
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30,
                .groups = 'drop') %>%
  ggplot(aes(x = 여성비율)) +
  geom_histogram(binwidth = .05, fill = 'gray75', color = 'white')

gocheock_attendance %>% 
  rep_sample_n(reps = 15 * 3,
               size = 30,
               replace = TRUE) %>%
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30) %>% 
  summarise(여성비율_평균 = mean(여성비율))

gocheock_attendance %>% 
  summarise(여성비율=mean(성별=='여'))

gocheock_attendance %>%
  rep_sample_n(reps = 15 * 100,
               size = 30,
               replace = TRUE) %>%
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30) %>% 
  summarise(여성비율_평균 = mean(여성비율))

gocheock_attendance %>% 
  rep_sample_n(reps=15*100,
               size=30,
               replace=TRUE) %>% 
  summarise(여성합계  = sum(성별  == '여'),
                여성비율 = 여성합계 / 30) %>% 
  ggplot(aes(x = 여성비율)) +
  geom_histogram(binwidth = .05, fill = 'gray75', color = 'white') +
  geom_vline(xintercept = .437)

crossing(
  x = c(1, 2, 3),
  y = c(1, 2, 3)
)

crossing(
  x = c(1, 2, 3),
  y = c(1, 2, 3)
) %>% 
  mutate(평균 = (x + y) /2)

crossing(
  x = c(1, 2, 3),
  y = c(1, 2, 3)
) %>% 
  mutate(평균 = (x + y) /2) %>% 
  summarise(평균_평균 = 평균 %>% mean())

crossing(
  x = c(1, 2, 3),
  y = c(1, 2, 3)
) %>% 
  mutate(평균 = (x + y) /2 ,
           편차 = 2 - 평균,
           편차_제곱 = 편차^2) %>% 
  summarise(across(평균:편차_제곱, mean))

crossing(
  x = c(1, 2, 3),
  y = c(1, 2, 3)
) %>% 
  mutate(평균 = (x + y) /2 ,
           오차 = 2 - 평균,
           오차_제곱 = 오차^2) %>% 
  summarise(across(평균:오차_제곱, mean))

tibble(
  x = (rnorm(n = 50000, mean = 50, sd = 10))
) %>% 
  bind_rows(
    tibble(x = rbeta(n = 50000, 50, 10) * 100)
  ) -> population1

population1 %>% 
  ggplot(aes(x=x)) +
  geom_histogram(bins = 30, fill = 'gray75', color = 'white')

crossing(size = c(1, 5, 10, 30, 50, 1000),
         reps = c(10, 100, 1000, 10000)) %>%
  rowwise() %>%
  mutate(
    sample_mean =
      population1 %>%
      rep_sample_n(
        size = size,
        reps = reps,
        replace = TRUE
      ) %>%
      group_by(replicate) %>%
      summarise(mean = mean(x), .groups = 'drop') %>%
      pull(mean) %>% lst()
  ) %>%
  unnest(sample_mean) -> sample_mean_tbl

sample_mean_tbl %>%
  ggplot(aes(x = sample_mean)) +
  geom_density(fill = 'gray75') +
  facet_grid(reps ~ size) +
  geom_vline(xintercept = population1$x %>% mean(),
             linetype = 'dotted')

sample_mean_tbl %>%
  summarise(mean = sample_mean %>% mean())

population1$x %>% mean()

sample_mean_tbl %>%
  group_by(size) %>%
  summarise(mean = sample_mean %>% mean(),
            se = sample_mean %>% sd(),
            .groups = 'drop') %>%
  mutate(sd = sd(population1$x) / sqrt(size))

crossing(
  size = 2,
  reps = 10
) %>%
  rowwise() %>%
  mutate(
    sample_mean = 
      population1 %>%
      rep_sample_n(
        size = size,
        reps = reps,
        replace = TRUE
      ) %>%
      group_by(replicate) %>%
      summarise(mean = mean(x), .groups = 'drop') %>%
      pull(mean) %>% lst()
  ) %>%
  unnest(sample_mean)

crossing(
  size = 2,
  reps = 10
) %>%
  rowwise() %>%
  mutate(
    sample_mean = 
      population1 %>%
      rep_sample_n(
        size = size,
        reps = reps,
        replace = TRUE
      ) %>%
      group_by(replicate) %>%
      summarise(mean = mean(x), .groups = 'drop') %>%
      pull(mean) %>% lst())

crossing(
  size = 2,
  reps = 10
) %>%
  rowwise() %>%
  mutate(
    sample_mean = 
      population1 %>%
      rep_sample_n(
        size = size,
        reps = reps,
        replace = TRUE
      ) %>%
      group_by(replicate) %>%
      summarise(mean = mean(x), .groups = 'drop') %>%
      pull(mean) %>% lst()) %>% 
  .$sample_mean

population1 %>%
  rep_sample_n(size = 2, reps = 5) %>%
  group_by(replicate) %>%
  summarise(mean = mean(x), .groups = 'drop')
