choose(45, 6)

pbirthday(28)

pacman::p_load(tidyverse, tidymodels)

set.seed(1234)

tibble(날짜=1:365) %>% 
  rep_sample_n(28)

c(1, 1, 3, 3, 3, 3) %>% 
  duplicated()

c(1, 1, 3, 3, 3, 3) %>% 
  duplicated() %>% 
  sum()

c(1, 1, 3, 3, 3, 3) %>% 
  duplicated() %>% 
  any()

tibble(날짜=1:365) %>% 
  rep_sample_n(28) %>% 
  summarise(중복 = 날짜 %>% duplicated() %>% any())

tibble(날짜=1:365) %>% 
  rep_sample_n(size = 28, replace = TRUE) %>% 
  summarise(중복 = 날짜 %>% duplicated() %>% any())

tibble(날짜 = 1:365) %>%
  rep_sample_n(reps = 100,
               size = 28,
               replace = TRUE) %>%
  group_by(replicate) %>%
  summarise(중복  =  날짜  %>% duplicated() %>% any(),
              .groups = 'drop')

tibble(날짜 = 1:365) %>%
  rep_sample_n(reps = 100,
               size = 28,
               replace = TRUE) %>%
  group_by(replicate) %>%
  summarise(중복  =  날짜  %>% duplicated() %>% any(),
              .groups = 'drop') %>%
  summarise(결과 = 중복 %>% mean())

tibble(
  사람 = 2:365
)

tibble(
  사람 = 2:365
) %>% 
  rowwise() %>% 
  mutate(
    결과 = tibble(날짜 = 1:365) %>%
      rep_sample_n(
        reps = 100,
        size = 사람,
        replace = TRUE
      ) %>%
      group_by(replicate) %>%
      summarise(중복 = 날짜 %>% duplicated() %>% any(),
                  .groups = 'drop') %>%
      summarise(결과 = 중복 %>% mean())
  ) 

tibble(
  사람 = 2:365
) %>% 
  rowwise() %>% 
  mutate(
    결과 = tibble(날짜 = 1:365) %>%
      rep_sample_n(
        reps = 100,
        size = 사람,
        replace = TRUE
      ) %>%
      group_by(replicate) %>%
      summarise(중복 = 날짜 %>% duplicated() %>% any(),
                  .groups = 'drop') %>%
      pull(중복) %>% mean()
    ) -> birthday_paradox_simulation

birthday_paradox_simulation %>% 
  filter(결과 >= .5)

birthday_paradox_simulation %>%
  ggplot(aes(x = 사람, y = 결과)) +
  geom_line()

birthday_paradox_simulation %>%
  ggplot(aes(x = 사람, y = 결과)) +
  geom_line() +
  coord_cartesian(xlim = c(2, 75))

tibble(
  사람 = 2:365
) %>% 
  rowwise() %>% 
  mutate(
    결과 = tibble(날짜 = 1:365) %>%
      rep_sample_n(
        reps = 10000,
        size = 사람,
        replace = TRUE
      ) %>%
      group_by(replicate) %>%
      summarise(중복 = 날짜 %>% duplicated() %>% any(),
                  .groups = 'drop') %>%
      pull(중복) %>% mean()
  ) -> birthday_paradox_simulation_10000

birthday_paradox_simulation_10000 %>% 
  filter(결과 >= .5)

pbirthday(23)

birthday_paradox_simulation_10000 %>%
  mutate(확률 = pbirthday(사람)) %>%
  ggplot(aes(x = 사람)) +
  geom_line(aes(y = 확률),
            lwd = 2.5,
            color = '#53bfd4',
            alpha = .25) +
  geom_line(aes(y = 결과), lwd = .75) +
  coord_cartesian(xlim = c(2, 75))

c(1, 2, 3) %>% 
  sample(1)

crossing(
  x = c(1, 2),
  y = c('a', 'b')
)

crossing(실험 = 1:100000,
           토스 = 1:100) %>% 
  rowwise() %>% 
  mutate(앞뒤 = sample(c(0, 1), 1)) -> coin_toss_simulation

coin_toss_simulation

coin_toss_simulation %>%
  group_by(실험) %>%
  summarise(앞 = sum(앞뒤)) %>%
  group_by(앞) %>%
  tally() %>%
  filter(앞 == 50)

coin_toss_simulation %>% 
  group_by(실험) %>%
  summarise(앞 = sum(앞뒤), .groups = 'drop') %>%
  group_by(앞) %>%
  ggplot(aes(x = 앞)) +
  geom_histogram(binwidth = 1, fill = 'gray70', color = 'white')

dbinom(x = 50, size = 100, prob = .5)

1 - pbinom(50, 100, .500)

pbinom(50, 100, .500, lower.tail = FALSE)

pbinom(60, 100, .500) - pbinom(40, 100, .500)

tibble(x = 0:100) %>%
  rowwise() %>%
  mutate(prob = dbinom(x, 100, 0.5)) %>%
  ggplot(aes(x = x, y = prob)) +
  geom_col(fill = 'gray70', color = 'white') +
  stat_function(
    fun = dnorm,
    args = list(mean = 50, sd = 5),
    lwd = 1,
    color = '#53bfd4'
  )
    
qnorm(p = .5, mean = 0, sd = 1)

tibble(
  x = rnorm(10000, mean = 0, sd = 1)
) %>%
  ggplot(aes(x = x)) +
  geom_histogram(fill = 'gray70', color = 'white', bins = 30)