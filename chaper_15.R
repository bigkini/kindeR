pacman::p_load(tidyverse, tidymodels)
set.seed(1234)

'2020_kbo_team_batting.csv' %>% 
  read.csv() %>% 
  as_tibble() -> batting_2020

batting_2020

batting_2020 %>%
  mutate(
    runs_z_score = (runs - mean(runs)) / sd(runs),
    avg_z_score = (avg - mean(avg)) / sd(avg),
    rectangle_size = runs_z_score * avg_z_score
  ) %>%
  summarise(size_sum = sum(rectangle_size))

8.50 / 9

tribble(
  ~team, ~avg, ~runs,
  'a', .245, .100,
  'b', .255, .110,
  'c', .265, .120,
  'd', .275, .130,
  'e', .285, .140
) %>% 
  mutate(
  runs_z_score = (runs - mean(runs)) / sd(runs),
  avg_z_score = (avg - mean(avg)) / sd(avg),
  rectangle_size = runs_z_score * avg_z_score
) %>%
  summarise(size_sum = sum(rectangle_size) / 4) 

batting_2020 %>%
  cor.test(.$runs, .$avg, data= .)

batting_2020 %>% 
  specify(runs ~ avg) %>% 
  calculate(stat = 'correlation') 

batting_2020 %>%
  specify(runs ~ avg) %>%
  hypothesize(null = 'independence') %>%
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 'correlation') %>%
  visualize() +
  shade_p_value(obs_stat = .945, direction = 'two-sided')

batting_2020 %>%
  specify(runs ~ avg) %>%
  hypothesize(null = 'independence') %>%
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 'correlation') %>%
  get_p_value(obs_stat = .945, direction = 'two-sided')

batting_2020 %>% 
  summarise(across(runs:avg, list(mean = mean, sd= sd)))  

batting_2020 %>%
  ggplot(aes(x = avg,
             y = runs)) +
  geom_point() +
  geom_abline(
    slope = .8965,
    intercept = -.1138,
    color = 'orange',
    lwd = 1.25
  ) +
  coord_fixed()

batting_2020 %>% 
  specify(runs ~ avg) %>% 
  hypothesize(null = 'point', mu=0) %>% 
  generate(reps= 1000, type = 'bootstrap') %>% 
  calculate(stat = 'slope') %>% 
  visualize() + 
  shade_p_value(obs_stat = .8965, direction = 'both')

batting_2020 %>%
  mutate(projected_runs = 0.8965 * avg - 0.1138,
         a = (runs - mean(runs)) ^ 2,
         b = (projected_runs - mean(runs)) ^ 2,
         c = (runs - projected_runs) ^ 2) %>%
  summarise(a_sum = sum(a), b_sum = sum(b), c_sum = sum(c), d = b_sum / a_sum)

.945^2

.9445977 ^ 2

batting_2020 %>%
  lm(runs ~ avg, .)  

batting_2020 %>%
  lm(runs ~ avg, .) %>%
  summary()

1- (10 - 1) * (1 - .8923) / (10 - 1 - 1)

batting_2020 %>%
  lm(runs ~ avg, .) %>%
  tidy()

batting_2020 %>% 
  lm(runs~avg, .) %>% 
  glance()

batting_2020 %>% 
  lm(runs~avg, .) %>% 
  augment()

batting_2020 %>%
  lm(runs ~ avg, .) %>% 
  predict()

batting_2020 %>%
  ggplot(aes(x = avg,
             y = runs)) +
  geom_point() +
  geom_line(aes(y = batting_2020 %>%
                  lm(runs ~ avg, .) %>% 
                  predict()))

batting_2020 %>%
  ggplot(aes(x = avg,
             y = runs)) +
  geom_point() +
  geom_line(aes(y = batting_2020 %>%
                  lm(runs ~ avg, .) %>% 
                  predict())) +
  geom_smooth(method = 'lm', se = FALSE)

'kbo_team_batting.csv' %>% 
  read.csv() %>% 
  as_tibble() -> team_batting

team_batting

team_batting %>%
  mutate(
    runs = r / tpa,
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab,
    ops = obp + slg,
    woba = (0.69 * (bb - ibb) + 0.719 * hbp + 0.87 * (h - X2b - X3b - hr) + 1.217 * X2b + 1.529 * X3b + 1.94 * hr) / (ab + bb - ibb + sf + hbp),
    .before = g
  ) -> team_batting

pacman::p_load(skimr)

team_batting %>% 
  select(runs, avg, obp, slg, ops) %>% 
  skimr::skim()

team_batting %>%
  ggplot(aes(x=avg, y=runs)) +
  geom_point() +
  geom_smooth()

tibble(
  x = seq(-5, 5, .1),
  y = dnorm(x)
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth()

team_batting %>%
  lm(runs ~ avg, .) %>%
  augment()

team_batting %>%
  lm(runs ~ avg, .) %>%
  augment() %>% 
  ggplot(aes(x= avg, y = .resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = 'dashed')

team_batting %>%
  lm(runs ~ avg, .) %>%
  augment() %>% 
  summarise(R = cor(avg, .resid))

team_batting %>%
  lm(runs ~ avg, .) %>%
  augment() %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(binwidth = .005, fill = 'gray75', color = 'white')

rnorm(10000, mean = 0, sd = 1) %>% 
  quantile(., c(.025, .25, .5, .75, .975))

tibble(
  x = rnorm(5000, mean = 100, sd = 50)
) %>% 
  ggplot(aes(sample = x)) +
  geom_qq() +
  geom_qq_line()

team_batting %>%
  lm(runs ~ avg, .) %>%
  augment() %>% 
  ggplot(aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line()

team_batting %>%
  lm(runs ~ avg, .) %>%
  augment() %>% 
  pull(.resid) %>% 
  shapiro.test(.)

team_batting %>%
  lm(runs ~ avg, .) %>%
  augment() %>% 
  rep_sample_n(reps = 30, size = 150, replace = TRUE) %>% 
  group_by(replicate) %>% 
  summarise(.resid_var = var(.resid),
            .groups = 'drop') -> var_eqaulity_test_sample

var_eqaulity_test_sample %>% 
  specify(response = .resid_var) %>% 
  hypothesize(null = 'point', sigma = 0) %>% 
  calculate('t')

var_eqaulity_test_sample %>% 
  specify(response = .resid_var)%>% 
  hypothesize(null = 'point', sigma = 0) %>% 
  generate(reps = 1000, type = 'bootstrap') %>% 
  calculate(stat = 't') %>% 
  get_p_value(obs_stat = 44.1, direction = 'two-sided')

team_batting %>%
  summarise(avg = cor(runs, avg),
            obp = cor(runs, obp),
            slg = cor(runs, slg),
            ops = cor(runs, ops),
            woba = cor(runs, woba))

team_batting %>%
  pivot_longer(cols = avg:woba, names_to = '구분', values_to = '기록') %>%
  group_by(구분) %>%
  summarise(cor.test(runs,  기록) %>% tidy(), .groups = 'drop') %>%
  arrange(estimate)

team_batting %>%
  pivot_longer(cols = avg:woba, names_to = '구분', values_to = '기록') %>%
  group_by(구분) %>%
  summarise(lm(runs ~ 기록) %>% tidy(), .groups = 'drop') %>%
  arrange(estimate)

team_batting %>%
  pivot_longer(cols = avg:woba, names_to = '구분', values_to = '기록') %>%
  group_by(구분) %>%
  summarise(lm(runs ~ 기록) %>% glance(), .groups = 'drop') %>%
  arrange(r.squared)

'kbo_batting_risp.csv' %>% 
  read.csv() %>% 
  as_tibble() -> kbo_batting_risp

kbo_batting_risp

kbo_batting_risp %>% 
  group_by(code) %>% 
  arrange(code, year) %>% 
  mutate(다음해 = lead(year),
            .after = year) 

kbo_batting_risp %>% 
  group_by(code) %>% 
  arrange(code, year) %>% 
  mutate(다음해 = lead(year),
            .after = year) %>% 
  arrange(code, year) %>% 
  drop_na()

kbo_batting_risp %>% 
  group_by(code) %>% 
  arrange(code, year) %>% 
  mutate(다음해 = lead(year),
         이듬해 = if_else(다음해 == year + 1, 1, 0),
         .after = year) %>% 
  filter(이듬해 == 1)

kbo_batting_risp %>% 
  group_by(code) %>% 
  arrange(code, year) %>% 
  mutate(다음해 = lead(year),
            이듬해 = if_else(다음해 == year + 1, 1, 0),
            .after = year) %>% 
  pivot_longer(cols = g:last_col(),
               names_to = '구분',
               values_to = '기록') %>% 
  group_by(code, 구분) %>% 
  mutate(이듬해_기록 = lead(기록)) %>% 
  filter(이듬해 == 1)

kbo_batting_risp %>%
  group_by(code) %>%
  mutate(다음해 = lead(year),
         이듬해 = if_else(다음해 == year + 1, 1, 0),
         .after = year) %>%
  pivot_longer(cols = g:last_col(),
               names_to = '구분',
               values_to = '기록') %>%
  group_by(code, 구분) %>%
  mutate(이듬해_기록 = lead(기록)) %>%
  filter(이듬해 == 1) %>%
  group_by(구분) %>%
  summarise(lm(이듬해_기록 ~ 기록) %>% glance(), .groups = 'drop') %>%
  arrange(-r.squared) %>%
  print(n = Inf)

kbo_batting_risp %>% 
  arrange(code, year) %>% 
  group_by(code) %>% 
  mutate(다음해 = lead(year),
         이듬해 = if_else(다음해 == year + 1, 1, 0),
         across(h:e, ~.x / g),
            .after = year) %>% 
  pivot_longer(cols = h:last_col(),
               names_to = '기록') %>% 
  arrange(code, 기록, year) %>% 
  group_by(code, 기록) %>% 
  mutate(next_value = lead(value)) %>% 
  filter(이듬해 == 1) %>% 
  group_by(기록) %>% 
  summarise(lm(next_value ~ value) %>% glance(), .groups = 'drop') %>% 
  arrange(-r.squared)

'kbo_pythagorean_expectation.csv' %>% 
  read.csv() %>% 
  as_tibble() -> kbo_pythagorean_expectation

kbo_pythagorean_expectation %>% 
  glimpse()

kbo_pythagorean_expectation %>%
  mutate(
    실제_승률  =  승 / (승 + 패),
    방식_1 =  득점 ^ 2 / (득점 ^ 2 + 실점 ^ 2),
    방식_2 =  득점 ^ 1.83 / (득점 ^ 1.83 + 실점 ^ 1.83),
    x = 0.45 + 1.5 * log10((득점 + 실점) / 경기),
    방식_3 =  득점 ^ x / (득점 ^ x + 실점 ^ x)
  ) %>%
  pivot_longer(cols = starts_with('방식'),
               names_to = '방식',
               values_to = '결과') %>%
  group_by(방식) %>%
  summarise(lm(실제_승률 ~ 결과) %>% glance())

kbo_pythagorean_expectation %>% 
  mutate(
    실제_승률  =  승 / (승 + 패),
    방식_1 =  득점 ^ 2 / (득점 ^ 2 + 실점 ^ 2),
    방식_2 =  득점 ^ 1.83 / (득점 ^ 1.83 + 실점 ^ 1.83),
    x = 0.45 + 1.5 * log10((득점 + 실점) / 경기),
    방식_3 =  득점 ^ x / (득점 ^ x + 실점 ^ x),
    차이_1 = (실제_승률 - 방식_1) ^ 2,
    차이_2 = (실제_승률 - 방식_2) ^ 2,
    차이_3 = (실제_승률 - 방식_3) ^ 2
  ) %>% 
  summarise(across(contains('차이'), mean)) %>% 
  summarise(across(everything(), sqrt))