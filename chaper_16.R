pacman::p_load(tidyverse, tidymodels)
set.seed(1234)

'kbo_team_batting.csv' %>% 
  read.csv() %>% 
  as_tibble() -> team_batting

team_batting %>%
  mutate(
    runs = r / tpa,
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab
  ) -> team_batting

team_batting %>% 
  select(avg, obp, slg) %>% 
  cor() 

pacman::p_load(car)

team_batting %>% 
  lm(runs ~ avg + obp + slg, data = .) %>%  
  vif()

team_batting %>% 
  lm(runs ~ avg + obp + slg, data = .) %>% 
  summary()

team_batting %>% 
  lm(runs ~ obp + slg, data = .) %>% 
  summary()

team_batting %>% 
  select(obp, slg) %>% 
  pivot_longer(obp:slg) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value), sd = sd(value))

team_batting %>%
  select(obp, slg) %>%
  pivot_longer(obp:slg) %>%
  ggplot((aes(x = value, fill = name))) +
  geom_histogram(position = 'identity',
                 binwidth = .01,
                 alpha = .75,
                 color = 'white')

team_batting %>%
  mutate(
    obp_z = (obp - mean(obp)) / sd(obp),
    slg_z = (slg - mean(slg)) / sd(slg),
    .before = g
  ) -> team_batting

team_batting %>% 
  lm(runs ~ obp_z + slg_z, data = .) %>% 
  summary()

pacman::p_load(vip)

team_batting %>% 
  lm(runs ~ obp + slg, data = .) %>% 
  vi()

team_batting %>% 
  lm(runs ~ obp + slg, data = .) %>% 
  vip() +
  geom_text(aes(label = Importance))

linear_reg() %>% 
  set_engine(engine = 'lm')

show_engines('linear_reg')

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(runs ~ obp + slg,
      data = team_batting)  

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(runs ~ obp + slg,
      data = team_batting) %>% 
  tidy()

team_batting %>% 
  initial_split() -> team_batting_split

team_batting_split

team_batting %>% 
  initial_split(prop = .7) -> team_batting_split

team_batting_split

team_batting_split %>% 
  training() -> team_batting_training

team_batting_split %>% 
  testing() -> team_batting_testing

team_batting_training

team_batting_testing

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(runs ~ obp + slg,
      data = team_batting_training) -> team_batting_fit

team_batting_fit %>% 
  predict(new_data = team_batting_testing)

team_batting_fit %>% 
  predict(new_data = team_batting_testing) %>% 
  mutate(truth = team_batting_testing$runs)

team_batting_fit %>% 
  predict(new_data = team_batting_testing) %>% 
  mutate(truth = team_batting_testing$runs) %>% 
  rmse(truth = truth, estimate = .pred)

team_batting_fit %>% 
  predict(new_data = team_batting_training) %>% 
  mutate(truth = team_batting_training$runs) %>% 
  rmse(truth = truth, estimate = .pred)

'kovo_sets_results.csv' %>% 
  read.csv() %>% 
  as_tibble() -> kovo_sets_results

kovo_sets_results

kovo_sets_results %>%
  mutate(남녀부 = as_factor(남녀부)) -> kovo_sets_results

kovo_sets_results %>%
  ggplot(aes(x = 리시브_효율, y = 승률, color = 남녀부)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(승률 ~ 리시브_효율 * 남녀부,
      data = kovo_sets_results) %>% 
  tidy()

kovo_sets_results %>% 
  filter(남녀부 == '여') %>% 
  specify(승률 ~ 리시브_효율) %>% 
  hypothesize(null = 'independence') %>%
  generate(1000, type = 'permute') %>% 
  calculate(stat = 'slope') %>% 
  visualize() +
  shade_p_value(obs_stat = -.013, direction = 'two-sided')

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(승률 ~ 세트당_서브 + 리시브_효율 + 공격_효율 + 세트당_블로킹 + 세트당_디그,
        data = kovo_sets_results %>% filter(남녀부 == '남')) %>% 
  tidy()

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(승률 ~ 세트당_서브 + 공격_효율 + 세트당_블로킹 + 세트당_디그,
        data = kovo_sets_results %>% filter(남녀부 == '남')) %>% 
  tidy()

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(승률 ~ 세트당_서브 + 공격_효율 + 세트당_블로킹 + 세트당_디그,
        data = kovo_sets_results %>% filter(남녀부 == '남')) %>% 
  vi()

linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  fit(승률 ~ 세트당_서브 + 리시브_효율 + 공격_효율 + 세트당_블로킹 + 세트당_디그,
        data = kovo_sets_results %>% filter(남녀부 == '여')) %>% 
  tidy()