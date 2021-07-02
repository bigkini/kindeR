pacman::p_load(tidyverse, tidymodels)

set.seed(1234)

'19_20_uefa_big_5.csv' %>% 
  read.csv() %>% 
  as_tibble -> uefa_big5_match_results

uefa_big5_match_results %>% 
  glimpse()

uefa_big5_match_results %>%
  mutate(
    Àå¼Ò = Àå¼Ò %>% fct_relevel('¾È¹æ', '¹æ¹®'),
    ½Ã±â = ½Ã±â %>% fct_relevel('BC', 'AC')
  ) -> uefa_big5_match_results

uefa_big5_match_results %>%
  group_by(ÆÀ, Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â¸®), .groups = 'drop') -> uefa_big5_results

uefa_big5_results %>%
  ggplot(aes(x = Àå¼Ò, y = ½Â·ü)) +
  geom_boxplot()

uefa_big5_results %>%
  group_by(Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â·ü), .groups = 'drop')

sample(100, 10)

tibble(
  x = 1:6
) %>% 
  mutate(rest = x %% 2)

uefa_big5_results %>%
  rowwise() %>%
  mutate(³­¼ö = sample(nrow(.), 1),
           ·£´ý_Àå¼Ò = if_else(³­¼ö %% 2 == 0, '¾È¹æ', '¹æ¹®')
  ) -> uefa_big5_results_permutated

uefa_big5_results_permutated

uefa_big5_results_permutated %>%
  pivot_longer(cols = c('Àå¼Ò', '·£´ý_Àå¼Ò'),
               names_to = '±¸ºÐ',
               values_to = 'Àå¼Ò') %>%
  group_by(ÆÀ, ±¸ºÐ, Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â·ü), .groups = 'drop') %>%
  ggplot(aes(x = Àå¼Ò %>% fct_relevel('¾È¹æ', '¹æ¹®'),
             y = ½Â·ü)) +
  geom_boxplot() +
  facet_grid(~±¸ºÐ)

uefa_big5_results_permutated %>%
  group_by(·£´ý_Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â·ü), .groups = 'drop')

uefa_big5_results %>%
  specify(response = ½Â·ü, explanatory = Àå¼Ò)

uefa_big5_results %>%
  specify(formula = ½Â·ü ~ Àå¼Ò)

uefa_big5_results %>%
  specify(½Â·ü ~ Àå¼Ò) %>%
  hypothesize(null = 'independence')

uefa_big5_results %>%
  specify(½Â·ü ~ Àå¼Ò) %>%
  hypothesize(null = 'independence') %>%
  generate(reps=1000, type='permute')

uefa_big5_results %>%
  specify(½Â·ü ~ Àå¼Ò) %>%
  hypothesize(null = 'independence') %>%
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 'diff in means',
            order = c('¾È¹æ', '¹æ¹®')) -> uefa_big5_results_null

uefa_big5_results_null %>%
  visualize()

uefa_big5_results_null %>%
  visualize() +
  geom_vline(xintercept = .128,
             color = '#53bfd4',
             lwd = 1)

uefa_big5_results_null %>%
  visualize() +
  geom_vline(xintercept = .05,
             color = '#53bfd4',
             lwd = 1)

uefa_big5_results_null %>%
  filter(stat >= .05)

uefa_big5_results %>%
  specify(formula = ½Â·ü ~ Àå¼Ò) %>%
  calculate(stat = 'diff in means',
            order=c('¾È¹æ', '¹æ¹®'))

uefa_big5_results_null %>%
  get_p_value(obs_stat = .128,
              direction = 'two-sided')

uefa_big5_results_null %>%
  visualize() +
  shade_p_value(obs_stat = .042,
                direction = 'two-sided')

uefa_big5_results_null %>%
  visualize() +
  shade_p_value(obs_stat = .128,
                direction = 'greater')

uefa_big5_results_null %>%
  visualize(fill = 'gray75') +
  shade_p_value(obs_stat = .05,
                direction = ¡®two-sided¡¯,
                fill = '#53bfd4', color = '#53bfd4')

uefa_big5_results_null %>%
  get_confidence_interval() -> uefa_big5_ci_endpoints

uefa_big5_results_null %>%
  visualize() +
  shade_p_value(obs_stat = .128,
                direction = 'greater') +
  shade_confidence_interval(endpoints = uefa_big5_ci_endpoints)

uefa_big5_match_results %>%
  filter(Àå¼Ò == '¾È¹æ' & ¸®±× != '¸®±×1') -> uefa_big5_match_results_period

uefa_big5_match_results_period %>%
  group_by(ÆÀ, ½Ã±â) %>%
  summarise(½Â·ü = mean(½Â¸®), .groups = 'drop') -> uefa_big5_results_period

uefa_big5_results_period %>%
  ggplot(aes(x = ½Ã±â, y = ½Â·ü)) +
  geom_boxplot()

uefa_big5_results_period %>%
  group_by(½Ã±â) %>%
  summarise(½Â·ü = mean(½Â·ü), .groups = 'drop')

uefa_big5_results_period %>%
  specify(½Â·ü ~ ½Ã±â) %>%
  hypothesize(null = 'independence') %>%
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 'diff in means',
            order = c('AC', 'BC')) -> uefa_big5_results_period_null

uefa_big5_results_period_null %>%
  visualize() +
  shade_p_value(obs_stat = -.018,
                direction = 'less')

uefa_big5_results_period_null %>%
  get_p_value(obs_stat = -.018, direction = 'less')

uefa_big5_results_period %>%
  t_test(formula = ½Â·ü ~ ½Ã±â,
         order = c('AC', 'BC'),
         alternative = 'less')