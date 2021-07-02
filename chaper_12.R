pacman::p_load(tidyverse, tidymodels)
set.seed(1234)

dt(x = 0, df = 1)

'19_20_uefa_big_5.csv' %>% 
  read.csv() %>% 
  as_tibble -> uefa_big5_match_results

uefa_big5_match_results %>%
  mutate(
    Àå¼Ò = Àå¼Ò %>% fct_relevel('¾È¹æ', '¹æ¹®'),
    ½Ã±â = ½Ã±â %>% fct_relevel('BC', 'AC')
  ) -> uefa_big5_match_results

uefa_big5_match_results %>% 
  filter(Àå¼Ò == '¾È¹æ' & ¸®±× != '¸®±×1') -> uefa_big5_match_results_period  

uefa_big5_match_results_period %>% 
  group_by(ÆÀ, ½Ã±â) %>% 
  summarise(½Â·ü = mean(½Â¸®), .groups = 'drop') -> uefa_big5_results_period

uefa_big5_results_period %>% 
  group_by(½Ã±â) %>% 
  summarise(½Â·ü = mean(½Â·ü), .groups = 'drop') 

uefa_big5_results_period %>% 
  t_test(formula =  ½Â·ü  ~  ½Ã±â,
         order = c('AC', 'BC'),
         alternative = 'less')

uefa_big5_results_period %>% 
  t_test(formula =  ½Â·ü  ~  ½Ã±â,
         order = c('AC', 'BC'),
         alternative = 'less',
         var.equal = TRUE)

uefa_big5_results_period  %>% 
  group_by(½Ã±â) %>% 
  tally()

uefa_big5_results_period %>% 
  group_by(½Ã±â) %>% 
  summarise(Æò±Õ½Â·ü = mean(½Â·ü), 
            ºĞ»ê = var(½Â·ü),
            °³¼ö = n(),
            .groups = 'drop')

uefa_big5_results_period %>% 
  t_test(formula =  ½Â·ü  ~  ½Ã±â,
         order = c('AC', 'BC'),
         alternative = 'less')

(.421 - .439) / sqrt(.07697/78 + .0337/78)

(.4213370 - .4388944) / sqrt(.07693693/78 + .03368264/78)

#uefa_big5_results_period %>% 
#  specify(½Â·ü ~ ½Ã±â) %>%
#  hypothesize(null = 'independence') %>%
#  generate(reps = 1000, type = 'permute') %>%
#  calculate(stat = 'diff in means',
#            order = c('AC', 'BC'))

uefa_big5_results_period %>% 
  specify(½Â·ü ~ ½Ã±â) %>%
  hypothesize(null = 'independence') %>%
  calculate(stat = 't',
            order = c('AC', 'BC')) -> uefa_big5_results_period_null_theoretical

uefa_big5_results_period_null_theoretical

uefa_big5_results_period_null_theoretical %>% 
  visualize(method = 'theoretical')

uefa_big5_results_period_null_theoretical %>% 
  visualize(method = 'theoretical') +
  shade_p_value(obs_stat = -.466, direction = 'less')

uefa_big5_results_period %>% 
  specify(½Â·ü ~ ½Ã±â) %>%
  calculate(stat = 't',
            order = c('AC', 'BC')) 

uefa_big5_results_period %>% 
  specify(½Â·ü ~ ½Ã±â) %>%
  hypothesize(null = 'independence') %>%
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 't',
            order = c('AC', 'BC')) %>% 
  visualize(method = 'both') +
  shade_p_value(obs_stat = -.466, direction = 'less')

uefa_big5_results_period %>% 
  t_test(formula =  ½Â·ü  ~  ½Ã±â,
         order = c('AC', 'BC'),
         alternative = 'less', paired = TRUE)

uefa_big5_results_period %>% 
  pivot_wider(names_from='½Ã±â', values_from='½Â·ü') %>% 
  mutate(Â÷ÀÌ = AC - BC) %>% 
  summarise(Â÷ÀÌ_Æò±Õ = mean(Â÷ÀÌ),
            Â÷ÀÌ_Ç¥ÁØÆíÂ÷ = sd(Â÷ÀÌ),
            Â÷ÀÌ_Ç¥ÁØ¿ÀÂ÷ = Â÷ÀÌ_Ç¥ÁØÆíÂ÷ / sqrt(78))     
                 
-.0176 / .0286

uefa_big5_results_period %>% 
  specify(½Â·ü ~ ½Ã±â) %>%
  hypothesize(null = 'point', mu = 0) %>%
  calculate(stat = 't',
            order = c('AC', 'BC')) %>% 
  visualize(method = 'theoretical') +
  shade_p_value(obs_stat = -.615, direction = 'less')

'19_20_nba.csv' %>% 
  read.csv() %>% 
  as_tibble -> nba_match_results

nba_match_results %>% 
  names()

nba_match_results %>% 
  mutate(
    Àå¼Ò = Àå¼Ò %>% fct_relevel('¾È¹æ', '¹æ¹®'),
    ½Ã±â = ½Ã±â %>% fct_relevel('BC', 'AC'),
    ¸®±× = ¸®±× %>% fct_relevel('Á¤±Ô¸®±×', 'ÇÃ·¹ÀÌ¿ÀÇÁ')
  ) -> nba_match_results

nba_match_results %>%
  filter(¸®±× != 'ÇÃ·¹ÀÌ¿ÀÇÁ') %>% 
  group_by(ÆÀ, Àå¼Ò, ½Ã±â) %>%
  summarise(½Â·ü  = mean(½Â¸®), .groups = 'drop') %>%
  ggplot(aes(x = Àå¼Ò, y = ½Â·ü)) +
  geom_boxplot() +
  facet_grid(. ~ ½Ã±â)

nba_match_results %>%
  filter(¸®±× != 'ÇÃ·¹ÀÌ¿ÀÇÁ') %>% 
  group_by(ÆÀ, Àå¼Ò, ½Ã±â) %>%
  filter(Àå¼Ò == '¾È¹æ') %>% 
  summarise(½Â·ü  = mean(½Â¸®), .groups = 'drop') %>% 
  t_test(formula =  ½Â·ü  ~  ½Ã±â,
         order = c('BC', 'AC'),
         paired = TRUE)

nba_match_results %>%
  filter(¸®±× != 'ÇÃ·¹ÀÌ¿ÀÇÁ') %>% 
  group_by(ÆÀ, Àå¼Ò, ½Ã±â) %>%
  filter(Àå¼Ò == '¾È¹æ') %>% 
  summarise(½Â·ü  = mean(½Â¸®), .groups = 'drop') %>% 
  pivot_wider(names_from = '½Ã±â', values_from = '½Â·ü') %>% 
  drop_na()

nba_match_results %>%
  filter(¸®±× != 'ÇÃ·¹ÀÌ¿ÀÇÁ') %>% 
  group_by(ÆÀ, Àå¼Ò, ½Ã±â) %>%
  filter(Àå¼Ò == '¾È¹æ') %>% 
  summarise(½Â·ü  = mean(½Â¸®), .groups = 'drop') %>% 
  pivot_wider(names_from = '½Ã±â', values_from = '½Â·ü') %>% 
  drop_na() %>% 
  pivot_longer(BC:AC, names_to = '½Ã±â', values_to = '½Â·ü') %>% 
  t_test(formula =  ½Â·ü  ~  ½Ã±â,
         order = c('BC', 'AC'),
         paired = TRUE)

nba_match_results %>%
  filter(½Ã±â == 'BC') %>% 
  group_by(ÆÀ, Àå¼Ò) %>%
  summarise(½Â·ü  = mean(½Â¸®), .groups = 'drop') %>% 
  specify(½Â·ü ~ Àå¼Ò) %>% 
  hypothesize(null = 'independence') %>%
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 'diff in means',
            order = c('¾È¹æ', '¹æ¹®')) %>%
  select(stat) %>% 
  mutate(type = 'h0') -> nba_simulation_h0

nba_simulation_h0 %>%
  summarise(low = quantile(stat, .95))

nba_match_results %>%
  filter(½Ã±â == 'BC') %>% 
  group_by(ÆÀ, Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â¸®),
              .groups = 'drop')  %>% 
  pivot_wider(names_from = 'Àå¼Ò',
              values_from = '½Â·ü') %>% 
  mutate(Â÷ÀÌ = ¾È¹æ - ¹æ¹®) %>% 
  rep_sample_n(reps = 1000, size = 30, replace = TRUE) %>% 
  group_by(replicate) %>% 
  summarise(stat = mean(Â÷ÀÌ), .groups = 'drop') %>% 
  select(stat) %>% 
  mutate(type = 'h1') -> nba_simulation_h1

bind_rows(
  nba_simulation_h0,
  nba_simulation_h1
) %>% 
  ggplot(aes(x=stat, fill = type)) +
  geom_histogram(color = 'white',  
                 binwidth = .01, alpha = .5,
                 position = 'identity') +
  geom_vline(xintercept = .0747,
             linetype = 'dashed', lwd=1)

nba_simulation_h1 %>% 
  filter(stat > .0747) %>% 
  tally()

nba_match_results %>%
  filter(½Ã±â == 'BC') %>% 
  group_by(ÆÀ, Àå¼Ò) %>% 
  summarise(½Â·ü = mean(½Â¸®),
              .groups = 'drop') %>% 
  t_test(½Â·ü ~ Àå¼Ò, order = c('¾È¹æ', '¹æ¹®'), paired = TRUE)

pacman::p_load(pwr)

args(power.t.test)

nba_match_results %>%
  filter(½Ã±â == 'BC') %>%
  group_by(ÆÀ, Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â¸®), .groups = 'drop') %>%
  group_by(Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â·ü), .groups = 'drop')

nba_match_results %>%
  filter(½Ã±â == 'BC') %>%
  group_by(ÆÀ, Àå¼Ò) %>%
  summarise(½Â·ü = mean(½Â¸®), .groups = 'drop') %>%
  pivot_wider(names_from = 'Àå¼Ò', values_from = '½Â·ü') %>%
  mutate(Â÷ÀÌ = ¾È¹æ - ¹æ¹®) %>%
  summarise(sd = sd(Â÷ÀÌ))

power.t.test(delta = .106, sd = .158,
             sig.level = .05,
             power = .8,
             type = 'paired',
             alternative = 'one.sided')