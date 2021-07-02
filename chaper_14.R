pacman::p_load('tidyverse', 'tidymodels')
set.seed(1234)

tribble(
  ~항목, ~능력치,
  '스파이크', 30,
  '디그', 75,
  '서브', 45,
  '리시브', 80,
  '블로킹', 25,
  '2단_연결', 70
) -> volleyball_status

volleyball_status %>%
  summarise(평균 = mean(능력치))

volleyball_status %>%
  mutate(편차 = 능력치 - mean(능력치)) %>%
  arrange(편차) -> volleyball_status

volleyball_status

volleyball_status %>%
  mutate(편차_제곱 = 편차 ^ 2) -> volleyball_status

volleyball_status

volleyball_status %>%
  mutate(카테고리 = rep(c('공격', '수비'), each = 3)) -> volleyball_status

rep(c('공격', '수비'), 3)

rep(c('공격', '수비'), each = 3)

volleyball_status %>% 
  group_by(카테고리) %>% 
  summarise(카테고리_평균 = mean(능력치),
            .groups = 'drop')

volleyball_status %>% 
  group_by(카테고리) %>% 
  mutate(카테고리_평균 = mean(능력치)) %>% 
  ungroup() %>% 
  mutate(카테고리_전체_제곱 = (카테고리_평균 - mean(능력치)) ^ 2) -> volleyball_status

volleyball_status

volleyball_status %>% 
  mutate(능력치_카테고리_제곱 = (능력치 - 카테고리_평균) ^ 2) %>% 
  relocate(ends_with('제곱'), .before=편차) -> volleyball_status

volleyball_status

volleyball_status %>% 
  summarise(across(ends_with('제곱'), sum))

volleyball_status %>% 
  specify(능력치 ~ 카테고리) %>% 
  calculate(stat = 'F', order = c('수비', '공격'))

(2604.167 / 1) / (266.6667 / 4)

volleyball_status %>% 
  specify(능력치 ~ 카테고리) %>% 
  calculate(stat = 't', order = c('수비', '공격'))

6.25 ^ 2

1 - 0.95^2

tibble(
  x = rchisq(5000, 10),
  y = rchisq(5000, 10),
) %>% 
  mutate(ratio = x/y) %>% 
  ggplot(aes(x = ratio)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = 'gray75',
                 color = 'white') +
  geom_function(fun = df, args = list(df1 = 10, df2 = 10),
                color = '#53bfd4', lwd = 1)

'nba_draft_data.csv' %>% 
  read.csv() %>% 
  as_tibble() -> nba_players

nba_players %>% 
  glimpse()

nba_players %>% 
  count(pos, sort=TRUE)

nba_players$g %>% 
  quantile()

nba_players %>% 
  filter(g > 67) %>% 
  mutate(pos = fct_collapse(pos,
                            PF = c('PF', 'C-PF', 'PF-SF'),
                            SG = c('SG', 'PG-SG', 'SG-PG')),
         across(mp:pts, ~ . / g)) %>%
  select(pos, mp:pts) -> nba_position 

nba_position %>% 
  ggplot(aes(x=pos, y=pts)) +
  geom_boxplot()

nba_position %>%
  group_by(pos) %>%
  summarise(ppg = mean(pts),
            .groups = 'drop') %>%
  arrange(-ppg)

nba_position %>% 
  specify(pts ~ pos) %>% 
  calculate(stat = 'F')

nba_position %>% 
  specify(pts ~ pos) %>% 
  calculate(stat = 'F') %>% 
  visualize(method = 'theoretical') +
  shade_p_value(obs_stat = 10.0, direction = 'greater')

nba_position %>% 
  specify(pts ~ pos) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'F') %>% 
  visualize(method = 'both') +
  shade_p_value(obs_stat = 10.0, direction = 'greater')

nba_position %>% 
  lm(pts ~ pos, data = .) %>% 
  summary()