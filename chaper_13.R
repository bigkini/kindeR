pacman::p_load(tidyverse, tidymodels)
set.seed(1234)

crossing(
  x = rnorm(2, 0, 1),
  y = rnorm(2, 0, 1)
) %>% 
  rowwise() %>% 
  mutate(v = var(c(x, y)))

crossing(
  x = rnorm(100, 0, 1),
  y = rnorm(100, 0, 1)
) %>% 
  rowwise() %>% 
  mutate(v = var(c(x, y))) %>% 
  ggplot(aes(x = v)) +
  geom_histogram(bins = 30, fill = 'gray75', color = 'white') 

crossing(
  x = rnorm(100, 0, 1),
  y = rnorm(100, 0, 1),
  z = rnorm(100, 0, 1),
) %>% 
  rowwise() %>% 
  mutate(v = var(c(x, y, z))) %>% 
  ggplot(aes(x = v)) +
  geom_histogram(bins = 30, fill = 'gray75', color = 'white') 

crossing(
  x = rnorm(30, 0, 1),
  y = rnorm(30, 0, 1),
  z = rnorm(30, 0, 1),
  a = rnorm(30, 0, 1)
) %>% 
  rowwise() %>% 
  mutate(v = var(c(x, y, z, a))) %>% 
  ggplot(aes(x = v)) +
  geom_histogram(bins = 30, fill = 'gray75', color = 'white') 

'tennis_big3_results.csv' %>% 
  read.csv() %>% 
  as_tibble() -> tennis_big3_results

tennis_big3_results %>% 
  glimpse()

pacman::p_load(janitor)

tennis_big3_results %>% 
  as_tibble(., .name_repair = janitor::make_clean_names) -> tennis_big3_results

tennis_big3_results

tennis_big3_results %>% 
  mutate(surface = str_to_lower(surface),
         w_l = str_to_lower(w_l)) -> tennis_big3_results

tennis_big3_results

tennis_big3_results %>% 
  filter(surface %in% c('clay', 'grass', 'hard')) %>% 
  group_by(player, surface) %>% 
  summarise(wins = sum(w_l == 'w'),
            loses = sum(w_l == 'l'),
            wp = wins / (wins + loses),
            .groups = 'drop') %>% 
  arrange(player, -wp)

tennis_big3_results %>% 
  filter(surface %in% c('clay', 'grass', 'hard')) %>% 
  group_by(player, surface) %>% 
  summarise(wins = sum(w_l == 'w'),
            loses = sum(w_l == 'l'),
            wp = wins / (wins + loses),
            .groups = 'drop') %>% 
  ggplot(aes(x = surface, y = wp)) +
  geom_col() +
  facet_grid(~player)

tennis_big3_results %>% 
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  specify(surface ~ w_l)

tennis_big3_results %>% 
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  specify(surface ~ w_l) %>% 
  hypothesize(null = 'independence')

tennis_big3_results %>% 
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  specify(surface ~ w_l) %>% 
  hypothesize(null = 'independence') %>% 
  calculate(stat = 'Chisq') 

tribble(
  ~코트, ~승, ~패,
  '잔디', 70, 19,
  '클레이', 434, 40, 
  '하드', 393, 96,
)

tribble(
  ~코트, ~승, ~패,
  '잔디', 70, 19,
  '클레이', 434, 40, 
  '하드', 393, 96,
) %>% 
  mutate(기대_승리 = (승 + 패) * (897 / (897 + 155)),
         기대_패배 = (승 + 패) - 기대_승리)

tribble(
  ~코트, ~승, ~패,
  '잔디', 70, 19,
  '클레이', 434, 40, 
  '하드', 393, 96,
) %>% 
  mutate(기대_승리 = (승 + 패) * (897 / (897 + 155)),
         기대_패배 = (승 + 패) - 기대_승리,
         승리_차이_제곱 = (승 - 기대_승리) ^ 2,
         패배_차이_제곱 = (패 - 기대_패배) ^ 2)

tribble(
  ~코트, ~승, ~패,
  '잔디', 70, 19,
  '클레이', 434, 40, 
  '하드', 393, 96,
) %>% 
  mutate(기대_승리 = (승 + 패) * (897 / (897 + 155)),
         기대_패배 = (승 + 패) - 기대_승리,
         승리_차이_제곱 = (승 - 기대_승리) ^ 2,
         패배_차이_제곱 = (패 - 기대_패배) ^ 2)

tribble(
  ~코트, ~승, ~패,
  '잔디', 70, 19,
  '클레이', 434, 40, 
  '하드', 393, 96,
) %>% 
  mutate(기대_승리 = (승 + 패) * (897 / (897 + 155)),
         기대_패배 = (승 + 패) - 기대_승리,
         승리_차이_제곱 = (승 - 기대_승리) ^ 2,
         패배_차이_제곱 = (패 - 기대_패배) ^ 2,
         승리_차이_비율 = 승리_차이_제곱 / 기대_승리,
         패배_차이_비율 = 패배_차이_제곱 / 기대_패배)

tribble(
  ~코트, ~승, ~패,
  '잔디', 70, 19,
  '클레이', 434, 40, 
  '하드', 393, 96,
) %>% 
  mutate(기대_승리 = (승 + 패) * (897 / (897 + 155)),
              기대_패배 = (승 + 패) - 기대_승리,
              승리_차이_제곱 = (승 - 기대_승리) ^ 2,
              패배_차이_제곱 = (패 - 기대_패배) ^ 2,
              승리_차이_비율 = 승리_차이_제곱 / 기대_승리,
              패배_차이_비율 = 패배_차이_제곱 / 기대_패배) %>% 
  summarise(across(contains('비율'), sum)) %>% 
  mutate(합 = sum(.))

tennis_big3_results %>% 
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  specify(surface ~ w_l) %>% 
  hypothesize(null = 'independence') %>% 
  calculate(stat = 'Chisq') %>% 
  visualize(method = 'theoretical') +
  shade_p_value(obs_stat = 27.4, direction = 'greater')

tennis_big3_results %>% 
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  specify(surface ~ w_l) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'Chisq') %>% 
  visualize(method = 'both') +
  shade_p_value(obs_stat = 27.4, direction = 'greater')

tennis_big3_results %>% 
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  chisq_test(surface ~ w_l)


tribble(
  ~코트, ~승, ~패, ~합계,
  '잔디', 70, 19, 89,
  '클레이', 434, 40, 474,
  '하드', 393, 96, 489,
  '합계', 897, 155, 1052
)

tribble(
  ~코트, ~승, ~패, ~합계,
  '잔디', 70, 0, 89,
  '클레이', 0, 40, 474,
  '하드', 0, 0, 489,
  '합계', 897, 155, 1052
)

tennis_big3_results %>% 
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  chisq_test(surface ~ w_l)

tennis_big3_results %>% 
  filter(player == 'Roger Federer',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  chisq_test(surface ~ w_l)

tennis_big3_results %>% 
  filter(player == 'Roger Federer',
         surface %in% c('grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  chisq_test(surface ~ w_l)

tennis_big3_results %>% 
  filter(player == 'Roger Federer',
       tournament %in% c('Australin Open', 'US Open', 'Wimbledon', 'Roland Garros'),
       surface %in% c('clay', 'grass', 'hard'),
       w_l %in% c('w', 'l')) %>% 
  chisq_test(surface ~ w_l)

tennis_big3_results %>% 
  filter(player == 'Novak Djokovic',
         tournament %in% c('Australin Open', 'US Open', 'Wimbledon', 'Roland Garros'),
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>% 
  chisq_test(surface ~ w_l)

'kbo_players_profiles.csv' %>% 
  read.csv() %>% 
  as_tibble -> kbo_profile

pacman::p_load(lubridate)

kbo_profile %>%
  filter(외국인 != 1) %>%
  mutate(
    생일 = ymd(생년월일),
    월 = month(생일),
  ) %>%
  group_by(월) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = 월, y = count)) + 
  geom_col() +
  scale_x_continuous(breaks = c(1:12))

kbo_profile %>%
  filter(외국인 != 1) %>%
  mutate(
    생일 = ymd(생년월일),
    월 = month(생일) %>% as_factor(),
  ) %>% 
  chisq_test(response = 월, 
             p = c('1' = 1/12,
                   '2' = 1/12,
                   '3' = 1/12,
                   '4' = 1/12,
                   '5' = 1/12,
                   '6' = 1/12,
                   '7' = 1/12,
                   '8' = 1/12,
                   '9' = 1/12,
                   '10' = 1/12,
                   '11' = 1/12,
                   '12' = 1/12)) 

kbo_profile %>%
  filter(외국인 != 1) %>%
  mutate(
    생일 = ymd(생년월일),
    연 = year(생일),
    월 = month(생일),
    분기 = case_when(
      월 >= 3 & 월 < 6 ~ '1',
      월 >= 6 & 월 < 9 ~ '2',
      월 >= 9 & 월 < 12 ~ '3',
      TRUE ~ '4'
    )
  ) %>% 
  ggplot(aes(x = 분기)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), nudge_y = 16)


kbo_profile %>%
  filter(외국인 != 1) %>%
  mutate(
    생일 = ymd(생년월일),
    연 = year(생일),
    월 = month(생일),
    분기 = case_when(
      월 >= 3 & 월 < 6 ~ '1',
      월 >= 6 & 월 < 9 ~ '2',
      월 >= 9 & 월 < 12 ~ '3',
      TRUE ~ '4'
    )
  ) %>% 
  chisq_test(response = 분기, 
             p = c('1' = 1/4,
                   '2' = 1/4,
                   '3' = 1/4,
                   '4' = 1/4))