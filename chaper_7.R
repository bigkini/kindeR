pacman::p_load(tidyverse, lubridate)

as.Date('2021-01-01')

as.Date(2021-01-01)

as.Date(20210101)

as.Date('20210101')

ymd('20210101')

ymd(20210101)

ymd(210101)

mdy('January 1st 2021')

dmy('1-jan-2021')

'kbo_players_profiles.csv' %>% 
  read.csv() %>% 
  as_tibble -> kbo_profile

kbo_profile

kbo_profile %>%
  select(内靛:积斥岿老) %>%
  mutate(积老 = ymd(积斥岿老))

kbo_profile %>%
  select(内靛:积斥岿老) %>%
  mutate(
    积老 = ymd(积斥岿老),
    楷 = year(积老),
    岿 = month(积老),
    老 = day(积老),
    夸老 = wday(积老)
  )

kbo_profile %>%
  select(内靛:积斥岿老) %>%
  mutate(
    积老 = ymd(积斥岿老),
    楷 = year(积老),
    岿 = month(积老),
    老 = day(积老),
    夸老 = wday(积老, label = TRUE)
  )

kbo_profile %>%
  select(内靛:积斥岿老,-c(捧鸥,  内靛)) %>%
  mutate(
    积老 = ymd(积斥岿老),
    楷 = year(积老),
    岿 = month(积老),
    老 = day(积老),
    夸老 = wday(积老, label = TRUE),
    朝楼 = yday(积老),
    馆扁 = semester(积老),
    盒扁 = quarter(积老)
  )

kbo_profile %>%
  select(内靛:积斥岿老,-c(捧鸥,  内靛)) %>%
  mutate(
    积老 = ymd(积斥岿老),
    岿 = month(积老),
  ) %>%
  group_by(岿) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = 岿, y = count)) +
  geom_col()

ymd(820327) %--% ymd(210101)

ymd(820327) %--% ymd(210101) %>% 
  as.period()

ymd(820327) %--% ymd(210101) %>% 
  as.period() / days(1)

ymd(820327) %--% ymd(210101) %>% 
  as.duration()

ymd(820327) %--% ymd(210101) %>% 
  as.duration() / ddays(1)

ymd(200101) + years(1)

ymd(200101) + dyears(1)

ymd(210101) + dyears(1)

leap_year(c(2020, 2021))

ymd_hm('820327 14:30')

ymd_hm('820327 14:30', tz = 'Asia/Seoul')

ymd_hm('820327 14:30', tz = 'Asia/Seoul') %>%
  with_tz('America/New_York')

ymd_hm('820327 14:30', tz = 'Europe/London') - ymd_hm('820327 14:30', tz = 'UTC')

ymd(820327) %>% 
  round_date(unit = 'year')

ymd(820327) %>% 
  round_date(unit = 'month')

ymd_hm('820327 14:30') %>% 
  round_date(unit = 'day')

ymd(820327) %>% 
  ceiling_date(unit = 'year')

ymd(820327) %>% 
  floor_date(unit = 'year')

'fifa_ranking.csv' %>% 
  read.csv() %>% 
  as_tibble() -> fifa_ranking

'soccer_matches_results_in_progress.csv' %>% 
  read.csv() %>% 
  as_tibble() -> results_in_progess

fifa_ranking %>%
  group_by(country_full) %>%
  mutate(previous_rank = lag(rank))

fifa_ranking %>%
  group_by(country_full) %>%
  mutate(
    previous_rank = lag(rank),
    rank_date = ymd(rank_date),
    floor_date = floor_date(rank_date, unit = 'month')
  )

fifa_ranking %>%
  group_by(country_full) %>%
  mutate(
    previous_rank = lag(rank),
    rank_date = ymd(rank_date),
    floor_date = floor_date(rank_date, unit = 'month')
  ) -> fifa_ranking_to_join

results_in_progess %>%
  arrange(team, date) %>%
  mutate(date = ymd(date),
         floor_date = floor_date(date, unit = 'month'))

results_in_progess %>%
  arrange(team, date) %>%
  mutate(date = ymd(date),
         floor_date = floor_date(date, unit = 'month')) %>%
  left_join(fifa_ranking_to_join,
            by = c('team_abrv' = 'country_abrv', 'floor_date')) %>%
  rename(team_rank = rank, team_previous_rank = previous_rank)

results_in_progess %>%
  arrange(team, date) %>%
  mutate(date = ymd(date),
         floor_date = floor_date(date, unit = 'month')) %>%
  left_join(fifa_ranking_to_join,
            by = c('team_abrv' = 'country_abrv', 'floor_date')) %>%
  rename(team_rank = rank, team_previous_rank = previous_rank) %>%
  left_join(fifa_ranking_to_join,
            by = c('opponent_abrv' = 'country_abrv', 'floor_date', 'rank_date')) %>%
  rename(opponent_rank = rank, opponent_previous_rank = previous_rank)

(ymd(201231) - ymd(210101)) / ddays(1)

results_in_progess %>%
  arrange(team, date) %>%
  mutate(date = ymd(date),
         floor_date = floor_date(date, unit = 'month')) %>%
  left_join(fifa_ranking_to_join,
            by = c('team_abrv' = 'country_abrv', 'floor_date')) %>%
  rename(team_rank = rank, team_previous_rank = previous_rank) %>%
  left_join(fifa_ranking_to_join,
            by = c('opponent_abrv' = 'country_abrv', 'floor_date', 'rank_date')) %>%
  rename(opponent_rank = rank, opponent_previous_rank = previous_rank) %>%
  select(date, rank_date, team:tournament, team_rank:last_col()) %>%
  mutate(difference_date = (date - rank_date) / ddays(1),
         team_rank = if_else(difference_date < 0, team_previous_rank, team_rank),
         opponent_rank = if_else(difference_date < 0, opponent_previous_rank, opponent_rank)
  )

results_in_progess %>%
  arrange(team, date) %>%
  mutate(date = ymd(date),
         floor_date = floor_date(date, unit = 'month')) %>%
  left_join(fifa_ranking_to_join,
            by = c('team_abrv' = 'country_abrv', 'floor_date')) %>%
  rename(team_rank = rank, team_previous_rank = previous_rank) %>%
  left_join(fifa_ranking_to_join,
            by = c('opponent_abrv' = 'country_abrv', 'floor_date', 'rank_date')) %>%
  rename(opponent_rank = rank, opponent_previous_rank = previous_rank) %>%
  select(date, rank_date, team:tournament, team_rank:last_col()) %>%
  mutate(difference_date = (date - rank_date) / ddays(1),
         team_rank = ifelse(difference_date < 0, team_previous_rank, team_rank),
         opponent_rank = ifelse(difference_date < 0, opponent_previous_rank, opponent_rank)) %>% 
  select(date, team, opponent, team_score, opponent_score, tournament, team_rank, opponent_rank) -> results_final

results_final %>%
  mutate(
    win = ifelse(team_score > opponent_score, 1, 0),
    draw = ifelse(team_score == opponent_score, 1, 0),
    lose = ifelse(team_score < opponent_score, 1, 0)
  ) -> results_final

results_final %>%
  filter(team == 'South Korea', win == 1) %>%
  arrange(opponent_rank)

results_final %>%
  filter(win==1, opponent_rank == 1) %>%
  arrange(-team_rank)
