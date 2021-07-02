pacman::p_load(tidyverse, readxl)

'kbo_team_slash_untidy.xlsx' %>% 
  read_excel() -> kbo_untidy

kbo_untidy

kbo_untidy %>%
  fill(팀)

kbo_untidy %>%
  fill(팀, .direction = 'up')


tribble(
  ~선수, ~타율, ~출루율, ~장타력,
  1, .123, .456, .789,
  2, .234, .567, .891
) %>% 
  pivot_longer(cols = 타율:장타력, names_to = '기록', values_to = '성적')

tribble(
  ~선수, ~기록, ~성적,
  1, '타율', .123,
  1, '출루율', .456,
  1, '장타력', .789,
  2, '타율', .234,
  2, '출루율', .567,
  2, '장타력', .891,
) %>% 
  pivot_wider(names_from='기록', values_from='성적')

kbo_untidy %>%
  fill(팀) %>%
  pivot_longer(
    cols = `1982`:`2020`,
    names_to = '연도',
    values_to = '기록'
  ) 

kbo_untidy %>%
  fill(팀) %>%
  pivot_longer(
    cols = `1982`:`2020`,
    names_to = '연도',
    values_to = '기록'
  ) %>% 
  sample_n(10)

kbo_untidy %>%
  fill(팀) %>%
  pivot_longer(
    cols = `1982`:`2020`,
    names_to = '연도',
    values_to = '기록',
    values_drop_na = TRUE
  ) %>% 
  sample_n(10)

kbo_untidy %>%
  fill(팀) %>%
  pivot_longer(
    cols = `1982`:`2020`,
    names_to = '연도',
    values_to = '기록',
    values_drop_na = TRUE
  ) %>%
  pivot_wider(names_from = '구분', values_from = '기록')

kbo_untidy %>%
  fill(팀) %>%
  pivot_longer(
    cols = `1982`:last_col(),
    names_to = '연도',
    values_to = '기록',
    values_drop_na = TRUE
  ) %>%
  pivot_wider(names_from = '구분', values_from = '기록') %>%
  mutate(연도 = 연도 %>% as_factor())