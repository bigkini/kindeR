pacman::p_load(tidyverse)

'kbo_team_batting.csv' %>% 
  read.csv() %>% 
  as_tibble() -> team_batting

team_batting

team_batting %>%
  arrange(team)

team_batting %>%
  arrange(team %>% desc())

team_batting %>%
  arrange(team, year)

team_batting %>%
  filter(year == 1982)

team_batting[team_batting$year == 1982,]

team_batting[1:6, ]

team_batting %>%
  slice(1:6)

team_batting[sample(1:nrow(team_batting), 5), ]

team_batting %>% 
  sample_n(5)

team_batting %>%
  filter(year == 1982) %>%
  arrange(hr %>% desc())

team_batting %>%
  filter(year == 1982) %>%
  arrange(-hr)

team_batting %>%
  filter(year == 1982) %>%
  select(year, team, h, X2b, X3b, hr)

team_batting %>%
  filter(year == 1982) %>%
  relocate(year, .before = team)

team_batting %>%
  filter(year == 1982) %>%
  relocate(g, .after = year)

team_batting %>%
  filter(year == 1982) %>%
  select(season = year, team, h, X2b, X3b, hr)

team_batting %>%
  filter(year == 1982) %>%
  select(year, team, h, X2b, X3b, hr) %>%
  select(-X3b)

team_batting %>% 
  filter(year == 1982) %>%
  select(bb:last_col())

team_batting %>%
  filter(year == 1982) %>%
  select(2, 1, 7:10)

team_batting %>%
  filter(year == 1982) %>%
  select(year, team, h:hr)

team_batting %>%
  filter(year == 1982) %>%
  select(year, team, h:hr) %>% 
  mutate(tb = h + X2b + 2 * X3b + 3 * hr)

team_batting %>%
  mutate(avg = h / ab) %>%
  select(year, team, avg)

team_batting %>%
  transmute(year, team, avg = h / ab)

team_batting %>%
  mutate(avg = h / ab, .keep = 'used')

team_batting %>%
  mutate(avg = h / ab, .keep = 'unused')

team_batting %>%
  mutate(avg = h / ab, .before = g)

team_batting %>%
  group_by(year) %>%
  mutate(avg = sum(h) / sum(ab), .before = g)

team_batting %>%
  group_by(year) %>%
  mutate(avg = h / ab, .before = g)

team_batting %>%
  group_by(year) %>%
  summarise(avg = sum(h) / sum(ab))

team_batting %>%
  group_by(year) %>%
  summarise(avg = sum(h) / sum(ab), .groups = 'drop')

team_batting %>%
  group_by(year) %>%
  summarise(avg = sum(h) / sum(ab), .groups = 'drop') %>%
  ggplot(aes(x = year, y = avg)) +
  geom_line()

team_batting %>%
  group_by(year) %>%
  summarise(avg = sum(h) / sum(ab), .groups = 'drop') %>%
  filter(year == 1999)

team_batting %>%
  group_by(year) %>%
  summarise(avg = sum(h) / sum(ab), .groups = 'drop') %>%
  arrange(-avg)

team_batting %>%
  group_by(year) %>%
  summarise(avg = sum(h) / sum(ab), .groups = 'drop') %>%
  filter(avg == max(avg))

team_batting %>%
  mutate(
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab,
    ops = obp + slg,
    .before = g
  )

team_batting %>%
  mutate(
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab,
    ops = obp + slg,
    .before = g
  ) %>%
  filter(ops >= 0.7 & hr < 70)

team_batting %>%
  mutate(
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab,
    ops = obp + slg,
    .before = g
  ) %>%
  filter(ops >= 0.7 & hr < 70) %>%
  summarise(count = n())

team_batting %>%
  mutate(
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab,
    ops = obp + slg,
    .before = g
  ) %>%
  filter(ops >= 0.7, hr < 70) %>%
  tally()

team_batting %>%
  mutate(
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab,
    ops = obp + slg,
    .before = g
  ) %>%
  filter(ops >= 0.7, hr < 70, year %in% 1991:2000)

team_batting %>%
  mutate(
    avg = h / ab,
    obp = (h + bb + hbp) / (ab + bb + hbp + sf),
    slg = (h + X2b + 2 * X3b + 3 * hr) / ab,
    ops = obp + slg,
    .before = g
  ) %>%
  filter(ops >= 0.7, hr < 70, year %in% 1991:2000) %>%
  transmute(year, team, rg = r / g, sb = sb / g)

team_batting %>%
  transmute(decades = if_else(year <= 1990, '1980',
                              if_else(
                                year <= 2000, '1990',
                                if_else(year <= 2010, '2000', '2010')
                              )),
            g, sh)

team_batting %>%
  distinct(year) %>%
  mutate(
    decades = case_when(
      year <= 1990 ~ '1980',
      year <= 2000 ~ '1990',
      year <= 2010 ~ '2000',
      year <= 2020 ~ '2010'
    )
  )

c(1, 1, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5) %>%
  as_tibble() %>%
  distinct()

team_batting %>%
  mutate(
    decades = case_when(
      year <= 1990 ~ '1980',
      year <= 2000 ~ '1990',
      year <= 2010 ~ '2000',
      year <= 2020 ~ '2010'
    )
  ) %>%
  group_by(decades) %>%
  summarise(sh_mean = sum(sh) / sum(g))

team_batting %>%
  mutate(
    decades = case_when(
      year <= 1990 ~ '1980',
      year <= 2000 ~ '1990',
      year <= 2010 ~ '2000',
      year <= 2020 ~ '2010'
    )
  ) %>%
  group_by(decades) %>%
  summarise(error_mean = e %>% sum() / g %>% sum())

team_batting %>%
  group_by(team) %>%
  summarise(gidp = sum(gidp)) %>%
  arrange(-gidp) %>%
  head(3)

team_batting %>%
  distinct(team)

team_batting %>%
  mutate(
    team_id = fct_collapse(
      team,
      두산  = c('OB', '두산'),
      키움  = c('히어로즈', '넥센', '키움'),
      한화  = c('빙그레', '한화'),
      현대  = c('삼미', '삼미·청보', '청보', '태평양'),
      KIA = c('해태', '해태·KIA', 'KIA'),
      LG = c('LG', 'MBC'),
    ), .before = year
  ) 

team_batting %>%
  mutate(
    team_id = fct_collapse(
      team,
      두산  = c('OB', '두산'),
      키움  = c('히어로즈', '넥센', '키움'),
      한화  = c('빙그레', '한화'),
      현대  = c('삼미', '삼미·청보', '청보', '태평양'),
      KIA = c('해태', '해태·KIA', 'KIA'),
      LG = c('LG', 'MBC'),
    )
  ) %>%
  group_by(team_id) %>%
  summarise(gidp = sum(gidp), .groups = 'drop') %>%
  arrange(-gidp)

team_batting %>%
  group_by(year) %>%
  summarise(batters = sum(batters) / sum(g))

map(1:5, ~.x+1)

team_batting %>%
  group_by(year) %>%
  summarise(across(-team, ~sum(.x) / sum(g)), .groups = 'drop')

team_batting %>%
  group_by(year) %>%
  summarise(across(-team, list(
    sum = ~sum(.x),
    mean = ~sum(.x) / sum(g))
    ), .groups = 'drop')
    
letters

letters[1:11]

rep(1:3, 3)

letters[1:11] %>%
  rep(c(64, 32, 16, 8, 4, 2, 1, 1, 1, 1, 1))

letters[1:11] %>%
  rep(c(64, 32, 16, 8, 4, 2, 1, 1, 1, 1, 1)) %>%
  table()

letters[1:11] %>%
  rep(c(64, 32, 16, 8, 4, 2, 1, 1, 1, 1, 1)) %>%
  fct_lump(3) %>%
  table()

letters[1:11] %>%
  rep(c(64, 32, 16, 8, 4, 2, 1, 1, 1, 1, 1)) %>%
  fct_lump(3)

letters[1:11] %>%
  rep(c(64, 32, 16, 8, 4, 2, 1, 1, 1, 1, 1)) %>%
  fct_lump(3, other_level = '기타') %>%
  table()

letters[1:11] %>%
  rep(c(64, 32, 16, 8, 4, 2, 1, 1, 1, 1, 1)) %>%
  fct_lump(prop = .2)

letters[1:11] %>%
  rep(c(64, 32, 16, 8, 4, 2, 1, 1, 1, 1, 1)) %>%
  fct_lump_min(2)

'kovo_team.csv' %>% 
  read.csv() %>% 
  as_tibble() -> kovo_team

kovo_team

kovo_team %>%
  distinct(팀) %>%
  arrange(팀) %>% 
  pull()

kovo_team %>%
  mutate(
    팀 = case_when(
      남녀부 == '남' & 팀 == '현대' ~ '현대캐피탈',
      남녀부 == '여' & 팀 == '현대' ~ '현대건설',
      TRUE ~  팀),
    팀_이름 = 팀,
    팀 = fct_collapse(
      팀,
      대한항공 = 'KAL',
      삼성화재 = '삼성',
      상무 = c('상무', '상무신협', '신협상무'),
      우리카드 = c('드림식스', '우리카드', '우리캐피탈'),
      한국전력 = c('한국전력', '한전', 'KEPCO', 'KEPCO45'),
      KB손해보험 = c('KB손보', 'LG', 'LIG', 'LIG손보'),
      OK금융그룹 = c('러시앤캐시', 'OK저축은행'),
      GS칼텍스 = 'GS',
      IBK기업은행 = 'IBK',
      KGC인삼공사 = c('인삼공사', 'KGC인삼공사', 'KT&G'),
      한국도로공사 = c('도공', '도로공사'),
      흥국생명 = '흥국'
    )
  ) -> kovo_team

kovo_team %>% 
  distinct(시즌) %>% 
  pull()

'volleyball' %>%
  str_sub(1)

'volleyball' %>%
  str_sub(2)

'volleyball' %>%
  str_sub(1, 2)

'volleyball' %>%
  str_sub(-4)

kovo_team %>% 
  mutate(시즌_이름=시즌,
              시즌=시즌 %>% str_sub(-4)) -> kovo_team

kovo_team

kovo_team %>% 
  select(시즌, 팀, contains('공격종합'))

kovo_team %>% 
  select(-contains('_'))

kovo_team %>% 
  select(시즌, 팀, starts_with('세트'))

kovo_team %>% 
  select(시즌, 팀, ends_with('세트당_평균'))

tribble(
  ~x, ~y,
  1, 2,
) %>% 
  rename(z = x)

kovo_team %>%
  select(시즌,  팀, ends_with('세트당_평균')) %>%
  rename(블로킹 =  블로킹_세트당_평균,
            서브 =  서브_세트당_평균)

str_replace('블로킹_세트당_평균', '_세트당_평균', '')

kovo_team %>% 
  select(시즌, 팀, ends_with('세트당_평균')) %>% 
  rename_with(~str_replace(., '_세트당_평균', ''))

kovo_team %>%
  group_by(시즌, 남녀부) %>%
  summarise(리시브_효율 = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도),
            .groups = 'drop') %>%
  ggplot(aes(x = 시즌, y = 리시브_효율, group = 남녀부, color = 남녀부)) +
  geom_line()

kovo_team %>%
  group_by(남녀부, 시즌) %>%
  summarise(리시브_효율 = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도)) %>%
  mutate(전_시즌 = lag(리시브_효율))

kovo_team %>%
  group_by(남녀부, 시즌) %>%
  summarise(리시브_효율 = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도)) %>%
  mutate(다음_시즌 = lead(리시브_효율)) %>%
  print(n = 17)

kovo_team %>%
  group_by(남녀부, 시즌) %>%
  summarise(리시브_효율 = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도)) %>% 
  mutate(차이 = 리시브_효율 - lag(리시브_효율))

kovo_team %>%
  group_by(남녀부,  시즌) %>%
  summarise(리시브_효율  = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도),
                  .groups = 'drop') %>%
  mutate(차이 = 리시브_효율 -lag(리시브_효율)) %>%
  summarise(차이_평균 = mean(차이))

kovo_team %>%
  group_by(남녀부, 시즌) %>%
  summarise(리시브_효율 = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도),
            .groups = 'drop') %>%
  mutate(차이 = 리시브_효율 -lag(리시브_효율)) %>%
  summarise(차이_평균 = mean(차이, na.rm = TRUE))

kovo_team %>%
  group_by(남녀부, 시즌) %>%
  summarise(리시브_효율 = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도),
                  .groups = 'drop') %>%
  mutate(차이 = 리시브_효율 -lag(리시브_효율)) %>%
  drop_na() %>% 
  summarise(차이_평균 = mean(차이))

kovo_team %>%
  group_by(남녀부, 시즌) %>%
  summarise(리시브_효율 = (sum(리시브_정확) - sum(리시브_실패)) / sum(리시브_시도)) %>% 
  mutate(차이 = 리시브_효율 -lag(리시브_효율)) %>%
  drop_na() %>% 
  arrange(차이)
