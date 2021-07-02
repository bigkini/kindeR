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
      �λ�  = c('OB', '�λ�'),
      Ű��  = c('�������', '�ؼ�', 'Ű��'),
      ��ȭ  = c('���׷�', '��ȭ'),
      ����  = c('���', '��̡�û��', 'û��', '�����'),
      KIA = c('����', '���¡�KIA', 'KIA'),
      LG = c('LG', 'MBC'),
    ), .before = year
  ) 

team_batting %>%
  mutate(
    team_id = fct_collapse(
      team,
      �λ�  = c('OB', '�λ�'),
      Ű��  = c('�������', '�ؼ�', 'Ű��'),
      ��ȭ  = c('���׷�', '��ȭ'),
      ����  = c('���', '��̡�û��', 'û��', '�����'),
      KIA = c('����', '���¡�KIA', 'KIA'),
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
  fct_lump(3, other_level = '��Ÿ') %>%
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
  distinct(��) %>%
  arrange(��) %>% 
  pull()

kovo_team %>%
  mutate(
    �� = case_when(
      ����� == '��' & �� == '����' ~ '����ĳ��Ż',
      ����� == '��' & �� == '����' ~ '����Ǽ�',
      TRUE ~  ��),
    ��_�̸� = ��,
    �� = fct_collapse(
      ��,
      �����װ� = 'KAL',
      �Ｚȭ�� = '�Ｚ',
      �� = c('��', '�󹫽���', '������'),
      �츮ī�� = c('�帲�Ľ�', '�츮ī��', '�츮ĳ��Ż'),
      �ѱ����� = c('�ѱ�����', '����', 'KEPCO', 'KEPCO45'),
      KB���غ��� = c('KB�պ�', 'LG', 'LIG', 'LIG�պ�'),
      OK�����׷� = c('���þ�ĳ��', 'OK��������'),
      GSĮ�ؽ� = 'GS',
      IBK������� = 'IBK',
      KGC�λ���� = c('�λ����', 'KGC�λ����', 'KT&G'),
      �ѱ����ΰ��� = c('����', '���ΰ���'),
      �ﱹ���� = '�ﱹ'
    )
  ) -> kovo_team

kovo_team %>% 
  distinct(����) %>% 
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
  mutate(����_�̸�=����,
              ����=���� %>% str_sub(-4)) -> kovo_team

kovo_team

kovo_team %>% 
  select(����, ��, contains('��������'))

kovo_team %>% 
  select(-contains('_'))

kovo_team %>% 
  select(����, ��, starts_with('��Ʈ'))

kovo_team %>% 
  select(����, ��, ends_with('��Ʈ��_���'))

tribble(
  ~x, ~y,
  1, 2,
) %>% 
  rename(z = x)

kovo_team %>%
  select(����,  ��, ends_with('��Ʈ��_���')) %>%
  rename(����ŷ =  ����ŷ_��Ʈ��_���,
            ���� =  ����_��Ʈ��_���)

str_replace('����ŷ_��Ʈ��_���', '_��Ʈ��_���', '')

kovo_team %>% 
  select(����, ��, ends_with('��Ʈ��_���')) %>% 
  rename_with(~str_replace(., '_��Ʈ��_���', ''))

kovo_team %>%
  group_by(����, �����) %>%
  summarise(���ú�_ȿ�� = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�),
            .groups = 'drop') %>%
  ggplot(aes(x = ����, y = ���ú�_ȿ��, group = �����, color = �����)) +
  geom_line()

kovo_team %>%
  group_by(�����, ����) %>%
  summarise(���ú�_ȿ�� = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�)) %>%
  mutate(��_���� = lag(���ú�_ȿ��))

kovo_team %>%
  group_by(�����, ����) %>%
  summarise(���ú�_ȿ�� = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�)) %>%
  mutate(����_���� = lead(���ú�_ȿ��)) %>%
  print(n = 17)

kovo_team %>%
  group_by(�����, ����) %>%
  summarise(���ú�_ȿ�� = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�)) %>% 
  mutate(���� = ���ú�_ȿ�� - lag(���ú�_ȿ��))

kovo_team %>%
  group_by(�����,  ����) %>%
  summarise(���ú�_ȿ��  = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�),
                  .groups = 'drop') %>%
  mutate(���� = ���ú�_ȿ�� -lag(���ú�_ȿ��)) %>%
  summarise(����_��� = mean(����))

kovo_team %>%
  group_by(�����, ����) %>%
  summarise(���ú�_ȿ�� = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�),
            .groups = 'drop') %>%
  mutate(���� = ���ú�_ȿ�� -lag(���ú�_ȿ��)) %>%
  summarise(����_��� = mean(����, na.rm = TRUE))

kovo_team %>%
  group_by(�����, ����) %>%
  summarise(���ú�_ȿ�� = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�),
                  .groups = 'drop') %>%
  mutate(���� = ���ú�_ȿ�� -lag(���ú�_ȿ��)) %>%
  drop_na() %>% 
  summarise(����_��� = mean(����))

kovo_team %>%
  group_by(�����, ����) %>%
  summarise(���ú�_ȿ�� = (sum(���ú�_��Ȯ) - sum(���ú�_����)) / sum(���ú�_�õ�)) %>% 
  mutate(���� = ���ú�_ȿ�� -lag(���ú�_ȿ��)) %>%
  drop_na() %>% 
  arrange(����)