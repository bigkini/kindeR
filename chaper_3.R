pacman::p_load(tidyverse)

'kbo_batting_qualified.csv' %>% 
  read.csv() %>% 
  as_tibble() -> batting

ggplot(data = batting)

ggplot(data = batting, mapping = aes(x = avg)) + 
  geom_histogram()

ggplot(batting, aes(avg)) +
  geom_histogram()

batting %>% 
  ggplot(aes(avg)) +
  geom_histogram()

batting %>% 
  ggplot(aes(avg)) +
  geom_histogram(binwidth = .001)

batting %>%
  ggplot(aes(avg)) +
  geom_histogram(bins = 30, fill = 'gray', color = 'red')

colors()

batting %>%
  ggplot(aes(avg)) +
  geom_histogram(bins = 30,
                 fill = rgb(0.325, 0.750, 0.830),
                 color = 'white')

rgb(0.324, 0.750, 0.830)

batting %>%
  ggplot(aes(avg)) +
  geom_histogram(bins = 30, fill = '#53bfd4', color = 'white')

batting %>%
  ggplot(aes(throw_bat)) +
  geom_bar()

batting %>%
  ggplot(aes(x = throw_bat,
             y = stat(count))) +
  geom_bar()

batting %>%
  ggplot(aes(x = throw_bat,
             y = ..count..)) +
  geom_bar()

batting %>%
  ggplot(aes(throw_bat)) +
  stat_count()

batting$throw_bat %>% 
  table()

tribble(
  ~throw_bat, ~count,
  '우양', 30,
  '우우', 1001,
  '우좌', 155,
  '좌좌', 435
) -> bar_example

bar_example %>%
  ggplot(aes(x = throw_bat,
             y = count)) +
  geom_bar()

bar_example %>%
  ggplot(aes(throw_bat, count)) +
  geom_bar(stat = 'identity')

bar_example %>%
  ggplot(aes(throw_bat, count)) +
  geom_col()

bar_example$throw_bat

bar_example$throw_bat %>% 
  as_factor()

bar_example$throw_bat %>% 
  as_factor() %>% 
  fct_relevel('우우', '좌좌', '우좌', '우양') 

bar_example$throw_bat %>% 
  fct_reorder(bar_example$count)

bar_example %>%  
  ggplot(aes(x = throw_bat %>% fct_reorder(count),
             y = count)) +
  geom_col()

bar_example %>% 
  ggplot(aes(x = throw_bat %>% fct_reorder(count, .desc = TRUE),
             y = count)) +
  geom_col()

bar_example %>%
  ggplot(aes(x = throw_bat %>% fct_reorder(-count),
             y = count)) +
  geom_col()

batting %>% 
  ggplot(aes(x = throw_bat %>% fct_infreq())) +
  geom_bar()

batting[batting$rank == 1,]

batting[batting$rank == 1,] %>%
  ggplot(aes(x = year,
             y = avg)) +
  geom_line()

batting[batting$rank == 1,] %>%
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lwd = 1)

batting[batting$rank == 1,] %>%
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(linetype = 'dashed')

batting[batting$rank == 1,] %>%
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lty = 2)

'2020_ryu.csv' %>% 
  read.csv() %>% 
  as_tibble() -> ryu

ryu %>% 
  dim()

ryu %>% 
  names()

ryu %>%
  ggplot(aes(x = pitch_name %>% fct_reorder(release_speed),
             y = release_speed)) +
  geom_point()

ryu %>%
  ggplot(aes(x = pitch_name %>% fct_reorder(release_speed),
             y = release_speed)) +
  geom_jitter()

ryu %>%
  ggplot(aes(x = pitch_name %>% fct_reorder(release_speed),
             y = release_speed)) +
  geom_violin() +
  geom_jitter(alpha = .2)

ryu %>%
  ggplot(aes(x = pitch_name %>% fct_reorder(release_speed),
             y = release_speed)) +
  geom_boxplot()

ryu %>%
  ggplot(aes(x = plate_x, plate_z)) +
  geom_point() 

ryu %>%
  ggplot(aes(x = plate_x, plate_z)) +
  geom_point() +
  facet_grid( ~ pitch_name) +
  coord_fixed()

ryu %>%
  ggplot(aes(x = plate_x, plate_z)) +
  geom_density_2d_filled() +
  facet_grid(stand ~ pitch_name) +
  coord_fixed() +
  guides(fill = FALSE)

ryu[ryu$pitch_name %in% c('4-Seam Fastball', 'Changeup'), ] %>%
  ggplot(aes(x = plate_x, plate_z)) +
  geom_density_2d_filled() +
  annotate(
    geom ='rect',
    xmin = 1,
    xmax = -1,
    ymin = 1,
    ymax = 3,
    color = 'white',
    alpha = .1,
    linetype = 'dashed'
  ) +
  facet_grid(pitch_name ~ stand) +
  coord_fixed() +
  guides(fill = FALSE)

batting %>%
  ggplot(aes(x = avg, y = obp, shape = throw_bat)) +
  geom_point()

ryu %>%
  ggplot(aes(x = release_speed)) +
  geom_density(fill = 'gray75')

ryu %>%
  ggplot(aes(x = release_speed)) +
  geom_density(fill = 'gray75') +
  facet_grid(pitch_name %>% fct_reorder(-release_speed) ~ .)

tibble(x = -5:5) %>% 
  ggplot(aes(x = x)) +
  geom_function(fun = dnorm)

tibble(x = -5:5) %>% 
  ggplot(aes(x = x)) +
  geom_area(stat = 'function', fun = dnorm)

tribble(
  ~response, ~value,
  '이름 짓기', 49,
  '개발 가능 혹은 불가능한 사항 설명하기', 16,
  '개발 작업이 끝나는 시간 산정하기', 10,
  '다른 사람과 함께 일하기', 8,
  '다른 개발자 코드 작업하기', 8,
  '내가 수긍 못할 기능 구현하기', 3,
  '문서 작성', 2,
  '테스트 작성', 2,
  '해법 찾기', 2
) -> developers_chore

developers_chore %>%
  ggplot(aes(x = response %>% fct_reorder(value),
             y = value)) +
  geom_col()

developers_chore %>%
  ggplot(aes(x = response %>% fct_reorder(value),
             y = value)) +
  geom_col() +
  geom_text(aes(label = value), nudge_y = 2) +
  coord_flip()

developers_chore %>%
  ggplot(aes(
    x = '',
    y = value,
    fill = response %>% fct_reorder(value)
  )) +
  geom_col(width = 1) +
  coord_polar(theta = 'y')

batting[batting$rank == 1,] %>%
  ggplot(aes(x = year,
             y = avg)) +
  geom_line() +
  ylim(.350, .420)

batting[batting$rank == 1,] %>%
  ggplot(aes(x = year,
             y = avg)) +
  geom_line() +
  coord_cartesian(ylim=c(.350, .420))




