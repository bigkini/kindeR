mtcars[sample(1:nrow(mtcars), 10), ]

mtcars %>% 
  sample_n()

install.packages('tidyverse')

library('tidyverse')

stats::filter()
dplyr::filter()

install.packages('pacman')

pacman::p_load(tidyverse, tidymodels)

batting <- read_csv('kbo_batting_qualified.csv')

batting

batting <- read_csv('kbo_batting_qualified.csv', locale = locale('ko', encoding = 'euc-kr'))

glimpse(batting)

batting

class(batting)

batting <- read.csv('kbo_batting_qualified.csv')

batting <- as_tibble(batting)

class(batting)

df1 <- data.frame(x = c(1, 2, 3),
                  y = c(4, 5, 6))

df1

df2 <- tribble(~x, ~y,
               #---|---
               1,  4,
               2,  5,
               3,  6)

df2

df2 <- read.csv(textConnection("
x, y
1, 4
2, 5
3, 6
"))

df2

1:10 %>% sum()

batting %>% 
  print(n = 20)

options(dplyr.print_min = inf)

options(tibble.width = inf)

kbo %>%
  View() #V 대문자에 주의!

'kbo_batting_qualified.csv' %>% 
  read.csv() %>%
  as_tibble() -> batting
