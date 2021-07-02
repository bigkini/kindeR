pacman::p_load(tidyverse, tidymodels)
set.seed(1234)

tibble(
  추가_승수 = rbinom(100000, 100, .5),
  예상_승률 = (추가_승수 + 22) / 144,
  마지노선 = sample(seq(.486, .559, .010), 100000, replace = TRUE),
  진출_성공 = if_else(예상_승률 >= 마지노선, 1, 0)
) -> lotte_simulation

lotte_simulation %>% 
  group_by(추가_승수) %>% 
  summarise(
    전체_횟수 = n(),
    진출_성공 = sum(진출_성공),
    진출_확률 =  진출_성공 / 전체_횟수,
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = 추가_승수, y = 진출_확률)) +
  geom_line()

tibble(
  x = rbinom(100000, 100, .5),
  y = rbinom(100000, 100, .5),
  z = if_else(x > y, 1, 0)
) %>% 
  group_by(x) %>% 
  summarise(
    n = n(),
    z = sum(z),
    p = z / n,
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = x, y = p)) +
  geom_line() +
  geom_hline(yintercept = .5, linetype = 'dotted')

crossing(
  x = runif(100, 0, 1),
  y = runif(100, 0, 1)
) %>% 
  mutate(
    odds_x = x / (1 - x),
    odds_y = y / (1 - y),
    odds_ratio = odds_x / odds_y,
    logit = log(odds_ratio)
  ) %>% 
  ggplot(aes(x = logit)) +
  geom_histogram(bins = 30, fill = 'gray75', color = 'white')

tibble(
  x = seq(-10, 10, .1),
  y = exp(x) / (1 + exp(x))
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line()

'kovo_set_by_set.csv' %>% 
  read.csv() %>% 
  as_tibble() -> kovo_sets

kovo_sets

kovo_sets %>%
  mutate(승리 = 승리 %>% as_factor()) -> kovo_sets

kovo_sets %>% 
  filter(남녀부 == '남') -> kovo_set_male

kovo_set_male %>%
  glm(승리  ~  서브효율 + 리시브효율 + 공격효율 + 블로킹 + 디그,
        family = binomial,
        data = .) %>% 
  tidy()

kovo_set_male %>%
  glm(승리  ~  서브효율 + 리시브효율 + 공격효율 + 블로킹 + 디그,
        family = binomial,
        data = .) %>% 
  vip::vi()

kovo_set_male %>%
  lm(공격효율 ~ 리시브효율, data = .) %>% 
  glance()

kovo_set_male %>% 
  initial_split(strata = '승리') -> set_split

set_split %>% training() -> set_train

set_split %>% testing() -> set_test

recipe(승리 ~ 서브효율 + 리시브효율 + 공격효율 + 블로킹 + 디그, data = set_train)

#주석 코드
grep("^step_", ls("package:recipes"), value = TRUE)

recipe(승리 ~ 서브효율 + 리시브효율 + 공격효율 + 블로킹 + 디그, data = set_train) %>% 
  step_corr(all_predictors()) %>%
  step_normalize(all_predictors(), -all_outcomes()) -> set_recipe

set_recipe %>% 
  prep()

set_recipe %>% 
  prep() %>% 
  juice()

logistic_reg() %>% 
  set_engine('glm') -> set_lr_model

set_lr_model

workflow() %>% 
  add_model(set_lr_model) %>% 
  add_recipe(set_recipe) -> set_lr_workflow

set_lr_workflow

set_lr_workflow %>% 
  fit(data = set_train) -> set_lr_fit

set_lr_fit

set_lr_fit %>% 
  tidy()

set_lr_fit %>% 
  predict(set_train)

set_lr_fit %>% 
  predict(set_train) %>% 
  bind_cols(set_train) %>% 
  relocate(승리, .before = 시즌)

set_lr_fit %>% 
  predict(set_train) %>% 
  bind_cols(set_train) %>% 
  metrics(truth = 승리, estimate = .pred_class)

set_lr_fit %>% 
  predict(set_train) %>% 
  bind_cols(set_train) %>% 
  conf_mat(truth = 승리, estimate = .pred_class) 

set_lr_fit %>% 
  predict(set_test) %>% 
  bind_cols(set_test) %>% 
  metrics(truth = 승리, estimate = .pred_class)

set_lr_fit %>% 
  predict(set_train, type='prob') 

tibble(
  x = runif(100000, 0, 100),
  t = sample(seq(0, 100, 10), 100000, replace=TRUE),
  pred = if_else(x >= t, 1, 0),
  actual = sample(c(0, 1), 100000, replace=TRUE)
) %>% 
  group_by(t) %>% 
  summarise(
    true_true = sum(actual == 1 & pred == 1),
    true_false = sum(actual == 1 & pred == 0),
    false_true = sum(actual == 0 & pred == 1),
    false_false = sum(actual == 0 & pred == 0),
    sensitivity = true_true / (true_true + true_false),
    specificity = false_false / (false_true + false_false),
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path()

set_lr_fit %>% 
  predict(set_train, type='prob') %>% 
  bind_cols(set_train) %>% 
  roc_curve(truth = 승리, estimate = .pred_0)

set_lr_fit %>% 
  predict(set_train, type='prob') %>% 
  bind_cols(set_train) %>% 
  roc_curve(truth = 승리, estimate = .pred_0) %>% 
  autoplot()

set_lr_fit %>% 
  predict(set_test, type='prob') %>% 
  bind_cols(set_test) %>% 
  roc_curve(truth = 승리, estimate = .pred_0) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(linetype = 'dotted') +
  coord_equal()

set_lr_fit %>% 
  predict(set_train, type='prob') %>% 
  bind_cols(set_train) %>% 
  roc_auc(truth = 승리, .pred_0) 

set_lr_fit %>% 
  predict(set_test, type='prob') %>% 
  bind_cols(set_test) %>% 
  roc_auc(truth = 승리, .pred_0)