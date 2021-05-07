require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

raw <- list.files(here::here("ignore", "data", "raw"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-1152")

pretest <- read_csv(here::here("ignore", "data", "task_pretest.csv")) %>%
  select(-n_correct) %>%
  pivot_wider(names_from = category_num, values_from = category, names_prefix = "category") %>%
  mutate(categories = paste(category1, category2, sep = "_"))

less_raw <- raw %>% 
  select(subj_num = `Participant Private ID`,
         group,
         group_trial_num = trial_num,
         trial_screen = `Screen Name`,
         zone = `Zone Name`,
         rt = `Reaction Time`,
         resp = `Response`,
         timeout = `Timed Out`,
         starts_with("encoding_sentence")) %>% 
  filter(trial_screen == "test") %>% 
  mutate(across(c(group_trial_num, resp), as.integer),
         timeout = if_else(lead(zone, 1) == "timelimit", 1L, timeout)) %>% 
  nest(data = -c(subj_num, group, group_trial_num)) %>% 
  # stuff to pull only the row of scale trials where the slider stopped moving
  mutate(data = map(data, get_slider_rt)) %>%
  unnest(data) %>% 
  select(-c(trial_screen, zone)) %>% 
  nest(data = -subj_num) %>% 
  left_join(pretest, by = "subj_num") %>% 
  mutate(data = map2(data, categories,
                     ~.x %>% select(group,
                                    group_trial_num,
                                    resp,
                                    rt,
                                    encoding_sentence = paste0("encoding_sentence.", .y)))) %>%
  unnest(data) %>% 
  mutate(category = if_else(group == "academic", category1, category2),
         encoding_block = if_else(group_trial_num <= 20, 0L, 1L),
         resp = if_else(encoding_block == 1, resp - 50, -(resp - 50)),
         resp_binary = if_else(resp > 0, 1L, 0L)) %>% 
  select(-c(category1, category2, categories))

write_csv(less_raw, file = here::here("ignore", "data", "task_retrieval_source.csv"))
