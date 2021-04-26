require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

raw <- list.files(here::here("ignore", "data", "raw"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-3n9k")

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
         encoding_pic = `counterbalance-mqv3`,
         retrieval_pic = `counterbalance-ekxi`,
         starts_with("pic_c"),
         starts_with("pic_d")) %>% 
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
                                    ends_with("_pic"),
                                    resp,
                                    rt,
                                    pic_c = paste0("pic_c.", .y),
                                    pic_d = paste0("pic_d.", .y)))) %>%
  unnest(data) %>% 
  mutate(category = if_else(group == "academic", category1, category2),
         pic_shown = if_else(retrieval_pic == "c", pic_c, pic_d),
         resp_binary = if_else(resp > 50, 1L, 0L)) %>% 
  separate(pic_shown, into = c("pic_shown", NA)) %>% 
  separate(pic_shown, into = c(NA, "is_old"), sep = -1L) %>% 
  mutate(is_old = if_else(is_old == encoding_pic, 1L, 0L)) %>% 
  select(-c(category1, category2, categories, pic_c, pic_d, encoding_pic, retrieval_pic))

write_csv(less_raw, file = here::here("ignore", "data", "task_retrieval_pics.csv"))
