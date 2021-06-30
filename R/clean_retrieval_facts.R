require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

## reading in and cleaning ----

raw <- list.files(here::here("ignore", "data", "raw", "real"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-txhz")

pretest <- read_csv(here::here("ignore", "data", "task_pretest.csv")) %>%
  select(-n_correct) %>%
  pivot_wider(names_from = category_num, values_from = category, names_prefix = "category") %>%
  mutate(categories = paste(category1, category2, sep = "_"))

less_raw <- raw %>% 
  select(subj_num = `Participant Private ID`,
         group,
         encoding_block,
         group_trial_num = trial_num,
         retrieval_trial_num = `Trial Number`,
         trial_screen = `Screen Name`,
         zone = `Zone Name`,
         rt = `Reaction Time`,
         resp = `Response`,
         timeout = `Timed Out`,
         starts_with("test_question"),
         starts_with("test_answer")) %>%
  filter(trial_screen == "recall") %>%
  mutate(retrieval_trial_num = as.integer(retrieval_trial_num),
         timeout = if_else(lead(zone, 1) == "timelimit", 1L, timeout)) %>%
  nest(data = -c(subj_num, retrieval_trial_num)) %>% 
  # patch in a "response" row if fully timed out
  mutate(data = map(data,
                    function (x) {
                      if (nrow(x) == 1) {
                        if (x$zone == "timelimit") {
                          x$zone <- "response"
                        }
                      }
                      return (x)
                    })) %>% 
  unnest(data) %>% 
  filter(zone == "response") %>%
  select(-c(trial_screen, zone)) %>% 
  nest(data = -c(subj_num, retrieval_trial_num)) %>% 
  mutate(data = map(data, get_typing_rts)) %>% 
  unnest(data) %>% 
  nest(data = -subj_num) %>% 
  left_join(pretest, by = "subj_num") %>% 
  mutate(data = map2(data, categories,
                     ~.x %>% select(group,
                                    group_trial_num,
                                    retrieval_trial_num,
                                    resp,
                                    rt_start,
                                    rt_end,
                                    test_question = paste0("test_question.", .y),
                                    test_answer = paste0("test_answer.", .y)))) %>%
  unnest(data) %>% 
  mutate(category = if_else(group == "academic", category1, category2)) %>% 
  select(-c(category1, category2, categories)) %>%
  mutate(encoding_is_late = if_else(group_trial_num <= 20, 0L, 1L)) %>% 
  group_by(subj_num) %>% 
  mutate(retrieval_block = rep(1:16, each = 5)) %>% 
  group_by(subj_num, retrieval_block) %>% 
  mutate(ep_is_same = if_else(length(unique(encoding_is_late)) == 1, 1L, 0L),
         sem_is_same = if_else(length(unique(group)) == 1, 1L, 0L))

## write out ----

write_csv(less_raw, file = here::here("ignore", "data", "task_retrieval_facts_unscored.csv"))
