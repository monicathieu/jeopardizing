require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

## reading in and cleaning ----

raw <- list.files(here::here("ignore", "data", "raw"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-v9u2")

less_raw <- raw %>% 
  select(subj_num = `Participant Private ID`,
         trial_num = `Trial Number`,
         trial_screen = `Screen Name`,
         zone = `Zone Name`,
         rt = `Reaction Time`,
         resp = `Response`,
         timeout = `Timed Out`,
         question,
         answer) %>%
  filter(trial_screen %in% c("recall", "confidence")) %>%
  mutate(trial_num = as.integer(trial_num),
         timeout = if_else(lead(zone, 1) == "timelimit", 1L, timeout),
         # Should catch slider trials where the slider was never clicked
         # bc I think timeout will still be true if submitResp was never clicked but the slider was moved
         resp = if_else(zone == "scale" & rt >= 14900, NA_character_, resp)) %>%
  filter(!(zone %in% c("continueButton", "timelimit"))) %>%
  # stuff to pull only the row of scale trials where the slider stopped moving
  nest(data = -c(subj_num, trial_num, trial_screen)) %>%
  pivot_wider(names_from = trial_screen, values_from = data) %>% 
  mutate(confidence = map(confidence, ~.x %>% 
                            mutate(resp = as.integer(resp)) %>% 
                            get_slider_rt()),
         confidence = map(confidence,
                          ~.x %>%
                            select(resp, rt, timeout) %>% 
                            rename_with(~paste0(., "_conf"), everything())),
         recall = map(recall, ~.x %>%
                        select(-zone) %>% 
                        mutate(rt = if_else(!is.na(timeout), 15000, rt)) %>% 
                        rename_with(~paste0(., "_recall"), c(resp, rt, timeout)))) %>%
  unnest(c(recall, confidence)) %>% 
  mutate(acc_recall_rough = map2_lgl(resp_recall, answer,
                                     ~(grepl(.x, .y, ignore.case = T) | grepl(.y, .x, ignore.case = T)) %>% 
                                       coalesce(FALSE)))

## scoring some by hand----

less_raw %>%
  arrange(answer) %>%
  select(subj_num, resp_recall, answer, acc_recall_rough, question, everything())

scored <- less_raw %>% 
  mutate(acc_recall = case_when(
    answer == "Andrea Bocelli" & grepl("Bocelli", resp_recall, ignore.case = T) ~ TRUE,
    answer == "SF Giants" & grepl("Giants", resp_recall, ignore.case = T) ~ TRUE,
    answer == "Spanish-American War" & grepl("Spanish American", resp_recall, ignore.case = T) ~ TRUE,
    TRUE ~ acc_recall_rough
  ))

write_csv(scored, file = here::here("ignore", "data", "task_jeopardy_recall.csv"))
