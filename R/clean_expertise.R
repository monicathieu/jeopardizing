require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

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
  mutate(confidence = map(confidence,
                          ~.x %>%
                            mutate(trial_num_within = 1:nrow(.),
                                   resp = as.integer(resp),
                                   resp_diff = c(diff(resp), NA_integer_),
                                   resp_diff = coalesce(resp_diff, 0L)) %>%
                            # this SHOULD keep one row per trial and also keep no responses
                            filter(trial_num_within == min(trial_num_within[resp_diff == 0])) %>% 
                            select(resp, rt, timeout) %>% 
                            rename_with(~paste0(., "_conf"), everything())),
         recall = map(recall, ~.x %>%
                        select(-zone) %>% 
                        mutate(rt = if_else(!is.na(timeout), 15000, rt)) %>% 
                        rename_with(~paste0(., "_recall"), c(resp, rt, timeout)))) %>%
  unnest(c(recall, confidence)) %>% 
  mutate(acc_recall_rough = map2_lgl(resp_recall, answer, grepl, ignore.case = TRUE))

write_csv(less_raw, file = here::here("ignore", "data", "task_jeopardy_recall.csv"))
