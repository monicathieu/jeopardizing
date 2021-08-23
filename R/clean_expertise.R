require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

## reading in and cleaning ----

raw <- list.files(here::here("ignore", "data", "raw", "real"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-v9u2")

less_raw <- raw %>% 
  # Just this one P had a weird task lapse where 2.5 trials repeated
  filter(!(`Participant Private ID` == 4488331 & `Event Index` %in% 74:86)) %>% 
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
         resp = if_else(zone == "scale" & rt >= 14900, NA_character_, resp),
         # Fix the typo for the older subjects so the question text doesn't appear repeated
         question = str_replace(question, "1890", "1899"),
         answer = if_else(answer == "SF Giants", "San Francisco Giants", answer)) %>%
  nest(data = -c(subj_num, trial_num, trial_screen)) %>% 
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
                        get_typing_rts() %>% 
                        rename_with(~paste0(., "_recall"), c(resp, rt_start, rt_end, timeout)))) %>%
  unnest(c(recall, confidence))

write_csv(less_raw, file = here::here("ignore", "data", "task_jeopardy_recall_unscored.csv"))
