require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

raw <- list.files(here::here("ignore", "data", "raw"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-wlkb")

less_raw <- raw %>% 
  select(subj_num = `Participant Private ID`,
         trial_num = `Trial Number`,
         trial_screen = `Screen Name`,
         zone = `Zone Name`,
         rt = `Reaction Time`,
         resp = `Response`,
         timeout = `Timed Out`,
         question,
         answer)

mem_states <- less_raw %>%
  filter(trial_screen == "recall") %>% 
  select(-c(trial_screen, zone))

write_csv(mem_states, file = here::here("ignore", "data", "task_jeopardy_meta_states.csv"))

mem_descriptions <- less_raw %>%
  filter(trial_screen == "describe" & zone == "response") %>% 
  select(subj_num, trial_num, question, resp)

write_csv(mem_descriptions, file = here::here("ignore", "data", "task_jeopardy_meta_descriptions.csv"))
