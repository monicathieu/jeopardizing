require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

raw <- list.files(here::here("ignore", "data", "raw", "real"), recursive = T, full.names = T) %>% 
  read_gorilla_data("questionnaire-qdx5")

less_raw <- raw %>% 
  select(subj_num = `Participant Private ID`, q_key = `Question Key`, resp = Response) %>%
  filter(!endsWith(q_key, "QUESTIONNAIRE")) %>% 
  pivot_wider(names_from = q_key, values_from = resp) %>% 
  rename_with(~str_replace(., "-", "_"), everything())

write_csv(less_raw, file = here::here("ignore", "data", "q_posttask.csv"))
