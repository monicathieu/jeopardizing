require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

raw <- list.files(here::here("ignore", "data", "raw"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-4elo")

less_raw <- raw %>% 
  select(subj_num = "Participant Private ID",
         category_num = "Zone Name",
         category = "Response",
         n_correct = "Reaction Time") %>% 
  filter(startsWith(category_num, "category")) %>% 
  mutate(category_num = as.integer(str_sub(category_num, start = -1L)))

write_csv(less_raw, file = here::here("ignore", "data", "task_pretest.csv"))
