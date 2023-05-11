## setup ----

require(tidyverse)
require(magrittr)

master_spreadsheet <- read_csv(here::here("stim_stuff", "master_spreadsheet_parsed.csv"))

## format spreadsheet ----

retrieval_source <- master_spreadsheet %>% 
  select(encoding_trial_num,
         group,
         trial_num,
         starts_with("encoding_sentence")) %>% 
  arrange(encoding_trial_num) %>% 
  mutate(encoding_block = if_else(encoding_trial_num <= 40, "early", "late"),
         display = "test",
         # DO randomise trials completely for the source test
         randomise_trials = 1) %>% 
  select(randomise_trials, display, encoding_block, group, trial_num, everything()) %>% 
  bind_rows(tibble(display = "instructions"),
            .,
            tibble(display = "finish"))

## write out ----

write_csv(retrieval_source,
          here::here("stim_stuff", "stimlist_retrieval_source.csv"), na = "")