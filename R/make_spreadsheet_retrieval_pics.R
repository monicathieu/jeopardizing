## setup ----

require(tidyverse)
require(magrittr)

master_spreadsheet <- read_csv(here::here("stim_stuff", "master_spreadsheet_parsed.csv"))

## format spreadsheet ----

retrieval_pics <- master_spreadsheet %>%
  select(trial_num,
         group,
         starts_with("pic_c"),
         starts_with("pic_d")) %>% 
  mutate(randomise_trials = 1L,
         display = "test") %>% 
  select(trial_num,
         display,
         randomise_trials,
         everything()) %>% 
  bind_rows(tibble(display = "instructions"),
            .,
            tibble(display = "finish"))


## write out ----

write_csv(retrieval_pics,
          here::here("stim_stuff", "stimlist_retrieval_pics.csv"), na = "")