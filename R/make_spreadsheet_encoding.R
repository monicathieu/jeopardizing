## setup ----

require(tidyverse)
require(magrittr)

master_spreadsheet <- read_csv(here::here("stim_stuff", "master_spreadsheet_parsed.csv"))

## format spreadsheet ----

encoding <- master_spreadsheet %>% 
  # NOT INCLUDING RANDOMISE_BLOCKS RIGHT NOW
  # retrieval facts requires knowing which blocks appeared in which order
  # using randomise_blocks appears not to allow easy feeding of block order
  # into the next task
  # so right now, encoding order is fixed for everybody
  # bind on the start/finish display rows LATER
  # when encoding is split into 2 before writing into 1st/2nd half CSVs
  select(encoding_trial_num,
         group,
         trial_num,
         starts_with("encoding_sentence"),
         starts_with("mp3"),
         starts_with("pic_a"),
         starts_with("pic_b"),
         starts_with("mp3"),
         starts_with("wait_duration")) %>% 
  arrange(encoding_trial_num)


## write out ----

encoding %>% 
  slice(1:40) %>% 
  mutate(display = "memorise_firsthalf") %>% 
  bind_rows(tibble(display = "instructions_firsthalf"),
            .,
            tibble(display = "finish_firsthalf")) %>% 
  write_csv(here::here("stim_stuff", "stimlist_encoding_firsthalf.csv"), na = "")

encoding %>% 
  slice(41:80) %>% 
  mutate(display = "memorise_secondhalf") %>% 
  bind_rows(tibble(display = "instructions_secondhalf"),
            .,
            tibble(display = "finish_secondhalf")) %>% 
  write_csv(here::here("stim_stuff", "stimlist_encoding_secondhalf.csv"), na = "")
