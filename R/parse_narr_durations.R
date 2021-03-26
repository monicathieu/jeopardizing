require(tidyverse)
require(magrittr)

durations <- read_delim(here::here("ignore", "narration", "durations.txt"),
                        delim = "=", col_names = c("key", "value")) %>%
  mutate(across(everything(), str_trim, side = "both"),
         id = rep(1:(n()/2), each = 2)) %>%
  pivot_wider(names_from = key, values_from = value) %>%
  rename(mp3_duration = kMDItemDurationSeconds,
         mp3 = kMDItemFSName) %>%
  mutate(mp3_duration = as.numeric(mp3_duration),
         wait_duration = pmin(25, mp3_duration),
         category = str_sub(mp3, end = 4L),
         trial_num = as.integer(str_sub(mp3, start = 5L, end = -6L)),
         counterbalance = str_sub(mp3, start = -5L, end = -5L)) %>%
  select(-id, -mp3_duration) %>%
  # Get it wide by category and counterbalance to fit the master_spreadsheet format
  pivot_wider(id_cols = trial_num, names_from = c(category, counterbalance),
              values_from = c(mp3, ends_with("duration")),
              names_glue = "{category}_{.value}_{counterbalance}") %>% 
  arrange(trial_num)

write_csv(durations, file = here::here("ignore", "narration", "durations_parsed.csv"))
