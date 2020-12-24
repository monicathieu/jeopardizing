require(tidyverse)
require(googledrive)

# Should download master spreadsheet to local copy for R manipulation
# Trust me, the ID is correct
# Checked it using drive_get("~/jstudy/master_spreadsheet")
# But that takes a little bit to run so I hard coded it here
drive_download(file = as_id("1degtvNziMvUM3V7wBX6VPkYyuUsEKOezWb8VZ933Irw"),
               path = here::here("stim_stuff", "master_spreadsheet.csv"),
               type = "csv",
               overwrite = TRUE)

master_spreadsheet_raw <- read_csv(here::here("stim_stuff", "master_spreadsheet.csv"))

master_spreadsheet <- master_spreadsheet_raw %>% 
  # Remove the instructions and finish displays
  # don't forget to put them back later!
  filter(display == "test") %>% 
  pivot_longer(cols = -c(trial_num:display),
               names_to = c("category", ".value"),
               names_sep = 5L) %>% 
  mutate(category = str_sub(category, end = -2L),
         group = if_else(category %in% c("musi", "arms", "gems"),
                         "academic",
                         "nonacademic"),
         category_num = fct_recode(category,
                                `1` = "musi",
                                `1` = "cars",
                                `2` = "arms",
                                `2` = "cook",
                                `3` = "gems",
                                `3` = "dino")) %>% 
  group_by(category) %>% 
  mutate(encoding_trial_num = 1:n() * 2,
         encoding_trial_num = if_else(group == "academic",
                                      encoding_trial_num - 1,
                                      encoding_trial_num)) %>% 
  ungroup() %>% 
  nest(info = -c(group, category_num)) %>% 
  pivot_wider(names_from = group,
              values_from = info,
              names_prefix = "info_") %>% 
  rename(category_num_nonacademic = category_num) %>% 
  mutate(category_num_academic = category_num_nonacademic) %>% 
  expand(nesting(category_num_nonacademic, info_nonacademic),
         nesting(category_num_academic, info_academic)) %>% 
  mutate(info_all = map2(info_nonacademic,
                         info_academic,
                         bind_rows)) %>% 
  select(-info_nonacademic, -info_academic) %>% 
  unnest(info_all) %>% 
  # TODO: Specify these args correctly so we don't get list-col output
  pivot_wider(names_from = starts_with("category_num"),
              values_from = -starts_with("category_num"))
  
# TODO: Save widened spreadsheet to csv
