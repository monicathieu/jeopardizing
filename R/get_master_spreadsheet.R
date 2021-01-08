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
  mutate(category_num_academic = category_num_nonacademic,
         category_label_academic = fct_recode(category_num_academic,
                                              musi = "1",
                                              arms = "2",
                                              gems = "3"),
         category_label_nonacademic = fct_recode(category_num_nonacademic,
                                              cars = "1",
                                              cook = "2",
                                              dino = "3")) %>%
  select(-starts_with("category_num")) %>% 
  # This does the heavy lifting of permuting the category pairs
  expand(nesting(category_label_nonacademic, info_nonacademic),
         nesting(category_label_academic, info_academic)) %>% 
  mutate(info_all = map2(info_nonacademic,
                         info_academic,
                         ~bind_rows(nonacademic = .x,
                                    academic = .y,
                                    .id = "group"))) %>% 
  select(-info_nonacademic, -info_academic) %>% 
  unnest(info_all) %>% 
  select(-category) %>% 
  pivot_wider(names_from = starts_with("category_label"),
              # encoding_trial_num is the ID col here
              # only specify it as omitted from values_from
              # and it will be implied into id_cols
              values_from = c(encoding_sentence,
                              test_question,
                              starts_with("pic"),
                              test_answer),
              names_glue = "{.value}.{category_label_academic}_{category_label_nonacademic}")


encoding <- master_spreadsheet %>% 
  select(encoding_trial_num,
         group,
         randomise_blocks,
         trial_num,
         starts_with("encoding_sentence"),
         starts_with("pic_a"),
         starts_with("pic_b")) %>% 
  arrange(encoding_trial_num) %>% 
  mutate(display = "memorise") %>% 
  bind_rows(tibble(display = "study_instructions"),
            .,
            tibble(display = "finish"))

retrieval_facts_order <- list(
  tibble(randomise_blocks = 1L,
         block_trial_num = 1:5,
         group = rep("academic", 5),
         encoding_block = rep("early", 5)),
  tibble(randomise_blocks = 2L,
         block_trial_num = 1:5,
         group = rep("nonacademic", 5),
         encoding_block = rep("early", 5)),
  tibble(randomise_blocks = 3L,
         block_trial_num = 1:5,
         group = rep("academic", 5),
         encoding_block = rep("late", 5)),
  tibble(randomise_blocks = 4L,
         block_trial_num = 1:5,
         group = rep("nonacademic", 5),
         encoding_block = rep("late", 5)),
  tibble(randomise_blocks = rep(5:6, each = 5),
         block_trial_num = rep(1:5, 2),
         group = rep(c("academic", "nonacademic"), 5),
         encoding_block = rep("early", 10)),
  tibble(randomise_blocks = rep(7:8, each = 5),
         block_trial_num = rep(1:5, 2),
         group = rep(c("academic", "nonacademic"), 5),
         encoding_block = rep("late", 10)),
  tibble(randomise_blocks = rep(9:10, each = 5),
         block_trial_num = rep(1:5, 2),
         group = rep("academic", 10),
         encoding_block = rep(c("early", "late"), 5)),
  tibble(randomise_blocks = rep(11:12, each = 5),
         block_trial_num = rep(1:5, 2),
         group = rep("nonacademic", 10),
         encoding_block = rep(c("early", "late"), 5)),
  tibble(randomise_blocks = rep(13:14, each = 5),
         block_trial_num = rep(1:5, 2),
         group = rep(c("academic", "nonacademic"), 5),
         encoding_block = rep(c("early", "late"), 5)),
  tibble(randomise_blocks = rep(15:16, each = 5),
         block_trial_num = rep(1:5, 2),
         group = rep(c("academic", "nonacademic"), 5),
         encoding_block = rep(c("late", "early"), 5))
) %>% 
  bind_rows() %>% 
  group_by(group, encoding_block) %>%
  mutate(temp_trial_num = 1:n()) %>%
  ungroup()

retrieval_facts <- master_spreadsheet %>% 
  select(encoding_trial_num,
         group,
         trial_num,
         starts_with("test_question"),
         starts_with("test_answer")) %>% 
  mutate(display = "test",
         encoding_block = if_else(encoding_trial_num <= 40, "early", "late"),
         # temp_trial_num x encoding_block = trial_num
         temp_trial_num = if_else(encoding_block == "late",
                                  trial_num - 20,
                                  trial_num)) %>% 
  select(-encoding_trial_num) %>% 
  inner_join(retrieval_facts_order,
             .,
             by = c("group", "encoding_block", "temp_trial_num")) %>% 
  select(-temp_trial_num) %>% 
  mutate(randomise_trials = case_when(group == "academic" & encoding_block == "early" ~ 1L,
                                      group == "nonacademic" & encoding_block == "early" ~ 2L,
                                      group == "academic" & encoding_block == "late" ~ 3L,
                                      group == "nonacademic" & encoding_block == "late" ~ 4L,
                                      TRUE ~ NA_integer_)) %>% 
  arrange(randomise_blocks,
          block_trial_num) %>% 
  select(trial_num,
         block_trial_num,
         group,
         encoding_block,
         display,
         randomise_blocks,
         randomise_trials,
         everything()) %>% 
  bind_rows(tibble(display = "study_instructions"),
            .,
            tibble(display = "finish"))

retrieval_pics <- master_spreadsheet %>%
  select(trial_num,
         group,
         starts_with("pic_c"),
         starts_with("pic_d")) %>% 
  mutate(randomise_trials = 1L,
         display = "mem_intrusion") %>% 
  select(trial_num,
         display,
         randomise_trials,
         everything()) %>% 
  bind_rows(tibble(display = "mem_intrusion_instructions"),
            .,
            tibble(display = "finish"))

# TODO: Save widened spreadsheet to csv

write_csv(encoding,
          here::here("stim_stuff", "stimlist_encoding.csv"), na = "")

write_csv(retrieval_facts,
          here::here("stim_stuff", "stimlist_retrieval_facts.csv"), na = "")

write_csv(retrieval_pics,
          here::here("stim_stuff", "stimlist_retrieval_pics.csv"), na = "")
