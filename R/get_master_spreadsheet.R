## setup ----

require(tidyverse)
require(googledrive)
require(magrittr)

# Should download master spreadsheet to local copy for R manipulation
# Trust me, the ID is correct
# Checked it using drive_get("~/jstudy/master_spreadsheet")
# But that takes a little bit to run so I hard coded it here
drive_download(file = as_id("1degtvNziMvUM3V7wBX6VPkYyuUsEKOezWb8VZ933Irw"),
               path = here::here("stim_stuff", "master_spreadsheet.csv"),
               type = "csv",
               overwrite = TRUE)

master_spreadsheet_raw <- read_csv(here::here("stim_stuff", "master_spreadsheet.csv"))

## parse master spreadsheet ----

master_spreadsheet <- master_spreadsheet_raw %>% 
  select(-ends_with("pretest")) %>% 
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

## pretest ----

pretest <- master_spreadsheet_raw %>%
  select(trial_num, ends_with("pic_a"), ends_with("pic_b"), ends_with("pretest")) %>%
  pivot_longer(cols = -trial_num,
               names_to = c("category", ".value"),
               names_sep = 5L) %>%
  filter(pretest) %>%
  select(-pretest) %>% 
  arrange(category, trial_num) %>% 
  mutate(category = str_sub(category, end = -2L),
         # manually label all the object names depicted in each pic
         # because the test answer is not always the name of the object depicted
         object_name = case_when(
           category == "arms" & trial_num == 15 ~ "wonna",
           category == "arms" & trial_num == 19 ~ "bagh nakh",
           category == "arms" & trial_num == 25 ~ "shamshir",
           category == "arms" & trial_num == 32 ~ "hauberk",
           category == "arms" & trial_num == 36 ~ "besagew",
           category == "cars" & trial_num == 12 ~ "flywheel",
           category == "cars" & trial_num == 13 ~ "torque converter",
           category == "cars" & trial_num == 16 ~ "catalytic converter",
           category == "cars" & trial_num == 23 ~ "differential",
           category == "cars" & trial_num == 33 ~ "drum brake",
           category == "cook" & trial_num == 2 ~ "blewit",
           category == "cook" & trial_num == 3 ~ "ramp",
           category == "cook" & trial_num == 10 ~ "escabeche",
           category == "cook" & trial_num == 11 ~ "galantine",
           category == "cook" & trial_num == 13 ~ "involtino",
           category == "dino" & trial_num == 15 ~ "Megalosaurus",
           category == "dino" & trial_num == 23 ~ "Cetiosaurus",
           category == "dino" & trial_num == 25 ~ "Citipati",
           category == "dino" & trial_num == 33 ~ "Aepyornis",
           category == "dino" & trial_num == 35 ~ "Shonisaurus",
           category == "gems" & trial_num == 11 ~ "pegmatite",
           category == "gems" & trial_num == 13 ~ "botryoidal crystal",
           category == "gems" & trial_num == 15 ~ "chalcedony",
           category == "gems" & trial_num == 26 ~ "howlite",
           category == "gems" & trial_num == 35 ~ "cabochon crystal",
           category == "musi" & trial_num == 6 ~ "rabab",
           category == "musi" & trial_num == 11 ~ "theorbo",
           category == "musi" & trial_num == 13 ~ "hurdy-gurdy",
           category == "musi" & trial_num == 17 ~ "hardingfele",
           category == "musi" & trial_num == 32 ~ "duduk",
         )) %>% 
  nest(objects = -category) %>% 
  # pivot wider and repeat rows the button images
  # to prepare for 5 trials per category with same 5 buttons on each
  mutate(buttons = map(objects,
                       ~.x %>%
                         mutate(button_num = 1:n()) %>%
                         select(button_num, starts_with("pic")) %>%
                         pivot_wider(names_from = button_num,
                                     values_from = -button_num,
                                     names_sep = ".",
                                     names_prefix = "button") %>%
                         slice(rep(1, 5))),
         objects = map(objects,
                       ~.x %>% 
                         rename_with(~paste0(., ".correct"), starts_with("pic")) %>% 
                         mutate(randomise_trials = 1,
                                display = "test") %>% 
                         select(display,
                                randomise_trials,
                                everything())),
         # paste on so each row has the correct answer and the 5 choices
         objects = map2(objects, buttons, bind_cols),
         objects = map(objects,
                       ~.x %>% 
                         bind_rows(tibble(display = "instructions"),
                                   .,
                                   tibble(display = "finish")))) %>% 
  select(-buttons)

## encoding ----

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
         starts_with("pic_a"),
         starts_with("pic_b")) %>% 
  arrange(encoding_trial_num)

## retrieval_facts ----

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
  bind_rows(tibble(display = "instructions"),
            .,
            tibble(display = "finish"))

## retrieval_pics ----

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

## retrieval_source ----

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

## write out csvs ----

pretest %>% 
  mutate(category = glue::glue("stimlist_pretest_{category}.csv")) %$% 
  walk2(objects, category,
        ~write_csv(.x, here::here("stim_stuff", .y), na = ""))

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

write_csv(retrieval_facts,
          here::here("stim_stuff", "stimlist_retrieval_facts.csv"), na = "")

write_csv(retrieval_pics,
          here::here("stim_stuff", "stimlist_retrieval_pics.csv"), na = "")

write_csv(retrieval_source,
          here::here("stim_stuff", "stimlist_retrieval_source.csv"), na = "")

