## setup ----

require(tidyverse)
require(magrittr)

master_spreadsheet_raw <- read_csv(here::here("stim_stuff", "master_spreadsheet.csv"))

## format spreadsheet ----

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

## write out ----

pretest %>% 
  mutate(category = glue::glue("stimlist_pretest_{category}.csv")) %$% 
  walk2(objects, category,
        ~write_csv(.x, here::here("stim_stuff", .y), na = ""))
