require(tidyverse)
require(magrittr)
source(here::here("R", "utils_read_gorilla.R"))

## reading in and cleaning ----

raw <- list.files(here::here("ignore", "data", "raw"), recursive = T, full.names = T) %>% 
  read_gorilla_data("task-txhz")

pretest <- read_csv(here::here("ignore", "data", "task_pretest.csv")) %>%
  select(-n_correct) %>%
  pivot_wider(names_from = category_num, values_from = category, names_prefix = "category") %>%
  mutate(categories = paste(category1, category2, sep = "_"))

less_raw <- raw %>% 
  select(subj_num = `Participant Private ID`,
         group,
         encoding_block,
         group_trial_num = trial_num,
         retrieval_trial_num = `Trial Number`,
         trial_screen = `Screen Name`,
         zone = `Zone Name`,
         rt = `Reaction Time`,
         resp = `Response`,
         timeout = `Timed Out`,
         starts_with("test_question"),
         starts_with("test_answer")) %>%
  filter(trial_screen == "recall") %>%
  mutate(retrieval_trial_num = as.integer(retrieval_trial_num),
         timeout = if_else(lead(zone, 1) == "timelimit", 1L, timeout)) %>%
  filter(zone == "response") %>%
  select(-c(trial_screen, zone)) %>% 
  nest(data = -c(subj_num, retrieval_trial_num)) %>% 
  mutate(data = map(data, get_typing_rts)) %>% 
  unnest(data) %>% 
  nest(data = -subj_num) %>% 
  left_join(pretest, by = "subj_num") %>% 
  mutate(data = map2(data, categories,
                     ~.x %>% select(group,
                                    group_trial_num,
                                    retrieval_trial_num,
                                    resp,
                                    rt_start,
                                    rt_end,
                                    test_question = paste0("test_question.", .y),
                                    test_answer = paste0("test_answer.", .y)))) %>%
  unnest(data) %>% 
  mutate(category = if_else(group == "academic", category1, category2)) %>% 
  select(-c(category1, category2, categories)) %>%
  mutate(acc_recall_rough = map2_lgl(resp, test_answer,
                                     function (a, b) {
                                       # trim non alpha nums in response in case they make grepl yell
                                       a <- str_remove_all(a, "[^a-zA-Z\\d\\s-']")
                                       if (!is.na(a) & nchar(a) == 0) {
                                         return (FALSE)
                                       } else {
                                         out <- (grepl(a, b, ignore.case = T) | grepl(b, a, ignore.case = T)) %>% 
                                           coalesce(FALSE)
                                         
                                         return (out)
                                       }
                                     }),
         encoding_is_late = if_else(group_trial_num <= 20, 0L, 1L)) %>% 
  group_by(subj_num) %>% 
  mutate(retrieval_block = rep(1:16, each = 5)) %>% 
  group_by(subj_num, retrieval_block) %>% 
  mutate(ep_is_same = if_else(length(unique(encoding_is_late)) == 1, 1L, 0L),
         sem_is_same = if_else(length(unique(group)) == 1, 1L, 0L))

## scoring some by hand----

scored <- less_raw %>% 
  mutate(acc_recall = case_when(
    nchar(resp) == 1 ~ 0, # implies couldn't finish typing
    group_trial_num == 9 & category == "arms" & grepl("naptha", resp, ignore.case = T) ~ 1,
    group_trial_num == 15 & category == "arms" & grepl("wana", resp, ignore.case = T) ~ 1,
    group_trial_num == 19 & category == "arms" & grepl("lion", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 25 & category == "arms" & grepl("shamishar", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 26 & category == "arms" & grepl("wazakahi", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 28 & category == "arms" & grepl("sedeko", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 32 & category == "arms" & grepl("halberk", resp, ignore.case = T) ~ 1,
    group_trial_num == 35 & category == "arms" & grepl("paldron", resp, ignore.case = T) ~ 1,
    group_trial_num == 3 & category == "dino" & grepl("potassium argon", resp, ignore.case = T) ~ 1,
    group_trial_num == 14 & category == "dino" & grepl("apotasaurus", resp, ignore.case = T) ~ 1,
    group_trial_num == 16 & category == "dino" & grepl("nose", resp, ignore.case = T) ~ 1,
    group_trial_num == 16 & category == "dino" & grepl("head", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 25 & category == "dino" & !grepl("big mama", resp, ignore.case = T) ~ 0,
    group_trial_num == 29 & category == "dino" & (grepl("pnycto", resp, ignore.case = T) | grepl("pyco", resp, ignore.case = T)) ~ 0.5,
    group_trial_num == 31 & category == "dino" & grepl("chicken", resp, ignore.case = T) ~ 1,
    group_trial_num == 32 & category == "dino" & grepl("arms", resp, ignore.case = T) ~ 1,
    group_trial_num == 36 & category == "dino" & grepl("sawtooth", resp, ignore.case = T) ~ 1,
    group_trial_num == 37 & category == "dino" & grepl("vertical", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 1 & category == "gems" & grepl("cleav", resp, ignore.case = T) ~ 1,
    group_trial_num == 18 & category == "gems" & grepl("pleochrom", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 19 & category == "gems" & tolower(resp) == "scope" ~ 0,
    group_trial_num == 22 & category == "gems" & grepl("adamant", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 34 & category == "gems" & grepl("cat eye", resp, ignore.case = T) ~ 1,
    group_trial_num == 39 & category == "gems" & grepl("flame", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 9 & category == "musi" & grepl("amadillo", resp, ignore.case = T) ~ 1,
    group_trial_num == 19 & category == "musi" & grepl("claw form", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 29 & category == "musi" & grepl("vocorder", resp, ignore.case = T) ~ 1,
    group_trial_num == 35 & category == "musi" & grepl("fipper", resp, ignore.case = T) ~ 0.5,
    group_trial_num == 7 & category == "cook" & !grepl("bain", resp, ignore.case = T) ~ 0,
    group_trial_num == 29 & category == "cook" & grepl("Portug", resp, ignore.case = T) ~ 1,
    group_trial_num == 38 & category == "cook" & grepl("head of hen", resp, ignore.case = T) ~ 0.5,
    TRUE ~ as.numeric(acc_recall_rough)
  ))

scored %>%
  arrange(group, category, group_trial_num) %>%
  select(subj_num, group_trial_num, resp, test_answer, acc_recall_rough, acc_recall, test_question, everything())

write_csv(scored, file = here::here("ignore", "data", "task_retrieval_facts.csv"))
