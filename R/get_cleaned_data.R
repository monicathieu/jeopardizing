require(tidyverse)
require(magrittr)

demos_ids <- read_csv(here::here("ignore", "data", "demos_ids.csv"))
q_google_demos <- read_csv(here::here("ignore", "data", "q_google_demos.csv")) %>% 
  right_join(demos_ids, by = "demographics_id") %>% 
  mutate(subj_num = factor(subj_num)) %>% 
  # One subject seems to have filled it out twice but gave the same data both times (thank god)
  distinct(subj_num, age, edu, gender, ethnicity, race)

j_recall <- read_csv(here::here("ignore", "data", "task_jeopardy_recall.csv"))

j_exp <- j_recall %>% 
  group_by(subj_num) %>% 
  summarize(j_score = mean(acc_recall))

j_recall %<>%
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))

j_meta <- read_csv(here::here("ignore", "data", "task_jeopardy_meta_states.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score),
         resp = fct_relevel(resp, "Learning Memory", after = Inf))
j_meta_desc <- read_csv(here::here("ignore", "data", "task_jeopardy_meta_descriptions.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))

encoding <- read_csv(here::here("ignore", "data", "task_encoding.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))

r_facts <- read_csv(here::here("ignore", "data",  "task_retrieval_facts.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))
r_pics <- read_csv(here::here("ignore", "data", "task_retrieval_pics.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))
r_source <- read_csv(here::here("ignore", "data", "task_retrieval_source.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))
q_trivia_demos  <- read_csv(here::here("ignore", "data", "q_trivia_demos.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))

q_posttask  <- read_csv(here::here("ignore", "data", "q_posttask.csv"))

