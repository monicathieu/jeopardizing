require(tidyverse)
require(magrittr)

demos_ids <- read_csv(here::here("ignore", "data", "demos_ids.csv"))
q_google_demos <- read_csv(here::here("ignore", "data", "q_google_demos.csv")) %>% 
  right_join(demos_ids, by = "demographics_id") %>% 
  mutate(subj_num = factor(subj_num))

j_recall <- read_csv(here::here("ignore", "data", "task_jeopardy_recall.csv")) %>% 
  mutate(resp_conf = if_else(is.na(resp_recall), NA_real_, resp_conf))

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
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score),
         already_knew = fct_recode(already_knew,
                                   none = "Did not know this before",
                                   some = "Knew some of this before",
                                   all = "Knew all of this before"),
         already_knew = fct_relevel(already_knew, "all", after = Inf))

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

retrieval <- r_facts %>%
  select(subj_num, j_score, group, group_trial_num, retrieval_block, ep_is_same, sem_is_same, retrieval_trial_num, category, acc_recall, rt_start_recall = rt_start, rt_end_recall = rt_end) %>% 
  left_join(r_pics %>% 
              select(subj_num, group, group_trial_num, category, resp_pic = resp, rt_pic = rt)) %>% 
  left_join(r_source %>%
              select(subj_num, group, group_trial_num, category, resp_source = resp, rt_source = rt)) %>%
  left_join(encoding %>% 
              select(subj_num, group, group_trial_num, category, interest = resp, already_knew)) %>% 
  mutate(retrieval_block_type = case_when(ep_is_same == 1 & sem_is_same == 1 ~ "ep_same_sem_same",
                                          ep_is_same == 1 & sem_is_same == 0 ~ "ep_same_sem_diff",
                                          ep_is_same == 0 & sem_is_same == 1 ~ "ep_diff_sem_same",
                                          ep_is_same == 0 & sem_is_same == 0 ~ "ep_diff_sem_diff",
                                          TRUE ~ NA_character_),
  #mutate(retrieval_block_type = case_when(retrieval_block <= 4 ~ "ep_same_sem_same",
   #                                       retrieval_block <= 8 ~ "ep_same_sem_diff",
    #                                      retrieval_block <= 12 ~ "ep_diff_sem_same",
     #                                     retrieval_block <= 16 ~ "ep_diff_sem_diff",
      #                                    TRUE ~ NA_character_),
         encoding_trial_num = if_else(group == "academic",
                                      group_trial_num * 2 - 1,
                                      group_trial_num * 2))
