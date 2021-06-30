## setup ----

require(tidyverse)
require(magrittr)
j_recall <- read_csv(here::here("ignore", "data", "task_jeopardy_recall.csv"))

j_exp <- j_recall %>% 
  group_by(subj_num) %>% 
  summarize(j_score = mean(acc_recall))

j_recall %<>%
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))

encoding <- read_csv(here::here("ignore", "data", "task_encoding.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))
r_facts <- read_csv(here::here("ignore", "data", "task_retrieval_facts.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))
r_pics <- read_csv(here::here("ignore", "data", "task_retrieval_pics.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))
r_source <- read_csv(here::here("ignore", "data", "task_retrieval_source.csv")) %>% 
  left_join(j_exp, by = "subj_num") %>% 
  mutate(subj_num = fct_reorder(as.character(subj_num), j_score))

retrieval <- r_facts %>%
  select(subj_num, j_score, group, group_trial_num, retrieval_block, ep_is_same, sem_is_same, retrieval_trial_num, category, acc_recall, rt_start_recall = rt_start, rt_end_recall = rt_end) %>% 
  left_join(r_pics %>% 
              select(subj_num, group, group_trial_num, category, resp_pic = resp, rt_pic = rt)) %>% 
  left_join(r_source %>%
              select(subj_num, group, group_trial_num, category, resp_source = resp, rt_source = rt)) %>%
  left_join(encoding %>% 
              select(subj_num, group, group_trial_num, category, interest = resp)) %>% 
  mutate(retrieval_block_type = case_when(retrieval_block <= 4 ~ "ep_same_sem_same",
                                          retrieval_block <= 8 ~ "ep_same_sem_diff",
                                          retrieval_block <= 12 ~ "ep_diff_sem_same",
                                          retrieval_block <= 16 ~ "ep_diff_sem_diff",
                                          TRUE ~ NA_character_))

## original model ----

glmer_retrieval_by_interest_expertise <- retrieval %>% 
  mutate(acc_recall = if_else(acc_recall == 0.5, 1, acc_recall),
         interest = (interest - 50) / 100,
         j_score = j_score - 0.5) %>% 
  lme4::glmer(acc_recall ~ interest * j_score + (1 | subj_num),
              family = binomial(link = "logit"),
              data = .)

coefs_retrieval_by_interest_expertise <- glmer_retrieval_by_interest_expertise %>%
  broom.mixed::tidy() %>%
  select(effect:estimate)

## thick ass simulation ----

sim_glmer_retrieval_by_interest_expertise <- crossing(n_subjs = c(50, 100, 150, 200),
                                                      iteration = 1:100) %>% 
  mutate(data = map(n_subjs, ~tibble(subj_num = 1:.x) %>% 
                      mutate(j_score = runif(nrow(.), min = -0.4, max = 0.4),
                             intercept = rnorm(nrow(.),
                                               coefs_retrieval_by_interest_expertise$estimate[1],
                                               coefs_retrieval_by_interest_expertise$estimate[5])))) %>%
  unnest(data) %>% 
  # 80 fact trials per fake subject
  mutate(data = map(subj_num, ~tibble(interest = rnorm(80, 58, 22)))) %>% 
  unnest(data) %>% 
  mutate(interest = pmax(interest, 0),
         interest = pmin(interest, 100),
         interest = (interest - 50)/100,
         acc_recall = rbernoulli(n(), p = boot::inv.logit(intercept + coefs_retrieval_by_interest_expertise$estimate[2]*interest + coefs_retrieval_by_interest_expertise$estimate[3]*j_score + coefs_retrieval_by_interest_expertise$estimate[4]*interest*j_score)),
         acc_recall = as.integer(acc_recall)) %>% 
  nest(data = -c(iteration, n_subjs)) %>% 
  mutate(coefs = map(data, ~lme4::glmer(acc_recall ~ interest * j_score + (1 | subj_num),
                                        family = binomial(link = "logit"),
                                        data = .) %>% 
                       broom.mixed::tidy())) %>% 
  select(-data) %>% 
  unnest(coefs)
