clean_retrieval_facts <- function (retrieval_data, pretest_data) {
  out <- retrieval_data %>% 
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
    nest(data = -c(subj_num, retrieval_trial_num)) %>% 
    # patch in a "response" row if fully timed out
    mutate(data = map(data,
                      function (x) {
                        if (nrow(x) == 1) {
                          if (x$zone == "timelimit") {
                            x$zone <- "response"
                          }
                        }
                        return (x)
                      })) %>% 
    unnest(data) %>% 
    filter(zone == "response") %>%
    select(-c(trial_screen, zone)) %>% 
    nest(data = -c(subj_num, retrieval_trial_num)) %>% 
    mutate(data = map(data, get_typing_rts)) %>% 
    unnest(data) %>% 
    nest(data = -subj_num) %>% 
    left_join(pretest_data, by = "subj_num") %>% 
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
    mutate(encoding_is_late = if_else(group_trial_num <= 20, 0L, 1L)) %>% 
    group_by(subj_num) %>% 
    mutate(retrieval_block = rep(1:16, each = 5)) %>% 
    group_by(subj_num, retrieval_block) %>% 
    mutate(ep_is_same = if_else(length(unique(encoding_is_late)) == 1, 1L, 0L),
           sem_is_same = if_else(length(unique(group)) == 1, 1L, 0L))
  
  return (out)
}

# write_csv(less_raw, file = here::here("ignore", "data", "task_retrieval_facts_unscored.csv"))

clean_retrieval_pics <- function (retrieval_data, pretest_data) {
  out <- retrieval_data %>% 
    select(subj_num = `Participant Private ID`,
           group,
           trial_num,
           trial_screen = `Screen Name`,
           zone = `Zone Name`,
           rt = `Reaction Time`,
           resp = `Response`,
           timeout = `Timed Out`,
           encoding_pic = `counterbalance-mqv3`,
           # retrieval_pic_oldnew = `counterbalance-ekxi`,
           retrieval_pic_2afc = `randomiser-snec`,
           starts_with("pic_c"),
           starts_with("pic_d")) %>% 
    filter(trial_screen == "test") %>% 
    mutate(across(c(trial_num, resp), as.integer),
           timeout = if_else(lead(zone, 1) == "timelimit", 1L, timeout)) %>% 
    nest(data = -c(subj_num, group, trial_num)) %>% 
    # stuff to pull only the row of scale trials where the slider stopped moving
    mutate(data = map(data, get_slider_rt)) %>%
    unnest(data) %>% 
    select(-c(trial_screen, zone)) %>% 
    nest(data = -subj_num) %>% 
    left_join(pretest_data, by = "subj_num") %>% 
    mutate(data = map2(data, categories,
                       ~.x %>% select(group,
                                      trial_num,
                                      contains("_pic"),
                                      resp,
                                      rt,
                                      pic_c = paste0("pic_c.", .y),
                                      pic_d = paste0("pic_d.", .y)))) %>%
    unnest(data) %>% 
    filter(!is.na(retrieval_pic_2afc)) %>% 
    mutate(category = if_else(group == "academic", category1, category2),
           pic_r = if_else(retrieval_pic_2afc == "retrievalPicLeftC", pic_d, pic_c)) %>% 
    separate(pic_r, into = c("pic_r", NA)) %>% 
    separate(pic_r, into = c(NA, "group_trial_num", "r_is_old"), sep = c(4L, -1L), convert = TRUE) %>%
    mutate(r_is_old = if_else(r_is_old == encoding_pic, 1L, 0L),
           resp = if_else(r_is_old == 1, resp - 50, -(resp - 50)),
           resp_binary = if_else(resp > 0, 1L, 0L)) %>% 
    select(-c(trial_num, category1, category2, categories, pic_c, pic_d, encoding_pic, starts_with("retrieval_pic")))
  
  return (out)
}

# write_csv(d_2afc, file = here::here("ignore", "data", "task_retrieval_pics.csv"))

clean_retrieval_source <- function (retrieval_data, pretest_data) {
  out <- retrieval_data %>% 
    select(subj_num = `Participant Private ID`,
           group,
           group_trial_num = trial_num,
           trial_screen = `Screen Name`,
           zone = `Zone Name`,
           rt = `Reaction Time`,
           resp = `Response`,
           timeout = `Timed Out`,
           starts_with("encoding_sentence")) %>% 
    filter(trial_screen == "test") %>% 
    mutate(across(c(group_trial_num, resp), as.integer),
           timeout = if_else(lead(zone, 1) == "timelimit", 1L, timeout)) %>% 
    nest(data = -c(subj_num, group, group_trial_num)) %>% 
    # stuff to pull only the row of scale trials where the slider stopped moving
    mutate(data = map(data, get_slider_rt)) %>%
    unnest(data) %>% 
    select(-c(trial_screen, zone)) %>% 
    nest(data = -subj_num) %>% 
    left_join(pretest_data, by = "subj_num") %>% 
    mutate(data = map2(data, categories,
                       ~.x %>% select(group,
                                      group_trial_num,
                                      resp,
                                      rt,
                                      encoding_sentence = paste0("encoding_sentence.", .y)))) %>%
    unnest(data) %>% 
    mutate(category = if_else(group == "academic", category1, category2),
           encoding_block = if_else(group_trial_num <= 20, 0L, 1L),
           resp = if_else(encoding_block == 1, resp - 50, -(resp - 50)),
           resp_binary = if_else(resp > 0, 1L, 0L)) %>% 
    select(-c(category1, category2, categories))
  
  return (out)
}

# write_csv(less_raw, file = here::here("ignore", "data", "task_retrieval_source.csv"))

bind_retrieval <- function (fact_data_scored, pic_data_clean, source_data_clean, encoding_data_clean) {
  out <- fact_data_scored %>%
    select(subj_num, j_score, group, group_trial_num, retrieval_block, ep_is_same, sem_is_same, retrieval_trial_num, category, acc_recall, rt_start_recall = rt_start, rt_end_recall = rt_end) %>% 
    left_join(pic_data_clean %>% 
                select(subj_num, group, group_trial_num, category, resp_pic = resp, rt_pic = rt)) %>% 
    left_join(source_data_clean %>%
                select(subj_num, group, group_trial_num, category, resp_source = resp, rt_source = rt)) %>%
    left_join(encoding_data_clean %>% 
                select(subj_num, group, group_trial_num, category, interest = resp, already_knew)) %>% 
    mutate(retrieval_block_type = case_when(ep_is_same == 1 & sem_is_same == 1 ~ "ep_same_sem_same",
                                            ep_is_same == 1 & sem_is_same == 0 ~ "ep_same_sem_diff",
                                            ep_is_same == 0 & sem_is_same == 1 ~ "ep_diff_sem_same",
                                            ep_is_same == 0 & sem_is_same == 0 ~ "ep_diff_sem_diff",
                                            TRUE ~ NA_character_),
           encoding_trial_num = if_else(group == "academic",
                                        group_trial_num * 2 - 1,
                                        group_trial_num * 2))
  
  return (out)
}
