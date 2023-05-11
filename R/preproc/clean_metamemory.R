clean_metamemory <- function (metamemory_data) {
  out <- metamemory_data %>% 
    select(subj_num = `Participant Private ID`,
           trial_num = `Trial Number`,
           trial_screen = `Screen Name`,
           zone = `Zone Name`,
           rt = `Reaction Time`,
           resp = `Response`,
           timeout = `Timed Out`,
           question,
           answer) %>% 
    mutate(question = str_replace(question, "1890", "1899"),
           answer = if_else(answer == "SF Giants", "San Francisco Giants", answer))
  
  return (out)
}

clean_metamemory_states <- function (metamemory_data) {
  out <- metamemory_data %>%
    clean_metamemory() %>% 
    filter(trial_screen == "recall") %>% 
    select(-c(trial_screen, zone))
  
  return (out)
}

# write_csv(mem_states, file = here::here("ignore", "data", "task_jeopardy_meta_states.csv"))

clean_metamemory_descriptions <- function (metamemory_data) {
  out <- metamemory_data %>%
    clean_metamemory() %>% 
    filter(trial_screen == "describe" & zone == "response") %>% 
    select(subj_num, trial_num, question, resp)
  
  return (out)
}

# write_csv(mem_descriptions, file = here::here("ignore", "data", "task_jeopardy_meta_descriptions.csv"))
