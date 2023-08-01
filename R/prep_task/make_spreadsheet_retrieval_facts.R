## format spreadsheet ----

make_spreadsheet_retrieval_facts <- function (master_spreadsheet_data, stimlist_folder) {
  
  retrieval_facts_order <- list(
    # 1-4: same episodic same semantic
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
    # 5-8: same episodic different semantic
    tibble(randomise_blocks = rep(5:6, each = 5),
           block_trial_num = rep(1:5, 2),
           group = rep(c("academic", "nonacademic"), 5),
           encoding_block = rep("early", 10)),
    tibble(randomise_blocks = rep(7:8, each = 5),
           block_trial_num = rep(1:5, 2),
           group = rep(c("academic", "nonacademic"), 5),
           encoding_block = rep("late", 10)),
    # 9-12: different episodic same semantic
    tibble(randomise_blocks = rep(9:10, each = 5),
           block_trial_num = rep(1:5, 2),
           group = rep("academic", 10),
           encoding_block = rep(c("early", "late"), 5)),
    tibble(randomise_blocks = rep(11:12, each = 5),
           block_trial_num = rep(1:5, 2),
           group = rep("nonacademic", 10),
           encoding_block = rep(c("early", "late"), 5)),
    # 12-16: different episodic different semantic
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
  
  retrieval_facts <- master_spreadsheet_data %>% 
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
  
  ## write out ----
  
  out_path <- here::here(stimlist_folder, "stimlist_retrieval_facts.csv")
  
  write_csv(retrieval_facts,
            out_path, na = "")
  
  return (out_path)
}
