## format spreadsheet ----

make_spreadsheet_retrieval_source <- function (master_spreadsheet_data, stimlist_folder) {
  
  retrieval_source <- master_spreadsheet_data %>% 
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
  
  # write out
  out_path <- here::here(stimlist_folder, "stimlist_retrieval_source.csv")
  write_csv(retrieval_source,
            out_path, na = "")
  
  # returns path to file bc targets-compatible
  return (out_path)
}
