## format spreadsheet ----

make_spreadsheet_retrieval_pics <- function (master_spreadsheet_data, stimlist_folder) {
  
  retrieval_pics <- master_spreadsheet_data %>%
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
  
  # write out
  out_path <- here::here(stimlist_folder, "stimlist_retrieval_pics.csv")
  write_csv(retrieval_pics,
            out_path, na = "")
  
  # returns path to file bc targets-compatible
  return (out_path)
}
