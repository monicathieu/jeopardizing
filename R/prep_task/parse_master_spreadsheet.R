## parse master spreadsheet ----

parse_master_spreadsheet <- function (master_spreadsheet_path, duration_data) {
  
  out <- master_spreadsheet_path %>%
    read_csv() %>% 
    select(-ends_with("pretest")) %>% 
    left_join(duration_data, by = "trial_num") %>% 
    pivot_longer(cols = -c(trial_num:display),
                 names_to = c("category", ".value"),
                 names_sep = 5L) %>% 
    mutate(category = str_sub(category, end = -2L),
           group = if_else(category %in% c("musi", "arms", "gems"),
                           "academic",
                           "nonacademic"),
           category_num = fct_recode(category,
                                     `1` = "musi",
                                     `1` = "cars",
                                     `2` = "arms",
                                     `2` = "cook",
                                     `3` = "gems",
                                     `3` = "dino")) %>% 
    group_by(category) %>% 
    mutate(encoding_trial_num = 1:n() * 2,
           encoding_trial_num = if_else(group == "academic",
                                        encoding_trial_num - 1,
                                        encoding_trial_num)) %>% 
    ungroup() %>% 
    nest(info = -c(group, category_num)) %>% 
    pivot_wider(names_from = group,
                values_from = info,
                names_prefix = "info_") %>% 
    rename(category_num_nonacademic = category_num) %>% 
    mutate(category_num_academic = category_num_nonacademic,
           category_label_academic = fct_recode(category_num_academic,
                                                musi = "1",
                                                arms = "2",
                                                gems = "3"),
           category_label_nonacademic = fct_recode(category_num_nonacademic,
                                                   cars = "1",
                                                   cook = "2",
                                                   dino = "3")) %>%
    select(-starts_with("category_num")) %>% 
    # This does the heavy lifting of permuting the category pairs
    expand(nesting(category_label_nonacademic, info_nonacademic),
           nesting(category_label_academic, info_academic)) %>% 
    mutate(info_all = map2(info_nonacademic,
                           info_academic,
                           ~bind_rows(nonacademic = .x,
                                      academic = .y,
                                      .id = "group"))) %>% 
    select(-info_nonacademic, -info_academic) %>% 
    unnest(info_all) %>% 
    select(-category) %>% 
    pivot_wider(names_from = starts_with("category_label"),
                # encoding_trial_num is the ID col here
                # only specify it as omitted from values_from
                # and it will be implied into id_cols
                values_from = c(encoding_sentence,
                                starts_with("mp3"),
                                starts_with("wait_duration"),
                                test_question,
                                starts_with("pic"),
                                test_answer),
                names_glue = "{.value}.{category_label_academic}_{category_label_nonacademic}")
  
  # write_csv(out, here::here("stim_stuff", "master_spreadsheet_parsed.csv"))
  return (out)
}
