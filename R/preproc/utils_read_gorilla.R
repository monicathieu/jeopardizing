# No packages libraried because this now is only called by targets!

get_typing_rts <- function (df) {
  out <- df %>% 
    mutate(timeout = if_else(any(!is.na(timeout)), 1L, NA_integer_))
  if (nrow(df) > 1) {
    out %<>% 
      mutate(rt_type = c("start", "end"), resp = tail(resp, 1)) %>%
      pivot_wider(names_from = rt_type, values_from = rt, names_prefix = "rt_")
  } else {
    out %<>%
      rename(rt_end = rt) %>% 
      mutate(rt_start = NA_real_)
  }
  out %<>% 
    mutate(rt_end = if_else(!is.na(timeout), 15000, rt_end))
  
  return (out)
}

get_slider_rt <- function (df) {
  # stuff to pull only the row of scale trials where the slider stopped moving
  out <- df %>% 
    mutate(trial_num_within = 1:n(),
           resp_diff = c(diff(resp), NA_integer_),
           resp_diff = coalesce(resp_diff, 0L)) %>%
    # this SHOULD keep one row per trial and also keep no responses
    filter(trial_num_within == min(trial_num_within[resp_diff == 0])) %>% 
    select(-trial_num_within, -resp_diff)
  
  return (out)
}

# Always returns one mega-df
read_gorilla_data <- function (paths) {
  out <- paths %>%
    map(~read_lines(.)) %>%
    .[lengths(.) > 2] %>%
    map(~head(., n = -1)) %>%
    map(~paste(., collapse = "\n")) %>%
    map(~read_csv(., na = c("", "NA", "LOADING DELAY", "A loading delay of more than 10s was detected."))) %>%
    map(~mutate(., across(any_of(c("randomise_blocks", "Timed Out")), as.integer))) %>%
    bind_rows()
  return (out)
}

filter_paths_grepl <- function (paths, pattern) {
  paths[grepl(pattern, paths)]
}
