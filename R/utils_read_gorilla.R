require(tidyverse)
require(magrittr)

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

read_gorilla_data <- function (paths, node_id) {
  out <- paths[grepl(node_id, paths)] %>%
    map(~read_lines(.)) %>%
    .[lengths(.) > 2] %>%
    map(~head(., n = -1)) %>%
    map(~paste(., collapse = "\n")) %>%
    map(~read_csv(., na = c("", "NA", "LOADING DELAY", "A loading delay of more than 10s was detected."))) %>%
    map(~mutate(., across(any_of(c("randomise_blocks", "Timed Out")), as.integer))) %>%
    bind_rows()
  return (out)
}
