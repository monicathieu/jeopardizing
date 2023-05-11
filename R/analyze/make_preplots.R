posterior_preplot_params <- function (object) {
  out <- object %>%
    extract_fit_engine() %>% 
    as.data.frame() %>%
    as_tibble() %>%
    mutate(iteration = 1:nrow(.)) %>%
    # removes all random effects and sigma terms. should yield fixed effects only
    select(-contains("["), -contains("sigma")) %>%
    rename(intercept = "(Intercept)") %>%
    pivot_longer(cols = -iteration, names_to = "term", values_to = "estimate")
  
  return (out)
}

posterior_preplot_bysubj <- function (object, newdata, pred_col = "y_pred", draws = 1000) {
  
  preproc <- object %>% 
    extract_recipe()
  
  newdata <- preproc %>% 
    bake(new_data = newdata)
  
  out <- object %>%
    extract_fit_engine() %>% 
    rstanarm::posterior_epred(newdata = newdata,
                              re.form = NULL,
                              draws = draws)
  
  # Just the median of every predicted point for every subject
  # Note that this doesn't select in a paired way for the "median iteration"
  # returns a VECTOR which needs to get slapped onto newdata
  out %<>%
    apply(2, median) %>%
    t() %>%
    c()
  
  newdata %<>%
    mutate(!!pred_col := out)
  
  return (newdata)
}

posterior_preplot_fixef <- function (object, newdata, pred_col = "y_pred", draws = 1000) {
  
  preproc <- object %>% 
    extract_recipe()
  
  newdata <- preproc %>% 
    bake(new_data = newdata)
  
  out <- object %>%
    extract_fit_engine() %>% 
    rstanarm::posterior_epred(newdata = newdata,
                              re.form = NA,
                              draws = draws)
  
  newdata %<>%
    # Must expand AFTER predictions is already done to repeat for iterations
    # need the "obs" column to make damn sure that the correct predicted points get pasted on the correct x values
    mutate(obs = 1:nrow(.),
           iteration = map(obs, ~1:draws)) %>%
    unchop(iteration)
  
  out %<>%
    as_tibble() %>%
    mutate(iteration = 1:nrow(.)) %>%
    pivot_longer(cols = -iteration, names_to = "obs", values_to = pred_col) %>%
    mutate(obs = as.integer(obs)) %>%
    right_join(newdata, by = c("obs", "iteration")) %>%
    # the obs column obstructs any later pivoting, and is technically redundant if all the x cols are in there,
    # so get outta here
    select(-obs)
  
  return(out)
}

# save(preplot_params_fact_alone,
#      preplot_params_fact_by_pic,
#      preplot_params_fact_by_source,
#      preplot_by_subj_fact_by_pic,
#      preplot_by_subj_fact_by_source,
#      preplot_fixef_fact_by_pic,
#      preplot_fixef_fact_by_source,
#      file = here::here("ignore", "data", "preplots.rda"))

