# Note: Run it with 4 cores if you can

## prep data further ----

prep_retrieval_data <- function (retrieval_data, extra_cols = NULL) {
  out <- retrieval_data %>%
    # Just drop those extra columns now
    # The recipe validation is super annoying when there's a bunch of leftover cols
    select(subj_num, 
           acc_recall, 
           resp_pic, resp_source, 
           j_score, 
           encoding_trial_num, 
           interest, 
           already_knew,
           {{extra_cols}})
  
  return (out)
}

step_retrieval_covariates <- function (in_recipe) {
  in_recipe %>% 
    # So that there is a value that will get treated as the true middle
    # Should never appear for the purposes of modeling
    # But we need to be able to get a hypothetical middle trial for preplots
    # with the effect code at 0
    step_num2factor(encoding_trial_num,
                    transform = \(x) case_when(x >= 41 ~ 3,
                                               x > 40 & x < 41 ~ 2,
                                               x <= 40 ~ 1,
                                               TRUE ~ NA_real_),
                    levels = c("early", "mid", "late"),
                    ordered = TRUE) %>% 
    step_ordinalscore(encoding_trial_num) %>%
    # to get it to -0.5 and +0.5
    # have to use step_range because ordinalscore requires int output
    step_range(encoding_trial_num, min = -0.5, max = 0.5) %>% 
    # scaling these to range of 10 makes it so unit change is 10% of the full range
    step_range(interest, min = -5, max = 5) %>% 
    # the actual lowest j_score is .26 so this gets it to the same range where a score of 0 would range down to -7
    step_range(j_score, min = -4.4, max = 3) %>%
    step_rename(from_encoding_late = encoding_trial_num)
}

make_retrieval_recipe <- function (retrieval_data_prepped, novel_facts = TRUE) {
  out <- recipe(retrieval_data_prepped)
  
  if (novel_facts) {
    out %<>%
      step_filter(already_knew == "none")
  } else {
    out %<>%
      step_filter(already_knew != "none")
  }
  
  out %<>% 
    update_role(acc_recall, new_role = "outcome") %>% 
    update_role(c(resp_pic, resp_source, j_score, encoding_trial_num, interest, subj_num), new_role = "predictor") %>% 
    # so I don't have to put already_knew into the preplot new data
    update_role_requirements(role = "NA", bake = FALSE) %>% 
    # update_role(subj_num, new_role = "ID") %>% 
    # Sort of a roundabout way of discretizing and then re-numeric-izing these variables
    # But it just seems better than changing the global contrasts option
    # and using the dummy variables steps
    # Have to use num2factor instead of cut because cut doesn't currently output ordered
    step_num2factor(acc_recall,
                    # num2factor expects factor levels to start at 1
                    transform = \(x) ifelse(x > 0, 2, 1),
                    levels = c("incorrect", "correct"),
                    ordered = TRUE,
                    # bc it's an outcome var, skip when running with predict
                    skip = TRUE) %>% 
    # so that leaving the slider as 0 gets binned in with incorrect
    step_num2factor(c(resp_pic, resp_source),
                    transform = \(x) ifelse(x > 0, 2, 1),
                    levels = c("incorrect", "correct"),
                    ordered = TRUE) %>% 
    step_ordinalscore(c(resp_pic, resp_source)) %>%
    # to get it to -0.5 and +0.5
    # have to use step_range because ordinalscore requires int output
    step_range(c(resp_pic, resp_source), min = -0.5, max = 0.5) %>% 
    step_retrieval_covariates()
  
  return (out)
}

make_retrieval.rt_recipe <- function (retrieval_data_prepped, novel_facts = TRUE) {
  out <- recipe(retrieval_data_prepped)
  
  if (novel_facts) {
    out %<>%
      step_filter(already_knew == "none", acc_recall > 0)
  } else {
    out %<>%
      step_filter(already_knew != "none", acc_recall > 0)
  }
  
  out %<>% 
    update_role(c(rt_start_recall, j_score, encoding_trial_num, interest, subj_num), new_role = "predictor") %>% 
    # so I don't have to put already_knew into the preplot new data
    update_role_requirements(role = "NA", bake = FALSE) %>% 
    # so that leaving the slider as 0 gets binned in with incorrect
    # all_outcomes() should run this on whichever of resp_pic or resp_source is the outcome
    step_num2factor(all_outcomes(),
                    transform = \(x) ifelse(x > 0, 2, 1),
                    levels = c("incorrect", "correct"),
                    ordered = TRUE,
                    skip = TRUE) %>% 
    step_normalize(rt_start_recall) %>% 
    step_retrieval_covariates()
  
  return (out)
}

make_retrieval.category_recipe <- function (retrieval_data_prepped, 
                                            category_group, 
                                            novel_facts = TRUE) {
  out <- recipe(retrieval_data_prepped)
  
  if (category_group == "academic") ref_category <- "gems" else ref_category <- "cars"
  
  if (novel_facts) {
    out %<>%
      step_filter(already_knew == "none", group == category_group)
  } else {
    out %<>%
      step_filter(already_knew != "none", group == category_group)
  }
  
  out %<>% 
    update_role(acc_recall, new_role = "outcome") %>% 
    update_role(c(category, j_score, encoding_trial_num, interest, subj_num), new_role = "predictor") %>% 
    # so I don't have to put already_knew into the preplot new data
    update_role_requirements(role = "NA", bake = FALSE) %>% 
    # so that leaving the slider as 0 gets binned in with incorrect
    # all_outcomes() should run this on whichever of resp_pic or resp_source is the outcome
    step_num2factor(acc_recall,
                    transform = \(x) ifelse(x > 0, 2, 1),
                    levels = c("incorrect", "correct"),
                    ordered = TRUE,
                    skip = TRUE) %>% 
    step_relevel(category, ref_level = ref_category) %>% 
    step_dummy(category) %>% 
    step_retrieval_covariates()
    
  
  return (out)
}

make_interest_recipe <- function (retrieval_data_prepped, novel_facts = TRUE) {
  out <- recipe(retrieval_data_prepped)
  
  if (novel_facts) {
    out %<>%
      step_filter(already_knew == "none")
  } else {
    out %<>%
      step_filter(already_knew != "none")
  }
  
  out %<>% 
    update_role(interest, new_role = "outcome") %>% 
    update_role(c(j_score, encoding_trial_num, subj_num), new_role = "predictor") %>% 
    # so I don't have to put already_knew into the preplot new data
    update_role_requirements(role = "NA", bake = FALSE) %>% 
    # update_role(subj_num, new_role = "ID") %>% 
    # So that there is a value that will get treated as the true middle
    # Should never appear for the purposes of modeling
    # But we need to be able to get a hypothetical middle trial for preplots
    # with the effect code at 0
    step_num2factor(encoding_trial_num,
                    transform = \(x) case_when(x >= 41 ~ 3,
                                               x > 40 & x < 41 ~ 2,
                                               x <= 40 ~ 1,
                                               TRUE ~ NA_real_),
                    levels = c("early", "mid", "late"),
                    ordered = TRUE) %>% 
    step_ordinalscore(encoding_trial_num) %>%
    # to get it to -0.5 and +0.5
    # have to use step_range because ordinalscore requires int output
    step_range(encoding_trial_num, min = -0.5, max = 0.5) %>% 
    # re-centering but and scaling because it might behave better at smaller range
    step_range(interest, min = -50, max = 50) %>% 
    # scaling these to range of 10 makes it so unit change is 10% of the full range
    # the actual lowest j_score is .26 so this gets it to the same range where a score of 0 would range down to -7
    step_range(j_score, min = -4.4, max = 3) %>%  # %>% 
    step_rename(from_encoding_late = encoding_trial_num)
  
  return (out)
}

make_alreadyknew_recipe <- function (retrieval_data_prepped) {
  out <- recipe(retrieval_data_prepped) %>% 
    update_role(already_knew, new_role = "outcome") %>% 
    update_role(c(j_score, subj_num), new_role = "predictor") %>% 
    # so I don't have to put already_knew into the preplot new data
    update_role_requirements(role = "NA", bake = FALSE) %>% 
    # to specifically combine "all" into "some"
    # bc step_other collapses by number, not specific levels
    step_mutate(already_knew = fct_recode(already_knew, "some" = "all")) %>% 
    # scaling to range of 10 makes it so unit change is 10% of the full range
    # the actual lowest j_score is .26 so this gets it to the same range where a score of 0 would range down to -7
    step_range(j_score, min = -4.4, max = 3) %>%  # %>% 
    step_rename(from_encoding_late = encoding_trial_num)
  
  return (out)
}

## set up and fit models ----

fit_retrieval_model <- function (in_data, in_formula, novel_facts = TRUE) {
  in_data %<>%
    prep_retrieval_data()
  
  base_recipe <- in_data %>% 
    make_retrieval_recipe(novel_facts = novel_facts)
  
  # If it's one of the supplementary models, change the recipe roles to add the relevant column as a predictor
  if (as_name(f_lhs(in_formula)) != "acc_recall") {
    base_recipe %<>%
      update_role(!!f_lhs(in_formula), new_role = "outcome") %>% 
      # yes, redoing an old step to turn the outcome var back to factor
      # because resp_pic and resp_source come in effect-coded numerically
      step_num2factor(!!f_lhs(in_formula),
                      transform = \(x) ifelse(x > 0, 2, 1),
                      levels = c("incorrect", "correct"),
                      ordered = TRUE)
  }
  
  glmer_spec <- logistic_reg() %>% 
    set_engine("stan_glmer",
               prior = rstanarm::cauchy(0, 2.5),
               prior_intercept = rstanarm::cauchy(0, 2.5))
  
  out <- fit_general_model(in_data, base_recipe, glmer_spec, in_formula)
  
  return (out)
}

fit_retrieval.rt_model <- function (in_data, in_formula, novel_facts = TRUE) {
  in_data %<>%
    prep_retrieval_data(extra_cols = rt_start_recall)
  
  base_recipe <- in_data %>% 
    make_retrieval.rt_recipe(novel_facts = novel_facts) %>%
      update_role(!!f_lhs(in_formula), new_role = "outcome")
  
  glmer_spec <- logistic_reg() %>% 
    set_engine("stan_glmer",
               prior = rstanarm::cauchy(0, 2.5),
               prior_intercept = rstanarm::cauchy(0, 2.5))
  
  out <- fit_general_model(in_data, base_recipe, glmer_spec, in_formula)
  
  return (out)
}

fit_retrieval.category_model <- function (in_data, in_formula, category_group, novel_facts = TRUE) {
  in_data %<>%
    prep_retrieval_data(extra_cols = c(category, group))
  
  base_recipe <- in_data %>% 
    make_retrieval.category_recipe(category_group = category_group,
                                   novel_facts = novel_facts)
  
  glmer_spec <- logistic_reg() %>% 
    set_engine("stan_glmer",
               prior = rstanarm::cauchy(0, 2.5),
               prior_intercept = rstanarm::cauchy(0, 2.5))
  
  out <- fit_general_model(in_data, base_recipe, glmer_spec, in_formula)
  
  return (out)
}

fit_encoding_model <- function (in_data, in_formula, novel_facts = TRUE) {
  in_data %<>%
    prep_retrieval_data()
  
  base_recipe <- in_data %>% 
    make_interest_recipe(novel_facts = novel_facts)
  
  # do a linear reg because  interest ranges from 0-100 (or -50 - 50), good enough
  glmer_spec <- linear_reg() %>% 
    set_engine("stan_glmer",
               prior = rstanarm::normal(0, 2.5, autoscale = TRUE),
               prior_intercept = rstanarm::normal(0, 2.5, autoscale = TRUE),
               iter = 3000)
  
  out <- fit_general_model(in_data, base_recipe, glmer_spec, in_formula)
  
  return (out)
}

fit_alreadyknew_model <- function (in_data, in_formula) {
  in_data %<>%
    prep_retrieval_data()
  
  base_recipe <- in_data %>% 
    make_alreadyknew_recipe()
  
  glmer_spec <- logistic_reg() %>% 
    set_engine("stan_glmer",
               prior = rstanarm::cauchy(0, 2.5),
               prior_intercept = rstanarm::cauchy(0, 2.5))
  
  out <- fit_general_model(in_data, base_recipe, glmer_spec, in_formula)
  
  return (out)
}

fit_general_model <- function (in_data, in_recipe, in_spec, in_formula) {
  # I WANT TO SEE THE CHAINS SAMPLE
  # Theoretically this setting should send the rstanarm output to stdout but...
  # I'm not seeing it...
  control_spec <- control_parsnip(verbosity = 2L) %>% 
    control_workflow()
  
  model_workflow <- workflow() %>% 
    add_recipe(in_recipe) %>% 
    add_model(in_spec,
              formula = in_formula)
  
  out <- fit(model_workflow,
             data = in_data,
             control = control_spec)
  
  return (out)
}

# save(model_fact_alone,
#      model_fact_by_pic,
#      model_fact_by_source,
#      file = here::here("ignore", "data", "models.rda"))

