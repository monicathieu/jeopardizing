## setup ----

require(tidymodels)
require(multilevelmod)
require(magrittr)
require(rlang)
require(tidyverse)

options(mc.cores = 4)

## load data ----

source(here::here("R", "get_cleaned_data.R"))

## prep data further ----

retrieval_modelsafe <- retrieval %>%
  # Just drop those extra columns now
  # The validation is super annoying when there's a bunch of leftover cols
  select(subj_num, 
         acc_recall, 
         resp_pic, resp_source, 
         j_score, 
         encoding_trial_num, 
         interest, 
         already_knew)

base_recipe <- recipe(retrieval_modelsafe) %>% 
  step_filter(already_knew == "none") %>% 
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
  step_num2factor(encoding_trial_num,
                  transform = \(x) ifelse(x > 40, 2, 1),
                  levels = c("early", "late"),
                  ordered = TRUE) %>% 
  step_ordinalscore(c(resp_pic, resp_source, encoding_trial_num)) %>%
  # to get it to -0.5 and +0.5
  # have to use step_range because ordinalscore requires int output
  step_range(c(resp_pic, resp_source, encoding_trial_num), min = -0.5, max = 0.5) %>% 
  # scaling these to range of 10 makes it so unit change is 10% of the full range
  step_range(interest, min = -5, max = 5) %>% 
  # the actual lowest j_score is .26 so this gets it to the same range where a score of 0 would range down to -7
  step_range(j_score, min = -4.4, max = 3) %>%  # %>% 
  step_rename(from_encoding_late = encoding_trial_num)

## set up models ----

glmer_spec <- logistic_reg() %>% 
  set_engine("stan_glmer",
             prior = rstanarm::cauchy(0, 2.5),
             prior_intercept = rstanarm::cauchy(0, 2.5))

# I WANT TO SEE THE CHAINS SAMPLE
control_spec <- control_parsnip(verbosity = 2L) %>% 
  control_workflow()

workflow_fact_alone <- workflow() %>% 
  add_recipe(base_recipe) %>% 
  add_model(glmer_spec,
            formula = acc_recall ~ j_score + from_encoding_late + interest + (1 | subj_num))

workflow_fact_by_pic <- workflow() %>% 
  add_recipe(base_recipe) %>% 
  add_model(glmer_spec,
            formula = acc_recall ~ resp_pic * j_score + from_encoding_late + interest + (1 | subj_num))

workflow_fact_by_source <- workflow() %>% 
  add_recipe(base_recipe) %>% 
  add_model(glmer_spec,
            formula = acc_recall ~ resp_source * j_score + from_encoding_late + interest + (1 | subj_num))

## fit models ----

model_fact_alone <- fit(workflow_fact_alone,
                        data = retrieval_modelsafe,
                        control = control_spec)

model_fact_by_pic <- fit(workflow_fact_by_pic,
                         data = retrieval_modelsafe,
                         control = control_spec)

model_fact_by_source <- fit(workflow_fact_by_source,
                            data = retrieval_modelsafe,
                            control = control_spec)

## save that shit out immediately ----

save(model_fact_alone,
     model_fact_by_pic,
     model_fact_by_source,
     file = here::here("ignore", "data", "models.rda"))

