## setup ----

require(tidymodels)
require(magrittr)
require(rlang)
require(tidyverse)

options(mc.cores = parallel::detectCores())

## load data ----

source(here::here("R", "get_cleaned_data.R"))

## prep data further ----

base_recipe <- recipe(retrieval) %>% 
  step_cut(c(resp_pic, resp_source), breaks = 0) %>% 
  step_cut(encoding_trial_num, breaks = 41) %>% 
  step_range(interest, min = -5, max = 5) %>% 
  step_range(j_score, min = -7, max = 3)
  # step 1: binarize resp_pic and resp_source slider scores
  # step 2: dummy variable for second block
  # step 3: center and rescale interest to be width 10 instead of 100
  # step 4: same for j-score but center at 0.7

retrieval_modelsafe <- retrieval %>% 
  filter(already_knew == "none") %>% 
  mutate(acc_recall = as.numeric(acc_recall > 0),
         # YES binary! Leaving the slider at the center counted as a miss.
         # People were endpointing a lot. I think this is more stable
         # Effect-coded now
         across(c(resp_pic, resp_source), ~if_else(. > 0, 0.5, -0.5)),
         # dummy variable for whether stimulus came from the second (later) encoding block
         from_encoding_late = if_else(encoding_trial_num > 40, 0.5, -0.5),
         # Scaled to units of .1
         interest = (interest / 10) - 5,
         # Scaled to units of .1
         j_score = (j_score - 0.7) * 10)

## fit models ----

model_fact_by_pic <- rstanarm::stan_glmer(acc_recall ~ resp_pic * j_score + from_encoding_late + interest + (1 | subj_num),
                     family = binomial(link = "logit"),
                     data = retrieval_modelsafe,
                     prior = rstanarm::cauchy(0, 2.5),
                     prior_intercept = rstanarm::cauchy(0, 2.5))

model_fact_by_source <- rstanarm::stan_glmer(acc_recall ~ resp_source * j_score + from_encoding_late + interest + (1 | subj_num),
                                          family = binomial(link = "logit"),
                                          data = retrieval_modelsafe,
                                          prior = rstanarm::cauchy(0, 2.5),
                                          prior_intercept = rstanarm::cauchy(0, 2.5))

## save that shit out immediately ----

save(model_fact_by_pic,
     model_fact_by_source,
     file = here::here("ignore", "data", "models.rda"))

