## setup ----
# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidymodels",
               "multilevelmod",
               "rstanarm",
               "tidyverse",
               "magrittr",
               "rlang",
               "cowplot"), # packages that your targets need to run
  format = "rds", # default storage format
  # Set other options as needed.
  seed = 4L
)

# tar_make_clustermq() configuration (okay to leave alone):
options(# clustermq.scheduler = "multicore",
        # For the models, sigh
        mc.cores = 4)

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source(c("R/preproc/",
             "R/analyze/"))

paths_raw_data <- list.files(here::here("ignore", "data", "raw", "real"), recursive = T, full.names = T)

# For the figure fonts
font_ms <- "Helvetica Neue"
fontsize_ms <- 10

# source("other_functions.R") # Source other scripts as needed. # nolint

## targets for demographics and subject IDs ----
target_demos <- list(
  tar_target(name = demos_ids_raw,
             command = filter_paths_grepl(paths_raw_data, "questionnaire-4vcn"),
             format = "file"),
  tar_target(name = demos_ids,
             command = demos_ids_raw %>% 
               read_gorilla_data() %>% 
               clean_demos_ids()
  ),
  tar_target(name = q_google_demos_raw,
             command = get_googledrive_csv(drive_id = "1yW13tK4dVr8MXtJTaZxV_3M8IaD5TqhbzD8QKPuEndM",
                                           destination_path = here::here("ignore", "data", "raw", "real", "demographics.csv")),
             format = "file"),
  tar_target(name = q_google_demos,
             command = q_google_demos_raw %>% 
               read_csv() %>% 
               clean_q_google_demos(id_data = demos_ids)
  )
)

## targets for expertise pre-test and metamemory judgments ----
target_expertise <- list(
  tar_target(name = expertise_questions_raw,
             command = get_googledrive_csv(drive_id = "1e7R4CSDjSA5trC820aiwUbe7szjn7DTfMiZPI4my8Rk",
                                           destination_path = here::here("stim_stuff", "jeopardy_spreadsheet.csv")),
             format = "file"),
  tar_target(name = expertise_questions,
             command = expertise_questions_raw %>% 
               read_csv() %>% 
               clean_expertise_questions()
  ),
  tar_target(name = expertise_raw,
             command = filter_paths_grepl(paths_raw_data, "task-v9u2"),
             format = "file"),
  tar_target(name = expertise_unscored,
             command = expertise_raw %>% 
               read_gorilla_data() %>% 
               clean_expertise()
  ),
  # There IS code to generate this through responses at console but I don't want to re-run it every time...
  tar_target(name = expertise_handscoring,
             command = here::here("ignore", "data", "task_jeopardy_recall_handscoring.csv"),
             format = "file"),
  tar_target(name = expertise_scored,
             command = {
               # Do this extra shit to avoid a circular target dependency
               out <- expertise_unscored %>% 
                 score_expertise_firstpass(question_data = expertise_questions) %>% 
                 score_expertise_byhand(handscoring_data = read_csv(expertise_handscoring)) %>% 
                 score_expertise_lastpass()
               
               summarized <- out %>% 
                 group_by(subj_num) %>% 
                 summarize(j_score = mean(acc_recall))
                           
               out %>% 
                 bind_expertise_scores(expertise_scores = summarized)
             }
  ),
  tar_target(name = expertise_summarized,
             command = expertise_scored %>% 
               # basically, because of the semi-circularity,
               # subj_num already comes in as factor
               # but needs to get re-numeric'd for binding to everything else
               mutate(subj_num = as.numeric(as.character(subj_num))) %>% 
               distinct(subj_num, j_score)
  ),
  tar_target(name = metamemory_raw,
             command = filter_paths_grepl(paths_raw_data, "task-wlkb"),
             format = "file"),
  tar_target(name = metamemory_states,
             command = metamemory_raw %>% 
               read_gorilla_data() %>% 
               clean_metamemory_states() %>% 
               bind_expertise_scores(expertise_scores = expertise_summarized)
  ),
  tar_target(name = metamemory_descriptions,
             command = metamemory_raw %>% 
               read_gorilla_data() %>% 
               clean_metamemory_descriptions() %>% 
               bind_expertise_scores(expertise_scores = expertise_summarized)
  )
)

## targets for data from the main museum task ----
target_main_task <- list(
  tar_target(name = pretest_raw,
             command = filter_paths_grepl(paths_raw_data, "task-4elo"),
             format = "file"),
  tar_target(name = pretest,
             command = pretest_raw %>% 
               read_gorilla_data() %>% 
               clean_pretest()
  ),
  tar_target(name = pretest_reshaped,
             command = reshape_pretest(pretest)
  ),
  tar_target(name = encoding_raw,
             command = filter_paths_grepl(paths_raw_data, "task-ujps|task-wckg"),
             format = "file"),
  tar_target(name = encoding,
             command = encoding_raw %>% 
               read_gorilla_data() %>% 
               clean_encoding(pretest_data = pretest_reshaped) %>% 
               bind_expertise_scores(expertise_scores = expertise_summarized)
  ),
  tar_target(name = retrieval_facts_raw,
             command = filter_paths_grepl(paths_raw_data, "task-txhz"),
             format = "file"),
  tar_target(name = retrieval_facts_unscored,
             command = retrieval_facts_raw %>% 
               read_gorilla_data() %>% 
               clean_retrieval_facts(pretest_data = pretest_reshaped)
  ),
  tar_target(name = retrieval_facts_handscoring,
             command = here::here("ignore", "data", "task_retrieval_facts_handscoring.csv"),
             format = "file"),
  tar_target(name = retrieval_facts_scored,
             command = retrieval_facts_unscored %>% 
               score_retrieval_facts_firstpass() %>% 
               score_retrieval_facts_byhand(handscoring_data = read_csv(retrieval_facts_handscoring)) %>% 
               score_retrieval_facts_lastpass() %>% 
               bind_expertise_scores(expertise_scores = expertise_summarized)
  ),
  tar_target(name = retrieval_pics_raw,
             command = filter_paths_grepl(paths_raw_data, "task-3n9k|task-cuj6"),
             format = "file"),
  tar_target(name = retrieval_pics,
             command = retrieval_pics_raw %>% 
               read_gorilla_data() %>% 
               clean_retrieval_pics(pretest_data = pretest_reshaped) %>% 
               bind_expertise_scores(expertise_scores = expertise_summarized)
  ),
  tar_target(name = retrieval_source_raw,
             command = filter_paths_grepl(paths_raw_data, "task-1152|task-iqq7|task-dyao"),
             format = "file"),
  tar_target(name = retrieval_source,
             command = retrieval_source_raw %>% 
               read_gorilla_data() %>% 
               clean_retrieval_source(pretest_data = pretest_reshaped) %>% 
               bind_expertise_scores(expertise_scores = expertise_summarized)
  ),
  tar_target(name = retrieval,
             command = bind_retrieval(retrieval_facts_scored,
                                      retrieval_pics,
                                      retrieval_source,
                                      encoding)
  )
)

## targets for MODELS ----
target_models <- list(
  tar_target(name = model_fact_alone,
             command = fit_retrieval_model(in_data = retrieval,
                                           in_formula = acc_recall ~ j_score + from_encoding_late + interest + (1 | subj_num))
             ),
  tar_target(name = model_fact_by_both,
             command = fit_retrieval_model(in_data = retrieval,
                                   in_formula = acc_recall ~ resp_pic * resp_source * j_score + from_encoding_late + interest + (1 | subj_num))
  ),
  tar_target(name = model_pic,
             command = fit_retrieval_model(in_data = retrieval,
                                           in_formula = resp_pic ~ j_score + from_encoding_late + interest + (1 | subj_num))
  ),
  tar_target(name = model_source,
             command = fit_retrieval_model(in_data = retrieval,
                                           in_formula = resp_source ~ j_score + from_encoding_late + interest + (1 | subj_num))
  ),
  tar_target(name = model_interest,
             command = fit_encoding_model(in_data = retrieval,
                                          in_formula = interest ~ j_score + from_encoding_late + (1 | subj_num))
  ),
  tar_target(name = model_alreadyknew,
             command = fit_alreadyknew_model(in_data = retrieval,
                                          in_formula = already_knew ~ j_score + (1 | subj_num))
  )
)

## targets for post-model PRE-PLOT objects ----
target_preplots <- list(
  tar_target(name = preplot_params_fact_alone,
             command = posterior_preplot_params(model_fact_alone)
             ),
  tar_target(name = preplot_params_fact_by_both,
             command = posterior_preplot_params(model_fact_by_both)
  ),
  tar_target(name = preplot_params_pic,
             command = posterior_preplot_params(model_pic)
  ),
  tar_target(name = preplot_params_source,
             command = posterior_preplot_params(model_source)
  ),
  tar_target(name = preplot_params_interest,
             command = posterior_preplot_params(model_interest)
  ),
  tar_target(name = preplot_params_alreadyknew,
             command = posterior_preplot_params(model_alreadyknew)
  ),
  # Importante: now that preprocessing is done with a recipe,
  # create the newdata ON THE SCALE OF THE ORIGINAL/REAL DATA
  # and all the recoding and such will be handled by the recipe
  tar_target(name = newdata_preplot_by_subj,
             command = expertise_summarized %>%
               mutate(subj_num = fct_reorder(as.character(subj_num), j_score)) %>% 
               expand(nesting(subj_num, j_score), 
                      encoding_trial_num = 40.5, 
                      nesting(resp_pic = c(-50, 50),
                              resp_source = c(-50, 50)), 
                      interest = seq(0, 100, 10),
                      already_knew = "none")
  ),
  tar_target(name = newdata_preplot_fixef,
             command = crossing(j_score = c(0.5, 0.8),
                                encoding_trial_num = 40.5,
                                interest = 50,
                                resp_pic = c(-50, 50),
                                resp_source = c(-50, 50),
                                # we have to feed in a valid subject ID
                                # but trust that we are generating the fixef
                                subj_num = "4295606",
                                already_knew = "none")
             ),
  tar_target(name = preplot_fixef_fact_by_both,
             command = posterior_preplot_fixef(object = model_fact_by_both,
                                               newdata = newdata_preplot_fixef,
                                               pred_col = "acc_pred")
  )
)

## targets for PLOTS (in the manuscript) ----
target_ms_plots <- list(
  tar_target(name = plot_demos,
             command = make_plot_demos(q_google_demos)
  ),
  tar_target(name = plot_expertise_hist,
             command = make_plot_expertise_hist(expertise_summarized)
  ),
  tar_target(name = plot_fact_by_expertise,
             command = make_plot_retrieval_by_expertise(retrieval,
                                                        resp_col = acc_recall,
                                                        y_label = "P(recall) for novel facts")
  ),
  tar_target(name = plot_pic_by_expertise,
             command = retrieval %>% 
               mutate(resp_pic = as.numeric(resp_pic > 0)) %>% 
               make_plot_retrieval_by_expertise(resp_col = resp_pic,
                                                y_label = "Forced-choice photo memory")
  ),
  tar_target(name = plot_source_by_expertise,
             command = retrieval %>% 
               mutate(resp_source = as.numeric(resp_source > 0)) %>% 
               make_plot_retrieval_by_expertise(resp_col = resp_source,
                                                y_label = "Forced-choice museum memory")
  ),
  tar_target(name = plot_coefs_fact_by_both,
             command = preplot_params_fact_by_both %>% 
               make_plot_coefs_fact_by_both()
  ),
  tar_target(name = plot_fixef_fact_by_both,
             command = preplot_fixef_fact_by_both %>% 
               make_plot_fixef_fact_by_both(retrieval_data = retrieval)
  )
)

target_ms_figs <- list(
  tar_target(name = fig_3,
             command = {
               path <- here::here("ignore", "figs", "ms_fig3.png")
               figure <- plot_grid(plot_demos +
                                     theme_ms(base_size = fontsize_ms,
                                              base_family = font_ms,
                                              legend.position = c(1, 1),
                                              legend.justification = c(1, 1)),
                                   plot_expertise_hist +
                                     theme_ms(base_size = fontsize_ms,
                                              base_family = font_ms),
                                   labels = letters[1:2],
                                   ncol = 1)
               save_plot(path,
                         figure,
                         ncol = 1,
                         nrow = 2)
               
               path
             },
             format = "file"),
  tar_target(name = fig_4,
             command = {
               path <- here::here("ignore", "figs", "ms_fig4.png")
               figure <- plot_fact_by_expertise +
                 theme_ms(base_size = fontsize_ms,
                          base_family = font_ms)
               save_plot(path,
                         figure)
               
               path
             },
             format = "file"),
  tar_target(name = fig_5,
             command = {
               path <- here::here("ignore", "figs", "ms_fig5.png")
               figure <- plot_grid(plot_coefs_fact_by_both +
                                     theme_ms(base_size = fontsize_ms,
                                              base_family = font_ms),
                                   plot_fixef_fact_by_both + 
                                     scale_color_manual(values = c("correct" = "#b580b6", "incorrect" = "#2c2aa6")) +
                                     theme_ms(base_size = fontsize_ms,
                                              base_family = font_ms,
                                              legend.position = c(0, 1),
                                              legend.justification = c(0, 1)),
                                   nrow = 1,
                                   labels = letters[1:2])
               save_plot(path,
                         figure,
                         ncol = 3,
                         nrow = 1,
                         base_asp = 1.618/2)
               
               path
             },
             format = "file")
)

## Other shit we may as well track ----
target_misc_qs <- list(
  tar_target(name = q_posttask_raw,
             command = filter_paths_grepl(paths_raw_data, "questionnaire-qdx5"),
             format = "file"),
  tar_target(name = q_posttask,
             command = q_posttask_raw %>% 
               read_gorilla_data() %>% 
               clean_q_posttask()
  ),
  tar_target(name = q_trivia_demos_raw,
             command = filter_paths_grepl(paths_raw_data, "questionnaire-d6y6"),
             format = "file"),
  tar_target(name = q_trivia_demos,
             command = q_trivia_demos_raw %>% 
               read_gorilla_data() %>% 
               clean_q_trivia_demos() %>% 
               bind_expertise_scores(expertise_scores = expertise_summarized)
  )
)

target_rmds <- list(
  tar_render(name = figs_and_captions,
             path = here::here("writing", "ms.Rmd"))
)

## Instantiate targets ----
c(target_demos,
  target_expertise,
  target_main_task,
  target_misc_qs,
  target_models,
  target_preplots,
  target_ms_plots,
  target_ms_figs)

