# A function factory for getting integer y-axis values.
# From https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

make_plot_demos <- function (demo_data) {
  demo_data %>% 
    mutate(gender = coalesce(gender, "not reported")) %>% 
    left_join(demo_data %>%
                count(gender) %>%
                mutate(gender = coalesce(gender, "not reported"),
                       gender_n = glue::glue("{gender} (n = {n})")) %>%
                select(-n),
              by = "gender") %>% 
    ggplot(aes(x = age, fill = gender_n)) +
    geom_histogram(position = "stack", alpha = 0.5, binwidth = 2) +
    scale_y_continuous(breaks = integer_breaks()) +
    labs(y = "# of participants",
         fill = "gender")
}

make_plot_expertise_hist <- function (expertise_score_data, annotation_font = "Helvetica Neue") {
  median_score <- median(expertise_score_data$j_score)
  
  expertise_score_data %>% 
    ggplot(aes(x = j_score)) +
    geom_histogram(binwidth = .05, fill = "gray80") +
    geom_vline(xintercept = median_score, linetype = "dotted") +
    annotate("text",
             x = median_score - 0.02,
             y = 20,
             label = "median score",
             hjust = 1,
             family = annotation_font) +
    labs(x = "Expertise score (proportion correct out of 50 questions)",
         y = "# of participants") 
}

make_plot_retrieval_by_expertise <- function (retrieval_data, resp_col, y_label) {
  retrieval_data %>%
    filter(already_knew == "none") %>% 
    # pic and source memory should have been binarized before entering the function!
    group_by(subj_num, j_score) %>% 
    summarize(resp_mean = mean({{resp_col}})) %>% 
    ggplot(aes(x = j_score, y = resp_mean)) +
    geom_hline(yintercept = 0.5, linetype = "dotted") +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "black") +
    labs(x = "Trivia expertise", y = y_label)
}

summarize_params <- function (preplot_params) {
  out <- preplot_params %>% 
    group_by(term) %>% 
    summarize(q025 = quantile(estimate, .025),
              q100 = quantile(estimate, .1),
              q500 = median(estimate),
              q900 = quantile(estimate, .9),
              q975 = quantile(estimate, .975))
  
  return (out)
}

make_plot_coefs <- function (preplot_params) {
  preplot_params %>% 
    mutate(clears = if_else(sign(q025) == sign(q975), "yes", "no")) %>% 
    ggplot(aes(x = q500, y = fct_rev(term), color = clears)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_errorbarh(aes(xmin = q025, xmax = q975), height = 0, linewidth = 1) +
    geom_errorbarh(aes(xmin = q100, xmax = q900), height = 0, linewidth = 2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("no" = "black", "yes" = "springgreen3")) +
    guides(color = "none") +
    labs(x = "Coefficient estimate",
         y = NULL)
}

make_plot_coefs_fact_by_pic <- function (preplot_params) {
  preplot_params %>% 
    summarize_params() %>% 
    mutate(term = fct_relevel(term,
                              "intercept",
                              "interest",
                              "j_score",
                              "from_encoding_late",
                              "resp_pic",
                              "resp_pic:j_score"),
           term = fct_recode(term,
                             "Intercept" = "intercept",
                             "Interest at encoding" = "interest",
                             "Trivia expertise" = "j_score",
                             "First vs. second museum" = "from_encoding_late",
                             "Photo memory" = "resp_pic",
                             "Expertise x photo memory" = "resp_pic:j_score")) %>% 
    make_plot_coefs()
}

make_plot_coefs_fact_by_source <- function (preplot_params) {
  preplot_params %>% 
    summarize_params() %>%  
    mutate(term = fct_relevel(term,
                              "intercept",
                              "interest",
                              "j_score",
                              "from_encoding_late",
                              "resp_source",
                              "resp_source:j_score"),
           term = fct_recode(term,
                             "Intercept" = "intercept",
                             "Interest at encoding" = "interest",
                             "Trivia expertise" = "j_score",
                             "First vs. second museum" = "from_encoding_late",
                             "Museum memory" = "resp_source",
                             "Expertise x museum memory" = "resp_source:j_score")) %>% 
    make_plot_coefs()
}

make_plot_coefs_fact_by_both <- function (preplot_params) {
  preplot_params %>% 
    summarize_params() %>%  
    mutate(term = fct_relevel(term,
                              "intercept",
                              "interest",
                              "j_score",
                              "from_encoding_late",
                              "resp_pic",
                              "resp_source",
                              "resp_pic:j_score",
                              "resp_source:j_score",
                              "resp_pic:resp_source",
                              "resp_pic:resp_source:j_score"),
           term = fct_recode(term,
                             "Intercept" = "intercept",
                             "Interest at encoding" = "interest",
                             "Trivia expertise" = "j_score",
                             "First vs. second museum" = "from_encoding_late",
                             "Photo memory" = "resp_pic",
                             "Museum memory" = "resp_source",
                             "Expertise x photo memory" = "resp_pic:j_score",
                             "Expertise x museum memory" = "resp_source:j_score",
                             "Photo memory x museum memory" = "resp_pic:resp_source",
                             "Photo x museum x expertise" = "resp_pic:resp_source:j_score")) %>% 
    make_plot_coefs()
}

summarize_retrieval_for_fixef <- function (retrieval_data, resp_col) {
  out <- retrieval_data %>% 
    filter(already_knew == "none") %>% 
    mutate(acc_recall = as.numeric(acc_recall > 0),
           resp = fct_rev(if_else({{resp_col}} > 0, "correct", "incorrect")),
           j_score = if_else(j_score > 0.7, "upper half", "lower half")) %>% 
    group_by(subj_num, j_score, resp) %>% 
    summarize(acc_pred = mean(acc_recall))
  
  return (out)
}

make_plot_fixef_fact_by_other <- function (preplot_fixef, retrieval_data, resp_col, x_label) {
  retrieval_summarized <- inject(summarize_retrieval_for_fixef(retrieval_data, resp_col = {{resp_col}}))
  
  preplot_fixef %>% 
    mutate(j_score = recode_factor(as.character(j_score),
                                   `1` = "upper half",
                                   `-2` = "lower half"),
           resp = recode_factor(as.character({{resp_col}}),
                                    `-0.5` = "incorrect",
                                    `0.5` = "correct")) %>% 
    ggplot(aes(x = fct_rev(resp), y = acc_pred, color = fct_rev(j_score))) +
    geom_line(aes(group = interaction(subj_num, j_score)),
              data = retrieval_summarized,
              alpha = 0.2) +
    geom_jitter(data = retrieval_summarized,
                alpha = 0.5,
                width = 0.05) +
    geom_line(aes(group = interaction(j_score, iteration)), alpha = 0.04, linewidth = 0.5) +
    guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 3))) +
    labs(x = x_label,
         y = "P(recall) for novel facts",
         color = "Expertise (median split)")
}

make_plot_fixef_fact_by_both <- function (preplot_fixef, retrieval_data) {
  retrieval_summarized <- retrieval_data %>% 
    filter(already_knew == "none") %>% 
    mutate(acc_recall = as.numeric(acc_recall > 0),
           across(c(resp_pic, resp_source), \(x) if_else(x > 0, "correct", "incorrect")),
           j_score = if_else(j_score > 0.7, "upper half of expertise", "lower half of expertise")) %>% 
    group_by(subj_num, j_score, resp_pic, resp_source) %>% 
    summarize(acc_pred = mean(acc_recall))
  
  preplot_fixef %>% 
    mutate(j_score = recode_factor(as.character(j_score),
                                   `1` = "upper half of expertise",
                                   `-2` = "lower half of expertise"),
           across(c(resp_pic, resp_source),
                  \(x) recode_factor(as.character(x),
                                     `-0.5` = "incorrect",
                                     `0.5` = "correct"))) %>% 
    ggplot(aes(x = fct_rev(resp_pic), y = acc_pred, color = fct_rev(resp_source))) +
    geom_line(aes(group = interaction(subj_num, resp_source)),
              data = retrieval_summarized,
              alpha = 0.2) +
    geom_jitter(data = retrieval_summarized,
                alpha = 0.5,
                width = 0.05) +
    geom_line(aes(group = interaction(resp_source, iteration)), alpha = 0.04, linewidth = 0.5) +
    facet_grid(~ fct_rev(j_score)) +
    guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 3))) +
    labs(x = "Forced-choice photo memory",
         y = "P(recall) for novel facts",
         color = "Forced-choice museum memory")
}
