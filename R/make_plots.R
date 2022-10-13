## setup ----

require(tidyverse)
require(magrittr)

source(here::here("R", "get_cleaned_data.R"))

this_theme <- theme_bw(base_size = 14,
                       base_family = "Aileron") +
  theme(legend.background = element_blank(),
        plot.background = element_blank())

## demographics ----

plot_demos <- q_google_demos %>% 
  mutate(gender = coalesce(gender, "not reported")) %>% 
  left_join(q_google_demos %>%
              count(gender) %>%
              mutate(gender = coalesce(gender, "not reported"),
                     gender_n = glue::glue("{gender} (n = {n})")) %>%
              select(-n),
            by = "gender") %>% 
  ggplot(aes(x = age, fill = gender_n)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 2) +
  scale_y_continuous(breaks = scales::breaks_extended(6)) +
  labs(fill = "gender") +
  this_theme +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))

## trivia expertise ----

plot_expertise_hist <- j_exp %>% 
  ggplot(aes(x = j_score)) +
  geom_histogram(binwidth = .05, fill = "gray80") +
  geom_vline(xintercept = 0.7, linetype = "dotted") +
  annotate("text",
           x = .67,
           y = 20,
           label = "Approx cutoff for Jeopardy callback",
           hjust = 1,
           family = "Aileron Light") +
  labs(x = "Expertise score (fraction correct out of 50 questions)",
       y = "count") +
  this_theme

plot_expertise_gender <- j_exp %>% 
  mutate(subj_num = factor(subj_num)) %>% 
  left_join(q_google_demos, by = "subj_num") %>% 
  mutate(gender = if_else(gender != "male", "not male", gender, missing = "not male")) %>% 
  ggplot(aes(x = gender, y = j_score)) +
  geom_boxplot(fill = "gray80") +
  geom_hline(yintercept = 0.7, linetype = "dotted") +
  labs(y = "Expertise score") +
  this_theme

## encoding ----

plot_encoding_interest <- encoding %>% 
  group_by(subj_num, j_score) %>% 
  summarize(resp_median = median(resp),
            q25 = quantile(resp, .25),
            q75 = quantile(resp, .75)) %>% 
  ggplot(aes(x = j_score, y = resp_median)) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, alpha = 0.3) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Expertise score",
       y = "P's median interest (across facts)") +
  this_theme

plot_encoding_novel <- encoding %>%
  group_by(subj_num, j_score) %>%
  summarize(n_novel = sum(already_knew == "none", na.rm = T)) %>%
  ggplot(aes(x = j_score, y = n_novel)) +
  geom_hline(yintercept = 80, linetype = "dotted") +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Expertise score",
       y = "# facts rated as novel (out of 80)") +
  this_theme

## fact retrieval ----

plot_fact_by_expertise_alreadyknew <- retrieval %>% 
  filter(!is.na(already_knew)) %>% 
  group_by(subj_num, j_score, already_knew) %>% 
  summarize(acc_recall = mean(acc_recall), n_trials = n()) %>% 
  ggplot(aes(x = j_score, y = acc_recall, color = fct_rev(already_knew), fill = fct_rev(already_knew))) +
  geom_point(aes(size = n_trials), alpha = 0.2) +
  geom_smooth(method = "lm") +
  labs(x = "Expertise",
       y = "% correct recall",
       color = "Prior knowledge",
       fill = "Prior knowledge",
       size = "# trials") +
  this_theme

plot_fact_by_interest <- retrieval %>% 
  filter(already_knew == "none") %>% 
  mutate(acc_recall = as.numeric(acc_recall > 0)) %>% 
  ggplot(aes(x = interest, y = acc_recall, color = fct_rev(cut_number(j_score, 3)), fill = fct_rev(cut_number(j_score, 3)))) +
  geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  scale_fill_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "Interest at encoding", y = "% correct recall for novel facts",
       color = "Expertise\ntertile", fill = "Expertise\ntertile") +
  this_theme

## photo retrieval ----

plot_photo_by_expertise <- retrieval %>%
  filter(already_knew == "none") %>% 
  group_by(subj_num, j_score) %>% 
  summarize(resp_median = median(resp_pic),
            q25 = quantile(resp_pic, .25),
            q75 = quantile(resp_pic, .75),
            n_trials = n()) %>% 
  ggplot(aes(x = j_score, y = resp_median)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, alpha = 0.3) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Expertise score", y = " Median accuracy WITH confidence") +
  this_theme
  
## source retrieval ----

plot_source_by_expertise <- retrieval %>%
  filter(already_knew == "none") %>% 
  group_by(subj_num, j_score) %>% 
  summarize(resp_median = median(resp_source),
            q25 = quantile(resp_source, .25),
            q75 = quantile(resp_source, .75),
            n_trials = n()) %>% 
  ggplot(aes(x = j_score, y = resp_median)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, alpha = 0.3) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Expertise score", y = " Median accuracy WITH confidence") +
  this_theme

## across retrieval tasks ----

plot_fact_by_source <- retrieval %>% 
  filter(already_knew == "none") %>% 
  mutate(across(c(resp_pic, resp_source), ~if_else(. > 0, 1L, 0L)),
         acc_recall = if_else(acc_recall == 0.5, 1, acc_recall)) %>%
  select(subj_num, j_score, resp_pic, resp_source, acc_recall) %>% 
  pivot_longer(cols = starts_with("resp"),
               names_to = "source_type",
               values_to = "source_acc",
               names_prefix = "resp_") %>% 
  group_by(subj_num, j_score, source_type, source_acc) %>%
  summarize(acc_recall = mean(acc_recall)) %>% 
  ungroup() %>% 
  ggplot(aes(x = source_acc, y = acc_recall, color = fct_rev(cut_number(j_score, 3)), fill = fct_rev(cut_number(j_score, 3)))) + 
  geom_line(aes(group = interaction(subj_num, source_type)), alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.2) + 
  scale_x_continuous(breaks = 0:1) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  scale_fill_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  facet_grid(~ source_type) +
  labs(x = "Source recognition accuracy (binarized, WITHOUT confidence)",
       y = "Novel fact recall accuracy",
       color = "Expertise tertile",
       fill = "Expertise tertile") +
  this_theme

retrieval %>% 
  filter(already_knew == "none") %>% 
  mutate(across(c(resp_pic, resp_source), ~if_else(. > 0, 1L, 0L)),
         resp_either = resp_pic + resp_source,
         acc_recall = as.integer(acc_recall > 0)) %>%
  filter(resp_either > 0) %>% 
  group_by(subj_num, j_score, resp_either) %>%
  summarize(acc_recall = mean(acc_recall)) %>% 
  ggplot(aes(x = resp_either, y = acc_recall, color = fct_rev(cut_number(j_score, 3)), fill = fct_rev(cut_number(j_score, 3)))) + 
  geom_line(aes(group = subj_num), alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.2) + 
  scale_x_continuous(breaks = 1:2) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  scale_fill_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "Joint associate recognition accuracy",
       y = "Fact recall accuracy (generous)",
       color = "Expertise tertile",
       fill = "Expertise tertile") +
  this_theme

## print plots for keynote slides ----

ggsave(here::here("ignore", "figs", "plot_slides_demos.png"),
       plot = plot_demos,
       device = "png",
       width = 6,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_expertise_hist.png"),
       plot = plot_expertise_hist,
       device = "png",
       width = 6,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_expertise_gender.png"),
       plot = plot_expertise_gender,
       device = "png",
       width = 2,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_encoding_interest.png"),
       plot = plot_encoding_interest,
       device = "png",
       width = 4,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_encoding_novel.png"),
       plot = plot_encoding_novel,
       device = "png",
       width = 4,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_fact_by_expertise_alreadyknew.png"),
       plot = plot_fact_by_expertise_alreadyknew,
       device = "png",
       width = 7,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_fact_by_interest.png"),
       plot = plot_fact_by_interest,
       device = "png",
       width = 7,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_photo_by_expertise.png"),
       plot = plot_photo_by_expertise,
       device = "png",
       width = 4,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_source_by_expertise.png"),
       plot = plot_source_by_expertise,
       device = "png",
       width = 4,
       height = 4,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_slides_fact_by_source.png"),
       plot = plot_fact_by_source,
       device = "png",
       width = 8,
       height = 4,
       units = "in")
