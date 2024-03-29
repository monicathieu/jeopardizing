## setup ----

require(tidyverse)
require(magrittr)

source(here::here("R", "get_cleaned_data.R"))
# For the model outputs
load(here::here("ignore", "data", "preplots.rda"))



# Aileron family for slides
# Something else...? for manuscript?
theme_slides <- theme_bw(base_size = 14,
                       base_family = "Aileron") +
  theme(legend.background = element_blank(),
        plot.background = element_blank())

theme_slides_ppt <- theme_bw(base_size = 22,
                             base_family = "Helvetica Neue") +
  theme(legend.background = element_blank(),
        plot.background = element_blank())

font_poster <- "ITC Korinna Regular"
theme_poster <- theme_bw(base_size = 26,
                         base_family = font_poster) +
  theme(legend.background = element_blank(),
        plot.background = element_blank())

this_font <- font_ms
this_theme <- theme_ms
## demographics ----



## trivia expertise ----

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
  
## source retrieval ----



## across retrieval tasks ----

retrieval %>% 
  filter(already_knew == "none") %>% 
  mutate(acc_recall = as.numeric(acc_recall > 0),
         resp_pic = as.numeric(resp_pic > 0),
         j_score = if_else(j_score > 0.7, "upper half", "lower half"),
         from_encoding_late = if_else(encoding_trial_num > 40, 1L, 0L)) %>% 
  group_by(subj_num, j_score, resp_pic) %>% 
  summarize(acc_recall = mean(acc_recall))

plot_fixef_fact_by_pic <- preplot_fixef_fact_by_pic %>% 
  mutate(j_score = recode_factor(as.character(j_score),
                          `1` = "upper half",
                          `-2` = "lower half"),
         resp_pic = recode_factor(as.character(resp_pic),
                           `-0.5` = "incorrect",
                           `0.5` = "correct")) %>% 
  ggplot(aes(x = factor(resp_pic), y = acc_pred, color = fct_rev(j_score))) +
  geom_line(aes(group = interaction(subj_num, j_score)),
            data = retrieval %>% 
              filter(already_knew == "none") %>% 
              mutate(acc_recall = as.numeric(acc_recall > 0),
                     resp_pic = fct_rev(if_else(resp_pic > 0, "correct", "incorrect")),
                     j_score = if_else(j_score > 0.7, "upper half", "lower half")) %>% 
              group_by(subj_num, j_score, resp_pic) %>% 
              summarize(acc_pred = mean(acc_recall)),
            alpha = 0.2) +
  geom_jitter(data = retrieval %>% 
                filter(already_knew == "none") %>% 
                mutate(acc_recall = as.numeric(acc_recall > 0),
                       resp_pic = fct_rev(if_else(resp_pic > 0, "correct", "incorrect")),
                       j_score = if_else(j_score > 0.7, "upper half", "lower half")) %>% 
                group_by(subj_num, j_score, resp_pic) %>% 
                summarize(acc_pred = mean(acc_recall)),
              alpha = 0.5,
              width = 0.05) +
  geom_line(aes(group = interaction(j_score, iteration)), alpha = 0.04, size = 0.5) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(x = "Forced-choice photo memory",
       y = "P(recall) for novel facts",
       color = "Expertise (median split)") +
  theme(legend.position = 0:1,
        legend.justification = 0:1)

plot_fixef_fact_by_source <- preplot_fixef_fact_by_source %>% 
  mutate(j_score = recode_factor(as.character(j_score),
                                 `1` = "upper half",
                                 `-2` = "lower half"),
         resp_source = recode_factor(as.character(resp_source),
                                  `-0.5` = "incorrect",
                                  `0.5` = "correct")) %>% 
  ggplot(aes(x = factor(resp_source), y = acc_pred, color = fct_rev(j_score))) +
  geom_line(aes(group = interaction(subj_num, j_score)),
            data = retrieval %>% 
              filter(already_knew == "none") %>% 
              mutate(acc_recall = as.numeric(acc_recall > 0),
                     resp_source = fct_rev(if_else(resp_source > 0, "correct", "incorrect")),
                     j_score = if_else(j_score > 0.7, "upper half", "lower half")) %>% 
              group_by(subj_num, j_score, resp_source) %>% 
              summarize(acc_pred = mean(acc_recall)),
            alpha = 0.2) +
  geom_jitter(data = retrieval %>% 
                filter(already_knew == "none") %>% 
                mutate(acc_recall = as.numeric(acc_recall > 0),
                       resp_source = fct_rev(if_else(resp_source > 0, "correct", "incorrect")),
                       j_score = if_else(j_score > 0.7, "upper half", "lower half")) %>% 
                group_by(subj_num, j_score, resp_source) %>% 
                summarize(acc_pred = mean(acc_recall)),
              alpha = 0.5,
              width = 0.05) +
  geom_line(aes(group = interaction(j_score, iteration)), alpha = 0.03, size = 0.5) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(x = "Forced-choice museum memory",
       y = "P(recall) for novel facts",
       color = "Expertise (median split)") +
  theme(legend.position = 0:1,
        legend.justification = 0:1)


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

ggsave(here::here("ignore", "figs", "plot_slides_pic_by_expertise.png"),
       plot = plot_pic_by_expertise,
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

# TODO: Clean out the plots we're not actually using anymore
# ggsave(here::here("ignore", "figs", "plot_slides_fact_by_source.png"),
#        plot = plot_fact_by_source,
#        device = "png",
#        width = 8,
#        height = 4,
#        units = "in")

## save plot objects for CNS data blitz (specific shapes) ----

ggsave(here::here("ignore", "figs", "plot_cns2023talk_fact_by_expertise.png"),
       plot = plot_fact_by_expertise + theme_slides_ppt,
       device = "png",
       width = 5.71,
       height = 6.85,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_cns2023talk_fixef_fact_by_pic.png"),
       plot = plot_fixef_fact_by_pic + 
         scale_color_manual(values = c("upper half" = "#b580b6", "lower half" = "#2c2aa6")) +
         labs(color = "Expertise") +
         theme_slides_ppt + 
         theme(legend.position = c(0, 1), 
               legend.justification = c(0, 1)),
       device = "png",
       width = 5.71,
       height = 6.85,
       units = "in")

## save plot objects for CNS poster (specific shapes) ----

ggsave(here::here("ignore", "figs", "plot_cns2023poster_demos.png"),
       plot = j_exp %>% 
         ggplot(aes(x = j_score)) +
         geom_histogram(binwidth = .05, fill = "#4a71ea", alpha = 0.8) +
         geom_vline(xintercept = median(j_exp$j_score), linetype = "dotted") +
         annotate("text",
                  x = median(j_exp$j_score) - 0.02,
                  y = 20,
                  label = "median score",
                  hjust = 1,
                  family = this_font,
                  size = 8) +
         labs(x = "Trivia expertise\n(prop. correct out of 50 Qs)",
              y = "# of participants") + 
         theme_poster,
       device = "png",
       width = 7,
       height = 5,
       units = "in")

# plot_fact_by_expertise but with a different constant color
ggsave(here::here("ignore", "figs", "plot_cns2023poster_fact_by_expertise.png"),
       plot = retrieval %>% 
         filter(already_knew == "none") %>% 
         group_by(subj_num, j_score) %>% 
         summarize(acc_recall = mean(acc_recall)) %>% 
         ggplot(aes(x = j_score, y = acc_recall)) +
         geom_point(alpha = 0.5, color = "#4a71ea") +
         geom_smooth(method = "lm", color = "black", fill = "#4a71ea") +
         labs(x = "Trivia expertise",
              y = "P(recall) for novel facts") +
         theme_poster,
       device = "png",
       width = 8,
       height = 6,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_cns2023poster_fixef_fact_by_pic.png"),
       plot = plot_fixef_fact_by_pic + 
         scale_color_manual(values = c("upper half" = "#b580b6", "lower half" = "#2c2aa6")) +
         labs(color = "Expertise") +
         theme_poster + 
         theme(legend.position = c(0, 1), 
               legend.justification = c(0, 1)),
       device = "png",
       width = 8,
       height = 6,
       units = "in")

ggsave(here::here("ignore", "figs", "plot_cns2023poster_fixef_fact_by_source.png"),
       plot = plot_fixef_fact_by_source + 
         scale_color_manual(values = c("upper half" = "#b580b6", "lower half" = "#2c2aa6")) +
         labs(color = "Expertise") +
         theme_poster + 
         theme(legend.position = c(0, 1), 
               legend.justification = c(0, 1)),
       device = "png",
       width = 8,
       height = 6,
       units = "in")

## bulk save plot objects for ms caption draft rmd ----

save(plot_demos,
     plot_expertise_hist,
     plot_fact_by_expertise,
     plot_pic_by_expertise,
     plot_source_by_expertise,
     plot_coefs_fact_by_pic,
     plot_coefs_fact_by_source,
     plot_fixef_fact_by_pic,
     plot_fixef_fact_by_source,
     file = here::here("ignore", "data", "plots_ms.rda"))
