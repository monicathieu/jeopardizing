clean_expertise_questions <- function (question_data) {
  out <- question_data %>% 
    filter(use_question == 1) %>% 
    select(question, answer, answer_short) %>% 
    mutate(answer_short = coalesce(answer_short, answer))
  
  return (out)
}

score_expertise_firstpass <- function (expertise_data_unscored, question_data) {
  out <- expertise_data_unscored %>% 
    left_join(question_data) %>% 
    mutate(acc_recall_grepl_strict = map2_lgl(resp_recall, answer,
                                              function (a, b) {
                                                # trim non alpha nums in response in case they make grepl yell
                                                a <- str_remove_all(a, "[^a-zA-Z\\d\\s-]")
                                                if (is.na(a) | nchar(a) == 0) return (FALSE)
                                                out <- (grepl(a, b, ignore.case = T) | grepl(b, a, ignore.case = T))
                                                
                                                return (out)
                                              }),
           # Particular very common misspellings that get special treatment
           acc_recall_grepl_strict = case_when(answer == "Spanish-American War" & grepl("spanish american", resp_recall, ignore.case = T) ~ TRUE,
                                               answer == "Andrea Bocelli" & grepl("andrea boccelli", resp_recall, ignore.case = T) ~ TRUE,
                                               answer == "distillation" & grepl("^distill|^destill", resp_recall, ignore.case = T) ~ TRUE,
                                               answer == "uranium" & grepl("plutonium", resp_recall, ignore.case = T) ~ FALSE,
                                               TRUE ~ acc_recall_grepl_strict),
           acc_recall_grepl_fuzzy = map2_lgl(resp_recall, answer,
                                             function (a, b) {
                                               # so titles starting with The don't get disadvantaged
                                               # by the cost dividing by whole string length
                                               b <- str_remove(b, "^The ")
                                               agrepl(b, a, max.distance = 0.18, ignore.case = T)
                                             }),
           acc_recall_grepl_fuzzy = case_when(answer == "uranium" & grepl("plutonium", resp_recall, ignore.case = T) ~ FALSE,
                                              answer == "Speaker of the House" & tolower(resp_recall) == "house speaker" ~ TRUE,
                                              TRUE ~ acc_recall_grepl_fuzzy),
           acc_recall_grepl_lastname = map2_lgl(resp_recall, answer_short,
                                                ~agrepl(.y, .x, max.distance = 0.18, ignore.case = T)),
           acc_recall_grepl_lastname = case_when(answer == "uranium" & grepl("plutonium", resp_recall, ignore.case = T) ~ FALSE,
                                                 TRUE ~ acc_recall_grepl_lastname))
  
  return (out)
}

# stopifnot(scored %>% filter(!acc_recall_grepl_strict, acc_recall_grepl_fuzzy, !acc_recall_grepl_lastname) %>% nrow() == 0)

score_expertise_byhand <- function (expertise_data_prescored, handscoring_data, interactive = FALSE, reset_scores = FALSE) {
  byhand <- handscoring_data %>% 
    distinct()
  
  # This leaves non-hand-scored entries as NA
  scored <- expertise_data_prescored %>% 
    left_join(byhand)
  
  if (interactive) {
    if (reset_scores) scored$acc_recall_byhand <- NA
    
    for (i in 1:nrow(scored)) {
      # if strict is false, and at least one of fuzzy or lastname is true
      if (is.na(scored$acc_recall_byhand[i]) & nchar(scored$resp_recall[i]) > 2 & scored$acc_recall_grepl_strict[i] != (scored$acc_recall_grepl_fuzzy[i] | scored$acc_recall_grepl_lastname[i])) {
        
        cat("\nCorrect answer:", crayon::green(scored$answer[i]),
            " Response:", crayon::yellow(scored$resp_recall[i]))
        ans <- as.integer(readline("\n0 for incorrect, 1 for correct: "))
        
        if (ans == 1) {
          cat(crayon::green(scored$resp_recall[i], "for", scored$answer[i], "was CORRECT"))
          ans <- TRUE
        } else if (ans == 0) {
          cat(crayon::red(scored$resp_recall[i], "for", scored$answer[i], "was INCORRECT"))
          ans <- FALSE
        } else {
          cat(crayon::magenta("Unrecognized value entered, womp womp."))
          ans <- as.integer(readline("Enter again:"))
        }
        
        scored$acc_recall_byhand[i] <- ans
        
        scored %>%
          slice(i) %>% 
          select(subj_num, resp_recall, answer, acc_recall_byhand) %>% 
          write_csv(here::here("ignore", "data", "task_jeopardy_recall_handscoring.csv"), append = T)
      }
    } 
  }
  
  return (scored)
}

score_expertise_lastpass <- function (expertise_data_handscored) {
  out <- expertise_data_handscored %>%
    mutate(acc_recall = case_when(
      # If a hand scoring exists, that takes precedence
      !is.na(acc_recall_byhand) ~ acc_recall_byhand,
      # !strict, !fuzzy, !lastname: all wrong, totally wrong
      !acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & !acc_recall_grepl_lastname ~ FALSE,
      # !strict, fuzzy, !lastname: matches part of the answer but not the key part (e.g. "rebellion" but not "boxer")
      !acc_recall_grepl_strict & acc_recall_grepl_fuzzy & !acc_recall_grepl_lastname ~ FALSE,
      # strict, !fuzzy, !lastname: all wrong, too short
      acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & !acc_recall_grepl_lastname ~ FALSE,
      # strict, !fuzzy, lastname: all right, last name only spelled correctly
      acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ TRUE,
      # strict, fuzzy, !lastname: just distillation/distilling?
      acc_recall_grepl_strict & acc_recall_grepl_fuzzy & !acc_recall_grepl_lastname ~ TRUE,
      # strict, fuzzy, lastname: all right, totally spelled correctly
      acc_recall_grepl_strict & acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ TRUE,
      # These two below should never trigger because these are the conditions that trigger hand scoring above
      # !strict, !fuzzy, lastname: probably wrong, likely incorrect first name added or misspelled last name
      !acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ FALSE,
      # !strict, fuzzy, lastname: probably correct, lightly misspelled
      !acc_recall_grepl_strict & acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ TRUE,
      # hopefully this never happens
      TRUE ~ NA),
      # quickly drop confidence if there was no response
      resp_conf = if_else(is.na(resp_recall), NA_integer_, resp_conf))
  
  return (out)
}

bind_expertise_scores <- function(task_data, expertise_scores) {
  out <- task_data %>% 
    left_join(expertise_scores, by = "subj_num") %>% 
    mutate(subj_num = fct_reorder(as.character(subj_num), j_score))
  
  return (out)
}

# write_csv(scored, file = here::here("ignore", "data", "task_jeopardy_recall.csv"))
