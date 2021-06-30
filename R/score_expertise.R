## setup ----

require(tidyverse)
require(magrittr)

unscored <- read_csv(here::here("ignore", "data", "task_jeopardy_recall_unscored.csv"))

if (FALSE) {
  googledrive::drive_download(file = as_id("1e7R4CSDjSA5trC820aiwUbe7szjn7DTfMiZPI4my8Rk"),
                              path = here::here("stim_stuff", "jeopardy_spreadsheet.csv"),
                              type = "csv",
                              overwrite = TRUE)
}

questions <- read_csv(here::here("stim_stuff", "jeopardy_spreadsheet.csv")) %>% 
  filter(use_question == 1) %>% 
  select(question, answer, answer_short) %>% 
  mutate(answer_short = coalesce(answer_short, answer))


## grepl scoring first pass ----

scored <- unscored %>% 
  left_join(questions) %>% 
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
                                             answer == "distillation" & startsWith(tolower(resp_recall), "distill") ~ TRUE,
                                             answer == "uranium" & grepl("plutonium", resp_recall, ignore.case = T) ~ FALSE,
                                             TRUE ~ acc_recall_grepl_strict),
         acc_recall_grepl_fuzzy = map2_lgl(resp_recall, answer,
                                           function (a, b) {
                                             # so titles starting with The don't get disadvantaged
                                             # by the cost dividing by whole string length
                                             b <- str_remove(b, "^The ")
                                             agrepl(b, a, max.distance = 0.25, ignore.case = T)
                                           }),
         acc_recall_grepl_fuzzy = case_when(answer == "uranium" & grepl("plutonium", resp_recall, ignore.case = T) ~ FALSE,
                                            TRUE ~ acc_recall_grepl_fuzzy),
         acc_recall_grepl_lastname = map2_lgl(resp_recall, answer_short,
                                              ~agrepl(.y, .x, max.distance = 0.25, ignore.case = T)),
         acc_recall_grepl_lastname = case_when(answer == "uranium" & grepl("plutonium", resp_recall, ignore.case = T) ~ FALSE,
                                            TRUE ~ acc_recall_grepl_lastname))

stopifnot(scored %>% filter(!acc_recall_grepl_strict, acc_recall_grepl_fuzzy, !acc_recall_grepl_lastname) %>% nrow() == 0)

## hand scoring interactively ----

byhand <- read_csv(here::here("ignore", "data", "task_jeopardy_recall_handscoring.csv")) %>% 
  distinct()

# This leaves non-hand-scored entries as NA
scored %<>% left_join(byhand)

#scored$acc_recall_byhand <- NA
for (i in 1:nrow(scored)) {
  # if strict is false, and at least one of fuzzy or lastname is true
  if (is.na(scored$acc_recall_byhand[i]) & !scored$acc_recall_grepl_strict[i] & (scored$acc_recall_grepl_fuzzy[i] | scored$acc_recall_grepl_lastname[i])) {
    
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

## finalize and write out ----

scored %<>%
  mutate(acc_recall = case_when(
    # If a hand scoring exists, that takes precedence
    !is.na(acc_recall_byhand) ~ acc_recall_byhand,
    # !strict, !fuzzy, !lastname: all wrong, totally wrong
    !acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & !acc_recall_grepl_lastname ~ FALSE,
    # strict, !fuzzy, !lastname: all wrong, too short
    acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & !acc_recall_grepl_lastname ~ FALSE,
    # strict, !fuzzy, lastname: all right, last name only spelled correctly
    acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ TRUE,
    # strict, fuzzy, lastname: all right, totally spelled correctly
    acc_recall_grepl_strict & acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ TRUE,
    # These two below should never trigger because these are the conditions that trigger hand scoring above
    # !strict, !fuzzy, lastname: probably wrong, likely incorrect first name added or misspelled last name
    !acc_recall_grepl_strict & !acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ FALSE,
    # !strict, fuzzy, lastname: probably correct, lightly misspelled
    !acc_recall_grepl_strict & acc_recall_grepl_fuzzy & acc_recall_grepl_lastname ~ TRUE,
    
    TRUE ~ NA
  ))

write_csv(scored, file = here::here("ignore", "data", "task_jeopardy_recall.csv"))
