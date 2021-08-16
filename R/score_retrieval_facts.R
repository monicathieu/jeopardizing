## setup ----

require(tidyverse)
require(magrittr)

my_agrepl <- function (pattern, x, max.distance = 0.24, ignore.case = T) {
  return (coalesce(c(adist(pattern, x, ignore.case = ignore.case)) / nchar(pattern) <= max.distance, FALSE))
}

unscored <- read_csv(here::here("ignore", "data", "task_retrieval_facts_unscored.csv"))

## grepl scoring first pass----

scored <- unscored %>% 
  mutate(acc_recall_grepl_strict = map2_lgl(resp, test_answer,
                                            function (a, b) {
                                              # trim non alpha nums in response in case they make grepl yell
                                              a <- str_remove_all(a, "[^a-zA-Z\\d\\s-]")
                                              b <- str_remove_all(b, "[^a-zA-Z\\d\\s-]")
                                              if (is.na(a) | nchar(a) == 0) return (FALSE)
                                              out <- (grepl(a, b, ignore.case = T) | grepl(b, a, ignore.case = T))
                                              return (out)
                                            }),
         # Particular very common misspellings or key short phrases that get special treatment
         acc_recall_grepl_strict = case_when(
           nchar(str_remove_all(resp, "[^a-zA-Z\\d\\s-]")) / nchar(test_answer) <= 0.25 ~ FALSE,
           group_trial_num == 1 & category == "gems" & grepl("cleav", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 13 & category == "musi" & tolower(resp) == "hurdy gurdy" ~ TRUE,
           group_trial_num == 37 & category == "cook" & grepl("orchid", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 38 & category == "cook" & grepl("hen-of-the-woods", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 16 & category == "dino" & tolower(resp) == "nose" ~ TRUE,
           group_trial_num == 21 & category == "dino" & grepl("medullary", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 36 & category == "dino" & grepl("saw fish", resp, ignore.case = T) ~ TRUE,
           TRUE ~ acc_recall_grepl_strict
         ),
         acc_recall_grepl_fuzzy = map2_lgl(resp, test_answer,
                                           function (a, b) {
                                             # trim non alpha nums so they don't artificially pad the resp length
                                             a <- str_remove_all(a, "[^a-zA-Z\\d\\s-]")
                                             if (b == "medullary bone") b <- "medullary"
                                             out <- my_agrepl(b, a)
                                             return (out)
                                           }
         ),
         # exact spellings of short versions that should get fuzzy credit as well
         # or certain phrases to flag for visual inspection for partial credit
         acc_recall_grepl_fuzzy = case_when(
           group_trial_num == 5 & category == "arms" & !startsWith(tolower(resp), "a") ~ FALSE,
           group_trial_num == 39 & category == "arms" & grepl("head", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 1 & category == "gems" & tolower(resp)  %in% c("cleave", "cleavage", "clev") ~ TRUE,
           group_trial_num == 7 & category == "gems" & grepl("zirconi", resp, ignore.case = T) ~ FALSE,
           group_trial_num == 10 & category == "gems" & grepl("knoop", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 27 & category == "gems" & tolower(resp) == "pavillion" ~ TRUE,
           group_trial_num == 28 & category == "gems" & tolower(resp) == "step" ~ TRUE,
           group_trial_num == 30 & category == "gems" & tolower(resp) == "10x" ~ TRUE,
           group_trial_num %in% c(12, 14) & category == "musi" & grepl("viol", resp, ignore.case = T) ~ TRUE,
           group_trial_num %in% c(12, 14) & category == "musi" & tolower(resp) %in% c("viol", "viola") ~ TRUE,
           group_trial_num == 17 & category == "musi" & tolower(resp)  == "troll" | grepl("aeac#", str_remove_all(resp, "-\ "), ignore.case = T) ~ TRUE,
           group_trial_num == 19 & category == "musi" & acc_recall_grepl_strict & grepl("hand", resp, ignore.case = T) ~ FALSE,
           group_trial_num == 19 & category == "musi" & tolower(resp) == "claw" ~ FALSE,
           group_trial_num == 19 & category == "musi" & grepl("claw", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 2 & category == "cars" & grepl("exhaustion", resp, ignore.case = T) ~ FALSE,
           group_trial_num == 4 & category == "cars" & grepl("bug|beetle|vw|volkswag", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 5 & category == "cars" & tolower(resp)  == "watt" ~ TRUE,
           group_trial_num == 7 & category == "cars" & tolower(resp)  == "sleeve" ~ TRUE,
           group_trial_num == 11 & category == "cars" & tolower(resp)  == "planetary" ~ TRUE,
           group_trial_num == 19 & category == "cars" & grepl("turn|blinker|flasher", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 20 & category == "cars" & grepl(" ", resp, ignore.case = T) ~ FALSE,
           group_trial_num == 21 & category == "cars" & tolower(resp)  == "talcum" ~ TRUE,
           group_trial_num == 24 & category == "cars" & tolower(resp)  == "leaf" ~ TRUE,
           group_trial_num == 30 & category == "cars" & grepl("4-wheel|4 wheel|four wheel|four-wheel", resp, ignore.case = T) & grepl("brak", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 31 & category == "cars" & tolower(resp)  %in% c("disc", "disk") ~ TRUE,
           group_trial_num == 33 & category == "cars" & tolower(resp)  == "drum" ~ TRUE,
           group_trial_num == 36 & category == "cars" & tolower(resp)  %in% c("ply", "plie", "plies") ~ TRUE,
           group_trial_num == 37 & category == "cars" & tolower(resp)  == "radial" ~ TRUE,
           group_trial_num == 24 & category == "cook" & tolower(resp)  == "careme" ~ TRUE,
           group_trial_num == 33 & category == "cook" & grepl("muskmelon", str_remove_all(resp, " "), ignore.case = T) ~ TRUE,
           group_trial_num == 35 & category == "cook" & my_agrepl("maillard", resp) ~ TRUE,
           group_trial_num == 37 & category == "cook" & grepl("orchid", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 13 & category == "dino" & grepl("bone", resp, ignore.case = T) & grepl("war", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 16 & category == "dino" & grepl("nose", resp, ignore.case = T) ~ TRUE,
           group_trial_num == 21 & category == "dino" &  tolower(resp) == "medullary" ~ TRUE,
           group_trial_num == 21 & category == "dino" &  !startsWith(tolower(resp), "m") ~ FALSE,
           group_trial_num == 34 & category == "dino" & tolower(resp)  == "anning" ~ TRUE,
           !grepl(" ", test_answer) & nchar(resp) / nchar(test_answer) <= 0.81 ~ FALSE,
           TRUE ~ acc_recall_grepl_fuzzy
         ))

## hand scoring interactively ----

byhand <- read_csv(here::here("ignore", "data", "task_retrieval_facts_handscoring.csv")) %>% 
  distinct()

# This leaves non-hand-scored entries as NA
scored %<>% left_join(byhand)

# scored$acc_recall_byhand <- NA_real_
for (i in 1:nrow(scored)) {
  # if strict is false, and fuzzy is true
  if (is.na(scored$acc_recall_byhand[i]) & scored$acc_recall_grepl_strict[i] != scored$acc_recall_grepl_fuzzy[i]) {
    
    cat("\nCorrect answer:", crayon::green(scored$test_answer[i]),
        " Response:", crayon::cyan(scored$resp[i]))
    ans <- as.integer(readline("\n1 for incorrect, 2 for partial, 3 for correct: "))
    
    if (ans == 3) {
      cat(crayon::green(scored$resp[i], "for", scored$test_answer[i], "was CORRECT"))
      ans <- 1
    } else if (ans == 2) {
      cat(crayon::yellow(scored$resp[i], "for", scored$test_answer[i], "was PARTIALLY CORRECT"))
      ans <- 0.5
    } else if (ans == 1) {
      cat(crayon::red(scored$resp[i], "for", scored$test_answer[i], "was INCORRECT"))
      ans <- 0
    } else {
      cat(crayon::magenta("Unrecognized value entered, womp womp."))
      ans <- as.integer(readline("\nEnter again:"))
    }
    
    scored$acc_recall_byhand[i] <- ans
    # write with append = T in the loop so it doesn't depend on the whole loop finishing
    scored %>%
      slice(i) %>% 
      select(subj_num, resp, test_answer, acc_recall_byhand) %>% 
      write_csv(here::here("ignore", "data", "task_retrieval_facts_handscoring.csv"), append = TRUE)
  }
}

## finalize and write out ----

scored %<>%
  mutate(acc_recall = case_when(
    # If a hand scoring exists, that takes precedence
    !is.na(acc_recall_byhand) ~ acc_recall_byhand,
    # !strict, !fuzzy: fully wrong (but look through this sometimes)
    !acc_recall_grepl_strict & !acc_recall_grepl_fuzzy ~ 0,
    # strict, fuzzy, lastname: all right, totally spelled correctly
    acc_recall_grepl_strict & acc_recall_grepl_fuzzy ~ 1,
    # nothing even written for the conditions that trigger hand scoring
    # they should be NA to warn the user (me)
    TRUE ~ NA_real_
  ))

write_csv(scored, file = here::here("ignore", "data", "task_retrieval_facts.csv"))
