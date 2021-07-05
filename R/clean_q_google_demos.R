## setup ----

require(tidyverse)
require(magrittr)

googledrive::drive_download(as_id("1yW13tK4dVr8MXtJTaZxV_3M8IaD5TqhbzD8QKPuEndM"),
                            path = here::here("ignore", "data", "raw", "real", "demographics.csv"),
                            type = "csv",
                            overwrite = TRUE)

## read n parse ----

# Must do col transforming after dropping unwanted cols
# col_types and cols_only don't seem to drop cols smoothly
demos <- read_csv(here::here("ignore", "data", "raw", "real", "demographics.csv")) %>% 
  select(study_name = "Study Name [***Do Not Edit***]",
         timestamp = Timestamp,
         demographics_id = "Participant ID [***Do Not Edit***]",
         age = Age,
         edu = "Years of Education",
         gender = Gender,
         ethnicity = Ethnicity,
         race = "Race (check all that apply)") %>% 
  mutate(timestamp = lubridate::parse_date_time(timestamp, "mdY HMS")) %>% 
  filter(study_name == "JStudy", timestamp >= "2021-06-23") %>% 
  mutate(gender = case_when(tolower(gender) %in% c("m", "male") ~ "male",
                            tolower(gender) %in% c("f", "female") ~ "female",
                            grepl("nonbinary|genderqueer", gender, ignore.case = T) ~ "genderqueer",
                            grepl("male", gender, ignore.case = T) & !grepl("female", gender, ignore.case = T) ~ "male",
                            grepl("female", gender, ignore.case = T)  ~ "female",
                            grepl("man", gender, ignore.case = T) & !grepl("woman", gender, ignore.case = T) ~ "male",
                            grepl("woman", gender, ignore.case = T)  ~ "female",
                            TRUE ~ NA_character_))

write_csv(demos, here::here("ignore", "data", "q_google_demos.csv"))
