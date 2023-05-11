clean_q_google_demos <- function (demo_data, id_data) {
  # Must do col transforming after dropping unwanted cols
  # col_types and cols_only don't seem to drop cols smoothly
  out <- demo_data %>% 
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
                              TRUE ~ NA_character_)) %>% 
    right_join(id_data, by = "demographics_id") %>% 
    mutate(subj_num = factor(subj_num)) %>% 
    # One subject seems to have filled it out twice but gave the same data both times (thank god)
    distinct(subj_num, age, edu, gender, ethnicity, race)
  
  return (out)
}

# write_csv(demos, here::here("ignore", "data", "q_google_demos.csv"))
