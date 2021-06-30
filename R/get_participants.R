## setup ----

require(tidyverse)
require(qualtRics)

## get inquiries from qualtrics ----

inquiries_raw <- fetch_survey("SV_cx7HTf5yYOwaYrs",
             force_request = TRUE,
             label = TRUE)

inquiries <- inquiries_raw %>% 
  filter(consent == "YES, I would like to continue.",
         interested == "Yes",
         !is.na(email)) %>% 
  select(public_id = ResponseId, email) %>% 
  mutate(name = public_id,
         group = "")

write_csv(inquiries, here::here("ignore", "running_subjs", "participants_gorilla_upload.csv"))

## read in completion status from gorilla manual download ----

completions_raw <- read_csv(here::here("ignore", "running_subjs", "participants_gorilla_download.csv"))

completions <- completions_raw %>% 
  select(email = `Email Address`,
         checkpoint = Checkpoint,
         current_node = `Current Node`,
         status = Status)

## write out email addresses for next tango card payment batch ----

# By reading in previous pay files, should be able to ID who's been paid to avoid double paying

payments_past <- tibble(file = list.files(here::here("ignore", "running_subjs"),
                            full.names = TRUE)) %>% 
  filter(grepl("tango", file)) %>% 
  mutate(payment_date = lubridate::ymd(str_sub(file, -12L, -5L)),
         data = map(file, read_csv)) %>% 
  unnest(data)

payments <- completions %>% 
  filter(status == "Complete") %>% 
  anti_join(payments_past, by = "email") %>% 
  select(email) %>% 
  mutate(etid = "E029217",
         firstname = "Participant",
         lastname = NA,
         utid = "U579023",
         amt = 30,
         notes = NA,
         dnc = NA,
         message = NA,
         sender_name = "Columbia Aly Lab") %>% 
  select(etid, email, everything())

# Write out only one pay file per pay batch (with pay date as identifier)

write_csv(payments, here::here("ignore",
                               "running_subjs",
                               glue::glue("participants_tango_upload_{str_remove_all(Sys.Date(), '-')}.csv")),
          na = "")
