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
  select(public_id = ResponseId, email, inquiry_date = EndDate) %>% 
  mutate(name = public_id,
         group = "")

# just people after the LL posts were amended to say low rundle only 
inquiries %>% 
  filter(inquiry_date >= "2021-07-06 21:30:00") %>% 
  write_csv(here::here("ignore", "running_subjs", "participants_gorilla_upload.csv"))

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

write_csv(payments,
          here::here("ignore",
                     "running_subjs",
                     glue::glue("participants_tango_upload_{str_remove_all(Sys.Date(), '-')}.csv")),
          na = "")

## write out emails for rundle certification secondary prescreen ----

# run at noon-ish Pacific time July 6 2021 
inquiries_norundle <- inquiries %>% 
  left_join(completions %>% 
              select(email, status),
            by = "email") %>% 
  # People who have already started/finished the study don't need to certify
  filter(!(status %in% c("Live", "Complete"))) %>% 
  select(email)

write_csv(inquiries_norundle,
          here::here("ignore",
                     "running_subjs",
                     "participants_rundle_prescreen_upload_20210706.csv"))

## get data from rundle secondary prescreen ----

prescreen_rundle_raw <- fetch_survey("SV_4101dXMbVZpTADc",
                                     force_request = TRUE,
                                     label = TRUE)

prescreen_rundle <- prescreen_rundle_raw %>% 
  select(email = RecipientEmail, rundle, interested)

prescreen_rundle %>% 
  filter(interested == "Yes") %>% 
  select(email) %>% 
  left_join(inquiries, by = "email") %>% 
write_csv(here::here("ignore", "running_subjs", "participants_gorilla_upload_lowrundles.csv"))
