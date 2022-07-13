#load packages -----------------------------------------------------------

source(here::here("scripts", "0_load_packages.R"))

#load data ---------------------------------------------------------------

data_raw <- read.csv(here("data","cravings_diary_18.06.21.csv"))

#create data frame

variables <- c("account_id", "quit_date", "timezone_offset", "cigs_per_day", "time_to_first_cig",
                       "craving_record_created", "craving_date", "craving_severity", "craving_did_smoke",
                       "did_someone_smoke", "craving_env_alone", "craving_env_colleagues", "craving_env_family",
                       "craving_env_friends", "craving_env_partner", "craving_env_strangers", "craving_occupation_at_bar_or_event",
                       "craving_occupation_chatting", "craving_occupation_coffee_or_tea", "craving_occupation_drinking",
                       "craving_occupation_driving", "craving_occupation_going_to_bed", "craving_occupation_just_eaten",
                       "craving_occupation_just_had_sex", "craving_occupation_reading", "craving_occupation_relaxing",
                       "craving_occupation_taking_a_break", "craving_occupation_thinking", "craving_occupation_waking_up",
                       "craving_occupation_working", "craving_occupation_other", "craving_feeling_annoyed",
                       "craving_feeling_anxious", "craving_feeling_bored", "craving_feeling_down", "craving_feeling_happy",
                       "craving_feeling_hungry", "craving_feeling_lonely", "craving_feeling_stressed", "craving_feeling_other")

data <- data_raw %>%
  filter(cigs_per_day < 101 & time_to_first_cig != "" & craving_did_smoke != "" & craving_record_created != "") %>%
  select(all_of(variables))

data <- droplevels(data)

data <- data %>%
  mutate(quit_date = ymd_hms(quit_date),
         craving_record_created = ymd_hms(craving_record_created),
         craving_date = ymd_hms(craving_date),
         offset = lubridate::seconds(timezone_offset))

#apply timezone offset to date time variables (skipping NAs)

df1 <- data %>%
  mutate(adjusted_quit_date = case_when(is.na(offset) ~ quit_date, TRUE ~ quit_date + offset),
         adjusted_craving_record_created = case_when(is.na(offset) ~ craving_record_created, TRUE ~ craving_record_created + offset),
         adjusted_craving_date = case_when(is.na(offset) ~ craving_date, TRUE ~ craving_date + offset))

p <- df1 %>%
  group_by(account_id) %>%
  summarise(count = n())

#exclude participants with quit date before January 1st 2020 and after March 17th 2021

df2 <- df1 %>%
  filter(adjusted_quit_date >= "2020-01-01 00:00:00" & adjusted_quit_date < "2021-03-18 00:00:00" &
           adjusted_craving_record_created >= "2020-01-01 00:00:00")

p1 <- df2 %>%
  group_by(account_id) %>%
  summarise(count = n())

#count and exclude craving entries where the difftime from the quit date is >3 months or <0

df3 <- df2 %>%
  mutate(craving_diff_qd = as.numeric(difftime(adjusted_craving_record_created, adjusted_quit_date, units = "weeks")),
         craving_diff_exclude = as.factor(case_when(craving_diff_qd > 12 ~ "exclude",
                                                    craving_diff_qd < 0 ~ "exclude",
                                                    TRUE ~ "include")))

df3 %>%
  group_by(account_id) %>%
  janitor::tabyl(craving_diff_exclude)

df4 <- df3 %>%
  filter(craving_diff_exclude == "include")

df4 <- droplevels(df4)

p2 <- df4 %>%
  group_by(account_id) %>%
  summarise(count = n())

#count and exclude craving entries where the difftime between when the entry was made and the date-time the entry refers to is >24 hours or <0

df5 <- df4 %>%
  mutate(craving_diff_record = round(as.numeric(difftime(adjusted_craving_record_created, adjusted_craving_date, units = "hours")), 2),
         craving_diff_record_exclude = as.factor(case_when(craving_diff_record > 24 ~ "exclude",
                                                    craving_diff_record < 0 ~ "exclude",
                                                    TRUE ~ "include")))

df5 %>%
  group_by(account_id) %>%
  janitor::tabyl(craving_diff_record_exclude)

df6 <- df5 %>%
  filter(craving_diff_record_exclude == "include")

df6 <- droplevels(df6)

#count the number of participants and number of craving entries (prior to removing duplicate entries)

p3 <- df6 %>%
  group_by(account_id) %>%
  summarise(count = n())

(sum(p3$count>=1)/14398)*100
(sum(p3$count>=5)/14398)*100
(sum(p3$count>=10)/14398)*100
(sum(p3$count>=20)/14398)*100
(sum(p3$count>=30)/14398)*100
(sum(p3$count>=50)/14398)*100

#create day of the week (Mon-Sun) and time of day (morning, midday, evening, night) variables

df7 <- df6 %>%
  mutate(wkday_craving = case_when(!is.na(adjusted_craving_record_created) ~ wday(adjusted_craving_record_created, label = T)),
         wkday_craving = fct_explicit_na(wkday_craving, na_level = "NA"),
         hour_craving = hour(adjusted_craving_record_created),
         time_of_day = as.factor(case_when(hour_craving >= 5 & hour_craving < 11 ~ "morning",
                                           hour_craving >= 11 & hour_craving < 17 ~ "midday",
                                           hour_craving >= 17 & hour_craving < 22 ~ "evening",
                                           hour_craving >= 22 & hour_craving < 0 ~ "night",
                                           hour_craving >= 0 & hour_craving < 5 ~ "night",
                                           TRUE ~ "night")))

#exclude non-unique craving entries

df8 <- df7 %>%
  group_by(account_id, adjusted_craving_record_created) %>%
  distinct(adjusted_craving_record_created, .keep_all = TRUE) %>%
  ungroup()

#count the number of participants and number of craving entries

p4 <- df8 %>%
  group_by(account_id) %>%
  summarise(count = n())

(sum(p4$count>=1)/14398)*100
(sum(p4$count>=5)/14398)*100
(sum(p4$count>=10)/14398)*100
(sum(p4$count>=20)/14398)*100
(sum(p4$count>=30)/14398)*100
(sum(p4$count>=50)/14398)*100

#end of excluding participants/entries -----------------------------------

#create rolling number of craving entries for each participant

df9 <- df8 %>%
  group_by(account_id) %>%
  arrange(adjusted_craving_record_created) %>%
  mutate(event_nr = row_number()) %>%
  ungroup()

#create days since quit date for each entry and participant

df10 <- df9 %>%
  group_by(account_id) %>%
  mutate(days_since_qd = round(as.numeric(difftime(adjusted_craving_record_created, adjusted_quit_date, units = "days")))) %>%
  ungroup()

#create previous entry lapse (vs. not) variable

df11 <- df10 %>%
  group_by(account_id) %>%
  mutate(prior_event_lapse = as.factor(case_when(adjusted_craving_record_created > lag(adjusted_craving_record_created) 
                                                 & lag(craving_did_smoke) == "true" ~ "yes",
                                                 TRUE ~ "no"))) %>%
  ungroup()

#create time (mins) since previous entry variable

df12 <- df11 %>%
  group_by(account_id) %>%
  mutate(mins_since_prev_rep = round(as.numeric(difftime(adjusted_craving_record_created, lag(adjusted_craving_record_created),
                                        units = "mins")))) %>%
  ungroup()

#select variables of interest

clean_variables <- c("account_id", "adjusted_quit_date", "cigs_per_day",
                     "adjusted_craving_record_created", "craving_severity", "craving_did_smoke", "prior_event_lapse",
                     "craving_env_alone", "craving_env_colleagues", "craving_env_family",
                     "craving_env_friends", "craving_env_partner", "craving_env_strangers", "craving_occupation_at_bar_or_event",
                     "craving_occupation_chatting", "craving_occupation_coffee_or_tea", "craving_occupation_drinking",
                     "craving_occupation_driving", "craving_occupation_going_to_bed", "craving_occupation_just_eaten",
                     "craving_occupation_just_had_sex", "craving_occupation_reading", "craving_occupation_relaxing",
                     "craving_occupation_taking_a_break", "craving_occupation_thinking", "craving_occupation_waking_up",
                     "craving_occupation_working", "craving_occupation_other", "craving_feeling_annoyed",
                     "craving_feeling_anxious", "craving_feeling_bored", "craving_feeling_down", "craving_feeling_happy",
                     "craving_feeling_hungry", "craving_feeling_lonely", "craving_feeling_stressed", "craving_feeling_other",
                     "time_to_first_cig", "wkday_craving", "time_of_day", "event_nr", "days_since_qd", "mins_since_prev_rep")

data_clean <- df12 %>%
  select(all_of(clean_variables))

#format variables of interest

data_clean <- data_clean %>%
  mutate(across(where(is.character), .fns = as_factor))

#write to rds

write_rds(data_clean, here("data", "clean_data.rds"))