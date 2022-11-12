
# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# Read data and checking log

df_cleaning_log <- read_csv("inputs/combined_checks_livelihood.csv", col_types = cols(sheet = "c", index = "i")) %>% 
  filter(!adjust_log %in% c("delete_log")) %>%
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) %>% 
filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         relevant = NA) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

data_path <- "inputs/livelihoods_assessment_data.xlsx"

# main data
cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = data_path, col_types = c_types) %>% 
  filter(as_date(as_datetime(start)) > as_date("2022-08-10")) %>%
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# loops
hh_roster <- readxl::read_excel(path = data_path, sheet = "hh_roster")
hh_repeat_school_enrollment <- readxl::read_excel(path = data_path, sheet = "repeat_school_enrollment")

df_raw_data_hh_roster <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(hh_roster, by = c("_uuid" = "_submission__uuid") )

df_raw_data_hh_repeat_school_enrollment <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(hh_repeat_school_enrollment, by = c("_uuid" = "_submission__uuid") )

# tool
df_survey <- readxl::read_excel("inputs/livelihoods_assessment_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/livelihoods_assessment_tool.xlsx", sheet = "choices")


# main dataset ------------------------------------------------------------

df_cleaned_data <- implement_cleaning_support(input_df_raw_data = df_raw_data %>% select(-employee_business_hh_engaged_text),
                                            input_df_survey = df_survey,
                                            input_df_choices = df_choices,
                                            input_df_cleaning_log = df_cleaning_log_main) %>% 
mutate(across(.cols = -c(contains(cols_to_escape), matches("_age$|^age_|uuid")),
              .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{2,9}$"), "NA", .)))


# clean repeats -----------------------------------------------------------
other_repeat_col <- c("start", "end", "today", "consent_one", "consent_two", "hoh_equivalent", "respondent_gender", 
                      "respondent_age", "respondent_education", "gender_hoh", "age_hoh", 
                      "education_hoh", "location", "location_type", "status_intro", "ctry_origin_not_uganda", 
                      "ctry_origin_not_uganda_other", "date_arrival", "hh_living_status", "town_hh_living_in", 
                      "settlement_name", "status")

df_cleaning_log_roster <- df_cleaning_log %>% 
  filter(!is.na(sheet), uuid %in% df_raw_data_hh_roster$`_uuid`, name %in% colnames(df_raw_data_hh_roster))

df_cleaned_data_hh_roster <- implement_cleaning_support(input_df_raw_data = df_raw_data_hh_roster,
                                                      input_df_survey = df_survey,
                                                      input_df_choices = df_choices,
                                                      input_df_cleaning_log = df_cleaning_log_roster) %>% 
  select(other_repeat_col, any_of(colnames(hh_roster)), `_index` = index, `_submission__uuid` = uuid) %>% 
  mutate(across(.cols = -c(contains(cols_to_escape), matches("_age$|^age_|uuid")),
                .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{2,9}$"), "NA", .)))

df_cleaning_log_school <- df_cleaning_log %>% 
  filter(!is.na(sheet), uuid %in% df_raw_data_hh_repeat_school_enrollment$`_uuid`, name %in% colnames(df_raw_data_hh_repeat_school_enrollment))

df_clean_data_hh_repeat_school_enrollment <- implement_cleaning_support(input_df_raw_data = df_raw_data_hh_repeat_school_enrollment,
                                                                        input_df_survey = df_survey,
                                                                        input_df_choices = df_choices,
                                                                        input_df_cleaning_log = df_cleaning_log_school) %>% 
  select(other_repeat_col, any_of(colnames(hh_repeat_school_enrollment)), `_index` = index, `_submission__uuid` = uuid) %>% 
  mutate(across(.cols = -c(contains(cols_to_escape), matches("_age$|^age_|uuid")),
                .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{2,9}$"), "NA", .)))


# write final modified data -----------------------------------------------

list_of_clean_datasets <- list("UGA2205_HH Livelihood" = df_cleaned_data,
                               "hh_roster" = df_cleaned_data_hh_roster,
                               "hh_repeat_school_enrollment" = df_clean_data_hh_repeat_school_enrollment
)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_livelihood.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")






