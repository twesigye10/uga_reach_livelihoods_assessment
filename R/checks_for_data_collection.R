# checks for data collection

library(checksupporteR)
library(tidyverse)
library(lubridate)
library(glue)
library(sf)

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel(path = "inputs/livelihoods_assessment_data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = as.character(enumerator_id),
         i.check.district_name = district_name)%>% 
          
  filter(i.check.start_date > as_date("2022-08-07"))

df_survey <- readxl::read_excel(path = "inputs/livelihoods_assessment_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel(path = "inputs/livelihoods_assessment_tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/anif_rapid_settlement_samples.gpkg", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()

# time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120


df_survey_time <- check_survey_time(input_tool_data = df_tool_data, 
                                    input_min_time = min_time_of_survey,
                                    input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_survey_time")

# check the time between surveys
min_time_btn_surveys <- 5

df_time_btn_surveys <- check_time_interval_btn_surveys(input_tool_data = df_tool_data, 
                                                       input_min_time = min_time_btn_surveys)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_time_btn_surveys")

# outlier checks ----------------------------------------------------------

df_c_outliers <- checksupporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_outliers")

# spatial checks ----------------------------------------------------------

if("status" %in% colnames(df_sample_data)){
  sample_pt_nos <- df_sample_data %>% 
    mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
    pull(unique_pt_number) %>% 
    unique()
}else{
  sample_pt_nos <- df_sample_data %>% 
    mutate(unique_pt_number = Name) %>% 
    pull(unique_pt_number) %>% 
    unique()
}

# duplicate point numbers
df_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data, 
                                                  input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_duplicate_pt_nos")

# point number does not exist in sample

df_pt_number_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data, 
                                                             input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_pt_number_not_in_sample")


# check for exceeded threshold distance

threshold_dist <- 150

df_greater_thresh_distance <- check_threshold_distance(input_sample_data = df_sample_data, 
                                                       input_tool_data = df_tool_data, 
                                                       input_threshold_dist = threshold_dist)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_greater_thresh_distance")

# others checks -----------------------------------------------------------

df_others_data <- extract_other_specify_data(input_tool_data = df_tool_data, 
                                             input_survey = df_survey, 
                                             input_choices = df_choices)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_others_data")


# logical checks ----------------------------------------------------------

# HH reports 'crop production on own land' as a livelihood, but reports to not have arable land. i.e. 
#(selected(${hh_primary_livelihood}, "crop_production_on_own_land") OR selected(${other_livelihoods_hh_engaged_in}, 
#"crop_production_on_own_land")) AND farming_land_availability = 'no'

df_hh_livelihood_crop_production_on_own_land_1 <- df_tool_data %>% 
  filter(farming_land_availability == "no", str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_own_land") |
                                            str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_own_land")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "farming_land_availability",
         i.check.current_value = as.character(farming_land_availability),
         i.check.value = "",
         i.check.issue_id = "logic_c_farming_land_availability_no_1",
         i.check.issue = glue("farming_land_availability: {farming_land_availability}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has crop_production_on_own_land as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_livelihood_crop_production_on_own_land_1")


# HH reports 'livestock farming on own land' as a livelihood, but reports to not have arable land i.e. 
#(selected(${hh_primary_livelihood}, "livestock_farming_on_own_land") OR selected(${other_livelihoods_hh_engaged_in}, 
#"livestock_farming_on_own_land")) AND farming_land_availability = 'no' 

df_hh_livelihood_livestock_farming_on_own_land_2 <- df_tool_data %>% 
  filter(farming_land_availability == "no", str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land") |
                                            str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "farming_land_availability",
         i.check.current_value = as.character(farming_land_availability),
         i.check.value = "",
         i.check.issue_id = "logic_c_farming_land_availability_no_2",
         i.check.issue = glue("farming_land_availability: {farming_land_availability}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has livestock_farming_on_own_land as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_livelihood_livestock_farming_on_own_land_2")


# HH reports 'livestock farming on own land' AND/OR 'livestock farming on land of others' as a livelihood, but reports not owning any livestock 
# i.e. (selected(${hh_primary_livelihood}, "livestock_farming_on_own_land") OR selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_own_land") 
# OR selected(${hh_primary_livelihood}, "livestock_farming_on_land_of_others") OR selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others"))
# AND hh_own_livestock = 'no' or 'no_answer'

df_livestock_ownership_3 <- df_tool_data %>% 
  filter(hh_own_livestock == "no" | hh_own_livestock == "no_answer", str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land") |
                                      str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land") |
                                      str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others") |
                                      str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hh_own_livestock",
         i.check.current_value = as.character(hh_own_livestock),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_own_livestock_no_3",
         i.check.issue = glue("hh_own_livestock: {hh_own_livestock}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has 'livestock farming on own land' or/and 'livestock farming on land of others' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_livestock_ownership_3")


# HH reports owning arable land, but does not report 'crop production on own land' OR 'livestock farming on own land' as a livelihood 
# i.e.land_occupancy_arrangement = 'ownership' or  'land_was_assigned' AND (not(selected(${hh_primary_livelihood}, 
# "crop_production_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_own_land")) AND
# not(selected(${hh_primary_livelihood}, "livestock_farming_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, 
# "livestock_farming_on_own_land")))

df_land_occupancy_arrangement_ownership_4 <- df_tool_data %>% 
  filter(land_occupancy_arrangement == "ownership" | land_occupancy_arrangement == "land_was_assigned", 
                                       !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_own_land") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_own_land") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "land_occupancy_arrangement",
         i.check.current_value = as.character(land_occupancy_arrangement),
         i.check.value = "",
         i.check.issue_id = "logic_c_land_occupancy_arrangement_ownership_4",
         i.check.issue = glue("land_occupancy_arrangement: {land_occupancy_arrangement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'crop production on own land' or/and 'livestock farming on own land'"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_land_occupancy_arrangement_ownership_4")


# HH reports renting arable land, but does not report 'crop oroduction on own land' OR 'crop production on land of others' OR 'livestock farming on own land' OR 'livestock farming on land of others' as a livelihood i.e.
# land_occupancy_arrangement = 'renting' AND (not(selected(${hh_primary_livelihood}, "crop_production_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_own_land")) AND not(selected(${hh_primary_livelihood}, 
# "crop_production_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_land_of_others")) AND not(selected(${hh_primary_livelihood}, "livestock_farming_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, 
# "livestock_farming_on_own_land")) AND not(selected(${hh_primary_livelihood}, "livestock_farming_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others")))

df_land_occupancy_arrangement_renting_5 <- df_tool_data %>% 
  filter(land_occupancy_arrangement == "renting", !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_own_land") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_own_land") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_land_of_others") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_land_of_others") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "land_occupancy_arrangement",
         i.check.current_value = as.character(land_occupancy_arrangement),
         i.check.value = "",
         i.check.issue_id = "logic_c_land_occupancy_arrangement_renting_5",
         i.check.issue = glue("land_occupancy_arrangement: {land_occupancy_arrangement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no crop production or/and livestock farming as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_land_occupancy_arrangement_renting_5")


# HH reports squatting on arable land, but does not report 'crop production on land of others' OR 'livestock farming on land of others' as a livelihood
# i.e. land_occupancy_arrangement = 'squattingusing_unoccupied_land' AND (not(selected(${hh_primary_livelihood}, "crop_production_on_land_of_others"))
# AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_land_of_others")) AND not(selected(${hh_primary_livelihood}, 
# "livestock_farming_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others")))

df_land_occupancy_arrangement_using_unoccupied_land_6 <- df_tool_data %>% 
  filter(land_occupancy_arrangement == "squattingusing_unoccupied_land", 
         !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_land_of_others") |
           !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_land_of_others") |
           !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others") |
           !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "land_occupancy_arrangement",
         i.check.current_value = as.character(land_occupancy_arrangement),
         i.check.value = "",
         i.check.issue_id = "logic_c_land_occupancy_arrangement_using_unoccupied_land_6",
         i.check.issue = glue("land_occupancy_arrangement: {land_occupancy_arrangement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'crop production on land of others' or/and 'livestock farming on land of others' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_land_occupancy_arrangement_using_unoccupied_land_6")


# HH reports borrowing arable land, but does not report  'crop production on land of others' OR 'livestock farming on land of others' as a livelihood
# i.e. land_occupancy_arrangement = 'borrowing_friends_family_or_employer' AND (not(selected(${hh_primary_livelihood}, "crop_production_on_land_of_others")) 
# AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_land_of_others")) AND # not(selected(${hh_primary_livelihood}, 
# "livestock_farming_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others")))

df_land_occupancy_arrangement_borrowing_7 <- df_tool_data %>% 
  filter(land_occupancy_arrangement == "borrowing_friends_family_or_employer", 
           !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_land_of_others") |
           !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_land_of_others") |
           !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others") |
           !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "land_occupancy_arrangement",
         i.check.current_value = as.character(land_occupancy_arrangement),
         i.check.value = "",
         i.check.issue_id = "logic_c_land_occupancy_arrangement_borrowing_7",
         i.check.issue = glue("land_occupancy_arrangement: {land_occupancy_arrangement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'crop production on land of others' or/and 'livestock farming on land of others' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_land_occupancy_arrangement_borrowing_7")


# HH reports members travel back to settlement 'to work on own land', but do not report 'crop production on own land' OR 'livestock farming on own land' 
# as a livelihood i.e. reason_hh_member_travels_back_to_settlement = 'to_work_on_own_land' AND (not(selected(${hh_primary_livelihood}, 
# "crop_production_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_own_land")) AND
# not(selected(${hh_primary_livelihood}, "livestock_farming_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_own_land")))

df_reason_travel_back_to_settlement_work_own_land_8 <- df_tool_data %>% 
  filter(str_detect(string = reason_hh_member_travel_back_to_settlement, pattern = "to_work_on_own_land"), 
                                           !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_own_land") |
                                           !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_own_land") |
                                           !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land") |
                                           !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_hh_member_travel_back_to_settlement",
         i.check.current_value = as.character(reason_hh_member_travel_back_to_settlement),
         i.check.value = "",
         i.check.issue_id = "logic_c_reason_travel_back_to_settlement_work_own_land_8",
         i.check.issue = glue("reason_hh_member_travel_back_to_settlement: {reason_hh_member_travel_back_to_settlement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'crop production on own land' or/and 'livestock farming on own land' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reason_travel_back_to_settlement_work_own_land_8")


# HH reports members travel back to settlement 'to work on own land', but do not report having arable land i.e. 
# reason_hh_member_travels_back_to_settlement = 'to_work_on_own_land' AND farming_land_availability = 'no' 

df_farming_land_availability_in_settlement_9 <- df_tool_data %>% 
  filter(farming_land_availability == "no", !str_detect(string = reason_hh_member_travel_back_to_settlement, pattern = "to_work_on_own_land")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "farming_land_availability",
         i.check.current_value = as.character(farming_land_availability),
         i.check.value = "",
         i.check.issue_id = "logic_c_farming_land_availability_in_settlement_no_9",
         i.check.issue = glue("farming_land_availability: {farming_land_availability}, but reason_hh_member_travel_back_to_settlement: 
                              {reason_hh_member_travel_back_to_settlement}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_farming_land_availability_in_settlement_9")


# HH reports members travel back to settlement 'to run business(es)', but do not report 'own business' as a livelihood i.e. 
# reason_hh_member_travels_back_to_settlement = 'to_run_businesses' AND
# (not(selected(${hh_primary_livelihood}, "own_business_non_farming")) AND not(selected(${other_livelihoods_hh_engaged_in}, "own_business_non_farming")) 

df_reason_travel_back_to_settlement_business_10 <- df_tool_data %>% 
  filter(str_detect(string = reason_hh_member_travel_back_to_settlement, pattern = "to_run_businesses"), 
                             !str_detect(string = hh_primary_livelihood, pattern = "own_business_non_farming") |
                             !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "own_business_non_farming")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_hh_member_travel_back_to_settlement",
         i.check.current_value = as.character(reason_hh_member_travel_back_to_settlement),
         i.check.value = "",
         i.check.issue_id = "logic_c_reason_hh_member_travel_back_to_settlement_business_10",
         i.check.issue = glue("reason_hh_member_travel_back_to_settlement: {reason_hh_member_travel_back_to_settlement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'own business' selected as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reason_travel_back_to_settlement_business_10")

# HH reports members travel back to settlement to 'work on land of others', but do not report 'crop production on land of others' OR 
# 'livestock farming on land of others' as a livelihood i.e. type_work_done_by_close_family_member_in_settlement = 'work_on_land_of_others' AND
# (not(selected(${hh_primary_livelihood}, "crop_production_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_land_of_others")) AND
# not(selected(${hh_primary_livelihood}, "livestock_farming_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others")))

df_type_work_done_by_family_in_settlement_farming_11 <- df_tool_data %>% 
filter(str_detect(string = type_work_done_by_close_family_member_in_settlement, pattern = "work_on_land_of_others"), 
                                       !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_land_of_others") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_land_of_others") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "type_work_done_by_close_family_member_in_settlement",
         i.check.current_value = as.character(type_work_done_by_close_family_member_in_settlement),
         i.check.value = "",
         i.check.issue_id = "logic_c_type_work_done_by_family_in_settlement_farming_11",
         i.check.issue = glue("type_work_done_by_close_family_member_in_settlement: {type_work_done_by_close_family_member_in_settlement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'crop production on land of others' or/and 'livestock farming on land of others' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_type_work_done_by_family_in_settlement_farming_11")


# HH reports members travel back to settlement for 'non-agricultural daily labour', but did not report 'casual or daily labour' OR 
# 'salaried employment with a business' as a livelihood i.e. type_work_done_by_close_family_member_in_settlement = 'non_agricultural_daily_labour' AND
# (not(selected(${hh_primary_livelihood}, "casual_or_daily_labour_non_farming")) AND not(selected(${other_livelihoods_hh_engaged_in}, "casual_or_daily_labour_non_farming")) AND
# not(selected(${hh_primary_livelihood}, "salaried_employment_in_a_business")) AND not(selected(${other_livelihoods_hh_engaged_in}, "salaried_employment_in_a_business")))

df_type_work_done_by_family_in_settlement_non_agric_12 <- df_tool_data %>% 
  filter(str_detect(string = type_work_done_by_close_family_member_in_settlement, pattern = "non_agricultural_daily_labour"), 
                                       !str_detect(string = hh_primary_livelihood, pattern = "casual_or_daily_labour_non_farming") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "casual_or_daily_labour_non_farming") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_in_a_business") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_in_a_business")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "type_work_done_by_close_family_member_in_settlement",
         i.check.current_value = as.character(type_work_done_by_close_family_member_in_settlement),
         i.check.value = "",
         i.check.issue_id = "logic_c_type_work_done_by_family_in_settlement_non_agric_12",
         i.check.issue = glue("type_work_done_by_close_family_member_in_settlement: {type_work_done_by_close_family_member_in_settlement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'casual or daily labour' or/and 'salaried employment in a business' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_type_work_done_by_family_in_settlement_non_agric_12")


# HH reports members travel back to settlement for 'permanent salaried job', but did not report 'salaried employment with a business' OR 
# 'salaried employment with an NGO' OR 'salaried employment with the government' as a livelihood i.e. type_work_done_by_close_family_member_in_settlement = 'permanent_salaried_job'
# AND (not(selected(${hh_primary_livelihood}, "salaried_employment_in_a_business")) AND not(selected(${other_livelihoods_hh_engaged_in}, "salaried_employment_in_a_business")) AND
# not(selected(${hh_primary_livelihood}, "salaried_employment_with_an_ngo")) AND not(selected(${other_livelihoods_hh_engaged_in}, "salaried_employment_with_an_ngo")) AND 
# not(selected(${hh_primary_livelihood}, "salaried_employment_with_the_government")) AND not(selected(${other_livelihoods_hh_engaged_in}, "salaried_employment_with_the_government"))

df_type_work_done_by_family_in_settlement_salaried_job_13 <- df_tool_data %>% 
  filter(str_detect(string = type_work_done_by_close_family_member_in_settlement, pattern = "permanent_salaried_job"), 
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_in_a_business") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_in_a_business") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_with_an_ngo") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_with_an_ngo") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_with_the_government") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_with_the_government")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "type_work_done_by_close_family_member_in_settlement",
         i.check.current_value = as.character(type_work_done_by_close_family_member_in_settlement),
         i.check.value = "",
         i.check.issue_id = "logic_c_type_work_done_by_family_in_settlement_salaried_job_13",
         i.check.issue = glue("type_work_done_by_close_family_member_in_settlement: {type_work_done_by_close_family_member_in_settlement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'salaried employment in a business', 'salaried employment with an NGO' or/and 
                              'salaried employment with the government' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_type_work_done_by_family_in_settlement_salaried_job_13")


# HH reports members travel to urban centers 'to work on own land', but do not report 'crop production on own land' OR 'livestock farming on own land' 
# as a livelihood i.e. reason_hh_member_travels_to_towns = 'to_work_on_own_land'
# AND (not(selected(${hh_primary_livelihood}, "crop_production_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_own_land")) AND
# not(selected(${hh_primary_livelihood}, "livestock_farming_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_own_land")))

df_reason_hh_member_travels_to_towns_farming_own_land_14 <- df_tool_data %>% 
  filter(str_detect(string = reason_hh_member_travels_to_towns, pattern = "to_work_on_own_land"), 
                                     !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_own_land") |
                                     !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_own_land") |
                                     !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land") |
                                     !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_hh_member_travels_to_towns",
         i.check.current_value = as.character(reason_hh_member_travels_to_towns),
         i.check.value = "",
         i.check.issue_id = "logic_c_reason_hh_member_travels_to_towns_farming_own_land_14",
         i.check.issue = glue("reason_hh_member_travels_to_towns: {reason_hh_member_travels_to_towns}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'crop production on own land' or/and 'livestock farming on own land' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reason_hh_member_travels_to_towns_farming_own_land_14")


# HH reports members travel to urban centres 'to work on own land', but do not report having arable land i.e. 
# reason_hh_member_travels_to_towns = 'to_work_on_own_land' AND farming_land_availability = 'no' 

df_farming_land_availability_in_towns_15 <- df_tool_data %>% 
  filter(farming_land_availability == "no", !str_detect(string = reason_hh_member_travels_to_towns, pattern = "to_work_on_own_land")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "farming_land_availability",
         i.check.current_value = as.character(farming_land_availability),
         i.check.value = "",
         i.check.issue_id = "logic_c_farming_land_availability_in_towns_no_15",
         i.check.issue = glue("farming_land_availability: {farming_land_availability}, but reason_hh_member_travels_to_towns: 
                              {reason_hh_member_travels_to_towns}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_farming_land_availability_in_towns_15")


# HH reports members to urban centres 'to run business(es)', but do not report 'own business' as a livelihood i.e. reason_hh_member_travels_to_towns = 
# 'to_run_businesses' AND (not(selected(${hh_primary_livelihood}, "own_business_non_farming")) AND not(selected(${other_livelihoods_hh_engaged_in}, "own_business_non_farming"))


df_reason_hh_member_travels_to_towns_business_16 <- df_tool_data %>% 
  filter(str_detect(string = reason_hh_member_travels_to_towns, pattern = "to_run_businesses"),
                                   !str_detect(string = hh_primary_livelihood, pattern = "own_business_non_farming") |
                                   !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "own_business_non_farming")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_hh_member_travels_to_towns",
         i.check.current_value = as.character(reason_hh_member_travels_to_towns),
         i.check.value = "",
         i.check.issue_id = "logic_c_reason_hh_member_travels_to_towns_business_16",
         i.check.issue = glue("reason_hh_member_travels_to_towns: {reason_hh_member_travels_to_towns}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'Own business' selected as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reason_hh_member_travels_to_towns_business_16")


# HH reports members travel to urban centres to 'work on land of others', but do not report 'crop production on land of others' OR 
# 'livestock farming on land of others' as a livelihood i.e. type_work_done_by_hh_member_in_towns = 'work_on_land_of_others' AND
# (not(selected(${hh_primary_livelihood}, "crop_production_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_land_of_others")) AND
# not(selected(${hh_primary_livelihood}, "livestock_farming_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others")))

df_type_work_done_in_towns_farming_others_land_17 <- df_tool_data %>% 
  filter(str_detect(string = type_work_done_by_hh_member_in_towns, pattern = "work_on_land_of_others"), 
                                       !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_land_of_others") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_land_of_others") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "type_work_done_by_hh_member_in_towns",
         i.check.current_value = as.character(type_work_done_by_hh_member_in_towns),
         i.check.value = "",
         i.check.issue_id = "logic_c_type_work_done_by_hh_member_in_towns_farming_others_land_17",
         i.check.issue = glue("type_work_done_by_close_family_member_in_settlement: {type_work_done_by_close_family_member_in_settlement}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'crop production on land of others' or/and 'livestock farming on land of others' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_type_work_done_in_towns_farming_others_land_17") 


# HH reports members travel to urban centres for 'non-agricultural daily labour', but did not report 'casual or daily labour' OR 
#'salaried employment with a business' as a livelihood i.e. type_work_done_by_hh_member_in_towns = 'non_agricultural_daily_labour' AND
# (not(selected(${hh_primary_livelihood}, "casual_or_daily_labour_non_farming")) AND not(selected(${other_livelihoods_hh_engaged_in}, "casual_or_daily_labour_non_farming")) AND
# not(selected(${hh_primary_livelihood}, "salaried_employment_in_a_business")) AND not(selected(${other_livelihoods_hh_engaged_in}, "salaried_employment_in_a_business")))

df_type_work_done_in_towns_non_agric_labour_18 <- df_tool_data %>% 
  filter(str_detect(string = type_work_done_by_hh_member_in_towns, pattern = "non_agricultural_daily_labour"), 
                                       !str_detect(string = hh_primary_livelihood, pattern = "casual_or_daily_labour_non_farming") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "casual_or_daily_labour_non_farming") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_in_a_business") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_in_a_business")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "type_work_done_by_hh_member_in_towns",
         i.check.current_value = as.character(type_work_done_by_hh_member_in_towns),
         i.check.value = "",
         i.check.issue_id = "logic_c_type_work_done_by_hh_member_in_towns_non_agric_labour_18",
         i.check.issue = glue("type_work_done_by_hh_member_in_towns: {type_work_done_by_hh_member_in_towns}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'casual or daily labour' or/and 'salaried employment in a business' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_type_work_done_in_towns_non_agric_labour_18")


# HH reports members travel to urban centres for 'permanent salaried job', but did not report 'salaried employment with a business' OR 
# 'salaried employment with an NGO' OR 'salaried employent with the government' as a livelihood i.e. 
# type_work_done_by_hh_member_in_towns = 'permanent_salaried_job' AND
# (not(selected(${hh_primary_livelihood}, "casual_or_daily_labour_non_farming")) AND not(selected(${other_livelihoods_hh_engaged_in}, "casual_or_daily_labour_non_farming")) AND
# not(selected(${hh_primary_livelihood}, "salaried_employment_in_a_business")) AND not(selected(${other_livelihoods_hh_engaged_in}, "salaried_employment_in_a_business")))

df_type_work_done_by_hh_member_in_towns_salaried_job_19 <- df_tool_data %>% 
  filter(str_detect(string = type_work_done_by_hh_member_in_towns, pattern = "permanent_salaried_job"), 
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_in_a_business") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_in_a_business") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_with_an_ngo") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_with_an_ngo") |
                                       !str_detect(string = hh_primary_livelihood, pattern = "salaried_employment_with_the_government") |
                                       !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "salaried_employment_with_the_government")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "type_work_done_by_hh_member_in_towns",
         i.check.current_value = as.character(type_work_done_by_hh_member_in_towns),
         i.check.value = "",
         i.check.issue_id = "logic_c_type_work_done_by_hh_member_in_towns_salaried_job_19",
         i.check.issue = glue("type_work_done_by_hh_member_in_towns: {type_work_done_by_hh_member_in_towns}, but hh_primary_livelihood or 
                              other_livelihoods_hh_engaged_in has no 'salaried employment in a business', 'salaried employment with an NGO' or/and 
                              'salaried employment with the government' as options"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_type_work_done_by_hh_member_in_towns_salaried_job_19")


# HH reports no or low income (below MEB), but also reports to be able to meet all their basic needs i.e. 
# HH reports high income (above MEB), but also reports to not be able to meet their basic needs
# HH reports no or low income (below MEB), but also reports to have not used any coping strategies
# HH reports to be able to meet all of their needs, but also reports to have used coping strategies i.e. hh_able_to_meet_basic_needs = 'yes'
# AND [ANY grp_lcsi rows] = 'yes' 

df_hh_able_to_meet_basic_needs_23 <- df_tool_data %>% 
  filter(hh_able_to_meet_basic_needs == "yes", 
         if_any(c(increase_the_number_of_family_members_searching_for_work_outside_your_village:sold_more_animals_than_usual), ~ . == "yes")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hh_able_to_meet_basic_needs",
         i.check.current_value = as.character(hh_able_to_meet_basic_needs),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_able_to_meet_basic_needs_yes_23",
         i.check.issue = glue("hh_able_to_meet_basic_needs: {hh_able_to_meet_basic_needs}, 
                              but household also reports adopting coping strategies for survival"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_able_to_meet_basic_needs_23")


# HH reports not being able to meet all needs, but also reports to not have used coping strategies i.e. hh_able_to_meet_basic_needs = 'no'
# AND [all grp_lcsi rows] = 'no' 

df_hh_not_able_to_meet_basic_needs_24 <- df_tool_data %>%
  filter(hh_able_to_meet_basic_needs == "no", 
         if_all(c(increase_the_number_of_family_members_searching_for_work_outside_your_village:sold_more_animals_than_usual), ~ . == "no")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hh_able_to_meet_basic_needs",
         i.check.current_value = as.character(hh_able_to_meet_basic_needs),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_able_to_meet_basic_needs_no_24",
         i.check.issue = glue("hh_able_to_meet_basic_needs: {hh_able_to_meet_basic_needs}, 
                              but household does not report adopting any coping strategies for survival"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_not_able_to_meet_basic_needs_24")


# HH reports that 'child is too young' as a reason for non-attendance i.e. why_child_not_regularly_attending = 'child_or_children_were_too_young'

df_why_child_not_regularly_attending_25 <- df_tool_data %>% 
  filter(why_child_not_regularly_attending == "child_or_children_were_too_young") %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "why_child_not_regularly_attending",
         i.check.current_value = as.character(why_child_not_regularly_attending),
         i.check.value = "",
         i.check.issue_id = "logic_c_why_child_not_regularly_attending_25",
         i.check.issue = glue("why_child_not_regularly_attending: {why_child_not_regularly_attending}, 
                              check child age"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Verify age of non-attending school and IF child is 6, delete enrollment & attendance data for the child", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_why_child_not_regularly_attending_25")


# HH reports haven taken out a loan from a bank OR having access to loans from a bank, but reports not having access to banks i.e.
# (name_fsp_hh_sought_loan = 'bank' OR name_fsp_hh_to_seek_loan = 'bank') AND
# not(selected(financial_service_providers_present, any('banking_agents', 'banks_full_service'))))

df_hh_access_bank_loans_26 <- df_tool_data %>% 
  filter(str_detect(string = name_fsp_hh_sought_loan, pattern = "bank") |
                                str_detect(string = name_fsp_hh_to_seek_loan, pattern = "bank"),
                               !str_detect(string = financial_service_providers_present, pattern = "banking_agents") |
                               !str_detect(string = financial_service_providers_present, pattern = "banks_full_service")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "financial_service_providers_present",
         i.check.current_value = as.character(financial_service_providers_present),
         i.check.value = "",
         i.check.issue_id = "logic_c_financial_service_providers_present_bank_no_26",
         i.check.issue = glue("name_fsp_hh_sought_loan: {name_fsp_hh_sought_loan} or name_fsp_hh_to_seek_loan: {name_fsp_hh_to_seek_loan} 
                              but financial_service_providers_present has no 'banking_agents' or 'banks_full_service' selected as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_access_bank_loans_26")


# HH reports haven taken out a loan from a MFI OR having access to loans from a MFI, but reports not having access to MFIs i.e.
# (name_fsp_hh_sought_loan = 'mfi' OR name_fsp_hh_to_seek_loan = 'mfi') AND not(selected(financial_service_providers_present, 'mfi'))

df_hh_access_mfi_loans_27 <- df_tool_data %>% 
  filter(str_detect(string = name_fsp_hh_sought_loan, pattern = "mfi") |
                                          str_detect(string = name_fsp_hh_to_seek_loan, pattern = "mfi"),
                                         !str_detect(string = financial_service_providers_present, pattern = "mfi")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "financial_service_providers_present",
         i.check.current_value = as.character(financial_service_providers_present),
         i.check.value = "",
         i.check.issue_id = "logic_c_financial_service_providers_present_mfi_no_27",
         i.check.issue = glue("name_fsp_hh_sought_loan: {name_fsp_hh_sought_loan} or name_fsp_hh_to_seek_loan: {name_fsp_hh_to_seek_loan} 
                              but financial_service_providers_present has no 'microfinance institutions'selected as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_access_mfi_loans_27")


# HH reports haven taken out a loan from a VSLA OR having access to loans from a VSLA, but reports not having access to VSLAs i.e.
# (name_fsp_hh_sought_loan = 'vsla' OR name_fsp_hh_to_seek_loan = 'vsla') AND not(selected(financial_service_providers_present, 'vsla'))

df_hh_access_vsla_loans_28 <- df_tool_data %>% 
  filter(str_detect(string = name_fsp_hh_sought_loan, pattern = "vsla") |
                                     str_detect(string = name_fsp_hh_to_seek_loan, pattern = "vsla"),
                                    !str_detect(string = financial_service_providers_present, pattern = "vsla")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "financial_service_providers_present",
         i.check.current_value = as.character(financial_service_providers_present),
         i.check.value = "",
         i.check.issue_id = "logic_c_financial_service_providers_present_vsla_no_28",
         i.check.issue = glue("name_fsp_hh_sought_loan: {name_fsp_hh_sought_loan} or name_fsp_hh_to_seek_loan: {name_fsp_hh_to_seek_loan} 
                              but financial_service_providers_present has no 'vsla'selected as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_access_vsla_loans_28")


# HH reports haven taken out a loan from a SACCO OR having access to loans from a SACCO, but reports not having access to SACCOs i.e.
# (name_fsp_hh_sought_loan = 'sacco' OR name_fsp_hh_to_seek_loan = 'sacco') AND not(selected(financial_service_providers_present, 'sacco'))

df_hh_access_sacco_loans_29 <- df_tool_data %>% 
  filter(str_detect(string = name_fsp_hh_sought_loan, pattern = "sacco") |
                                       str_detect(string = name_fsp_hh_to_seek_loan, pattern = "sacco"),
                                      !str_detect(string = financial_service_providers_present, pattern = "sacco")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "financial_service_providers_present",
         i.check.current_value = as.character(financial_service_providers_present),
         i.check.value = "",
         i.check.issue_id = "logic_c_financial_service_providers_present_sacco_no_29",
         i.check.issue = glue("name_fsp_hh_sought_loan: {name_fsp_hh_sought_loan} or name_fsp_hh_to_seek_loan: {name_fsp_hh_to_seek_loan} 
                              but financial_service_providers_present has no 'sacco'selected as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_access_sacco_loans_29")


# HH reports haven taken out a loan from a local business or community member OR having access to loans from a local business or 
# community member, but reports not having access to financial services provided by local businesses or community members i.e. 
# (name_fsp_hh_sought_loan = 'local_business_or_community_member' OR name_fsp_hh_to_seek_loan = 'local_business_or_community_member')AND
# not(selected(financial_service_providers_present, 'financial_services_provided_by_local_businesses_or_community_members'))

df_hh_access_local_business_or_community_member_loans_30 <- df_tool_data %>% 
  filter(str_detect(string = name_fsp_hh_sought_loan, pattern = "local_business_or_community_member") |
           str_detect(string = name_fsp_hh_to_seek_loan, pattern = "local_business_or_community_member"),
         !str_detect(string = financial_service_providers_present, pattern = "financial_services_provided_by_local_businesses_or_community_members")) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "financial_service_providers_present",
         i.check.current_value = as.character(financial_service_providers_present),
         i.check.value = "",
         i.check.issue_id = "logic_c_financial_service_providers_present_local_business_no_29",
         i.check.issue = glue("name_fsp_hh_sought_loan: {name_fsp_hh_sought_loan} or name_fsp_hh_to_seek_loan: {name_fsp_hh_to_seek_loan} 
                              but financial_service_providers_present has no 'Financial services provided by local businesses or community members'selected as an option"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_access_local_business_or_community_member_loans_30")


# Respondent gives the same answer to all intra-group social cohesion questions for refugee i.e [all grp_intra_group_social_cohesion rows] = [same answer]

df_intra_group_social_cohesion_refugee_31a <- df_tool_data %>% 
  filter(joining_other_refugee_to_address_issues == trust_among_refugee & trust_among_refugee == friendliness_between_refugee & friendliness_between_refugee == 
            sense_of_belonging_to_refugee_community & sense_of_belonging_to_refugee_community == taken_advantage_of_by_fellow_refugee) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "joining_other_refugee_to_address_issues",
         i.check.current_value = as.character(joining_other_refugee_to_address_issues),
         i.check.value = "",
         i.check.issue_id = "logic_c_intra_group_social_cohesion_refugee_31a",
         i.check.issue = glue("All intra-group social cohesion questions have similar response"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_intra_group_social_cohesion_refugee_31a")


# Respondent gives the same answer to all intra-group social cohesion questions for host i.e [all grp_intra_group_social_cohesion rows] = [same answer]

df_intra_group_social_cohesion_host_31b <- df_tool_data %>% 
  filter(joining_other_nationals_to_address_issues == trust_among_other_nationals & trust_among_other_nationals == friendliness_among_nationals &
         friendliness_among_nationals == sense_of_belonging_to_community_here & sense_of_belonging_to_community_here == taken_advantage_of_by_other_nationals) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "joining_other_nationals_to_address_issues",
         i.check.current_value = as.character(joining_other_nationals_to_address_issues),
         i.check.value = "",
         i.check.issue_id = "logic_c_intra_group_social_cohesion_host_31b",
         i.check.issue = glue("All intra-group social cohesion questions have similar response"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check.")) %>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_intra_group_social_cohesion_host_31b")


# Respondent gives the same answer to all inter-group social cohesion questions for refugee i.e. [all grp_inter_group_social_cohesion rows] = [same answer]

 df_inter_group_social_cohesion_refugee_32a <- df_tool_data %>% 
  filter(joining_any_nationals_to_address_issues == trust_among_nationals & trust_among_nationals == friendliness_among_refugee_and_nationals &
           friendliness_among_refugee_and_nationals == sense_of_belonging_both_refugee_and_nationals & sense_of_belonging_both_refugee_and_nationals == 
           taken_advantage_of_by_nationals) %>% 
  mutate(i.check.type = "change_response",
        i.check.name = "joining_any_nationals_to_address_issues",
        i.check.current_value = as.character(joining_any_nationals_to_address_issues),
        i.check.value = "",
        i.check.issue_id = "logic_c_inter_group_social_cohesion_refugee_32a",
        i.check.issue = glue("All inter-group social cohesion questions have similar response"),
        i.check.other_text = "",
        i.check.checked_by = "",
        i.check.checked_date = as_date(today()),
        i.check.comment = "", 
        i.check.reviewed = "",
        i.check.adjust_log = "",
        i.check.so_sm_choices = "") %>% 
 dplyr::select(starts_with("i.check.")) %>% 
 rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

 add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_inter_group_social_cohesion_refugee_32a")


 # Respondent gives the same answer to all inter-group social cohesion questions for host community i.e. [all grp_inter_group_social_cohesion rows] = [same answer]
 
 df_inter_group_social_cohesion_host_32b <- df_tool_data %>% 
 filter(joining_any_refugee_to_address_issues == trust_any_refugee & trust_any_refugee == friendliness_among_refugee & 
            friendliness_among_refugee == sense_of_belonging_to_both_refugee_and_nationals & sense_of_belonging_to_both_refugee_and_nationals == 
            taken_advantage_of_by_any_refugee) %>% 
 mutate(i.check.type = "change_response",
        i.check.name = "joining_any_refugee_to_address_issues",
        i.check.current_value = as.character(joining_any_refugee_to_address_issues),
        i.check.value = "",
        i.check.issue_id = "logic_c_inter_group_social_cohesion_host_32b",
        i.check.issue = glue("All inter-group social cohesion questions have similar response"),
        i.check.other_text = "",
        i.check.checked_by = "",
        i.check.checked_date = as_date(today()),
        i.check.comment = "", 
        i.check.reviewed = "",
        i.check.adjust_log = "",
        i.check.so_sm_choices = "") %>% 
 dplyr::select(starts_with("i.check.")) %>% 
 rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_inter_group_social_cohesion_host_32b")

# HH reports to not have a shelter, but reports any occupancy status other than squatting i.e. shelter_type_hh_live = 'none'
# AND shelter_occupancy_arrangement != 'squatting' AND shelter_occupancy_arrangement != 'no_answer'

df_shelter_type_hh_live_33 <- df_tool_data %>% 
  filter(!shelter_occupancy_arrangement %in% c("squatting", "no_answer"), 
                                       shelter_type_hh_live =="none") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "shelter_type_hh_live",
         i.check.current_value = as.character(shelter_type_hh_live),
         i.check.value = "",
         i.check.issue_id = "logic_c_shelter_type_hh_live_none_33",
         i.check.issue = glue("shelter_type_hh_live: {shelter_type_hh_live},  but shelter_occupancy_arrangement: 
                              {shelter_occupancy_arrangement}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_shelter_type_hh_live_33")


# HH entered the same value for all FCS categories i.e. cereals == pulses == vegetables == fruits == tubers == protein == dairy == sugar == oils

df_grp_fd_consumption_score_same_34 <- df_tool_data %>% 
  filter(cereals == pulses, vegetables == cereals, cereals == fruits, cereals == tubers, cereals == protein, protein == dairy,  
         cereals == sugar, cereals == oils) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "cereals",
         i.check.current_value = as.character(cereals),
         i.check.value = "",
         i.check.issue_id = "logic_c_grp_fd_consumption_score_same_34",
         i.check.issue = glue("All questions about foods eaten in the past seven days have similar response"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_grp_fd_consumption_score_same_34")


# HH got a total FCS of 10 or less i.e. cereals/tubers * 2 + pulses * 3 + vegetables + fruits + protein *4 + dairy *4 + sugar * 0.5 + oils * 0.5 <=10
# Can be done in excel 

df_fd_consumption_score_less_than_ten_35 <- df_tool_data %>% 
  filter(((cereals > tubers | tubers > cereals) * 2 + pulses * 3 + (vegetables +  fruits + protein) * 4 + dairy * 4 + sugar * 0.5 + oils * 0.5) <= 10) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "cereals",
         i.check.current_value = as.character(cereals),
         i.check.value = "",
         i.check.issue_id = "logic_c_fd_consumption_score_less_than_ten_35",
         i.check.issue = glue("Food consumption score is less than ten"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_fd_consumption_score_less_than_ten_35")


# HH entered 0 for at least 3 categories of the FCS i.e. [at least 3 grp_fcs groups] = 0


df_three_or_more_fcs_equal_zero_36 <- df_tool_data %>%
    mutate(three_fcs_gps_equal_zero = rowSums(across(cereals:oils) == 0, na.rm = TRUE)) %>% 
    filter(three_fcs_gps_equal_zero >= 3) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "three_fcs_gps_equal_zero",
           i.check.current_value = as.numeric(three_fcs_gps_equal_zero),
           i.check.value = "",
           i.check.issue_id = "logic_c_three_or_more_fcs_equal_zero_36",
           i.check.issue = glue("Household has not eaten three or more food categories in past seven days"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_three_or_more_fcs_equal_zero_36")


# HH got poor FCS, but reported to never have used coping strategies i.e. cereals/tubers * 2 + pulses * 3 + vegetables + fruits + protein *4 + 
# dairy *4 + sugar * 0.5 + oils * 0.5 <=21 AND [all grp_lcsi rows] = 'no' 

df_fd_consumption_score_poor_37 <- df_tool_data %>% 
  mutate(poor_fcs = (cereals > tubers | tubers > cereals) * 2 + pulses * 3 + (vegetables +  fruits + protein) * 4 + dairy * 4 + sugar * 0.5 + oils * 0.5, 
       round(0)) %>% 
  filter(poor_fcs <= 21, if_all(c(increase_the_number_of_family_members_searching_for_work_outside_your_village:sold_more_animals_than_usual), ~ . != "yes")) %>% 
mutate(i.check.type = "change_response",
       i.check.name = "poor_fcs",
       i.check.current_value = as.numeric(poor_fcs),
       i.check.value = "",
       i.check.issue_id = "logic_c_fd_consumption_score_poor_37",
       i.check.issue = glue("Household has poor fcs but has not reported any coping strategy"),
       i.check.other_text = "",
       i.check.checked_by = "",
       i.check.checked_date = as_date(today()),
       i.check.comment = "", 
       i.check.reviewed = "",
       i.check.adjust_log = "",
       i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_fd_consumption_score_poor_37")


# HH got poor FCS, but reported to be able to meet all their needs i.e. cereals/tubers * 2 + pulses * 3 + vegetables + fruits + protein *4 + dairy *4 + 
# sugar * 0.5 + oils * 0.5 <=21 AND hh_able_to_meet_basic_needs = 'yes'

df_poor_fcs_but_meets_needs_38 <- df_tool_data %>% 
  mutate(poor_fcs = (cereals > tubers | tubers > cereals) * 2 + pulses * 3 + (vegetables +  fruits + protein) * 4 + dairy * 4 + sugar * 0.5 + oils * 0.5, 
         round(0)) %>% 
  filter(poor_fcs <= 21, hh_able_to_meet_basic_needs == "yes") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hh_able_to_meet_basic_needs",
         i.check.current_value = as.character(hh_able_to_meet_basic_needs),
         i.check.value = "",
         i.check.issue_id = "logic_c_poor_fcs_but_meets_needs_38",
         i.check.issue = glue("hh_able_to_meet_basic_needs: {hh_able_to_meet_basic_needs}, 
                              but hh does not have enough food"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_poor_fcs_but_meets_needs_38")


# Remittances reported under income, but not said to receive remittances under movement section i.e. income_remittances > 0 
# AND hh_receiving_money_from_family_or_friend_in_settlement = 'no' AND hh_receiving_money_from_family_or_friend_in_towns = 'no' AND
# hh_receiving_money_from_family_or_friend_in_home_country = 'no'

df_hh_reported_remittances_39 <- df_tool_data %>% 
    filter(income_remittances > 0, if_all(c(hh_receiving_money_from_family_or_friend_in_settlement, hh_receiving_money_from_family_or_friend_in_towns, 
                                            hh_receiving_money_from_family_or_friend_in_home_country), ~ . == "no")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "income_remittances",
         i.check.current_value = as.numeric(income_remittances),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_reported_remittances_39",
         i.check.issue = glue("income_remittances: {income_remittances}, 
                              but hh has not reported receiving remittances under movement section"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_reported_remittances_39")


# No remittances report under income, but regular remittances reported under movement section i.e. income_remittances = 0 AND
# (frequency_hh_receives_money_from_family_or_friend_in_settlement = 'approximately_once_a_month' or 'approximately_twice_a_month' or 
# 'approximately_3_times_a_month'  or 'approximately_more_than_3_times_a_month' 
# OR frequency_hh_receives_money_from_family_or_friend_in_towns = 'approximately_once_a_month' or 'approximately_twice_a_month' or 
# 'approximately_3_times_a_month'  or 'approximately_more_than_3_times_a_month'  
# OR frequency_hh_receives_money_from_family_or_friend_in_home_country = 'approximately_once_a_month' or 'approximately_twice_a_month' or 
#'approximately_3_times_a_month'  or 'approximately_more_than_3_times_a_month' )

df_hh_reported_no_remittances_40 <- df_tool_data %>% 
  filter(income_remittances == 0, (frequency_hh_receives_money_from_family_or_friend_in_settlement == "approximately_once_a_month" |
                                   frequency_hh_receives_money_from_family_or_friend_in_settlement == "approximately_twice_a_month"|
                                   frequency_hh_receives_money_from_family_or_friend_in_settlement == "approximately_3_times_a_month" |
                                   frequency_hh_receives_money_from_family_or_friend_in_settlement == "approximately_more_than_3_times_a_month")|
                                  (frequency_hh_receives_money_from_family_or_friend_in_towns == "approximately_once_a_month" |
                                   frequency_hh_receives_money_from_family_or_friend_in_towns == "approximately_twice_a_month"|
                                   frequency_hh_receives_money_from_family_or_friend_in_towns == "approximately_3_times_a_month" |
                                   frequency_hh_receives_money_from_family_or_friend_in_towns == "approximately_more_than_3_times_a_month")|
                                  (frequency_hh_receives_money_from_family_or_friend_in_home_country == "approximately_once_a_month" |
                                   frequency_hh_receives_money_from_family_or_friend_in_home_country == "approximately_twice_a_month"|
                                   frequency_hh_receives_money_from_family_or_friend_in_home_country == "approximately_3_times_a_month" |
                                   frequency_hh_receives_money_from_family_or_friend_in_home_country == "approximately_more_than_3_times_a_month")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "income_remittances",
         i.check.current_value = as.numeric(income_remittances),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_reported_no_remittances_40",
         i.check.issue = glue("income_remittances: {income_remittances}, 
                              but hh has reported frequently receiving remittances under movement section"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_reported_no_remittances_40")


# HH reports to have farm assets, but reports 0 for all items i.e. own_farm_land_items = 'yes' AND [all farm_items rows] = 0

df_hh_own_farm_land_items_41 <- df_tool_data %>% 
  filter(own_farm_land_items == "yes", if_all(c(hoe:water_pump), ~ . == 0)) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "own_farm_land_items",
         i.check.current_value = as.character(own_farm_land_items),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_own_farm_land_items_41",
         i.check.issue = glue("income_remittances: {income_remittances}, 
                              but hh has not reported receiving remittances under movement section"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_own_farm_land_items_41")


# HH reports to have farm assets, but does not report to be engaged in agriculture as a livelihood i.e. own_farm_land_items = 'yes' AND
# (not(selected(${hh_primary_livelihood}, "crop_production_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_own_land")) AND
# not(selected(${hh_primary_livelihood}, "crop_production_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_land_of_others")) AND
# not(selected(${hh_primary_livelihood}, "livestock_farming_on_own_land")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_own_land")) AND
# not(selected(${hh_primary_livelihood}, "livestock_farming_on_land_of_others")) AND not(selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others")))


df_hh_own_farm_assets_42 <- df_tool_data %>% 
  filter(own_farm_land_items == "yes", 
                         !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_own_land"),
                         !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_own_land"),
                         !str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_land_of_others"),
                         !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_land_of_others"),
                         !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land"),
                         !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land"),
                         !str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others"),
                         !str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "own_farm_land_items",
         i.check.current_value = as.character(own_farm_land_items),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_own_farm_assets_42",
         i.check.issue = glue("own_farm_land_items: {own_farm_land_items}, but hh has not reported to be engaged in agriculture as a livelihood"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_own_farm_assets_42")


# HH reports to not have farm assets, but does report to have agriculture as a livelihood i.e. own_farm_land_items != 'yes' AND
# selected(${hh_primary_livelihood}, "crop_production_on_own_land") OR selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_own_land") OR
# selected(${hh_primary_livelihood}, "crop_production_on_land_of_others") OR selected(${other_livelihoods_hh_engaged_in}, "crop_production_on_land_of_others") OR
# selected(${hh_primary_livelihood}, "livestock_farming_on_own_land") OR selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_own_land") OR
# selected(${hh_primary_livelihood}, "livestock_farming_on_land_of_others") OR selected(${other_livelihoods_hh_engaged_in}, "livestock_farming_on_land_of_others"))

df_hh_own_no_farm_assets_43 <- df_tool_data %>% 
  filter(own_farm_land_items != "yes", 
                       str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_own_land") |
                       str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_own_land") |
                       str_detect(string = hh_primary_livelihood, pattern = "crop_production_on_land_of_others") |
                       str_detect(string = other_livelihoods_hh_engaged_in, pattern = "crop_production_on_land_of_others") |
                       str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_own_land") |
                       str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_own_land") |
                       str_detect(string = hh_primary_livelihood, pattern = "livestock_farming_on_land_of_others") |
                       str_detect(string = other_livelihoods_hh_engaged_in, pattern = "livestock_farming_on_land_of_others")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "own_farm_land_items",
         i.check.current_value = as.character(own_farm_land_items),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_own_no_farm_assets_43",
         i.check.issue = glue("own_farm_land_items: {own_farm_land_items}, but hh reports to be engaged in agriculture as a livelihood"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_own_no_farm_assets_43")


# Same amount entered for all farm assets i.e. hoe = axe = spraying_machine = shovel = pick_axe = sickle = 
# rake = cart = tractor = conventional_yoke = ox_plough = wheelbarrow = panga_slasher = pruning_knife = water_pump

df_all_farm_assets_response_same_44 <- df_tool_data %>% 
  filter(hoe==axe, spraying_machine==hoe, shovel==hoe, pick_axe==hoe, sickle==hoe, rake==hoe, cart==hoe, tractor==hoe, conventional_yoke==hoe, 
         ox_plough==hoe, wheelbarrow==hoe, panga_slasher==hoe, pruning_knife==hoe, water_pump==hoe) %>% 
 mutate(i.check.type = "change_response",
         i.check.name = "panga_slasher",
         i.check.current_value = as.numeric(panga_slasher),
         i.check.value = "",
         i.check.issue_id = "logic_c_all_farm_assets_response_same_44",
         i.check.issue = glue("panga_slasher: {panga_slasher}, 
                              all values are the same like for panga"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_all_farm_assets_response_same_44")


# Same amount entered for all non-farm assets i.e. cell_phone = radio = car = tv = motorcycle = bicycle = solar_panel = fridge = generator = 
# cooker_or_stove = iron_box_or_pressing = computer_or_laptop = tables = chairs_or_stools_or_benches = beds = mattresses = mosquito_nets = jerrycan

df_all_non_farm_assets_response_same_45<- df_tool_data %>% 
  filter(cell_phone==tv, radio==tv, car==tv, motorcycle==tv, bicycle==tv, solar_panel==tv, fridge==tv, generator==tv, cooker_or_stove==tv, iron_box_or_pressing==tv, 
         computer_or_laptop==tv, tables==tv, chairs_or_stools_or_benches==tv, beds==tv, mattresses==tv, mosquito_nets==tv, jerrycan==tv) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "cell_phone",
         i.check.current_value = as.numeric(cell_phone),
         i.check.value = "",
         i.check.issue_id = "logic_c_all_non_farm_assets_response_same_45",
         i.check.issue = glue("cell_phone: {cell_phone}, 
                              all values are the same like for cell_phone"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_all_non_farm_assets_response_same_45")


# Amount of generators in unreasonable i.e. generator > 1  

df_generator_greater_than_one_46 <- df_tool_data %>% 
  filter(generator > 1) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "generator",
         i.check.current_value = as.numeric(cell_phone),
         i.check.value = "",
         i.check.issue_id = "logic_c_generator_greater_than_one_46",
         i.check.issue = glue("generator: {generator}, confirm hh has more than one generator"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_generator_greater_than_one_46")


# Amount of fridges is unreasonable i.e. fridge  > 1  

df_fridge_greater_than_one_47 <- df_tool_data %>% 
  filter(fridge  > 1) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "fridge ",
         i.check.current_value = as.numeric(cell_phone),
         i.check.value = "",
         i.check.issue_id = "logic_c_generator_greater_than_one_46",
         i.check.issue = glue("fridge : {fridge}, confirm hh has more than one fridge"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_fridge_greater_than_one_47")

















 write_csv(x = df_fd_consumption_score_poor_37, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_livelihood.csv"), na = "")

# combine and output checks -----------------------------------------------

# combine checks
df_combined_checks <- bind_rows(logic_output)

# output the combined checks
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_livelihood.csv"), na = "")




  