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
  filter(hh_able_to_meet_basic_needs == "yes", increase_the_number_of_family_members_searching_for_work_outside_your_village == "yes" | 
           sold_household_assets == "yes" | purchased_food_on_credit == "yes" | spent_savings == "yes" | borrowed_money == "yes" | 
           sold_productive_assets_or_means_of_transport == "yes" | reduced_expenditure_on_health_and_education == "yes" |
           withdrew_children_from_school == "yes" | begged_or_relied_on_charity == "yes" | sold_more_animals_than_usual == "yes") %>% 
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
  filter(hh_able_to_meet_basic_needs == "no", increase_the_number_of_family_members_searching_for_work_outside_your_village == "no" | 
           sold_household_assets == "no" | purchased_food_on_credit == "no" | spent_savings == "no" | borrowed_money == "no" | 
           sold_productive_assets_or_means_of_transport == "no" | reduced_expenditure_on_health_and_education == "no" |
           withdrew_children_from_school == "no" | begged_or_relied_on_charity == "no" | sold_more_animals_than_usual == "no") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "hh_able_to_meet_basic_needs",
         i.check.current_value = as.character(hh_able_to_meet_basic_needs),
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_able_to_meet_basic_needs_no_24",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_not_able_to_meet_basic_needs_24")









# combine and output checks -----------------------------------------------

# combine checks
df_combined_checks <- bind_rows(logic_output)

# output the combined checks
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_livelihood.csv"), na = "")
