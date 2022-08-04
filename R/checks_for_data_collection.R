# checks for data collection

library(checksupporteR)
library(tidyverse)
library(lubridate)
library(glue)

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel(path = "inputs/livelihoods_assessment_data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = as.character(enumerator_id),
         i.check.district_name = district_name,
         i.check.point_number = point_number) %>% 
  filter(i.check.start_date > as_date("2022-08-07"))

df_survey <- readxl::read_excel(path = "inputs/livelihoods_assessment_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel(path = "inputs/livelihoods_assessment_tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/anif_rapid_settlement_samples.gpkg", quiet = TRUE)
