# average time of survey --------------------------------------------------

get_average_survey_time <- function(input_tool_data) {
  
  df_tool_data_with_time_interval <- input_tool_data %>% 
    mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
           int.survey_time_interval = ceiling(int.survey_time_interval)
    )%>% 
    filter(int.survey_time_interval > 0 ) %>% # consider only surveys with positive time interval
    select(int.survey_time_interval)
  
  # lower and upper quantiles of survey duration
  lower_limit = quantile(df_tool_data_with_time_interval$int.survey_time_interval, 0.025, na.rm =TRUE)
  upper_limit = quantile(df_tool_data_with_time_interval$int.survey_time_interval, 0.975, na.rm =TRUE)
  
  df_tool_data_with_time_interval %>% 
    filter(int.survey_time_interval > lower_limit | int.survey_time_interval < upper_limit) %>% 
    summarise(average_time = round(mean(int.survey_time_interval, na.rm = TRUE), 0)) %>% 
    pull()
}