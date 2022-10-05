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

# for each survey, it finds the closest matching survey with the minimum number of different columns
calculateDifferences <- function(data, tool.survey){
  # 1) convert all columns to character
  data <- mutate_all(data, as.character)
  # 2) remove columns that are naturally different in each survey
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("_uuid", "enumerator_id")) &
             !str_starts(column, "_other$") &
             !str_detect(column, "_specify$"))
  data <- data[, all_of(cols$column)]
  # 3) convert NA to "NA" and all columns to factor
  data[is.na(data)] <- "NA"
  data <- data %>% mutate_if(is.character, factor)
  # 4) calculate gower distance
  gower_dist <- daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)
  # 5) convert distance to number of differences and determine closest matching survey
  r <- c()
  for (i in 1:nrow(data)) r <- c(r, sort(gower_mat[i,]*ncol(data))[2])
  # 6) store results
  data[["n.col.not.NA"]] <- rowSums(data!="NA")
  data[["survey.id"]] <- 1:dim(data)[1]
  data[["most.similar.survey"]] <- names(r)
  data[["number.different.columns"]] <- as.numeric(r)
  data <- data %>% arrange(number.different.columns, survey.id)
  return(data)
}