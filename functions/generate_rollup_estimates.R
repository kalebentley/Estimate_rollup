# Function to summarize 'mean' by specified columns and filter by Age

generate_rollup_estimates <- function(data, summary_cols, filter_age = c("age", "total"), add_param = NULL) {

  # Define valid choices for filter_age
    valid_choices_filter_age <- c("age", "total")
  
  # Try to match add_param with valid choices
    tryCatch({
      filter_age <- match.arg(filter_age, choices = valid_choices_filter_age, several.ok = TRUE)
    }, error = function(e) {
      stop("**ERROR - Invalid option entered for argument add_param.  Valid options are NULL, c('age'), c('total'), or c('age','total').**")
    })

  # Define valid choices for add_param
    valid_choices_add_param <- c("pHOS", "pAge")

  # If add_param is NULL, set it to "blank"
    if (is.null(add_param)) {
      add_param <- "blank"
    } else {
      # Try to match add_param with valid choices
      tryCatch({
        add_param <- match.arg(add_param, choices = valid_choices_add_param, several.ok = TRUE)
      }, error = function(e) {
        stop("**ERROR - Invalid option entered for argument add_param.  Valid options are NULL, c('pHOS'), c('pAge'), or c('pHOS','pAge').**")
      })
    }

  if (identical(filter_age, "age")) {
    data_filt<- data %>% filter(Age != "Total")
    group_cols <- c(summary_cols, "Age")
  } else if (identical(filter_age,"total")) {
    data_filt <- data %>% filter(Age == "Total")
    group_cols <- summary_cols
  } else if (all(c("total", "age") %in% filter_age)) {
    data_filt<- data
    group_cols <- c(summary_cols, "Age")
  } else{
    handle_error("**ERROR: Something went wrong with filtering the data by age prior to L")
  } 
  
  summarized_abund_data <- 
    data_filt %>%
    group_by(across(all_of(group_cols))) %>%
    #summarize(mean_summary = sum(mean, na.rm = TRUE), .groups = 'drop')
    summarise("Mean" = sum(mean), "SD" = sqrt(sum(sd^2)), .groups = 'drop') %>%
    ungroup() %>%
    mutate(alpha = lognorm_parameters(Mean, SD)$alpha, beta = lognorm_parameters(Mean, SD)$beta ) %>%
    mutate(Median = exp(alpha), l95 = exp(alpha - 1.96*beta), u95 = exp(alpha + 1.96*beta), CV = SD/Mean) %>%
    select(any_of(group_cols), Mean, SD, l95, Median, u95, CV)
  
  if ("pHOS" %in% add_param) {
    hos_data <- data %>% filter(str_detect(Param, "HOS") & Age == "Total")
    tsa_data <- data %>% filter(str_detect(Param, "TSA"))
    
    pHOS_summary <- 
      hos_data %>%
      group_by(across(all_of(summary_cols))) %>%
      summarize(mean_hos = sum(mean, na.rm = TRUE), .groups = 'drop') %>%
      left_join(
        tsa_data %>%
          group_by(across(all_of(summary_cols))) %>%
          summarize(mean_tsa = sum(mean, na.rm = TRUE), .groups = 'drop') |> 
          select(-Param),
        by = summary_cols[summary_cols != "Param"]
      ) %>%
      #mutate(Mean = mean_hos / mean_tsa, Param = "pHOS") %>%
      mutate(
        Mean = mean_hos / mean_tsa,
        Param = case_when(
          str_detect(Param, "EJ|ej") ~ "pHOSej",
          str_detect(Param, "IJ|ij") ~ "pHOSij",
          TRUE ~ "pHOS"
        ), 
        Age = "Total"
      ) |> 
      select(-mean_hos, -mean_tsa)
    
  }else{
    pHOS_summary<-c() 
  }
  
  if ("pAge" %in% add_param) {
    age_total_byYearParam <- 
      data %>%
      filter(Age == "Total") |> 
      group_by(across(all_of(summary_cols))) %>%
      summarize(total_mean = sum(mean, na.rm = TRUE), .groups = 'drop')
    
    pAge_summary <- 
      data %>%
      filter(Age != "Total") %>%
      group_by(across(all_of(c(summary_cols, "Age")))) %>%
      summarize(mean_age = sum(mean, na.rm = TRUE), .groups = 'drop') %>%
      left_join(age_total_byYearParam, by = summary_cols) %>%
      mutate(Mean = mean_age / total_mean, 
             Param = paste(Param, paste0("Age", Age, "Prop"), sep = "_"),
             Age = NA
             ) %>%
      select(-mean_age, -total_mean)
  }else{
    pAge_summary<-c()
  }
  
  return(list(abund = summarized_abund_data, prop=bind_rows(pHOS_summary, pAge_summary)))
}