# Function checks if the mean values that were entered in the dataset for NOSA & HOSA combined equal TSA for a given SpawningYear and CommonPopName 
QAQC_HOSA_NOSA_TSA <- function(data) {
  
  values_to_check <- c("HOSA", "NOSA", "TSA")
    
  if(all(sapply(values_to_check, function(val) any(str_detect(data$Param, val))))==FALSE){
    stop("**NOTICE - this QAQC function only works with datasets that contain 'Params' with all three values containing NOSA, HOSA, and TSA.**")
  }  
  
if (all("Age" %in% colnames(data) & any(filter_age %in% "total"))) {
  data %>%
    filter(Age == "Total") %>%
    group_by(SpawningYear, CommonPopName) %>%
    summarise(
      HOSA_NOSA_Mean = sum(Mean[grepl("HOSA|NOSA", Param)]),
      TSA_Mean = sum(Mean[grepl("TSA", Param)]),
      HOSA_NOSA_equals_TSA = HOSA_NOSA_Mean == TSA_Mean,
      .groups = 'drop'
    )
}else if ("Age" %in% colnames(data) ==FALSE){
  data %>%
    group_by(SpawningYear, CommonPopName) %>%
    summarise(
      HOSA_NOSA_Mean = sum(Mean[grepl("HOSA|NOSA", Param)]),
      TSA_Mean = sum(Mean[grepl("TSA", Param)]),
      HOSA_NOSA_equals_TSA = HOSA_NOSA_Mean == TSA_Mean,
      .groups = 'drop'
    )
}else{
  print("Param containing TSA does not exist in dataset entered into this function")
}
  
}