#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate roll-up estimates for Lewis Basin tule Chinook (i.e., combining independent estimates from NF Lewis, EF Lewis, and Cedar Creek
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed by Kale Bentley
# Updated: March 18, 2025

#---------------------------------------------------------------------------------------------------------- -
# Load functions and packages                                                 ----
#---------------------------------------------------------------------------------------------------------- -  
# Load functions
  sapply(FUN = source, paste(getwd(), "functions", list.files("functions"), sep="/"))

# Load packages
  package_list<-c("here", "tidyverse", "openxlsx", "R2jags", "tidybayes", "shinystan", "glue") 
  install_or_load_pack(package_list)

#---------------------------------------------------------------------------------------------------------- -
# Import data                                                                                            ----
#---------------------------------------------------------------------------------------------------------- -  
# Specify location and file name of input data
  ui_datafiles_wd   <-c("T:/DFW-Team FP Lewis River M&E - General/Analysis/Lewis tule rollup/data")       ## full file path location of data file; if stored locally it would be something like: glue("{here::here()}/data files"); if stored on Teams, it would be something like: c("T:/DFW-Team FP Lewis River M&E - General/Analysis/Lewis tule rollup/data")  
  ui_datafile_name<-c("data_Lewis_tule_abundance_by_year_pop_origin_age_excluding_jacks_2024.csv")  ## name of data file; NOTE: file must be stored as a .csv and file name must include the .csv suffix

# Import
  dat<-import_data(wd = ui_datafiles_wd, file_name=ui_datafile_name) # Import data
  
# Preview data set
  dat %>% tibble  
  colnames(dat)
  dat |> pivot_wider(id_cols = c(SpawningYear, WaterBody, Param), names_from = Age, values_from = mean) |> arrange(SpawningYear, WaterBody, Param) # Summary of estimates by SpawningYear, WaterBody, and parameter
  dat |> group_by(SpawningYear, CommonPopName) |> count(WaterBody) |> pivot_wider(names_from = WaterBody, values_from = n) # number of estimates by SpawningYear and WaterBody

#---------------------------------------------------------------------------------------------------------- -
# Generate rollup/combined estimates of abundance using log-normal moment matching                       ----
#---------------------------------------------------------------------------------------------------------- -
# Step 1: Specify which data columns/fields to summarize estimates of abundance
  summary_cols <- c("SpawningYear", "CommonPopName", "Param") #typically, you should enter: c("SpawningYear", "CommonPopName", "Param")

# Step 2: Using "add_param" argument, specify if you want to generate estimates by age (for each Param) or not (i.e., total), or both
  filter_age = c("age", "total") #options: "age" or "total"; default value is c("age", "total") if NULL

# Step 3: Using "add_param" argument, specify if you want to also generate proportional estimates of origin (pHOS) and/or age (pAge)
  add_param = c("pHOS") #options: NULL -- enter as c() -- "pHOS", "pAge", or both c("pHOS", "pAge")

# Step 4: generate estimates of abundance and desired "add_param"(s)
  est_rollup<-
    generate_rollup_estimates(
      data = dat
    , summary_cols = summary_cols
    , filter_age = filter_age
    , add_param = add_param
    ) 
  est_rollup$abund|>print(n=Inf)
  est_rollup$prop |>print(n=Inf)
  
# Step 5 (optional): Check to make sure the mean values for NOSA plus HOSA for a given SpawningYear and CommonPopName equal TSA
  QAQC_HOSA_NOSA_TSA(data = est_rollup$abund)
  
#------------------------------------------------------------------------------------------------------------------- -  
# **OPTIONAL** - Format "est_rollup" to match SPi formatting and generate an Excel file of the formatted dataset  ----
#------------------------------------------------------------------------------------------------------------------- - 
# Step #1 - review and update object/arguments that may change year-to-year
  ui_central_tendency_metric = "Median" # specify "Mean" or "Median"
  ui_alpha = 0.05 # default 0.05 (assuming summary stats reporting 95% CIs); adjust as needed

# Step 2: format est_rollup and create SPi_format
    SPi_format_LewisTules_rollup<- 
      format_estimates_SPi(
          abundance_estimates = est_rollup$abund
        , proportional_estimates = est_rollup$prop
        , central_tendency_metric = ui_central_tendency_metric
        , alpha = ui_alpha

        , species = "Chinook salmon"
        , run = c("Fall")
        , CommonPopName = "Lewis River Fall (Tule) Chinook" 
        , RecoveryDomain = "Willamette/Lower Columbia"
        , ESU_DPS = "Salmon, Chinook (Lower Columbia River ESU)"
        , MajorPopGroup = "Cascade"
        , PopID = "217" 
        
        , PopFit_rollup= c("Same")
        , PopFit_Notes_rollup = c("combined NF, EF, and Cedar Creek estimates by Spawning Year and Origin")
        , EstimateType = "NOSA"
        , waterbody_name_rollup = c("Lewis River")
        , BestValue_rollup = c("Yes")
        
        , ProtMethURL = c("https://github.com/kalebentley/Carcass_M-R")
        , ProtMethDoc_rollup = c("https://wdfw.wa.gov/publications/02004")
        , Comments = glue("Reported abundance estimates are all {ui_central_tendency_metric}s except for Limits; proportional estimates (e.g., pHOS) are means")
        , ContactAgency_rollup = c("Washington Department of Fish and Wildlife")
        , ContactPerFirst_rollup = c("Kale")
        , ContactPerLast_rollup = c("Bentley")
        , ContactPerPhone_rollup = c("303-550-3004")
        , ContactPerEmail_rollup = c("kale.bentley@dfw.wa.gov") 
      )
    
  # preview summarized results (transposed for viewing ease)
    if(length(unique(SPi_format_LewisTules_rollup$SpawningYear))>1){
      SPi_format_LewisTules_rollup |> print(width=Inf, n=Inf)
    }else{
      
    t(SPi_format_LewisTules_rollup) }

# Step #3: Save output as Excel file
  generate_SPi_output_file(
    wd = glue("T:/DFW-Team FP Lewis River M&E - General/Analysis/Lewis tule rollup/output")      # Specify working directory for output files
    , file_name = glue("Lewis_tule_rollup_2024_for_SPi")
    , data = rbind(SPi_format_LewisTules_rollup)
    , meta = 
      list(
        date_estimates_generated = as.character(Sys.Date())
      , central_tendency_metric = ui_central_tendency_metric
      )
  )
