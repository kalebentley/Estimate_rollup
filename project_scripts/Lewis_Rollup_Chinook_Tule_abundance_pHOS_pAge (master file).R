#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate roll-up estimates for Lewis Basin tule Chinook (i.e., combining independent estimates from NF Lewis, EF Lewis, and Cedar Creek
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed by Kale Bentley
# Updated: Aug 2, 2024

#---------------------------------------------------------------------------------------------------------- -
# Load functions and packages                                                 ----
#---------------------------------------------------------------------------------------------------------- -  
# Load functions
  sapply(FUN = source, paste(getwd(), "functions", list.files("functions"), sep="/"))

# Load packages
  package_list<-c("here", "tidyverse", "openxlsx", "R2jags", "tidybayes", "shinystan") 
  install_or_load_pack(package_list)
  
#---------------------------------------------------------------------------------------------------------- -  
# Specify location and file name of input data
#---------------------------------------------------------------------------------------------------------- -   
## Location, file path, and file name for the input data files
  loc_data_files<-c("Teams") # Specify the location (loc) of M-R data file: enter either "Local: (i.e., stored locally within this project) or "Teams"  
  wd_data_files   <-c("T:/DFW-Team FP Lewis River M&E - General/Analysis/Lewis tule rollup/data") 
      ## NOTE: if "loc_" is Teams, you must provide the full file path to the data folder 
      ## NOTE: if "loc_" is Local, provide the file path starting at the folder name of where the data are stored within this project" 
  file_name<-c("data_Lewis_tule_abundance_by_year_pop_origin_age_excluding_jacks.csv") 
      ## NOTE file must be stored as a .csv and file name must include the .csv suffix

#---------------------------------------------------------------------------------------------------------- -
# IMPORT DATA                                                                                        ----
#---------------------------------------------------------------------------------------------------------- -  
  dat<-import_data(loc = loc_data_files, wd = wd_data_files, file_name=file_name) # Import data
  dat %>% tibble  # Preview imported data   

#---------------------------------------------------------------------------------------------------------- -
# GENERATE ROLLED-UP ESTIMATES OF ABUNDANCE USING LOG-NORMAL MOMEMENT MATCHING     ----
#---------------------------------------------------------------------------------------------------------- -
# Step 1: Specify which data columns/fields to summarize estimates of abundance
  summary_cols <- c("SpawningYear", "CommonPopName", "Param") #typically, you should enter: c("SpawningYear", "CommonPopName", "Param")

# Step 2: Using "add_param" argument, specify if you want to generate estimates by age (for each Param) or not (i.e., total), or both
  filter_age = c("age", "total") #options: "age" or "total"; default value is c("age", "total") if NULL

# Step 3: Using "add_param" argument, specify if you want to also generate proportional estimates of origin (pHOS) and/or age (pAge)
  add_param = c("pAge", "pHOS") #options: NULL -- enter as c() -- "pHOS", "pAge", or both c("pHOS", "pAge")

# Step 4: generate estimates of abundance and desired "add_param"(s)
  est_rollup<-
    generate_rollup_estimates(
      data = dat
    , summary_cols = summary_cols
    , filter_age = filter_age
    , add_param = add_param
    ) 
  est_rollup|>print(n=Inf)
  
# Step 5 (optional): Check to make sure the mean values for NOSA plus HOSA for a given SpawningYear and CommonPopName equal TSA
  QAQC_HOSA_NOSA_TSA(data = est_rollup)
  
#------------------------------------------------------------------------------------------------------------------- -  
# **OPTIONAL** - Format "est_rollup" to match SPi formatting and generate an Excel file of the formatted dataset  ----
#------------------------------------------------------------------------------------------------------------------- -   
# Step 1: Organize output to match SPi formatting
  # part A: define values for various SPi columns
    PopFit_rollup<- c("Same")
    PopFit_Notes_rollup <-c("combined NF, EF, and Cedar Creek estimates by Spawning Year and Origin")
    waterbody_name_rollup <-c("Lewis River")
    ContactAgency_rollup <- c("Washington Department of Fish and Wildlife")
    BestValue_rollup <- c("Yes")
    ProtMethDoc_rollup <-c("TBD")
    ContactPerFirst_rollup<-c("Kale")
    ContactPerLast_rollup<-c("Bentley")
    ContactPerPhone_rollup<-c("303-550-3004")
    ContactPerEmail_rollup<-c("kale.bentley@dfw.wa.gov")
  
# Step 2: format est_rollup and create SPi_format
    SPi_format<-  
      est_rollup |> 
      mutate(
        PopFit = PopFit_rollup,
        PopFitNotes = PopFit_Notes_rollup,
        WaterBody = waterbody_name_rollup,
        ContactAgency = ContactAgency_rollup,
        BestValue = BestValue_rollup
      ) |> 
      filter(Age == "Total" | is.na(Age) == TRUE) |> 
      select(any_of(summary_cols), PopFit, PopFitNotes, WaterBody, ContactAgency, BestValue, Mean) |> 
      pivot_wider(names_from = "Param", values_from = Mean) |> 
      mutate(
        ProtMethDocumentation = ProtMethDoc_rollup,
        ContactPersonFirst = ContactPerFirst_rollup,
        ContactPersonLast = ContactPerLast_rollup,
        ContactPhone = ContactPerPhone_rollup, 
        ContactEmail = ContactPerEmail_rollup
      )
  
    SPi_format |> print(n = Inf, width=Inf)
    
# Step 3: Specify location, file path, and name of output folder and file name
  loc_output_files<-c("Teams") # Specify the location (loc) of outputs: enter either "Local: (i.e., stored locally within this project) or "Teams"  
  wd_output_files <-c("T:/DFW-Team FP Lewis River M&E - General/Analysis/Lewis tule rollup/output") # Specify working directory for output files
  output_file_name<-c("TEST_Lewis_tule_rollup_2022-2023_for_SPi") #specify name of output file (leave off suffix; .xlsx)
    ## NOTE: if "loc_" is Teams, you must provide the full file path up to the top-level outputs folder which should be name "project_outputs/" & already exist 
    ## NOTE: if "loc_" is Local, leave "wd_output_files" blank i.e., c()   

# Step 4: Specify desired meta-data (will attach this information in a seperate Excel sheet)  
  output_meta_data<-
    list(
      data_generated = as.character(Sys.Date()),
      estimate_values = "means"
    )
  
#Step 5: Save output as Excel file 
  generate_output_files(
    loc = loc_output_files
    , wd = wd_output_files
    , file_name = output_file_name
    , data = SPi_format
    , meta = output_meta_data
  )
  
