format_estimates_SPi<-
function(
    abundance_estimates, proportional_estimates, central_tendency_metric, alpha
  , species, run #, filt_runsub, filt_age_group, add_param = NULL
  , CommonPopName, RecoveryDomain, ESU_DPS, MajorPopGroup, PopID
  , PopFit_rollup, PopFit_Notes_rollup, EstimateType, waterbody_name_rollup, SpawningYear , BestValue_rollup
  , ProtMethURL, ProtMethDoc_rollup, Comments, ContactAgency_rollup, ContactPerFirst_rollup, ContactPerLast_rollup, ContactPerPhone_rollup, ContactPerEmail_rollup
  ) 
{

# Format abundance estimates  
  abund_tot<-
    abundance_estimates |> 
    filter(Age == "Total" | is.na(Age) == TRUE) |>
    rename_with(
      .fn = ~ ifelse(.x %in% c("l95", "u95"), c("LowerLimit", "UpperLimit")[match(.x, c("l95", "u95"))], .x),
      .cols = everything()
    ) |> 
    #rename(LowerLimit = `l95`, UpperLimit = `u95`) |> 
    select(all_of(summary_cols), !!sym(central_tendency_metric), LowerLimit, UpperLimit) |> 
    mutate(Alpha = alpha) |> 
    pivot_longer(cols = -c(summary_cols), names_to = "stat", values_to = "value") |> 
    mutate(Param = if_else(!grepl("Limit|Alpha", stat), Param, paste0(Param, stat))) 


# Format proportional estimates 
  # Determine the column to use based on central_tendency_metric
  selected_column <- if (central_tendency_metric %in% names(proportional_estimates)) {
    central_tendency_metric
  } else if ("Mean" %in% names(proportional_estimates)) {
    "Mean"
  } 
  
  pHOS_tot<-
    proportional_estimates |> 
    filter(Age == "Total" | is.na(Age) == TRUE) |> 
    rename_with(
      .fn = ~ ifelse(.x %in% c("l95", "u95"), c("LowerLimit", "UpperLimit")[match(.x, c("l95", "u95"))], .x),
      .cols = everything()
    ) |> 
    select(all_of(summary_cols), selected_column, any_of(c("LowerLimit", "UpperLimit"))) |> 
    mutate(Alpha = alpha) |> 
    pivot_longer(cols = -c(summary_cols), names_to = "stat", values_to = "value") |> 
    mutate(Param = if_else(!grepl("Limit|Alpha", stat), Param, paste0(Param, stat))) 

# Combine estimates and format
  estimates<-
    bind_rows(abund_tot, pHOS_tot) |> 
    select(-stat) |> 
    mutate(
        CommonName = species
      , Run = run
      , RecoveryDomain = RecoveryDomain
      , ESU_DPS = ESU_DPS
      , MajorPopGroup = MajorPopGroup
      , PopID = PopID
      , PopFit = PopFit_rollup
      , PopFitNotes = PopFit_Notes_rollup
      , EstimateType = EstimateType
      , WaterBody = waterbody_name_rollup
      , ContactAgency = ContactAgency_rollup
      , MethodNumber = NA
      , BestValue = BestValue_rollup,
    ) |>
    pivot_wider(names_from = "Param", values_from = value) |>
    mutate(
      ProtMethURL = ProtMethURL,
      ProtMethDocumentation = ProtMethDoc_rollup,
      Comments = Comments,
      ContactPersonFirst = ContactPerFirst_rollup,
      ContactPersonLast = ContactPerLast_rollup,
      ContactPhone = ContactPerPhone_rollup,
      ContactEmail = ContactPerEmail_rollup
    )

return(estimates)

}  

