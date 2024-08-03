# save outputs of estimates
generate_output_files<- function(loc, wd, file_name, data, meta) {

# Define output file path  
  wd_output_files<-ifelse(loc == "Teams", wd, paste(here::here(), "outputs", sep="/"))
  
# If needed, created new sub-folder to save output  
  ifelse(!dir.exists(wd_output_files), {dir.create(wd_output_files); print("new output sub-folder CREATED")}, print("output sub-folder already EXISTS"))

# Format meta-data
  out_meta<-data.frame(field = names(meta), value = unlist(meta), row.names = NULL)
  
  if(!file.exists(paste0(wd_output_files, "/", file_name, ".xlsx"))){  
    openxlsx::write.xlsx(
      x = list(out_meta, data), 
      file = paste0(wd_output_files, "/", file_name, ".xlsx"),
      sheetName = c("meta_data", "estimates_for_SPi")
    )
    print(paste0("file CREATED - ", file_name, ".xlsx"))
    
  }else{print(paste0("file ALREADY exists - ", file_name, ".xlsx"))}
  
 
}