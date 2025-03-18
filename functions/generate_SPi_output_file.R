# save outputs of estimates
generate_SPi_output_file<- function(wd, file_name, data, meta) {
  
  full_fp<-glue("{wd}/{file_name}.xlsx")
  
  # Format meta-data
  out_meta<-data.frame(field = names(meta), value = unlist(meta), row.names = NULL)
  
  if(!file.exists(glue("{full_fp}"))){  
    openxlsx::write.xlsx(
      x = list(out_meta, data), 
      file = glue("{full_fp}"),
      sheetName = c("meta_data", "estimates_for_SPi")
    )
    print(glue("file CREATED - {file_name}.xlsx"))
    
  }else{
    file_info <- file.info(full_fp)
    creation_time <- file_info$ctime
    print(glue("file ALREADY exists - {file_name}.xlsx\n ** Created on {creation_time}\n ** To replace, delete file and rerun function"))
  }
  
  
}