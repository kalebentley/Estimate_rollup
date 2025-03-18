# Import master data file that contains estimates of abundance (mean, sd) excluding jacks (age-2) by year, param (NOR, HOR, total), and sub-basin/pop
import_data<-function(wd, file_name){

full_fp<-glue("{wd}/{file_name}")
  
# Step 1: Read the CSV file as all characters
initial_data <- 
  read_csv(
      file = full_fp #paste(here::here(), wd, file_name, sep="/")
    , col_types = cols(.default = col_character())
  )

# Step 2: Determine the columns that contain only numeric data
is_numeric <- function(x) all(grepl("^[-+]?[0-9]*\\.?[0-9]+$", x[!is.na(x) & x != ""]))

numeric_columns <- 
  initial_data %>%
  summarise_all(is_numeric) %>%
  gather() %>%
  filter(value == TRUE) %>%
  pull(key)

# Step 3: Create a custom col_types specification
col_types_str <- paste(
  sapply(names(initial_data), function(col) {
    if (col %in% numeric_columns) {
      "d"
    } else {
      "c"
    }
  }),
  collapse = ""
)

# Step 4: Read the CSV file again with the specified col_types
out <- 
  read_csv(
    file = full_fp
    , col_types = col_types_str
  )


print("Data imported successfully!!")
return(out)
}