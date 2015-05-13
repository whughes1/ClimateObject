setwd(dirname(parent.frame(2)$ofile))
source('labels_and_defaults.R')
source('climate_data_refclass.R')
source('climate_refclass.R')
files <- sort(dir(file.path(getwd(), '/ClimateMethods/DataManipulation/'),pattern="$.R", full.names = TRUE))
lapply(files, source, chdir = TRUE)
files <- sort(dir(file.path(getwd(), '/ClimateMethods/Graphics/'),pattern="$.R", full.names = TRUE))
lapply(files, source, chdir = TRUE)
files <- sort(dir(file.path(getwd(), '/ClimateMethods/Models/'),pattern="$.R", full.names = TRUE))
lapply(files, source, chdir = TRUE)
files <- sort(dir(file.path(getwd(), '/R_front_ends/'),pattern="$.R", full.names = TRUE))
lapply(files, source, chdir = TRUE)

ClimateCO <- function (data_tables = list(), climate_obj_meta_data = list(), 
                       data_tables_meta_data = rep(list(list()),length(data_tables)),
                       data_tables_variables = rep(list(list()),length(data_tables)), 
                       imported_from = as.list(rep("",length(data_tables))),
                       messages=TRUE, date_format = "%d/%m/%Y") {
  CO<-climate$new(data_tables, climate_obj_meta_data, data_tables_meta_data,
                  data_tables_variables, imported_from, messages, date_format)  
}