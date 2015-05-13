climate$methods( missing_data_table=function( data_list=list(), interest_var=rain_label,data_period_label=daily_label ){ 
  #--------------------------------------------------------------------------------------------#
  # This function returns a data frame with two columns, the first one containing  years and the second
  #      one the number of missing rain obsevations per year. 
  #-------------------------------------------------------------------------------------------#
  
  # rain variable is required for this method
  data_list = add_to_data_info_required_variable_list( data_list, list(interest_var) )
  
  # daily data is required for this method
  data_list=add_to_data_info_time_period( data_list, data_period_label)
  
  # use data_list to get the required data objects
  climate_data_objs = get_climate_data_objects( data_list )
  # Initialise output
  out = list()
  j = 1
  
  for( data_obj in climate_data_objects ){
    #If no column of years present
    if( !(data_obj$is_present(year_label) ) ) {
      data_obj$add_year_month_day_cols()
    }
    
    year_col = data_obj$getvname(year_label)
    interest_col = data_obj$getvname(interest_var)
        
    # Access data in methods
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      # Add a column of rain to the data with a specific: "Rain" name for ddply use
      curr_data = cbind(curr_data, new_rain=curr_data[[interest_col]])
      
      missing_data<-ddply( curr_data, c(Year = year_col), summarize, days = sum(is.na(new_rain)))
      missing_data<-missing_data[missing_data$days>0,]
      names(missing_data)<-c("Year","Nos of Missing Days")
      missing_data<-as.list(missing_data)
      missing_data<-as.data.frame(missing_data)
      curr_data$new_rain=NULL
    }
    out[[j]] = missing_data
    # Give the name of each data to each element in the list out
    names(out)[[j]] = data_obj$get_meta( data_name_label )
    j = j+1
  }
  return( out )
}
)