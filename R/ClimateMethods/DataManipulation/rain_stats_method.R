climate$methods(rain_stats = function(data_list = list(),thresholds=c(0.85,2.5),col_name = "rainday",thresh_amount=0.85){
  #=========================================================================#
  # This method makes rainfall statistics for the dataset             
  # You can choose some thresholds for different rain day statistics
  # Plus set thresh_amount for the threshold used in the RainAmount statistic  
  #=========================================================================#
  # rain required
  data_list = add_to_data_info_required_variable_list(data_list, list(rain_label))
  # data time period is "daily"
  data_list = add_to_data_info_time_period(data_list, daily_label)
  # a list of climate data objects
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    
    curr_threshold = data_obj$get_meta(threshold_label,thresh_amount)
    # must have rain column in the data    
    rain_col  = data_obj$getvname(rain_label)   

  curr_data_list = data_obj$get_data_for_analysis(data_list)

  for( curr_data in curr_data_list ) {
      rainday <- curr_data[[rain_col]]
      rainday[(curr_data[[rain_col]] > 0)] <- 1
      rainday[(curr_data[[rain_col]] <= 0)] <- 0
      print(rainday)
      
      dryday <- curr_data[[rain_col]]
      dryday[(curr_data[[rain_col]] > 0)] <- 0
      dryday[(curr_data[[rain_col]] <= 0)] <- 1
      
      for(n in 1:length(thresholds)){
        curr_data[,(dim(curr_data)[2]+1)] <-curr_data[[rain_col]]
        names(curr_data)[dim(curr_data)[2]] <- paste("RainDay.Thresh",thresholds[n],sep=".")
        curr_data[,dim(curr_data)[2]] <- curr_data[[rain_col]]
        curr_data[((is.na(curr_data[[rain_col]])==FALSE)&(curr_data[[rain_col]] <= thresholds[n])),dim(curr_data)[2]] <- 0
        curr_data[((is.na(curr_data[[rain_col]])==FALSE)&(curr_data[[rain_col]] > thresholds[n])),dim(curr_data)[2]] <- 1
      }
      
        curr_data$RainAmount <- curr_data[[rain_col]]
        curr_data$RainAmount[(curr_data[[rain_col]] <= thresh_amount)] <- NA
        
        #return(curr_data)
      }
  }
data_obj$append_column_to_data(rainday, col_name)
 
}
)
