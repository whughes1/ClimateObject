climate$methods(boxplot_new = function(data_list = list(), var,  names = "", method = "jitter",jitter = 0.1, time_period = daily_label, connect.mean = FALSE,
                                       horizontal = FALSE, plot_jitter = FALSE, add = TRUE, colpoints = "red", na.rm = TRUE,connect.median = FALSE,
                                       plot.sd = FALSE, lty = 1, col.sd = "blue", main = "title", varwidth = FALSE, outline = TRUE){
  
  #required variable
  data_list = add_to_data_info_required_variable_list(data_list, list(var))
  # data time period is daily
  data_list = add_to_data_info_time_period( data_list, time_period )
  climate_data_objs = get_climate_data_objects( data_list )
  
  for( data_obj in climate_data_objs ) {
    #get required variable name
    interest_var = data_obj$getvname( var )
    
    # access data for analysis
    curr_data_list = data_obj$get_data_for_analysis( data_list )
    
    for( curr_data in curr_data_list ) {
      
      dat <- subset(curr_data, select = c(interest_var))
      
      boxplot(  dat, names = names, horizontal = horizontal, varwidth = varwidth, outline = outline)
      if( plot_jitter == TRUE){
      stripchart( interest_var , method = method, jitter = jitter, vertical = !(horizontal), add = add, col = colpoints )
      }
      if( connect.mean == TRUE ){
      lines( sapply(  interest_var, na.rm = na.rm, mean) )
      }      
      if( connect.median == TRUE) {
        lines(  sapply( interest_var, na.rm = na.rm, median) )
      }
      #if a line for the standard deviation is required 
      if( plot.sd == TRUE){
        #store the mean and standard values in different variables
        m =  sapply( interest_var, na.rm = na.rm, mean)
        s =  sapply( interest_var, na.rm = na.rm, sd)
        # counter of sd from the mean
        i = 1
        abline( h = m + (i * s), lty = lty, col = col.sd )
        
      }
    }
    
  }
}
)