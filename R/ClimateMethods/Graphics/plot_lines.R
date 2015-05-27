# Still have to generalize this method to be able to plot more than two variables. By now it fiiting well with SIAC-practical
# assignmet topic 5.
# need to fix the legend for each plot


climate$methods(plot_lines = function(data_list=list(), var1, var2, col1 = "blue", type1 = "h",type2="p",
                                                col2 = "red", col3 = "green", xlabel = "Year",lwd=2, finite = TRUE,
                                                pch1 = 1, pch2 = 1, pch3 = 1, main=c(),  time_period = yearly_label){    
  
  
  # get_climate_data_objects returns a list of the climate_data objects specified
  # in the arguments.
  # If no objects specified then all climate_data objects will be taken by default
  # the var1 and var2 must be label. 
   data_list = add_to_data_info_required_variable_list(data_list, list( var1 )) 
   data_list = add_to_data_info_required_variable_list(data_list, list( var2 ))
  # we should be able to specify the time period.
   data_list = add_to_data_info_time_period(data_list, time_period) 
  
   climate_data_objs_list = get_climate_data_objects(data_list)

  for(data_obj in climate_data_objs_list) {
     # get the columns of interest for the plot.
    var1 = data_obj$getvname( var1 )
    var2 = data_obj$getvname( var2 )
    
    date_col = data_obj$getvname( date_label )
    #adding year column if not present 
    if( !(data_obj$is_present( year_label ) ) ) {
      data_obj$add_year_month_day_cols() 
    }
    year_col = data_obj$getvname( year_label )
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
#     #Check will be done here. var1 is a list
#     l = length(var1)
#     #Start for loop for each variable entered here
#     for( i in  1: l ){
#       
#       plot( xaxis[[ i ]], var1[[ i ]], type = "c", col= col3[[i]],
#             ylim = c( range( var1[[ i ]], na.rm = TRUE)+ 20 ), lty = lty2[[ i ]],
#             main = main[[ i ]], ylab = ylab[[ i ]], xlab = xlab[[ i ]])
#     }
    
    for( curr_data in curr_data_list ) {
       # plotting the first plot. 
      plot(curr_data[[ year_col ]], curr_data[[var1]], type = type1, lwd = lwd, col = col1, xlab = xlabel,main = main,
            ylim = c( range( curr_data[[var1]], curr_data[[var2]], finite = finite) ))
      #Adding points to the plot
      lines(curr_data[[ year_col ]], curr_data[[var1]], type=type2, col=col2, pch = pch1)
      #Adding the second plot
      points(curr_data[[ year_col ]], curr_data[[var2]], type = type1, col=col3, pch = pch2 )
      #Adding points to the second plot
      lines(curr_data[[ year_col ]], curr_data[[var2]], type=type2, col=col2, pch = pch1)
      
    }
  }
}
)




