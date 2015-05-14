climate$methods(display_daily = function(data_list = list(), print_tables = FALSE, variable = rain_label, months_list = month.abb, day_display = "Day"){
    
  data_list = add_to_data_info_required_variable_list(data_list, list(variable))
  data_list = add_to_data_info_time_period(data_list, daily_label)
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    
    interest_var = data_obj$getvname(variable)
    
    # Must add these columns if not present 
    if( !(data_obj$is_present( year_label ) && data_obj$is_present( month_label ) && data_obj$is_present( day_label )) ) {
      data_obj$add_year_month_day_cols()
    }
    year_col = data_obj$getvname( year_label )
    month_col = data_obj$getvname( month_label )
    day_col = data_obj$getvname( day_label )
    
    # access data for analysis
    curr_data_list = data_obj$get_data_for_analysis( data_list )
    
    for( curr_data in curr_data_list ) {
      # initialize tables as a list
      tables = list()
      # Split curr_data into single data frames for each year
      # It returns a list of data.frames, split by year 
      # This is much faster (6x faster when checked) than subsetting
      # Split is not always appropriate but it is in this case
      years_split <- split( curr_data, list( as.factor( curr_data[[year_col]] ) ) )
      # counter used in the loop
      i = 1
      # loop through the splited data frames 
      for ( single_year in years_split ) {
        # produce table with data
        tables[[i]] <- dcast( single_year, single_year[[ day_col ]] ~ single_year[[ month_col ]], value.var = interest_var)
        # Added day_display and months_list as extra arguments so it is more flexible
        end = length( colnames( tables[[i]] ) )
        names( tables[[i]] )[ 1 ] <- day_display
        colnames( tables[[i]] )[2:end] <- months_list[1:end-1]
        i = i + 1
      }
      # The names of years_split is the list of years as strings.
      # These are better labels than numbers so they can be identified better
      names( tables ) <- names( years_split )
    }
    # Only print if requested
    if( print_tables ) {print( tables) }
    # Always return the tables list because If we don't return and don't print then the method does nothing!
    return( tables )
    
  }  
}
)




# Kibos1952<-subset(kibos,Year==1952)
# 
# #produce table with data
# Kb52<-dcast(Kibos1952,Day~Month,value.var="obs_value")
# #add column names as months
# colnames(Kb52)[2:13]<-month.abb[1:12]
# 
# #create quick function to count number of obs larger than a certain value which can be run in an apply
# largerthan<-function(x,val){
#   length(na.omit(x[x>val]))
# }
# 
# #produce second table with summary stats
# Kb52_2<-rbind(colSums(Kb52[,-1],na.rm=T),apply(Kb52[,-1],2,max,na.rm=T),apply(Kb52[,-1],2,largerthan,val=0.85))
# 
# ## add dimnames
# Kb52_3<-cbind(c("Total","Maximum","Number >0.85"),Kb52_2)
# 
# ## Making dataframe for second table
# Kb52_4=data.frame(Kb52_3)
# 
# #add dimnames for the first column.
# colnames(Kb52_4)[1]<-("Day")
# 
# # #add overall totals
# # Kb52_2<-cbind(Kb52_2,c(sum(Kb52[,-1],na.rm=T),max(Kb52[,-1],na.rm=T),largerthan(Kb52[,-1],val=0.85)))
# 
# ### merge the tables
# Kb52_5<-rbind(Kb52,Kb52_4)
# 
# ###
# ## making overall totals
# Overall_totals<-c(rep("",31),addmargins((Kb52_2),2)[,13])
# 
# ## combining tables with overall totals
# Kb52_7<-cbind(Kb52_5,Overall_totals)
