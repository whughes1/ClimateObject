# Defining the reference class "climate"
# This reference class can contain multiple climate_data objects
# The fields are the properties every climate_data object will have.

# climate_data_objects : A list of climate_data objects
# used_data_tables     : A list of extra climate_data objects, created during use
# meta_data            : Any information about the climate object. e.g. "name"


climate <- setRefClass("climate", 
                            fields = list(climate_data_objects = "list", used_data_objects = "list", 
                                          meta_data = "list")
)

# INITIALIZE method
##############################################################################################
# Functions of reference classes are called methods.
# This is how you define a method for a specific reference class.
# Every reference class has an initialize method which tells R how to create new 
# climate objects.
# By the end of this method, all fields of climate_data should be defined.
# Notice that we don't have a climate object as an input.
# We can refer to any field of a climate_data object by name. e.g. climate_data_objects

# data_tables           : list of data.frames - each one will be used to create a climate_data
#                         object
# data_tables_meta_data : list of meta_data lists - one for each data.frame
# data_tables_variables : list of variables lists - one for each data.frame
# climate_obj_meta_data : the meta_data for the climate object
# imported_from         : list of strings telling us where the data has come from
#                         - one for each data.frame                         

climate$methods(initialize = function(data_tables = list(), climate_obj_meta_data = list(), 
                                      data_tables_meta_data = rep(list(list()),length(data_tables)),
                                      data_tables_variables = rep(list(list()),length(data_tables)), 
                                      imported_from = as.list(rep("",length(data_tables))),
                                      messages=TRUE, date_format = "%d/%m/%Y") 
{

  meta_data <<- climate_obj_meta_data
  used_data_objects <<- list()
  
  if (missing(data_tables) || length(data_tables) == 0) {
    climate_data_objects <<- list()
  }
  
  else {
    .self$import_data(data_tables,data_tables_meta_data, data_tables_variables,
                                               imported_from, messages = messages, date_format = date_format)
  }
  
}
)

# IMPORT DATA FUNCTION
##############################################################################################

climate$methods(import_data = function(data_tables = list(), data_tables_meta_data = rep(list(list()),length(data_tables)),
                                       data_tables_variables = rep(list(list()),length(data_tables)), 
                                       imported_from = as.list(rep("",length(data_tables))), messages=TRUE, date_format = "%d/%m/%Y")
{

  if (missing(data_tables) || length(data_tables) == 0) {
    stop("No data found. No climate_data objects can be created.")
  }
  
  else {
    
    if ( ! (class(data_tables) == "list") )  {
      stop("data_tables must be a list of data frames")
    }
    
    if (length(unique(names(data_tables))) != length(names(data_tables)) ) {
      stop("There are duplicate names in the data tables list.")
    }
    
    if ( !(length(data_tables_meta_data) == length(data_tables)) ) { 
      stop("If data_tables_meta_data is specified, it must be a list of meta_data lists with the same
           length as data_tables.")
    }
    
    if ( !(length(data_tables_variables) == length(data_tables)) ) { 
      stop("If data_tables_variables is specified, it must be a list of variables lists with the same
           length as data_tables.")
    }
    
    if ( !(class(imported_from) == "list") || ! (length(imported_from) == length(data_tables))  ) { 
      stop("imported_from must be a list of the same length as data_tables")
    }
    
    # loop through the data_tables list and create a climate_data object for each
    # data.frame given
    
    new_climate_data_objects = list()
    
    for ( i in (1:length(data_tables)) ) {
      
      new_data = climate_data$new(data=data_tables[[i]], data_name = names(data_tables)[[i]], 
                                  meta_data = data_tables_meta_data[[i]], variables = data_tables_variables[[i]], 
                                  imported_from = imported_from[[i]], start_point = i, messages = messages, date_format = date_format)
      
      # Add this new climate_data object to our list of climate_data objects
      .self$append_climate_data_objects( new_data$meta_data[[data_name_label]],new_data)
      
    }
  }
}
)



# Getter methods
###############################################################################################
# We can create methods to extract fields from a climate_data object.
# These are called getter methods and are usually very simple functions.
# Notice that no input is needed.

# get_climate_data_objects returns some of the climate_data objects in the climate object
# It will also create new ones if needed
# Specification by data_info

climate$methods(get_climate_data_objects = function(data_info= list()) {
  
  climate_data_list = list()

  if(time_period_label %in% names(data_info) ) {
    time_period=data_info[[time_period_label]]
  }
  else stop("Specify the time period the data needs to be in")
  #TO DO check that this is correct in all contexts might it be sensible to do analysis over multiple time periods?

  for (temp in climate_data_objects) {
    if (required_variable_list_label %in% names(data_info)){
      if (!temp$is_present(data_info[[required_variable_list_label]])){
        next
      }
    }
    name = temp$meta_data[[data_name_label]]
    if (time_period==temp$data_time_period){
      climate_data_list[[name]] <- temp 
    }
    else if (convert_data_label %in% names(data_info)){
      if (data_info[[convert_data_label]]){
        if(compare_time_periods(time_period,temp$data_time_period)){
          # Check if needs to be created first.
          summary_created = FALSE
          for(used_obj in used_data_objects) {
            if(used_obj$is_meta_data(summarized_from_label) 
               && used_obj$get_meta(summarized_from_label) == name 
               && used_obj$data_time_period == time_period) {
              summarized_name = used_obj$get_meta(data_name_label)
              climate_data_list[[summarized_name]] <- used_obj
              summary_created = TRUE
              break
            }
          }

          if(!summary_created) {
            temp_summarized <- temp$summarize_data(time_period, start_point = length(used_data_objects)+1)
            name = temp_summarized$meta_data[[data_name_label]]
            .self$append_used_data_objects(name, temp_summarized)
            climate_data_list[[name]] <- temp_summarized 
          }
        }
      }
    }
    #TO DO think hard whether we should restrict based on stations or not my inclination is not at the data object level.
  }

  if (merge_data_label %in% names(data_info)){
    if (data_info[[merge_data_label]]){
      if (length(climate_data_list)>1){
        merge_obj <- .self$merge_vertical(climate_data_list)
        name = merge_obj$meta_data[[data_name_label]]
        climate_data_list <- list()
        climate_data_list[[name]] <- merge_obj 
      }
    }
  }

  return(climate_data_list)
  
}
)

climate$methods(get_used_data_objects = function() {
  return(used_data_objects)
}
)

climate$methods(get_meta_data = function() {
  return(meta_data)
}
)

# Create and edit data info methods
###############################################################################################
climate$methods(create_data_info = function(time_period="", station_list="", date_list="", required_variable_list="") {
  data_info=list()
  if (!missing(time_period)){
    #TO DO check that it is a valid time period
    data_info[[time_period_label]] <- time_period
  }
  if (!missing(station_list)){
    #TO DO check that it is valid 
    data_info[[station_list_label]] <- station_list
  }
  if (!missing(date_list)){
    #TO DO check that it is valid 
    data_info[[date_list_label]] <- date_list
  }
  if (!missing(required_variable_list)){
    #TO DO check that it is valid 
    data_info[[required_variable_list_label]] <- required_variable_list
  }
  
  return (data_info)
}
)

climate$methods(add_to_data_info_merge = function(data_info=list(), merged=FALSE) {
  if (merge_data_label %in% names(data_info)){
    if (data_info[[merge_data_label]]!=merged & !missing(merged)){
      warning ("overwriting user choice for merging data")
      data_info[[merge_data_label]]<-merged
    }
  }
  else data_info[[merge_data_label]]<-merged
  return (data_info)
}
)

climate$methods(add_to_data_info_time_period = function(data_info=list(), time_period="") {
  if (time_period_label %in% names(data_info)){
    if (data_info[[time_period_label]]!=time_period & !missing(time_period)){
      warning ("overwriting user choice for time period")
      data_info[[time_period_label]]<-time_period
    }
  }
  else data_info[[time_period_label]]<-time_period
  return (data_info)
}
)

climate$methods(add_to_data_info_required_variable_list = function(data_info=list(), required_variable_list="") {
  if (!missing(required_variable_list)){
    #TO DO check required_variable_list is valid
    if (required_variable_list_label %in% names(data_info)){
      data_info[[required_variable_list_label]]<-c(data_info[[required_variable_list_label]],required_variable_list) #TO DO Check what happens to repeats 
    }
    else data_info[[required_variable_list_label]]<-required_variable_list
  }
  return (data_info)
}
)

#TO DO other creat and add methods

# Append methods
###############################################################################################
climate$methods(append_climate_data_objects = function(name, obj) {
  if( !class(name) == "character") {
    stop("name must be a character")
  }
  
  if ( !class(obj) == "climate_data") {
    stop("obj must be a climate_data object")
  }

  climate_data_objects[[name]] <<- obj
}
)

climate$methods(append_used_data_objects = function(name, obj) {
  if( !class(name) == "character") {
    stop("name must be a character")
  }
  
  if ( !class(obj) == "climate_data") {
    stop("obj must be a climate_data object")
  }
  
  used_data_objects[[name]] <<- obj
}
)


# Other methods
#############################################################################################
# All analysis functions will be methods of climate objects and NOT climate_data objects.
# This is because we can call the method once to do the calculations on multiple data.frames
# at the same time.
# All these methods will allow the user to specify which climate_data objects they want
# to do the analysis on.



  #------------------------------------------------------------------------
  # This function plots the missing values for the rainfall amount, per year
  # It is here to demonstrate how an output method 
  #
  # It has the following optional arguments:
  # data_list: Specify the subset of the data to use. 
  # threshold: threshold which determines if a day is dry if the rainfall amount is below it. This overrides the threshold stored in the metadata if provided.
  # fill_col: A list of colours to use the first is for rain days the second for dry and the third for missing, missing dates are blank
  # 
  # ----------------------------------------------------------------------------

climate$methods(plot_missing_values_rain = function(data_list=list(), threshold = 0.85, fill_col=c("blue","yellow","red"))
{    
  # get_climate_data_objects returns a list of the climate_data objects specified
  # in the arguements.
  # If no objects specified then all climate_data objects will be taken by default
  # TO DO have options such as colours and the rest
  data_list=add_to_data_info_required_variable_list(data_list, list(rain_label))
  data_list=add_to_data_info_time_period(data_list, daily_label)
  climate_data_objs_list = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs_list) {
    curr_threshold = data_obj$get_meta(threshold_label,threshold)
    
    rain_col  = data_obj$variables[[rain_label]]
    # If doy or year column is not in the data frame, create it.
    if ( !(data_obj$is_present(dos_label)&data_obj$is_present(season_label))) {
      # add_doy_col function does not exist yet.
      data_obj$add_doy_col()
    }
    dos_col = data_obj$variables[[dos_label]]
    season_col = data_obj$variables[[season_label]]
    curr_data_list=data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      a2<-subset(curr_data, curr_data[[rain_col]] > curr_threshold)
      a3<-subset(curr_data, curr_data[[rain_col]] <= curr_threshold)
      a1<-curr_data[!complete.cases(curr_data),]
      plot2<-plot.new()
      
      #print(unique(curr_data[[season_col]]))
      #print(curr_data[[dos_col]])
      #print(c(min(curr_data[[season_col]]),max(curr_data[[season_col]]))
      # #       
      #       plot(unique(curr_data[[season_col]]),ylim=c(0,500))
      
      # plot(unique(curr_data[[season_col]]),curr_data[[dos_col]], log = "", asp = NA)
      plot(curr_data[[season_col]],curr_data[[dos_col]], ylim=c(0,500), log = "", asp = NA, xlab="Year",ylab="Day of Year", main="Rain Present")
      #plot.window(xlim=c(min(curr_data[[season_col]]),max(curr_data[[season_col]])),ylim=c(0,500), log = "", asp = NA) #TO DO Tidy up graphical parameters
      #title(xlab="Year",ylab="Day of Year", main="Rain Present") #TO DO Need to think hard about how display name are stored
      legend("topright",c("Rain","Dry","NA"),fill=fill_col,horiz=TRUE)
      points(as.numeric(a1[[season_col]]),a1[[dos_col]],pch="-",col=fill_col[3])
      points(as.numeric(a2[[season_col]]),a2[[dos_col]],pch="-",col=fill_col[1])
      points(as.numeric(a3[[season_col]]),a3[[dos_col]],pch="-",col=fill_col[2])
      
      # TO DO output multiple plots in multiple ways
      plot2
    }
  }
}
)


#==========================================================================
# date_col_check
# Method to check if date column is present and in correct format
# If the column is not there then it is created
# Danny's changes:
# Changed name
# Added convert and create arguements
# Added date_format as arguement
# Date column name is not changed if date column is already there
# Created replace_column_in_data method for climate_data to use to change class of date column


climate$methods(date_col_check = function(data_list=list(), date_format = "%d/%m/%Y", convert = TRUE,
                                          create = TRUE) 
{
  
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs){
    data_obj$date_col_check( date_format, convert, create)
    
  }
}
)

climate$methods(merge_vertical = function(climate_data_objs = climate_data_objects,
                                          identifier = "Identifier", merge_name = "", 
                                          start_point = length(used_data_objects)+1) 
{

  # TO DO: should argument be data_list instead of climate_data_objs?
  #        do we allow to merge subsets of the data or only whole data objects?
  #        what meta data should be stored in the merged data object so it can be
  #        uniquiely identified later.
  
  
  if(length(climate_data_objs) == 0) {
    stop("No climate_data objects have been given to merge.")
  }
  
  time_periods = list()
  for(data_obj in climate_data_objs) {
    time_periods = c(time_periods,data_obj$data_time_period)
  }
  
  if(length(unique(time_periods)) != 1) {
    stop("Cannot merge data sets that are using different time periods.")
  }
  
  merge_time_period = time_periods[[1]]
  
  for(used_obj in used_data_objects) {
    if(merge_time_period == used_obj$data_time_period && used_obj$is_meta_data(merged_from_label)
      && length(used_obj$meta_data[[merged_from_label]]) == length(names(climate_data_objs))
      && length(union(used_obj$meta_data[[merged_from_label]], names(climate_data_objs))) == length(names(climate_data_objs)) ) {
        return(used_obj)
    }
  }
  
  # identified_variables : data frame showing which recognised variables are in each data set
  identified_variables = data.frame(data_object=names(climate_data_objs))

  # vars : the list of variables found in the variables list for data objects
  #       some of these variables may not actually be in the data set
  vars = list()
  
  for(data_obj in climate_data_objs) {
    vars = c(vars,names(data_obj$variables))
  }
  vars = unique(vars)
  
  # used_vars : the subset of vars containing only the variables that appear in at least 
  #             one of the data sets
  used_vars = list()

  for(curr_var in vars) {

    new_col = c()
    
    # new_col : logical vector showing which data sets contain curr_var
    for(data_obj in climate_data_objs) {    
      new_col = c(new_col,data_obj$is_present(curr_var))
    }
    
    # We are only interested in variables that appear in at least 1 data set
    if(sum(new_col) >= 1) {
      
      # Add new_col to identified_variables data frame
      identified_variables[,curr_var] = new_col
      used_vars = c(used_vars, curr_var)
    }
  }
  print(used_vars)
  
  #######################################################################

  i = 1
  data_to_merge = list()
  for(data_obj in climate_data_objs) {
    
    curr_data_list = data_obj$get_data_for_analysis(data_info = list())
    
    for(curr_data in curr_data_list ) {
      
      if(identifier %in% names(curr_data)) {
        stop(paste0("There is already a column in: '", data_obj$get_meta("data_name"), "' with name: '",
                    identifier,"'. The identifier cannot be an exisiting column name."))
      }
      
      # Add an identifier column to each data set containing the data object name
      data_name = data_obj$get_meta(data_name_label)
      curr_data[[identifier]] <- rep(data_name,nrow(data_obj$data))
      
      for(var_name in used_vars) {
        # The same variable may have different names in different data sets
        # so we rename these columns to be the same in each data set.
        if(identified_variables[i,var_name]) {
          old_col_name = data_obj$getvname(var_name)
          names(curr_data)[names(curr_data) == old_col_name] <- var_name
        }
    
        # If the variable is not present, but can be generated from other columns
        # create that column. e.g. year can be created from date column
          
        else if( var_name == year_label ) {
          curr_data[[var_name]] <- year(curr_data[[date_label]]) 
        }
    
        else if( var_name == month_label ) {
          curr_data[[var_name]] <- month(curr_data[[date_label]]) 
        }
    
        else if( var_name == day_label ) {
          curr_data[[var_name]] <- month(curr_data[[date_label]]) 
        }
          
      }
    data_to_merge[[length(data_to_merge)+1]] <- curr_data
    }
    i = i + 1
  }
  merge = rbind.fill(data_to_merge)

  merged_obj = climate_data$new(data = merge, data_name = merge_name, start_point = start_point,
                                  data_time_period = merge_time_period)
    
  merged_obj$append_to_meta_data(merged_from_label, names(climate_data_objs))
    
  .self$append_used_data_objects(merged_obj$meta_data[[data_name_label]],merged_obj)
  
  # return the merged object
    used_data_objects[[ merged_obj$get_meta(data_name_label) ]]

}
)

climate$methods(get_summary_name = function(time_period, data_obj) 
{
  if(missing(time_period)) {
    stop("Specify the time period of the summarized data.")
  }
  
  data_name = data_obj$get_meta_new(data_name_label)
  
  if(compare_time_periods(time_period,data_obj$data_time_period)) {
    # Check if needs to be created first.
    summary_created = FALSE
    for(used_obj in used_data_objects) {
      if(used_obj$is_meta_data(summarized_from_label) 
         && used_obj$get_meta(summarized_from_label) == data_name
         && used_obj$data_time_period == time_period) {
        summarized_obj = used_obj
        summary_created = TRUE
        break
      }
    }
    
    if(!summary_created) {
      summarized_obj <- data_obj$summarize_data(time_period, start_point = length(used_data_objects)+1)
      name = summarized_obj$meta_data[[data_name_label]]
      .self$append_used_data_objects(name, summarized_obj)
    }
#    return(summarized_obj$meta_data[[data_name_label]])
    return(summarized_obj)
  }
  
  else stop("Cannot create a summary for these time periods.")
    
}
)

climate$methods(append_to_summary = function(time_period, data_obj, col_data, col_name="", label,
                                             replace=FALSE)
{
  if(missing(time_period)) {
    stop("Specify the time period of the summarized data.")
  }
  
  if(missing(col_data)) {
    stop("Specify the data to be added to the summary data.")
  }
  
  data_name = data_obj$get_meta_new(data_name_label)
  
  if(compare_time_periods(time_period,data_obj$data_time_period)) {
    # Check if needs to be created first.
    summary_created = FALSE
    for(used_obj in used_data_objects) {
      if(used_obj$is_meta_data(summarized_from_label) 
         && used_obj$get_meta(summarized_from_label) == data_name
         && used_obj$data_time_period == time_period) {
        summarized_obj = used_obj
        summary_created = TRUE
        break
      }
    }
    
    if(!summary_created) {
      summarized_obj <- data_obj$summarize_data(time_period, start_point = length(used_data_objects)+1)
      name = summarized_obj$meta_data[[data_name_label]]
      .self$append_used_data_objects(name, summarized_obj)
    }

    if( !missing(label) && summarized_obj$is_present(label) ) {
      if(replace) {
        col_name = summarized_obj$getvname(label)
        message(paste0("Replacing column ", col_name, " with the new data."))
        summarized_obj$replace_column_in_data(col_name, col_data)
      }
      else message(paste("A column named ",col_name,"already exists. It will not be replaced.
                         To replace this column, re run the method and specify replace = TRUE."))
    }
    
    else {
      if(missing(label)) summarized_obj$append_column_to_data(col_data,col_name)
      else summarized_obj$append_column_to_data(col_data,col_name, label)
    }
  }
  else stop(paste0(data_name, " cannot be summarized to ",time_period,"."))

  
}
)

climate$methods(add_end_rain = function(data_list=list(), earliest_day = 228, water_balance_col_name = "Water Balance", 
                                               col_name = "End of the rains", capacity_max = 100, evaporation = 5,
                                               replace=FALSE) {
  
  
  # We don't restrict the years when calculating end of rain. We calculate for the
  # whole data set. When displaying we can show a subset of the data if needed.
  # year has been removed as an argument.
  
  data_list=add_to_data_info_required_variable_list(data_list, list(rain_label))
  data_list=add_to_data_info_time_period(data_list, daily_label)
  climate_data_objs = get_climate_data_objects(data_list)
  
  
  for(data_obj in climate_data_objs) {

    end_rain = list()
    
    summary_obj <- get_summary_name(yearly_label, data_obj)
    
    continue = TRUE
    
    if(col_name %in% names(summary_obj$get_data()) && !replace) {
      message(paste("A column named", col_name, "already exists. The column will not be replaced.
                     To replace to column, re run this function and specify replace = TRUE."))
      continue = FALSE
    }
    
    if(col_name %in% names(summary_obj$get_data()) && replace) {
      message(paste("A column named", col_name, "already exists. The column will replaced 
                    in the data."))
    }
    
    # 3. check if definition already exists, then do not add
    # need to more carefully define evaporation
    curr_definition = list(earliest_day = earliest_day, capacity_max = capacity_max, 
                           evaporation = evaporation)
    
    if( continue && summary_obj$is_definition(rain_label,end_of_label,curr_definition)) {
      message("A column with this defintion already exists in the data.
              The column will not be added again.")
      continue = FALSE
    }
    
    if(continue) {
      
      # rain is required so we don't need to check if it's present
      rain_col = data_obj$getvname(rain_label)
      
      # Complete dates needed for calculations
      data_obj$missing_dates_check()
      

      #if doy or year/dos season is not in the data frame, create it.
      if( !( data_obj$is_present(dos_label) && data_obj$is_present(season_label) ) ) {
        data_obj$add_doy_col()
      }

      season_col = data_obj$getvname(season_label)
      dos_col = data_obj$getvname(dos_label)
      
      if( !(data_obj$is_present(waterbalance_label)) ) {
        if(missing(capacity_max)) {
          data_obj$add_water_balance_col(col_name=water_balance_col_name,evaporation=evaporation)
        }
        else { data_obj$add_water_balance_col(water_balance_col_name,capacity_max,evaporation) }
      }
      # Don't need to append to variables. This is done by add_waterbalance_col.
      
      waterbalance_col = data_obj$getvname(waterbalance_label)
      
      # get the data with empty list so we do not subset the data here
      curr_data_list = data_obj$get_data_for_analysis(list())
      
      for( curr_data in curr_data_list ) {
        
        # Split curr_data into single data frames for each year
        # It returns a list of data.frames, split by year 
        # This is much faster than subsetting each time
        # Split is not always appropriate but it is in this case
        seasons_split <- split(curr_data, list(as.factor(curr_data[[season_col]])))
        
        i = 1
        for (single_season in  seasons_split)  {
          
          single_season <- single_season[single_season[[dos_col]] >= earliest_day,c(dos_col,waterbalance_col, season_col)]

          # default value if end of season not found
          end_rain[i] = NA
          
          # subsetting above may give an empty data frame
          if(nrow(single_season)==0) next
          
          for( j in 1:nrow(single_season) ) {
            if( !is.na(single_season[[waterbalance_col]][[j]]) 
                && single_season[[waterbalance_col]][[j]] == 0 ) {  
              end_rain[i] = single_season[[dos_col]][[j]]
              break
            }
          }
          i = i + 1  
        }
        names(end_rain) <- names(seasons_split)
      }
      
      #   if( plot == TRUE ){
      #     plot( year, endday, type = "b", col = "blue", ylab = "End of the rain",
      #           xlab = "Year", main = main)
      #   }
      summary_obj$append_column_to_data(end_rain, col_name)
      label = summary_obj$get_summary_label(rain_label, end_of_label, curr_definition)
      summary_obj$append_to_variables(label,col_name)
    }
  }
}
)


climate$methods(add_start_rain = function(data_list=list(), earliest_day=92, total_days=2, rain_total=20, dry_length=30,
                                              dry_days=10, dry_spell_condition=FALSE, threshold = 0.85, col_name = "Start of Rain",
                                              replace=FALSE) {
  
  data_list=add_to_data_info_required_variable_list(data_list, list(rain_label))
  data_list=add_to_data_info_time_period(data_list, daily_label)
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    
    summary_obj <- get_summary_name(yearly_label, data_obj)

    # use get_meta to determine the correct threshold value to use
    threshold = data_obj$get_meta_new(threshold_label,missing(threshold),threshold)
    
    # to do
    continue = TRUE
    
    if(col_name %in% names(summary_obj$get_data()) && !replace) {
      message(paste("A column named", col_name, "already exists. The column will not be replaced.
                     To replace to column, re run this function and specify replace = TRUE."))
      continue = FALSE
    }
    
    if(col_name %in% names(summary_obj$get_data()) && replace) {
      message(paste("A column named", col_name, "already exists. The column will replaced 
                    in the data."))
    }
    
    # 3. check if definition already exists, then do not add
    curr_definition = list(earliest_day = earliest_day, total_days = total_days, 
                           rain_total = rain_total, dry_spell_condition = dry_spell_condition, 
                           threshold = threshold)

    if( continue && summary_obj$is_definition(rain_label,start_of_label,curr_definition)) {
      message("A column with this defintion already exists in the data.
              The column will not be added again.")
      continue = FALSE
    }
    
    if(continue) {
    
      #if doy or year/dos is not in the data frame, create it.
      if( !( data_obj$is_present(dos_label) && data_obj$is_present(season_label) ) ) {
        data_obj$add_doy_col()
      }
      
      # get names of columns in the data
      rain_col = data_obj$variables[[ rain_label ]]
      dos_col = data_obj$variables[[ dos_label ]]
      season_col = data_obj$variables[[ season_label ]]
      
      # column to store day of year of start of the rain
      start_of_rain_col <- list()
      
      # Use an empty data_list here because we want to calculate start of rains
      # for the whole data set.
      curr_data_list = data_obj$get_data_for_analysis(data_info = list())
      
      # adding start of rain column
      for(curr_data in curr_data_list ) {
        
        # split the data by year to do calculations
        seasons_split <- split(curr_data[,c(dos_col,rain_col)], list(as.factor(curr_data[[season_col]])))
        
        
        j = 1 
        for( single_season in seasons_split ) {
          
          # initialize to NA incase conditions are never met
          start_of_rain_col[j] = NA
          
          # initialize current earliest day
          curr_earliest_day = earliest_day
          
          # if dry spell required use the simple sum_check to get start of the rain
          if(!dry_spell_condition) {
            start_of_rain_col[j] = sum_check(single_season, curr_earliest_day, total_days, rain_total)[1]
          }
          
          else {
            # If sum and dry spell conditions are required
            
            # indicates whether both conditions have been met and 
            # start of rain has been found
            # initialize to FALSE
            found = FALSE
            
            num_rows = nrow(single_season)
            
            # while start of the rain has not been found and our earliest day to check is not too
            # close to the end of year we continue looking for the start of the rain
            # if the dry_length is greater than the remaining number of rows
            # we will not be able to check for dry spells so we cannot get a start of the rain
            # NA will be returned
            if(data_obj$meta_data$data_name=="chief") {
              print(dry_length)
              print(num_rows)
              print(curr_earliest_day)
              
            }
            while( !found && sum(single_season[[1]]==curr_earliest_day)>0 && dry_length <= num_rows -  which(single_season[[1]]==curr_earliest_day) ) {
              # get the first day after earliest_day which is over rain_total
              day = sum_check(single_season, curr_earliest_day, total_days, rain_total)[1]
              
              # if the dry_length is greater than the remaining number of rows
              # we can no longer check for dry spells so end the loop
              # also if day is missing, end the loop.
              # NA will be returned
              if( is.na(day) || dry_length > num_rows - which(single_season[[1]]==day) ) break
              
              # start day to check for a dry spell is the day after the day found by sum_check
              start_row = which(single_season[[1]]==day+1)
              
              # if there is no dry spell we have found the start of the rain
              # found = TRUE will mean the loop does not run again
              if( !dry_spell_check(single_season[start_row:num_rows, 2], dry_length, dry_days, threshold) ) {
                start_of_rain_col[j] = day
                found = TRUE
              }
              else {
                # in the worst case there was a dry spell of length dry_days start after day.
                # The next check should begin after this potential dry spell.
                # if this day is beyond the end of the year, exit the loop to return NA.
                if(is.na(which(single_season[[1]]==day + dry_length))) break
                else curr_earliest_day = day + dry_length
              }
            }
          }
          j = j + 1
        }
      }
      # append this column to the yearly summary for each data object
      summary_obj$append_column_to_data(start_of_rain_col, col_name)
      label = summary_obj$get_summary_label(rain_label, start_of_label, curr_definition)
      summary_obj$append_to_variables(label,col_name)
      
    }  
  }
  
}
)

climate$methods(display_water_balance = function(data_list = list(), print_tables = TRUE, col_name = "Water Balance",
                                                 capacity_max = 100, evaporation = 5, decimal_places = 0, 
                                                 months_list = month.abb, day_display = "Day")
{
  
  # rain required
  data_list=add_to_data_info_required_variable_list(data_list, list(rain_label))
  
  # date period is "daily"
  data_list=add_to_data_info_time_period(data_list, daily_label)
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    
    # check if the waterbalance column is present
    if( !(data_obj$is_present(waterbalance_label)) ) {
      # If not, add the column
      if(missing(capacity_max)) {
        data_obj$add_water_balance_col(col_name=col_name,evaporation=evaporation)
      }
      else { data_obj$add_water_balance_col(col_name,capacity_max,evaporation) }
    }
    
    # Do this after if to save repeating 
    waterbalance_col = data_obj$getvname(waterbalance_label)
    
    # Must add these columns if not present to display this way
    if( !(data_obj$is_present(year_label) && data_obj$is_present(month_label) && data_obj$is_present(day_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    
    year_col = data_obj$getvname(year_label)
    month_col = data_obj$getvname(month_label)
    day_col = data_obj$getvname(day_label)
    
    # This is always how we access data in methods now.
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      
      # Added extra argument decimal_places to allow flexibility
      curr_data[[waterbalance_col]] <- round(curr_data[[waterbalance_col]], digits = decimal_places)
      
      
      # initialize list of tables
      tables = list()
      
      # Split curr_data into single data frames for each year
      # It returns a list of data.frames, split by year 
      # This is much faster (6x faster when I checked) than subsetting
      # Split is not always appropriate but it is in this case
      years_split <- split(curr_data, list(as.factor(curr_data[[year_col]])))
      
      # counter to use in the loop
      i = 1
      
      # loop through the split data frames 
      for ( single_year in years_split ) {
        
        # Make data into table - rows:days, columns:months, values:water balance
        tables[[i]] <- dcast(single_year, single_year[[ day_col ]]~single_year[[ month_col ]], value.var = waterbalance_col)
        
        # Rename columns
        # Added day_display and months_list as extra arguments so it is more flexible
        
        end = length(colnames(tables[[i]]))
        names(tables[[i]])[ 1 ] <- day_display
        colnames(tables[[i]])[2:end] <- months_list[1:end-1]
        i = i + 1
      }
      
      # The names of years_split is the list of years as strings.
      # These are better labels than numbers so they can be identified better
      names(tables) <- names(years_split)
    }
    
    # Only print if requested
    if(print_tables) {print(tables)} 
    
    # Always return the tables list
    # If we don't return and don't print then the method does nothing!
    return(tables)
    
  }
  
}
)

climate$methods(new_plot = function() {

}
)


climate$methods(cumulative_exceedance_graphs = function(data_list=list(),interest_var,cumulative_graph =TRUE,
                                                 color=rainbow(12),percent=TRUE, main="", 
                                                 xlabel="",ylabel="",
                                                 convert=TRUE, data_period_label=daily_label,
                                                 legend_position="center",legend_label=c("plot1","plot2"))
{  
  
  if (!is.list(interest_var)){
    interest_var=list(interest_var)
  }
  data_list=add_to_data_info_required_variable_list(data_list, interest_var)  
  data_list=add_to_data_info_time_period(data_list, data_period_label)
  data_list=c(data_list,convert_data=convert)
  climate_data_objs_list = get_climate_data_objects(data_list)
  #print(climate_data_objs_list)
  #print(data_list)
  
  for(data_obj in climate_data_objs_list) {
    
    data_name = data_obj$get_meta(data_name_label)    
    
    # Access data in methods
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    #print(curr_data_list)
    #-----------------------------------------------------------------------------------#
    #print(curr_data_list)
    
    for( curr_data in curr_data_list ) {
      #---------------------------------------------------------------------------------#
      
      sort_col <- list()
      prop_col <- list()
      cum_perc_col <- list()
      exceedance_col <- list()
      exceedance_prop <- list()
      for (i in 1:length(interest_var)) 
        {
        interest_col=data_obj$getvname(interest_var[[i]])
      
          # sort the data
        sort_col[[i]]=sort(curr_data[[interest_col]])
      #---------------------------------------------------------------------------------#
      #---------------------------------------------------------------------------------#
      #calculate the proportions
      #---------------------------------------------------------------------------------#
      
        prop_col[[i]]=(1:length(sort_col[[i]]))/length(sort_col[[i]])
      
        if (percent == TRUE){     
      #--------------------------------------------------------------------------------#
     
      #calculate the percentage of the cumulative proportions
      #--------------------------------------------------------------------------------#
      
          cum_perc_col[[i]]= prop_col[[i]]*100 
      #------------------------------------------------------------------------------
      #=====Add the values for plotting the exceedance graph==========================
      #--------------------------------------------------------------------------------#
          exceedance_col[[i]]=100-cum_perc_col[[i]]        
         }else {
           
          #Values for exceedance graph using the probabilities     
          exceedance_prop[[i]]=1-prop_col[[i]] 
          }
      }
      #print(exceedance_prop)
      #====Plotting the cumulative graph when true=====================================
      #----------------------------------------------------------------------------------#
      
      if(cumulative_graph == TRUE){
        par(new=FALSE)
        for (i in 1:length(sort_col)){
          #--------------------------------------------------------------------------------#
          #====Plotting the cumulative================================================
          if (percent ==TRUE){
            plot(sort_col[[i]], cum_perc_col[[i]],
                 main=c(data_name,main), xlab=xlabel, ylab=ylabel,type="o", col=color[i],
                 xlim=range(sort_col),ylim=range(cum_perc_col))
            }else{
              plot(sort_col[[i]], prop_col[[i]],main=c(data_name,main), xlab=xlabel, 
                   ylab=ylabel,type="o", col=color[i],
                   xlim=range(sort_col),ylim=range(prop_col))
              }
          par(new=TRUE)
          }
        }else{
          par(new=FALSE)
          #====Plotting the exceedance graph  when true======================================== 
          for (i in 1:length(sort_col)){
            #--------------------------------------------------------------------------------#
            if(percent == TRUE){
              # Plotting the exceedance graph
              plot(sort_col[[i]], exceedance_col[[i]],xlab=xlabel,ylab=ylabel,xlim=range(sort_col),
                   ylim=range(exceedance_col),col=color[i], main=c(data_name,main))
              #         par(new=TRUE)
              }else{
                plot(sort_col[[i]], exceedance_prop[[i]],xlab=xlabel,ylab=ylabel,xlim=range(sort_col),
                     ylim=range(exceedance_prop),col=color[i],main=c(data_name,main))
                }
            par(new=TRUE)
            }
          }
      if (length(interest_var)>1){
        legend(legend_position,legend_label,fill = color, bty = "n")
        }
      }
    }
  }
)#To Do: same plot window. Add line type as arguement. Add sensible defaults to the plot.

#===================================================================================================
# this name will be vertical line at the end of this method.
climate$methods(yearly_vertical_line = function(data_list=list(), all=list(), ylabel= "Meaningfull Text", col1 = "blue", type1 = "h",type2="p",
                                                col2 = "red", col3 = "green", xlabel = "Year", pch1 = 1, pch2 = 1, pch3 = 1, data_period_label = yearly_label)
{    
  # get_climate_data_objects returns a list of the climate_data objects specified
  # in the arguments.
  # If no objects specified then all climate_data objects will be taken by default
  
  # the col_var1 and col_var2 must be label. e.g col_var1_label
  # I should be able to use many variables. Can I make a list of variables? Yes we can. 
  # The analysis should take account of the structure of the data.
  data_list = add_to_data_info_required_variable_list(data_list, all) 
  #data_list = add_to_data_info_required_variable_list(data_list, list(col_var2))
  # we should be able to specify any time period. we need to fix this. Danny is working on it. 
  data_list = add_to_data_info_time_period(data_list, data_period_label) 
  
  #data_list = c(data_list, convert_data = FALSE)
  
  climate_data_objs_list = get_climate_data_objects(data_list)
  
  #print(climate_data_objs_list)
  
  for(data_obj in climate_data_objs_list) {
    
    # we need to get the column of interest for the plot.
    # The columns of interest are required so we don't need to check if there are present
    #I have to see how to use ggplot function
    for(i in 1:length(all)){
      col_var1[[i]] = data_obj$getvname(all[[i]])
      #print(col_var1)
    }
    
   # col_var1 = data_obj$getvname(all[[1]])
    #col_var2 = data_obj$getvname(all[[2]])
   
    
    
    data_obj$date_col_check(date_format = "%d/%m/%Y", convert = TRUE, create = TRUE, messaging=TRUE)
    
    date_col = data_obj$getvname(date_label)
    
    #adding year column if not present 
    if( !(data_obj$is_present(year_label) && data_obj$is_present(month_label) && data_obj$is_present(day_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    year_col = data_obj$getvname(year_label)
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      # plotting the first plot.  
      plot(curr_data[[ year_col ]], curr_data[[col_var1]], type = type1, lwd=2, col=col1, xlab=xlabel,ylab=ylabel,
           ylim = c( range( curr_data[[col_var1]], curr_data[[col_var2]], na.rm = TRUE) ))
      #Adding points to the plot
      lines(curr_data[[ year_col ]], curr_data[[col_var1]], type=type2, col=col2, pch = pch1)
      #Adding the second plot
      points(curr_data[[ year_col ]], curr_data[[col_var2]], type = type1, col=col3, pch = pch2 )
      
      #Adding points to the second plot
      lines(curr_data[[ year_col ]], curr_data[[col_var2]], type=type2, col=col2, pch = pch1)
    }
    
  }
}
)
#===========================================================================================
climate$methods(yearly_trellis_plot = function(data_list = list(),interest_variable,xlab = "Year",ylab,layout = c(6, 2),fac_column,
                                                    main_title = "Plot - Trellis Plot")
{  
  require(lattice)
  
  # get_climate_data_objects returns a list of the climate_data objects specified
  # in the arguments.
  # If no objects specified then all climate_data objects will be taken by default
  
  data_list = add_to_data_info_required_variable_list(data_list, list(interest_variable))
  data_list = c(data_list, convert_data=TRUE)
  
  data_list = add_to_data_info_time_period(data_list, daily_label)
  climate_data_objs_list = get_climate_data_objects(data_list)
  #print(data_list)
  # print(climate_data_objs_list)
  
  for(data_obj in climate_data_objs_list) {
    data_name = data_obj$get_meta(data_name_label)
    
   
    interest_col = data_obj$getvname(interest_variable)
    
    factor_col = data_obj$getvname(fac_column)
    
    # Must add these columns if not present.
    if( !(data_obj$is_present(year_label) && data_obj$is_present(month_label) && data_obj$is_present(day_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    year_col = data_obj$getvname(year_label)
    month_col = data_obj$getvname(month_label)
    #day_col = data_obj$getvname(day_label)
    
    
    # if ylabel is missing, use the column name of the interest variable. 
    if(missing(ylab)){
      ylab = data_obj$getvname(interest_variable)
    }
        
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      # the factor column should be a factor vector giving the name of the class.
      plot1 <- xyplot(curr_data[[ interest_col]]  ~ curr_data[[year_col]] | curr_data[[factor_col]],
                      layout = layout,
                      panel = function(x, y) {
                        #panel.grid(v=2) 
                        panel.xyplot(x, y)
                        panel.loess(x, y)
                        panel.abline(lm(y~x))
                      },
                      xlab = xlab,
                      ylab = ylab, main = c(data_name, main_title))
      print(plot1)
      
    }  
  } 
  #fitted line by month.  
#   # this gives 12 month intercept and one gradient.
#   summary(lm(curr_data[[ interest_col]]  ~ curr_data[[year_col]]+curr_data[[factor_col]]/curr_data[[factor_col]]))

#   # this gives the common intercept. 
#  summary(lm(curr_data[[ interest_col]]  ~ curr_data[[year_col]]/curr_data[[factor_col]]))
# I think this is not useful.
# The coefficients in both cases cannot be interpreted as a simple gradient and intercept for each month: 
# the gradients for month2 and so on are modelled as an additive increment on the first month gradient.
}
)

#=================================================================================
climate$methods(Plot_yearly_sumamry = function (data_list=list(), col1="blue",ylab,xlab="Year",pch=20,type="b",lty=2,col2="red",lwd = 2,interest_var,var_label = rain_label,
                                                        main_title="Plot - Summary per Year")
{
  # rain required
  data_list = add_to_data_info_required_variable_list(data_list, list(var_label))
  # convert data 
  data_list = c(data_list, convert_data=TRUE)
  # time period
  data_list = add_to_data_info_time_period(data_list, yearly_label)
  
  climate_data_objs = get_climate_data_objects(data_list)
  
  #print(length(climate_data_objs))
  
  for(data_obj in climate_data_objs) {
    data_name = data_obj$get_meta(data_name_label)
    
    # Must add these columns if not present to display this way
    if( !(data_obj$is_present(year_label) ) ) { 
      #data_obj$add_year_month_day_cols()
      data_obj$add_year_col() 
    }
    year_col = data_obj$getvname(year_label)
        
    # get the total rain. we need to think about a good way to do this from getvname.
    interset_var_col = data_obj$getvname (interest_var) #   "rain total 1"
    
    if(missing(ylab)){
      ylab = data_obj$getvname(interest_var)
    }
        
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    # loop for plotting 
    for( curr_data in curr_data_list ) { 
      # curr_data should have two columns which are year and rainfall totals 
      plot_yearly_summary <- plot(curr_data[[year_col]], curr_data[[interset_var_col]],type=type,pch=pch,xlab=xlab, col=col1,ylim= c(0, max(curr_data[[interset_var_col]], na.rm=TRUE)),
                          xlim = c( min(curr_data[[year_col]], na.rm=TRUE), max( curr_data[[year_col]], na.rm=TRUE)),
                          ylab=ylab, main= c( data_name, main_title))
       abline(h = mean(curr_data[[interset_var_col]][curr_data[[interset_var_col]] > 0]),lty=lty,col=col2)  
       grid(length(curr_data[[year_col]]),0, lwd = lwd)
      
      
      #reg=lm(curr_data[[interset_var_col]] ~ curr_data[[year_col]], na.rm = TRUE)
      #abline(reg,col="red",lwd=1.5)
      #summary(reg)
      
    }
  } 
  
}
)

#====================================================================================================
climate$methods(Boxplot = function(data_list= list(), fill_col="blue",interest_var,factor=month_label,
                                                             whisker_col="red", convert=TRUE,data_period_label=daily_label,
                                                             title="Rain Amount boxplot",whisklty=1,xlab="Months",
                                                             ylab=interest_var,horizontal=FALSE){
  #--------------------------------------------------------------------------------------------#
  # This function plots the boxplot for the variable of interest i.e interest_var
  # To Do: how to handle threshold    
  #-------------------------------------------------------------------------------------------#
  
  # Specifying the needed variable
  data_list = add_to_data_info_required_variable_list( data_list, list(interest_var,factor))
  #Using convert_data
  data_list=c(data_list,convert_data=convert)
  # Specifying the data_time_period
  data_list=add_to_data_info_time_period( data_list, data_period_label)
  # use data_list to get the required data objects
  climate_data_objs = get_climate_data_objects( data_list ) 
  
  
  for( data_obj in climate_data_objs){
    #threshold = data_obj$get_meta_new(threshold_label,missing(threshold),threshold)
    data_name = data_obj$get_meta(data_name_label)
    
    # Access data in methods
    curr_data_list = data_obj$get_data_for_analysis(data_list)
   
    for( curr_data in curr_data_list ) {
      interest_col=data_obj$getvname(interest_var)
      
      # Get the title of the column of factors
     
      factor_col = data_obj$getvname(factor) 
      #print(curr_data[[factor_col]])
      # Draw the boxplot
      boxplot(as.formula(paste(interest_col,"~",factor_col)), data=curr_data, whiskcol=whisker_col,col=fill_col, xlab=xlab,ylab=ylab,
               main= c( data_name, title), whisklty=whisklty,horizontal=horizontal)
    } 
  }
}
)

#=======================================================================================================================
climate$methods(summary_statistics = function(data_list=list(),interest_var, Proportions=c(),counts=TRUE, percents=FALSE,
                                              data_period_label=daily_label, digits=4, statistics=TRUE, percentiles=c(),
                                              convert=FALSE,na.rm=FALSE,append=TRUE,output_name="my_output3.txt")
  
{    
  if (!is.list(interest_var)){
    interest_var=list(interest_var)
  }  
  data_list=add_to_data_info_required_variable_list(data_list, interest_var)
  data_list=c(data_list,convert_data=convert)
  data_list=add_to_data_info_time_period(data_list, data_period_label)
  climate_data_objs_list = get_climate_data_objects(data_list)
  #print(climate_data_objs_list)
  #print(data_list)
  for(data_obj in climate_data_objs_list) {
    data_name = data_obj$get_meta(data_name_label)
    # Access data in methods
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    #print(curr_data_list)
    for( curr_data in curr_data_list ) {
      
      #Check if the column of interest is inputted
      #---------------------------------------------------------------------------------#      
      for (j in 1:length(interest_var)){
      interest_col=data_obj$getvname(interest_var[[j]])
      
      #---------------------------------------------------------------------------------#
      #Check if the vector of proportions is inputted
      #---------------------------------------------------------------------------------#
   
      sink(output_name,append=append,type="output")
      
      print(paste("summary statistics for",interest_col,"in",data_name,"data"))
      
       if (statistics==TRUE){
        #-------------------------------------------------------------------------------#
        #get the number of observations 
        #-------------------------------------------------------------------------------#
        print(paste(" data No. of observations:",  nrow(curr_data)))
        
        #-------------------------------------------------------------------------------#
        #get the minimum, mean, median and the maximum from the column of interest
        #-------------------------------------------------------------------------------#
        print(summary(curr_data[[interest_col]]))
        
        #------------------------------------------------------------------------------#
        #get the range of the data
        #------------------------------------------------------------------------------#
        print(paste("Range:", max(curr_data[[interest_col]], na.rm = na.rm) - min(curr_data[[interest_col]], na.rm = na.rm) ))
        
        #-------------------------------------------------------------------------------#
        #calculate the standard deviation
        #-------------------------------------------------------------------------------#
        print(paste("sd.deviation:", round(sd(curr_data[[interest_col]], na.rm = na.rm), digits =digits )))
        
        #-------------------------------------------------------------------------------#
        #calculate the percentiles
        #-------------------------------------------------------------------------------#
        if( !(length( percentiles ) == 0 ) ) {
          print(quantile(curr_data[[interest_col]], percentiles,na.rm=na.rm))
        }
      }
      
      #---------------------------------------------------------------------------------#
      #Initializing empty vectors and looping to get the counts and percentages 
      #---------------------------------------------------------------------------------#
      count=c()
      percent=c()
      for (i in 1:length(Proportions)){
        
        #---------------------------------------------------------------------------------#
        #returns count only if true
        #---------------------------------------------------------------------------------#
        if (counts==TRUE){
          count[i]=sum(curr_data[[interest_col]]<=Proportions[i], na.rm = na.rm)
          print(paste("count <=", Proportions[i], "is", count[i]))
        }
        
        #----------------------------------------------------------------------------------#
        #returns percents only if true
        #----------------------------------------------------------------------------------#
        if (percents==TRUE){
          percent[i]=round((count[i]/nrow(curr_data))*100,digits=digits)
          print(paste("% of data <=", Proportions[i], "is", percent[i]))
        }
      }
    }
    sink()
#     unlink("my_output.txt")
    }
  }
}
)
# After looking and findout that the function ggplot can plot multiple columns on the same plot,
# We can adopt it for yearly_vertical_line method easly
#=========================================================================================================

climate$methods(vertical_line = function(data_list=list(), all, data_period_label = yearly_label)
{   
  require(ggplot2)
  require(reshape)
  
  # get_climate_data_objects returns a list of the climate_data objects specified
  # in the arguments.
  # If no objects specified then all climate_data objects will be taken by default
  
  # Can I use a list of variables? Yes . 
  # The analysis should take account of the structure of the data.
  data_list = add_to_data_info_required_variable_list(data_list, all) 
  # we should be able to specify any time period. we need to fix this. Danny is working on it. 
  data_list = add_to_data_info_time_period(data_list, data_period_label) 
    
  climate_data_objs_list = get_climate_data_objects(data_list)
    
  for(data_obj in climate_data_objs_list) {
    
    # we need to get the column of interest for the plot.
    # since the column of interest is a list, the loop gets all at the same time.
    interest_variable =list()
    for(i in 1:length(all)){
      
      interest_variable[[i]] <- data_obj$getvname(all[[i]]) 
    }
   #print(interest_variable)
    
    
    date_col = data_obj$getvname(date_label)
    
    #adding year column if not present 
    if( !(data_obj$is_present(year_label)) ) {
      data_obj$add_year_col()
    }
    year_col = data_obj$getvname(year_label)
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      # subset the data. Here get only time period and the interest variables 
      dat <- subset(curr_data, select=c( year_col, interest_variable = unlist(interest_variable)))
      print(head(dat))
      #Melt the data into a form suitable for easy casting
      dat2 <- melt(dat ,  id = 'Year')
      print(head(dat2))
      print(names(dat2))
     #"Year"     "variable" "value"
      # plot all variables on the same graph
      # Need to read more about ggplot bcse here it is not plotting.
      #?ggplot() is typically used to construct a plot incrementally.
      ggplot(dat2, aes(Year, value)) +
      geom_histogram(  position="dodge",  stat = "identity", aes(fill = variable))
     
    }
    
  }
}
)



# dd <- subset(data, select=c(Date,Start.of.Rain..i.,Start.of.Rain..ii.,Start.of.Rain..iii.))
# names(dd)
# View(dd)
# d2 <- melt(dd ,  id = 'Date')
# #ggplot(d2, aes(Date,value)) + geom_line(aes(colour = variable))
# 
# ggplot(d2, aes(Date, value)) +
#   geom_histogram(  position="dodge",  stat = "identity", aes(fill = variable))
#==================================================================================================


climate$methods(compute_raindays = function(data_list = list(), monStart=1, monEnd=3, threshold=0.85,
                                            na.rm=FALSE)
{
  
  # rain required
  data_list=add_to_data_info_required_variable_list(data_list, list(rain_label))
  # date time period is "daily"
  data_list=add_to_data_info_time_period(data_list, daily_label)
  # a list of climate data objects
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    data_name = data_obj$get_meta(data_name_label)
    
    curr_threshold = data_obj$get_meta(threshold_label,threshold)
    
    rain_col  = data_obj$getvname(rain_label)
        
    # Must add these columns if not present to compute raindays
    if( !(data_obj$is_present(year_label) && data_obj$is_present(month_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    
    year_col = data_obj$getvname(year_label)
    month_col = data_obj$getvname(month_label)
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    # loop for computing 
    for( curr_data in curr_data_list ) { 
          selRows <- curr_data[[month_col]]>=monStart & curr_data[[month_col]] <=monEnd & curr_data[[rain_col]] > curr_threshold
      ndays <- tapply(selRows, curr_data[[year_col]], sum, na.rm=na.rm)
      print(class(ndays))
       return(ndays)        
    }

  }
}

) #To Do: better way to present the output. Flexibility of the months- 1 or Jan or J or January

#=============================================================================================
climate$methods(compute_seasonal_summary = function(data_list = list(), month_start, month_end = 3, season_start_day = 1, print_season = FALSE, year = 1952,  na.rm = FALSE, season = "JFM", threshold = 0.85, col_name = "season_tot_JFM", col_name2 = "season_raindays_JFM",replace = FALSE)

{
  library(plyr)
  # rain required
  data_list = add_to_data_info_required_variable_list(data_list, list(rain_label))
  # date time period is "daily"
  data_list = add_to_data_info_time_period(data_list, daily_label)
  # a list of climate data objects
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
        
    
    curr_threshold = data_obj$get_meta(threshold_label,threshold)
    
    rain_col  = data_obj$getvname(rain_label)    
    
    summary_obj <- get_summary_name(yearly_label, data_obj)
    #print(summary_obj)
    
    continue = TRUE
    
    if(c(col_name,col_name2) %in% names(summary_obj$get_data()) && !replace) {
      message(paste("A column named", col_name, col_name2, "already exists. The column will not be replaced.
                     To replace to column, re run this function and specify replace = TRUE."))
      continue = FALSE
    }
    
    if(c(col_name,col_name2) %in% names(summary_obj$get_data()) && replace) {
      message(paste("A column named", col_name,col_name2, "already exists. The column will replaced 
                    in the data."))
    }
     
    if(continue) {
    # Must add this column if not present 
    if( !( data_obj$is_present(month_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    
    month_col = data_obj$getvname(month_label)


#     if(missing(month_start)){
#       month_start = match("Jan", month.abb)
#     }
  
    # If  year column is not in the data frame, create it.
    if ( !(data_obj$is_present(season_label))) {
      # add_doy_col function does not exist yet.
      data_obj$add_doy_col()
    }
    season_col = data_obj$getvname(season_label)

    curr_season_start_day = data_obj$get_meta(season_start_day_label,season_start_day)
   # print(curr_season_start_day)

    #print(date)
    
    if(missing(month_start)){
      date = doy_as_date(season_start_day, year)  
      month_start = month(date)
    }
    print(month_start)
#     mon = match(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), month.abb)
#     month_end = c()
#     for (i in 1:length(mon)){
#       month_end = mon[i]
#     }
#     print(month_end)
# 
#     if (! month_start %in% c(1, 2, 3, 4, 6, 12)) stop("Months must divide into 12!")
#     period <- 12/month_start
#     grps <- rep(1:period, each=month_start)
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    # loop for computing rain total
    for( curr_data in curr_data_list ) {
     # group <- grps[curr_data[[month_col]]]
      
      #CREATE THE SEASON FOR EACH YEAR
      curr_data$season<-""
      curr_data$season[curr_data[[month_col]] >= month_start & curr_data[[month_col]] <= month_end] <- season
      # seasonal column
      curr_data2<-curr_data[curr_data$season!="",]
      curr_data2$season<-paste(curr_data2[[season_col]],curr_data2$season,sep="")      
      # Add a column of rain to the data with a specific: "Rain" name for ddply use
      curr_data2 = cbind(curr_data2, new_rain_col=curr_data2[[rain_col]], season_col = curr_data2[[season_col]] )
      #Get summaries for each year and season in each year
      season.rain<-ddply(curr_data2,.( season_col ,season),summarize,sum( new_rain_col, na.rm = FALSE), length(na.omit(new_rain_col[new_rain_col > 0.85])))
      names(season.rain)<-c("Year","Season","season_total", "season_rain_days")
      print(class(season.rain$season_total))
      # Delete the new rain column added
      season.rain$new_rain_col=NULL

      # Only print if requested
      if(print_season) {print(season.rain)}
    }# curr_data
    summary_obj$append_column_to_data(season.rain$season_total, col_name)
    summary_obj$append_column_to_data(season.rain$season_rain_days, col_name2)
    label = summary_obj$get_summary_label(rain_label, seasonal_total_label)
    label2 = summary_obj$get_summary_label(rain_label, seasonal_raindays_label)
    summary_obj$append_to_variables(label, col_name)
    summary_obj$append_to_variables(label2,col_name2)

    }# if continue
  
  }# data_obj
}
)# We need to be able to get the summary for any season. forexample: Nov to Jan. 
# By now we have the season_col which gives us the year of the season from add_doy_col but don't have season_month_col.
# The function compute only for the season starting from Jan to Dec. 

# We also need to append the computed columns to the summary_obj. This is a challenge bcse ddply produce a data frame and summary_obj is also a data frame.







