#there are still some bits of work to be done to finish cleaning this code.
#Appending functions there is currently a mismatch between adding col's to the data and adding variables I think these should probably be joined together in certain cases. 
#To get this to work really properly with respect to the get, set and append functions we may want to move to an R6 class where issues of private and public are properly adressed.


# Defining the reference class "climate_data".
# The fields are the properties every climate_data object will have.

climate_data <- setRefClass("climate_data", 
                            fields = list(data = "data.frame", meta_data = "list", 
                                          variables = "list", changes = "list", data_time_period="character")
                            ) #, split_data = "list", is_data_split = "logical" removed for now this may have speed implications but can be adressed later if thhat is the case



# INITIALIZE method
##############################################################################################
# Functions of reference classes are called methods.
# This is how you define a method for a specific reference class.
# Every reference class has an initialize method which tells R how to create new 
# climate_data objects.
# By the end of this method, all fields of climate_data should be defined.
# Notice that we don't have a climate_data object as an input.
# We can refer to any field of a climate_data object by name. e.g. data or variables

climate_data$methods(initialize = function(data = data.frame(), data_name = "", meta_data = list(), 
                                           variables = list(),data_time_period=daily_label, imported_from = "", 
                                           messages = TRUE, identify_variables = TRUE, 
                                           start_point=1, check_dates=TRUE, check_missing_dates = TRUE, 
                                           date_format = "%m/%d/%Y")
  {

    # Set up the Climate data object
  
    .self$set_changes(list())
    .self$set_data(data, messages)
    .self$set_meta (add_defaults_meta(imported_from, meta_data))
    if (identify_variables) {
      .self$set_variables(add_defaults(imported_from, ident_var(data,variables)))
    }
    else{
      .self$set_variables(add_defaults(imported_from, variables))
    }
    .self$set_data_time_period(data_time_period)
    
    # If no name for the data.frame has been given in the list we create a default one.
    # Decide how to choose default name index
    if (!.self$is_meta_data(data_name_label)) {    
      if ( ( is.null(data_name) || data_name == "" || missing(data_name))) {
        meta_data[[data_name_label]] <<- paste0("data_set_",sprintf("%03d", start_point))
        if (messages) {
          message(paste0("No name specified in data_tables list for data frame ", start_point, ". 
                         Data frame will have default name: ", "data_set_",sprintf("%03d", start_point)))
        }
      }
      else meta_data[[data_name_label]] <<- data_name      
    }
    
    if (check_dates){
      .self$date_col_check(date_format=date_format)
    }
    
    if (check_missing_dates){
      .self$missing_dates_check()
    }

    .self$check_multiple_data()

    
  }
)




# Getter methods
##############################################################################################
# We can create methods to extract fields from a climate_data object.
# These are called getter methods and are usually very simple functions.
# Notice that no input is needed.

climate_data$methods(get_data = function() {
    return(data)
  }
)

climate_data$methods(get_data_for_analysis = function(data_info) {
#TO DO error checking
#This method for returning the subsets of the data is not optermised! It can be improved in a number of ways but should work very solidly and reliably in it's current form.

  merged_data=FALSE
  if (merge_data_label %in% names(data_info)){
    if (data_info[[merge_data_label]]){
      merged_data=TRUE
    }
  }
  return_data = data
#  if (merged_data) return_data = data
#  else{
#    .self$check_split_data()
#    return_data = split_data
#  }
  if (station_list_label %in% names(data_info) & .self$is_present(station_label)) {
    return_data=return_data[return_data[[.self$getvname(station_label)]]==data_info[[station_list_label]],] #TO DO check this syntax is correct
  }
  if (date_list_label %in% names(data_info)) {
    for (tempname in names(data_info[[date_list_label]])){
      if (.self$is_present(tempname)){
        return_data=return_data[return_data[[.self$getvname(tempname)]]==data_info[[date_list_label]][[tempname]],] #TO DO check this syntax is correct and also add functionallity for start and end dates.
      }
    }
  }  
  if (!merged_data) return_data = .self$get_split_data(return_data)
  return (return_data)

}
)

climate_data$methods(get_variables = function() {
    return(variables)
  }
)

#TO DO replace all direct calls with this? Or remove this
climate_data$methods(getvname = function(label) {
  return(variables[[label]])
}
)

climate_data$methods(get_changes = function() {
    return(changes)
  }
)

climate_data$methods(get_data_time_period = function() {
  return(data_time_period)
}
)

climate_data$methods(get_meta = function(label="", overrider="") {

  if (label=="") return(meta_data)
  else if ( !(is.na(overrider)||(overrider=="")||missing(overrider) )) return(overrider)
  else if (.self$is_meta_data(label)) return(meta_data[[label]])
  else return (overrider)
}
)

climate_data$methods(get_meta_new= function(label="", value_missing = FALSE, overrider="") {
  
  if (label=="") return(meta_data)
  else if ( !(missing(overrider)) && !(is.na(overrider)) && !value_missing ) return(overrider)
  else if (.self$is_meta_data(label)) return(meta_data[[label]])
  else return (overrider)
}
)

# Setter methods
##############################################################################################
# Similar to getter methods but used for setting a new value to one of the fields
# We usually do some validation before assigning.
#TO DO these setting methods are very dangerous if called directly, we need to either be much more carefull or make them private

climate_data$methods(set_data = function(new_data, messages=TRUE) {
    if( ! is.data.frame(new_data) ) {
      stop("Data set must be of type: data.frame")
    }
    else {
      if ( length(new_data) == 0 && messages) {
        message("data of object:is empty. data will be an empty data frame.")
      }
      data <<- new_data
      .self$append_to_changes(list(Set_property, "data"))
#      is_data_split<<-FALSE
    }
  }
)

climate_data$methods(set_meta = function(new_meta) {
    if( ! is.list(new_meta) ) {
      stop("Meta data must be of type: list")
    }
    
    else {
      meta_data <<- new_meta
      .self$append_to_changes(list(Set_property, "meta data"))
    }
  }
)


climate_data$methods(set_variables = function(new_variables) {
    if( ! is.list(new_variables) ) {
      stop("Variables must be of type: list")
    }
    
    else {
      variables <<- new_variables
      .self$append_to_changes(list(Set_property, "variables"))
    }
  }
)

climate_data$methods(set_changes = function(new_changes) {
    if( ! is.list(new_changes) ) {
      stop("Changes must be of type: list")
    }
    
    else {
      changes <<- new_changes
      .self$append_to_changes(list(Set_property, "changes"))  }
  }
)

climate_data$methods(set_data_time_period = function(new_data_time_period) {
    if( ! is.character(new_data_time_period) ) {
      stop("Changes must be of type: character")
    }
    
    else {
      data_time_period <<- new_data_time_period
      .self$append_to_changes(list(Set_property, "data_time_period"))  }
  }
)

############################################################################################
# We may want to add something to a field instead of replacing the whole field.
# For that we use append methods.


climate_data$methods(append_column_to_data = function(column_data, col_name = "", label) {
  
    # Column name must be character
    if( ! is.character(col_name) ) {
      stop("Column name must be of type: character")
    }
    
    # Column data length must match number of rows of data.
    else if ( !( length(column_data) == nrow(data) ) )
      stop(paste("Number of rows in new column does not match the number of rows in the data set.
                 There must be", nrow(data), "entries in the new column."))
    
    else {
      # If no name given, generate a default column name.
      if (col_name == "") {
        col_name = paste0("column_",sprintf("%02d", length(names(data))+1))
      }
      column_data <- unlist(column_data)
      data[[col_name]] <<- column_data
      .self$append_to_changes(list(Added_col, col_name))
      
      if(!missing(label)) {
        .self$append_to_variables(label,col_name)
      }

    }
  }
)

climate_data$methods(replace_column_in_data = function(col_name = "", column_data) {
    
    # Column name must be character
    if( ! is.character(col_name) ) {
      stop("Column name must be of type: character")
    }
    
    else if (!(col_name %in% names(data))) {
      stop(paste0("Cannot replace column: ",col_name,". Column was not found in the data."))
    }
    
    # Column data length must match number of rows of data.
    else if ( !( length(column_data) == nrow(data) ) )
      stop(paste("Number of rows in new column does not match the number of rows in the data set.
                 There must be", nrow(data), "entries in the new column."))
    
    else {
      data[[col_name]] <<- column_data
      .self$append_to_changes(list(Replaced_col, col_name))
#      is_data_split<<-FALSE
    }
    if(col_name %in% variables) {
      label = names(variables[col_name])
      .self$append_to_variables(label,col_name)
    }
  }
)

climate_data$methods(rename_column_in_data = function(curr_col_name = "", new_col_name="") {
  
  # Column name must be character
  if( ! is.character(curr_col_name) ) {
    stop("Current column name must be of type: character")
  }
  
  else if (!(curr_col_name %in% names(data))) {
    stop(paste0("Cannot rename column: ",curr_col_name,". Column was not found in the data."))
  }
  
  else if (! is.character(new_col_name)) {
    stop("New column name must be of type: character")
  }

  else {
    if(sum(names(data) == curr_col_name) > 1) {
      warning(paste0("Multiple columns have name: '", curr_col_name,"'. All such columns will be 
                     renamed."))
    }
    names(data)[names(data) == curr_col_name] <<- new_col_name
    .self$append_to_changes(list(Renamed_col, curr_col_name, new_col_name))

  }
}
)

climate_data$methods(remove_column_in_data = function(col_name = "") {
  
  # Column name must be character
  if( ! is.character(col_name) ) {
    stop("Column name must be of type: character")
  }
  
  else if (!(col_name %in% names(data))) {
    stop(paste0("Column :'", col_name, " was not found in the data."))
  }
  
  else {
    data[[ col_name ]] <<- NULL 
    .self$append_to_changes(list(Removed_col, col_name))
  }
}
)

climate_data$methods(replace_value_in_data = function(col_name = "", index, new_value = "") {
  
  # Column name must be character
  if( ! is.character(col_name) ) {
    stop("Column name must be of type: character")
  }
  
  else if (!(col_name %in% names(data))) {
    stop(paste("Cannot find column:",col_name,"in the data."))
  }
  
  # Column data length must match number of rows of data.
  else if ( missing(index) || !(is.numeric(index)) ) {
    stop(paste("Specify the index of the value to be replaced as an integer."))
  }
  
  else if (   index != as.integer(index) || index < 1 || index >  nrow(data) ) {
    stop( paste("index must be an integer between 1 and", nrow(data), ".") )
  }
  
  if ( class(data[[col_name]][[index]]) != class(new_value)) {
    warning("Class of new value does not match the class of the replaced value.")
  }
  
  old_value = data[[col_name]][[index]]
  data[[col_name]][[index]] <<- new_value
  .self$append_to_changes(list(Replaced_value, col_name, index, old_value, new_value))
  
}
)


climate_data$methods(append_to_meta_data = function(name, value) {
  
    if( missing(name) || missing(value) ) {
      stop("name and value arguements must be specified.")
    } 
          
          
    else if ( ! is.character(name) ) {
      stop("name must be of type: character")
    }
    
    # Remember double brackets must be used when dealing with variable names.
    else {
      meta_data[[name]] <<- value 
      .self$append_to_changes(list(Added_metadata, name))
    }
  }
)

climate_data$methods(append_to_variables = function(label = "", value) {
  
    if( missing(label) || missing(value) ) {
      stop("label and value arguements must be specified.")
    } 
    
    
    else if ( ! is.character(label) ) {
      stop("label must be of type: character")
    }
    else {
      variables[[label]] <<- value 
      .self$append_to_changes(list(Added_col_label, label))
    }
  }
)

climate_data$methods(append_to_changes = function(value) {
  
    if( missing(value) ) {
      stop(" value arguements must be specified.")
    } 
    else {
      changes[[length(changes)+1]] <<- value 
    }
  }
)


# Other methods
############################################################################################

# is_present can check if a given variable name (or list of variable names) is in the data.frame or not.
# This will be used by other functions when doing calculations or to determine
# if extra columns should be added.
# TO DO check functionality for missing cols and if there are multiple elements in long format (currently will return true even if there are no instances possibly correct as like returning true when all values are missing?)

climate_data$methods(is_present = function(str, require_all=TRUE) {
  out = FALSE
  if (is.character(str)){
    if(str %in% names(variables) ) {
      var_name = variables[[str]]
      if(var_name %in% names(data)) {
        out = TRUE
      }
    }
  }
  else if (is.list(str)){
    for (temp in str){
      out=is_present(temp)
      if (require_all) if (!out) break
      if (!require_all) if (out) break
    }
  }
  return(out)
}
)

# is_meta_data can check if a given bit of meta data is available or not
# This will be used by other functions when doing calculations or to determine
# if extra columns should be added.

climate_data$methods(is_meta_data = function(str) {
  out = FALSE
  
  if(str %in% names(meta_data) ) {
      out = TRUE
  }
  return(out)
}
)

# check_multiple_data checks if there are factor columns with either more than one station or more than one element 
# This will be used to know if the data needs to be subsetted or not.

climate_data$methods(check_multiple_data = function() {

  if(!(.self$is_meta_data(multiple_station_label))) {
    if (.self$is_present(station_label)){
      if (nlevels(as.factor(data[[.self$getvname(station_label)]]))>1) meta_data[[multiple_station_label]]<<-TRUE
      else meta_data[[multiple_station_label]]<<-FALSE
    }
    else meta_data[[multiple_station_label]]<<-FALSE
  }
  if(!(.self$is_meta_data(multiple_element_label))) {
    if (.self$is_present(element_factor_label)){
      if (nlevels(as.factor(data[[.self$getvname(element_factor_label)]]))>1) meta_data[[multiple_element_label]]<<-TRUE
      else meta_data[[multiple_element_label]]<<-FALSE
    }
    else meta_data[[multiple_element_label]]<<-FALSE
  }
}
)

# check_split_data assigns the data to the split_data taking into account station and element TO DO this probably needs to be optermised for large data
# Replaced by get_split_data below since the split data is no longer stored
#
#climate_data$methods(check_split_data = function() {
#  
#  if(!(is_data_split)) {
#    if (meta_data[[multiple_station_label]]==TRUE & meta_data[[multiple_element_label]]==TRUE){
#      split_data <<- split(data, list(as.factor(data[[.self$getvname(station_label)]]),as.factor(data[[.self$getvname(element_factor_label)]])))
#    }
#    else if (meta_data[[multiple_station_label]]==TRUE){
#      split_data <<- split(data, as.factor(data[[.self$getvname(station_label)]]))
#    }
#    else if (meta_data[[multiple_element_label]]==TRUE){
#      split_data <<- split(data, as.factor(data[[.self$getvname(element_factor_label)]]))
#    }
#    else split_data <<- list(data)
#    is_data_split<<-TRUE
#  }
#}
#)

climate_data$methods(get_split_data = function(return_data) {
  
  if (meta_data[[multiple_station_label]]==TRUE & meta_data[[multiple_element_label]]==TRUE){
    split_data <- split(return_data, list(as.factor(return_data[[.self$getvname(station_label)]]),as.factor(return_data[[.self$getvname(element_factor_label)]])))
  }
  else if (meta_data[[multiple_station_label]]==TRUE){
    split_data <- split(return_data, as.factor(return_data[[.self$getvname(station_label)]]))
  }
  else if (meta_data[[multiple_element_label]]==TRUE){
    split_data <- split(return_data, as.factor(data[[.self$getvname(element_factor_label)]]))
  }
  else split_data <- list(return_data)
  return(split_data)
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

climate_data$methods(date_col_check = function(date_format = "%d/%m/%Y", convert = TRUE, create = TRUE, messaging=TRUE) 
  {    
    # Check if there is a date column already
    # Check if the date is in the Date class
    # If convert == TRUE
    # Convert class to date class
    if (data_time_period==daily_label){
      if (.self$is_present(date_label)) {
        date_col = variables[[date_label]]
        if (!class(data[[variables[[date_label]]]])=="Date"){
          if (messaging) message("date column is not stored as Date class.")
          if (convert == TRUE) {
            if (messaging) message("Attempting to convert date column to Date class.")
            new_col = as.Date(data[[date_col]], format = date_format)
            .self$replace_column_in_data(date_col,new_col)
          }
        } 
        
      }
      # If the year, month, day column are there and create == TRUE create date column
      else if (create == TRUE && is_present(year_label) && is_present(month_label) && is_present(day_label)) 
      {
        day_col = data[[getvname(day_label)]]
        month_col = data[[getvname(month_label)]]
        year_col = data[[getvname(year_label)]]

        if(all(month.abb %in% month_col)) {
          month_col = match(month_col,month.abb)
          replace_column_in_data(getvname(month_label),factor(data[[getvname(month_label)]], month.abb, ordered=TRUE))
        }
        
        if(all(month.name %in% month_col)) {
          month_col = match(month_col,month.abb)
          replace_column_in_data(getvname(month_label),factor(data[[getvname(month_label)]], month.name, ordered=TRUE))
        }

        #TO DO fix the issue with importing different types of month
        new_col = as.Date(paste(year_col, month_col, day_col, sep="-"))
#        print(sum(is.na(new_col)))
        .self$append_column_to_data(new_col, variables[[date_label]])
      }
      # Else if date string column is there and create == TRUE create date column
      else if (create == TRUE && is_present(date_asstring_label)) 
      {
        date_string_col = variables[[date_asstring_label]]
        new_col = as.Date(data[[date_string_col]], format = date_format)
        .self$append_column_to_data(new_col,variables[[date_label]])
      }
      #TO DO we should also be able to create this from year and doy
      else {warning("Cannot create or edit a date column. There is insufficient information in the
                    data frame to have a date column.")}
    }#TO DO else cases for non daily data.
  }
)

climate_data$methods(missing_dates_check = function()
{    
  if(!get_meta(complete_dates_label)) {
    date_col = getvname(date_label)
    if(data_time_period == daily_label) by = "day"
    else if(data_time_period == subyearly_label) by = "month"
    else if(data_time_period == yearly_label) by = "year"
    
    temp_start_date = doy_as_date(get_meta(season_start_day_label),year(min(data[[date_col]])))
    if(temp_start_date > min(data[[date_col]])) {
      start_date = temp_start_date
      year(start_date) <- year(start_date)-1
    }
    else {
      start_date = temp_start_date      
    }
    
    final_year = year(max(data[[date_col]]))
    final_month = month(start_date-1)
    final_day = day(start_date-1)
    temp_end_date = as.Date(paste(final_year,final_month,final_day,sep="-"))
    
    if(temp_end_date >= max(data[[date_col]])) {
      end_date = temp_end_date
    }
    else {
      end_date = as.Date(paste(final_year+1,final_month,final_day,sep="-"))
    }
    
    append_to_meta_data(data_start_date_label,start_date)
    append_to_meta_data(data_end_date_label,end_date)
    
    full_dates = seq(start_date, end_date, by = by)
    
    if(length(full_dates) != nrow(data)) {
      dates_table = data.frame(full_dates)
      names(dates_table) <- date_col
      if(is_present(year_label)) {
        year_col = getvname(year_label)
        dates_table[[year_col]] <- year(dates_table[[date_col]]) 
      }
      if(is_present(month_label)) {
        month_col = getvname(month_label)
        dates_table[[month_col]] <- month(dates_table[[date_col]]) 
      }
      if(is_present(day_label)) {
        day_col = getvname(day_label)
        dates_table[[day_col]] <- day(dates_table[[date_col]]) 
      }
      merged_data <- join(dates_table, get_data(), match="first")
      set_data(merged_data)
    }
    append_to_meta_data(complete_dates_label,TRUE)
  }
}
)

#add_year_month_day_cols simply converts a date column to Year month and day if they don't exist.
#TO DO Should be adapted for other types of data_time_period currently just for daily

climate_data$methods(add_year_month_day_cols = function(date_format="%d/%m/%Y", YearLabel="Year", MonthLabel="Month", DayLabel="Day")
{
  if (.self$is_present( date_label)){
    date_col = variables[[date_label]]
    if (!.self$is_present(year_label)){
#      append_column_to_data (as.numeric(format(as.POSIXlt(strptime(data[[date_col]], date_format)), format = "%Y")), YearLabel) this should not need more than strptime
      .self$append_column_to_data (year(data[[date_col]]), YearLabel) 
      .self$append_to_variables(year_label, YearLabel)
    }
    if (!.self$is_present(month_label)){
#      append_column_to_data (as.numeric(format(as.POSIXlt(strptime(data[[date_col]], date_format)), format = "%m")), MonthLabel) 
      .self$append_column_to_data (month(data[[date_col]]), MonthLabel) 
      .self$append_to_variables(month_label, MonthLabel)
    }    
    if (!.self$is_present(day_label)){
#      append_column_to_data (as.numeric(format(as.POSIXlt(strptime(data[[date_col]], date_format)), format = "%d")), DayLabel) 
      .self$append_column_to_data (day(data[[date_col]]), DayLabel) 
      .self$append_to_variables(day_label, DayLabel)
    }
  }
  else warning("No Date column check that your data has date information and create a date colum using date_col_check.")
  
}
)

climate_data$methods(add_doy_col = function(YearLabel="Year", DOYLabel="DOY", SeasonLabel="Season", DOSLabel="DOS")
  
{
  if (.self$is_present(date_label)){
    
    date_col = variables[[date_label]]
    if (!.self$is_present(year_label)){
      .self$append_column_to_data (year(data[[date_col]]), YearLabel) 
      .self$append_to_variables(year_label, YearLabel)
    }
    if (!.self$is_present(doy_label)){
      TEMPDOY = yday(data[[date_col]])      
      temply=leap_year(data[[date_col]])
      TEMPDOY[((TEMPDOY > 59) & (!(temply)))] <- 1 + TEMPDOY[((TEMPDOY > 59)&(!(temply)))]
      .self$append_column_to_data (TEMPDOY, DOYLabel) 
      .self$append_to_variables(doy_label, DOYLabel)
    }
    if (!(.self$is_present(dos_label)||.self$is_present(season_label))){
      #--------------------------------------------------------------#
      # find the specified start date of the year in 366 form
      #--------------------------------------------------------------#
      if (is_meta_data(season_start_day_label)){
        if (1<meta_data[[season_start_day_label]] & meta_data[[season_start_day_label]]<367){
          TEMPDOY <- data[[variables[[doy_label]]]]
          TEMPDOS <- TEMPDOY - meta_data[[season_start_day_label]] + 1
          #TO DO allow flexibility for how season is written.
          TEMPSEASON=data[[variables[[year_label]]]]
          TEMPSEASON[(TEMPDOS < 1)]<-paste(as.numeric(TEMPSEASON[(TEMPDOS < 1)])-1, TEMPSEASON[(TEMPDOS < 1)],sep = "-")
          TEMPSEASON[(TEMPDOS > 0)]<-paste(TEMPSEASON[(TEMPDOS > 0)], as.numeric(TEMPSEASON[(TEMPDOS > 0)])+1,sep = "-")
          TEMPDOS[(TEMPDOS < 1)] <- TEMPDOS[(TEMPDOS < 1)] + 366
          .self$append_column_to_data (TEMPDOS, DOSLabel) 
          .self$append_to_variables(dos_label, DOSLabel)
          .self$append_column_to_data (as.factor(TEMPSEASON), SeasonLabel) 
          .self$append_to_variables(season_label, SeasonLabel)
        }
        else{
          .self$append_to_variables(dos_label, DOYLabel)
          .self$append_to_variables(season_label, YearLabel)
        }
      }
      else{
        .self$append_to_variables(dos_label, DOYLabel)
        .self$append_to_variables(season_label, YearLabel)
      }      
    }
  }
  else warning("No Date column check that your data has date information and create a date colum using date_col_check.")  
}
)
#TO DO Conversions to other cases

climate_data$methods(summarize_data = function(new_time_period, day_format = "%d", month_format = "%b",
                                               year_format = "%Y", summarize_name = paste(.self$meta_data[[data_name_label]],new_time_period), 
                                               threshold = 0.85, na.rm = FALSE, start_point = 1, 
                                               num_rain_days_col = "Number of Rain Days", total_col = "Total",
                                               mean_col = "Mean", period_col_name = "Period", 
                                               mean_rain_name = "Average rain per rain day")
{
  if(missing(new_time_period)) {
    stop("Specify the time period you want the summarized data to be in.")
  }

  if(!compare_time_periods(new_time_period,data_time_period)) {
    stop("Cannot summarize data to a shorter time period.")
  }
  
  date_col_name = getvname(date_label)
  date_col = data[[date_col_name]]
  
  curr_data_name = get_meta(data_name_label)
  
  if(data_time_period == daily_label && new_time_period == subyearly_label) {
    
    if(is_meta_data(data_start_date_label)) {
      start_date = get_meta(data_start_date_label)
    }
    else start_date = min(date_col)
    
    if(is_meta_data(data_end_date_label)) {
      end_date = get_meta(data_end_date_label)+1
      month(end_date) <- month(end_date)-1
    }
    else {
      end_date = start_date
      month(end_date) <- month(max(date_col))
    }
    
    time_periods_list = seq(start_date,end_date,"month")
    split_list = list(month(data[[date_col_name]]), year(data[[date_col_name]]))
  }

  if(data_time_period == daily_label && new_time_period == yearly_label) {
    
    if(is_meta_data(data_start_date_label)) {
      start_date = get_meta(data_start_date_label)
    }
    else start_date = min(date_col)
    
    if(is_meta_data(data_end_date_label)) {
      end_date = get_meta(data_end_date_label)+1
      year(end_date) <- year(end_date)-1
    }
    else {
      end_date = start_date
      year(end_date) <- year(max(date_col))
    }
    
    time_periods_list = seq(start_date,end_date,"year")
    if(!is_present(season_label)) add_doy_col()
    split_col = getvname(season_label)
    split_periods = unique(data[[getvname(season_label)]])
  }

  summarized_data = data.frame(Date = time_periods_list)
    
  summary_obj = climate_data$new(data = summarized_data, data_name = summarize_name, 
                                   start_point = start_point, data_time_period = new_time_period)

  summary_obj$append_to_variables(date_label,getvname(date_label))
  
  summary_obj$append_column_to_data(split_periods,period_col_name)
  
  summary_obj$append_to_meta_data(summary_statistics_label,list())
  
  summ_date_col_name = summary_obj$getvname(date_label)

  if(data_time_period == daily_label && new_time_period == subyearly_label) {
    if(.self$is_present(month_label)) {
      summary_obj$append_column_to_data(month(time_periods_list),getvname(month_label))
      summary_obj$append_to_variables(month_label,getvname(month_label))
    }
    if(.self$is_present(year_label)) {
      summary_obj$append_column_to_data(year(time_periods_list),getvname(year_label))
      summary_obj$append_to_variables(year_label,getvname(year_label))
    }
  }

  if(.self$is_present(season_label)) {
    # Is this a bad way to do this? Assumes dates are in correct order.
    summary_obj$append_column_to_data(unique(data[[getvname(season_label)]]),getvname(season_label))
    summary_obj$append_to_variables(season_label,getvname(season_label))
  }

  summarized_row_num = nrow(summary_obj$data)

  for(var in c(rain_label, temp_min_label, temp_max_label, evaporation_label)) {
    # For the variables that are present we create summaries    
    if(is_present(var)) {
      curr_col_name = .self$getvname(var)
      
      # For rain we will add number of rainy days and total rainfall
      # and average rain on rainy day
      if(var == rain_label) {
        threshold = get_meta_new(threshold_label,missing(threshold),threshold)
        
        total_rain_data = as.vector(by(data[[curr_col_name]],data[[split_col]], sum, na.rm = na.rm))
        total_rain_name = paste(total_col,curr_col_name)
        summary_obj$append_column_to_data(total_rain_data, total_rain_name)
        rain_total_label = summary_obj$get_summary_label(var, total_label, list(na.rm=na.rm))
        summary_obj$append_to_variables(rain_total_label, total_rain_name)
        
        # Can't use by function here as there may be no values > threshold causing by to skip
        # a time period. causing an error when we try to append the column.
        mean_rain_data = c()
        for(period in split_periods) {
          curr_mean = mean(data[[curr_col_name]][data[[split_col]]==period & data[[curr_col_name]] > threshold])
          mean_rain_data = c(mean_rain_data, curr_mean)
        }
        

        summary_obj$append_column_to_data(mean_rain_data, mean_rain_name)
        mean_rain_label = summary_obj$get_summary_label(var, mean_label, list(na.rm=na.rm, threshold = threshold))
        summary_obj$append_to_variables(mean_rain_label, mean_rain_name)
        

        if(na.rm) {
          num_rain_days_data = as.vector(by(data[[curr_col_name]][!is.na(data[[curr_col_name]])] > threshold, 
                                  data[[split_col]][!is.na(data[[curr_col_name]])], sum))
        }
        else {
          num_rain_days_data = as.vector(by(data[[curr_col_name]] > threshold, 
                                  data[[split_col]], sum))
        }
        summary_obj$append_column_to_data(num_rain_days_data, num_rain_days_col)
        rain_days_label = summary_obj$get_summary_label(var, number_of_label, list(na.rm=na.rm, threshold=threshold))
        summary_obj$append_to_variables(rain_days_label,num_rain_days_col)
        
        
      }
      
      else {
        
        mean_var_data = as.vector(by(data[[curr_col_name]],data[[split_col]], mean, na.rm = na.rm))
        mean_var_name = paste(mean_col,curr_col_name)
        summary_obj$append_column_to_data(mean_var_data, mean_var_name)
        mean_var_label = summary_obj$get_summary_label(var, mean_label, list(na.rm=na.rm))
        summary_obj$append_to_variables(mean_var_label, mean_var_name)
      }  
          
      
    }
  }

  summary_obj$append_to_meta_data(summarized_from_label, curr_data_name)

  summary_obj$append_to_meta_data(summarized_from_label, curr_data_name)

  summary_obj

}
)

climate_data$methods(add_water_balance_col = function(col_name = "Water Balance", capacity_max = 100, evaporation = 5)
{

  # Complete dates needed for calculations
  missing_dates_check()

  # Always use the methods to get value from objects. Never access directly.
  rain_col = getvname(rain_label)
  date_col = getvname(date_label)

  # Do all data_object level checks before calling get_data_for_analysis
  evap_present = is_present(evaporation_label)
  if(evap_present) evaporation_col = getvname(evaporation_label)
  
  # New get_meta method (waiting for David to check)
  capacity_max = get_meta_new(capacity_label,missing(capacity_max),capacity_max)
  
  # Use an empty data_list here because we want to calculate water balance
  # for the whole data set.
  # Displaying water balance can use a non empty data_list here if we want to view a subset of the data
  curr_data_list = get_data_for_analysis(data_info = list())
  
  for( curr_data in curr_data_list ) {
    # This needs to be changed to consider case when data will come as split
    # Index comparisions etc.
    
    num_rows <- nrow(curr_data)
    
    # List of the indices of all NAs in the rain column
    #indicNAs <- which (curr_data[[ rain_col ]] %in% NA)
    
    # Create a column for water balance filled with NAs
    # We will append to data_object only when it is complete
    water_balance_col = rep(NA,num_rows)
    
    # Initialization
    if( is.na(curr_data[[rain_col]][[1]]) ) {
      water_balance_col[[1]] <- NA
    }
    else {water_balance_col[[1]] <- 0}
    
    #       if(length(indicNAs) != 0) {
    #         ind_nonrecord = indicNAs[ !(day(curr_data[[date_col]][indicNAs]) == 29 & month(curr_data[[date_col]][indicNAs]) == 2) ]
    #       }
    #       else {ind_nonrecord <- indicNAs}
    
    #      ind_nonrecord <- indicNAs
    
    # assign the NAs due to non recording values to be zero.
    #       for(  j in ind_nonrecord  ) {
    #         curr_data[[ rain_col ]][ j ] = 0
    #       }
    
    
    reset <- which(day(curr_data[[ date_col ]]) == 1 &  month(curr_data[[ date_col ]]) == 1)
    
    # Two different formulas for computing water balance depending on what kind of evaporation
    # you have.
    for (  iday in 2 : num_rows  ) {
      if(evap_present) {evaporation <- evap_col[[iday]]}
      
      if( iday %in% reset && is.na(water_balance_col[iday - 1]) ) {
        water_balance_col[[iday]] <- 0
      }
      else {
        water_balance_col[[iday]] <- water_balance_col[[iday - 1]] + curr_data[[rain_col]][iday] - evaporation
      }
      
      if( !is.na(water_balance_col[iday]) ) {
        if ( water_balance_col[iday] < 0 ) {
          water_balance_col[iday] <- 0
        }
        else if( water_balance_col[iday] > capacity_max ) {
          water_balance_col[iday] <- capacity_max
        }
      }
    }
  }
  
  # Last step is to append the water balance column to the data
  # and add to variables so that water balance can be recognised.
  append_column_to_data(water_balance_col,col_name)
  append_to_variables(waterbalance_label, col_name)
}
)


climate_data$methods(get_summary_label = function(element="", statistic="", definition=list()) {
  
  if( !is_meta_data(summary_statistics_label)) {
    append_to_meta_data(summary_statistics_label,list())
  }
  
  if( !(element %in% names(meta_data[[summary_statistics_label]])) ) {
    meta_data[[summary_statistics_label]][[element]] <<- list()
  }
  
  if( !(statistic %in% names(meta_data[[summary_statistics_label]][[element]])) ) {
    meta_data[[summary_statistics_label]][[element]][[statistic]] <<- list()    
  }
  
  label = paste(element,statistic,length(meta_data[[summary_statistics_label]][[element]][[statistic]])+1)
  meta_data[[summary_statistics_label]][[element]][[statistic]][[label]] <<- definition
  
  return(label)
}
)

climate_data$methods(is_definition = function(element="", statistic="", definition=list()) {
  
  found_match = FALSE
  
  if( is_meta_data(summary_statistics_label) 
      && element %in% names(meta_data[[summary_statistics_label]])
      && statistic %in% names(meta_data[[summary_statistics_label]][[element]]) ) {
    for(def in meta_data[[summary_statistics_label]][[element]][[statistic]]) {
      if(length(def) != length(definition)) break
      match = TRUE
      for(item in names(def)) {
        if( !(item %in% names(definition) && def[[item]] == definition[[item]]) ) {
          match = FALSE
          break
        }
      }
      if(match) {
        found_match = TRUE
        break
      }
    }
  }
  
  return(found_match)
  
}
)

climate_data$methods(view_definition = function(col_name) {
  
  if (col_name %in% names(data)) {
    
    label = names(which(variables==col_name))
    if(is_meta_data(summary_statistics_label)) {
      for(element in meta_data[[summary_statistics_label]]) {
        for(statistic in element) {
          if(label %in% names(statistic)) {
            ind = which(names(statistic)==label)
            return(statistic[[ind]])
          }
        }
      }
    }
  }
  else return(NA)
}
)