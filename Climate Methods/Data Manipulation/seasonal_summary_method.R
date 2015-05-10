# Seasonal Summaries
#' @title plot cumulative and exceedance graphs
#' @name seasonal_summary
#' @author Fanuel Otieno and Frederic Ntirenganya 2015 (AMI)

#' @description \code{seasonal_summary} 
#' Adds a column of sesonal summaries e.g rain totals and number of rain days

#' @return columns of seasonal summaries


climate$methods(seasonal_summary = function(data_list = list(), month_start, number_month = 3, threshold = 0.85, 
                                            col_name = "Rain Total JFM", season_rain_total=FALSE, season_rain_days=FALSE,
                                            col_name2 = "Number Raindays JFM", print_season = FALSE, 
                                            na.rm = FALSE, replace = FALSE)
  
{  
  # rain required
  data_list = add_to_data_info_required_variable_list(data_list, list(rain_label))
  # date time period is "daily"
  data_list = add_to_data_info_time_period(data_list, daily_label)
  # a list of climate data objects
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    
    curr_threshold = data_obj$get_meta(threshold_label,threshold)
    
    rain_col  = data_obj$getvname(rain_label) 
    
    # Must add month column if not present
    if( !( data_obj$is_present(month_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    
    month_col = data_obj$getvname(month_label)
    
    # must add seasonal column to the data
    if ( !(data_obj$is_present(season_label))) {
      data_obj$add_doy_col()
    }
    season_col = data_obj$getvname(season_label) 
    
    if(missing(month_start)){
      curr_season_start_day = data_obj$get_meta(season_start_day_label)
      year=1952
      date = doy_as_date(curr_season_start_day, year) 
      month_start = month(date)
    }
    
    if (is.character(month_start)){
      if (!month_start %in% c(month.abb, month.name,tolower(c(month.abb, month.name)))){
        stop("Enter the upper or lower case of English names for the months of the year; e.g Jan,January,jan,january")
      }
      month_start= 1 + ((match(tolower(month_start), tolower(c(month.abb, month.name))) - 1) %% 12)
    }else {
      month_start = month_start
    }
    
    months = 1+(((month_start:(month_start+number_month-1)) -1) %% 12)
    
    summary_obj <- get_summary_name(yearly_label, data_obj)
    
    continue = TRUE
    
    curr_definition = list(month_start = month_start, number_month = number_month, threshold = threshold)
    
    if(season_rain_total || !season_rain_days){
      if(col_name %in% names(summary_obj$get_data()) && !replace) {
        message(paste("A column named", col_name, "already exists. The column will not be replaced.
                      To replace to column, re run this function and specify replace = TRUE."))
        continue = FALSE
      }
      if(col_name %in% names(summary_obj$get_data()) && replace){
        message(paste("A column named", col_name, "already exists. The column will be replaced 
                      in the data."))
      }
      if( continue && summary_obj$is_definition(rain_label,seasonal_total_label,curr_definition)) {
        message("A column with this defintion already exists in the data.
                The column will not be added again.")
        continue = FALSE
      }
      }
    
    if (season_rain_days || !season_rain_total){
      if(col_name2 %in% names(summary_obj$get_data()) && !replace) {
        message(paste("A column named", col_name2, "already exists. The column will not be replaced.
                      To replace to column, re run this function and specify replace = TRUE."))
        continue = FALSE
      }
      if(col_name2 %in% names(summary_obj$get_data()) && replace){
        message(paste("A column named", col_name2, "already exists. The column will be replaced
                      in the data."))
      }
      if( continue && summary_obj$is_definition(rain_label,seasonal_raindays_label,curr_definition)) {
        message("A column with this defintion already exists in the data.
                The column will not be added again.")
        continue = FALSE
      }
      }    
    
    if(continue) {
      
      curr_data_list = data_obj$get_data_for_analysis(data_list)
      
      for( curr_data in curr_data_list ) {
        month_tot=matrix(NA,length(unique(curr_data[[season_col]])),12)
        rownames(month_tot)=as.character(unique(curr_data[[season_col]]))
        colnames(month_tot)=c(month.abb)
        raindays=month_tot
        #   loop over months and years to get summary statistics
        for (mon in months) {
          rain.season = curr_data[curr_data[month_col]==mon,c(season_col,rain_col)]   
          for (yr in unique(curr_data[[season_col]])) {
            if(season_rain_total || !season_rain_days){
              month_tot[yr-min(unique(curr_data[[season_col]])-1),mon]=sum(rain.season[rain.season[,season_col]==yr,rain_col])
            }
            if(season_rain_days || !season_rain_total){
              raindays[yr-min(unique(curr_data[[season_col]])-1),mon]=sum(rain.season[rain.season[,season_col]==yr,rain_col]>threshold)
            }
          }
        }
        month_tot <- rowSums(month_tot[,months], na.rm = na.rm) 
        raindays <-  rowSums(raindays[,months], na.rm = na.rm)
        
        if(season_rain_total && print_season){
          print(month_tot)
        }
        if(season_rain_days && print_season){
          print(raindays)
        }
        # Only print if requested
        if(print_season && !season_rain_total && !season_rain_days) {
          rain_tot <- data.frame(cbind(month_tot, raindays )) 
          colnames(rain_tot) <- c(col_name,col_name2)
          print(rain_tot)
        }
        
      }
      if (season_rain_total || !season_rain_days){
        summary_obj$append_column_to_data(month_tot, col_name)
        label = summary_obj$get_summary_label(rain_label, seasonal_total_label,curr_definition)
        summary_obj$append_to_variables(label, col_name)      
      }
      if (season_rain_days || !season_rain_total){
        summary_obj$append_column_to_data(raindays, col_name2)
        label2 = summary_obj$get_summary_label(rain_label, seasonal_raindays_label,curr_definition)
        summary_obj$append_to_variables(label2,col_name2)
        }
      }
    }
  }
)