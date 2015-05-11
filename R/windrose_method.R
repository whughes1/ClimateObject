#=============================================================================
# WIND ROSE
#' @title Windrose plots with the openair package
#' @name windrose
#' @author Jens Riede 2015 (SASSCAL/DWD)

#' @description \code{get.windrose} 
#' Creates windrose given a climate object
#' 
#' @param data_list list. 
#' 
#' @param WR.type character. Type of windrose to plot. It can be either
#' "single", "weekday", "season", "year". Default value: "single" 
#  
#' @examples
#' climObj <- climate(data_tables=list(data),date_format="%Y-%m-%d")
#' # where "data" is a data.frame containing the desired data to be plotted.
#' climObj$get.windrose()
#' @return Windrose plot
#' 
climate$methods(get.windrose = function(data_list = list(), WR.type="single", 
                                        wind_threshold=0.3){  
  # Libraries
  library(openair)
  # Wind required
  data_list=add_to_data_info_required_variable_list(data_list, list(wind_speed_label,wind_direction_label))
  #  date time period is "daily" todo check it works for all time periods
  data_list=add_to_data_info_time_period(data_list, daily_label)
  
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    data_name = data_obj$get_meta(data_name_label)    
    curr_threshold = data_obj$get_meta(wind_threshold_label,wind_threshold)
    wind_speed_col  = data_obj$getvname(wind_speed_label)
    wind_direction_col= data_obj$getvname(wind_direction_label)      
    
    # station_id_col = data_obj$getvname(station_label)
    station_id_col = station_label
    print(station_id_col)
    # Create new columns
    year_col = data_obj$getvname(year_label)
    month_col = data_obj$getvname(month_label)
    day_col = data_obj$getvname(day_label)
    season_col = data_obj$getvname(season_label)
    month_start = 1
    monEnd = 12
    
    # ****Get the data frames for analysis
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    # loop for computing 
    for( curr_data in curr_data_list ) {
      selRows <- curr_data[[month_col]]>=month_start & curr_data[[month_col]] <=monEnd & curr_data[[wind_speed_col]] > curr_threshold 
      ndays <- tapply(selRows, curr_data[[season_col]], sum, na.rm=na.rm)
      
      #############################################################
      # Dataframe with the required information
      df <- data.frame(ws=curr_data[[wind_speed_col]],
                       wd=curr_data[[wind_direction_col]],
                       date = curr_data[[date_label]])
      
      #######################################################
      # graphic settings Windrose plot
      
      if(WR.type=="single"){
        WR.type = "default"
      }
      first.date <- format(strptime(as.character(df[1,3]),"%Y-%m-%d"),"%Y%m%d")
      last.date <- format(strptime(as.character(df[length(df[,1]),3]),"%Y-%m-%d"),"%Y%m%d")
      
      
      #######################################################
      # plot four different typ of windrose depending on choosen type
      
      #define graphic working directory
      mainDir<-getwd()
      
      tperiod <- paste(first.date,"-",last.date, sep="")
      station_id <- unique(curr_data[[station_id_col]])
      graphic.settings<-(key = list(header=paste("Station.ID:",station_id,"/ WR.type =", WR.type,
                                                 "/ period: ",tperiod), 
                                    footer=paste(paste("Creation date:", date())),
                                    #plot.style = C("paddle"),
                                    par.settings=list(fontsize=list(text=16)),            
                                    plot.style = c("border", "ticks"),
                                    fit = "all", height = 1,
                                    space = "top"))
      
      pollutionRose(df, pollutant = "ws", key = graphic.settings,
                    annotate = FALSE,auto.text = FALSE, 
                    type = WR.type) 
      setwd(mainDir)
    }
  }
})