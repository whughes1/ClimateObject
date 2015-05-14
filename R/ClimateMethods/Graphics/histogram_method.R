##############################################################################
# HISTOGRAM
#' @title Get histogram from a dataframe
#' @name histogram
#' @author Rafael Posada 2015 (SASSCAL/DWD)
# 
#' @description 
#' Allows plotting the data in a histogram plot.
#  
#' @return It returns a histogram plot.

climate$methods(histogram = function(data_list = list()){
  
  # Find out which variable to plot 
  
  # The climate data objects should not be accessed directly by these methods. 
  # The hard coding like 'climate_data_objects[[1]]$data' means that the object will not work as expected in all instances and should therefor be avoided.
  # get_climate_data_objects returns the correct climate data objects for the data_list provided. 
#   var_labels <- colnames(climate_data_objects[[1]]$data)
#   variables <- climate_data_objects[[1]]$variables
#   var_labels2 <- as.data.frame(variables[variables %in% var_labels])
#   id <- which(colnames(var_labels2) != "station" & 
#                 colnames(var_labels2) != "date" & 
#                 colnames(var_labels2) != "element_type" &
#                 colnames(var_labels2) != "date_asstring")
#   
#   var_label <- as.character(as.matrix(var_labels2[id]))
  
  # Data required   
  # this is not doing anything as it simply states that the data available is required it can be left blank
#  data_list=add_to_data_info_required_variable_list(data_list, as.list(var_label))

  # date time period is "daily" todo check it works for all time periods
  data_list=add_to_data_info_time_period(data_list, daily_label)
  
  climate_data_objs = get_climate_data_objects(data_list)
#  print(climate_data_objs) I'm removing th print statments so that Ian can test with large data through the front end
  for(data_obj in climate_data_objs) {
    data_name = data_obj$get_meta(data_name_label)    
    # curr_threshold = data_obj$get_meta(wind_threshold_label,wind_threshold)

    #This is the appropriate place to find out which variable to plot I suggest you want to use the is_present function to check for the variables which may be appropriate this could be for even unknown elements if you think that is appropriate I've just implemented for Rain temp min temp max for Ian to test
    var_label=list()
    if (data_obj$is_present(rain_label)) var_label[[rain_label]]=rain_label
    if (data_obj$is_present(temp_min_label)) var_label[[temp_min_label]]=temp_min_label
    if (data_obj$is_present(temp_max_label)) var_label[[temp_max_label]]=temp_max_label
    if (data_obj$is_present(wind_speed_label)) var_label[[wind_speed_label]]=wind_speed_label
    if (data_obj$is_present(wind_direction_label)) var_label[[wind_direction_label]]=wind_direction_label
    for (i in var_label){
      var_col = data_obj$getvname(i)      
      
      station_id_col = data_obj$getvname(station_label)      
#      print(station_id_col)
      # Create new columns Not used in the code
#       year_col = data_obj$getvname(year_label)
#       month_col = data_obj$getvname(month_label)
#       day_col = data_obj$getvname(day_label)
#       season_col = data_obj$getvname(season_label)
#       month_start = 1
#       monEnd = 12
      
      # ****Get the data frames for analysis
      curr_data_list = data_obj$get_data_for_analysis(data_list)
      # loop for computing 
      for( curr_data in curr_data_list ) {
        first.date <- curr_data[[date_label]][1]
#        print(first.date)
        last.date <- curr_data[[date_label]][nrow(curr_data)]
        
#        print(last.date)
        #######################################################
        
        #define graphic working directory
        mainDir<-getwd()
        
        tperiod <- paste(first.date,"-",last.date, sep="")
        station_id <- unique(curr_data[[station_id_col]])
        
        x <- curr_data[[var_col]]
        y <- curr_data[[date_label]]
        
# This part of the code is causing errors so I have comment it out and fixed as follows 
# Replaced merged.data and merged.data$x2 with x and one incidence of merged.data$y2 with y
# I assume I have lost some functionality but needed to get a working version for Ian to demonstrate


#        x2 <- x
#        y2 <- as.POSIXct(strptime(y,format="%Y-%m-%d"),
#                         format="%Y-%m-%d %H:%M:%S",tz="UTC")
#        data2 <- data.frame(y2,x2)
#        
#        full <- seq.POSIXt(y2[1],y2[length(y2)],by="day")
#        all.dates.frame <- data.frame(list(y2=full))
#        
#        merged.data <- merge(all.dates.frame,data2,all=T)
        
        #####################################################
        # GET THE MAX & MIN LIMITS
        n.std <- 4
        std.dev <- sd(x,na.rm=TRUE)
        mean <- mean(x,na.rm=TRUE)
        xaxis.max <- round(mean+(n.std*std.dev),digits=2)
        xaxis.min <- round(mean-(n.std*std.dev),digits=2)
        if (xaxis.min<0 & grepl("temp",var_col)==FALSE){
          xaxis.min=0
        }
        
        ####################################################
        # SEQUENCE OF THE HISTOGRAM
        # maximum value for the sequence
        seq.max <- max(x,na.rm=TRUE)
        # minimum value for the sequence
        seq.min <- min(x,na.rm=TRUE)
        # sequence
        seq.interval <- round((abs(seq.max)-abs(seq.min))/100,digits=0)
        if (seq.interval==0){
          seq.interval <- round((abs(seq.max)-abs(seq.min))/100,digits=1)
          if (seq.interval==0){
            seq.interval <- round((abs(seq.max)-abs(seq.min))/100,digits=2)
          }
        }
        sequence <- seq(from=seq.min,to=seq.max,by=seq.interval)
        
        #####################################################
        # TITLES AND LABELS
        # title
        tit<- paste("Station id: ",station_id," - ",
                    var_col)
        # subtitle
        subtit <- paste(first.date,"-",last.date)
        # x and y labels
        xlabel <- paste("Intervals [from ",seq.min," to ",
                        seq.max,"; by ",seq.interval,"]",sep="")
        ylabel <- "Number of cases"
        
        #######################################################
        # LEGEND
        # Total number of cases for the given time inverval

        tt.total <- paste("Total Cases: ",nrow(x))
        # Number of cases above the maximum limit
        above.max <- which(x>xaxis.max)
        above.percent <- round((length(above.max)/nrow(x))*100,2)
        tt.above <- paste("Cases > ",
                          xaxis.max,": ",
                          length(above.max),
                          " (",above.percent,"%)",     
                          sep="")
        
        # Number of cases below the minimum limit
        below.min <- which(x<xaxis.min)
        below.percent <- round((length(below.min)/nrow(x))*100,2)
        tt.below <- paste("Cases < ",
                          xaxis.min,": ",
                          length(below.min),
                          " (",below.percent,"%)",
                          sep="")
        
        # Number of NA cases 
        na.cases <- which(is.na(x))
        na.percent <- round((length(na.cases)/nrow(x))*100,2)
        tt.na <- paste("'NA'-Cases: ",
                       length(na.cases)," (",na.percent,"%)",sep="")
        
        
        ######################################################################
        # Plot
        Sys.sleep(0.1) # this is to avoid an error that pops up when trying 
        # to plot two plots at a time (something to do with
        # rversion in grDevices)
        
        
        # Remove the data from "x" that are above the limit in order
        # to be able to make a histogram. If there are outsiders,
        # the function "hist" cannot be used
        id00 <- which(x<max(sequence) & x>min(sequence))
        y3 <- y[id00]
        x3 <- x[id00]
        
        # Histogram plot
        hist(x3,breaks=sequence,xlim=c(seq.min,seq.max),
             main="",
             xlab=xlabel,
             ylab=ylabel,cex.lab=.8,
             cex.axis=.8)
        
        # Title
        title(main=paste(tit," (",subtit,")"),cex.main=0.8,line =3)
        
        ###########################################################
        # WRITE THE LEGEND
        #Write down the legend 
        mtext(paste(tt.total,"\n",
                    tt.above,"\n",
                    tt.below,"\n",
                    tt.na),
              cex=.6*par("cex"),
              side=3,
              line=0)
        setwd(mainDir)
      }
    }
  }
})