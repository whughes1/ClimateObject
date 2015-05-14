##############################################################################
# TIMESERIES
#' @title Get timeseries from the Climsoft data
#' @name timeseries
#' @author Rafael Posada 2015 (SASSCAL/DWD)
#' 
#' @description 
#' Allows plotting the data in a timeseries plot.
#'  
#' @return It returns a timeseries plot.
#' 
#' 
climate$methods(timeseries = function(data_list = list()){
  
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
  # Removing this should allow the function to work with all types of data
#  data_list=add_to_data_info_time_period(data_list, daily_label)
  
  climate_data_objs = get_climate_data_objects(data_list)
#  print(climate_data_objs)
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
      date_col = data_obj$getvname(date_label)
      station_id_col = data_obj$getvname(station_label)      
#      print(station_id_col)
      # Create new columns not used in code
#      year_col = data_obj$getvname(year_label)
#      month_col = data_obj$getvname(month_label)
#      day_col = data_obj$getvname(day_label)
#      season_col = data_obj$getvname(season_label)
#      month_start = 1
#      monEnd = 12
      
      # ****Get the data frames for analysis
      curr_data_list = data_obj$get_data_for_analysis(data_list)
      # loop for computing 
      for( curr_data in curr_data_list ) {
        
        first.date <- curr_data[[date_col]][1]
#        print(first.date)
        last.date <- curr_data[[date_col]][nrow(curr_data)]
        
#        print(last.date)
        #######################################################
        
        #define graphic working directory
        mainDir<-getwd()
        
        tperiod <- paste(first.date,"-",last.date, sep="")
        station_id <- unique(curr_data[[station_id_col]])

        y <- curr_data[[var_col]]
        x <- curr_data[[date_col]]
        
# This part of the code is causing errors so I have comment it out and fixed as follows 
# Replaced merged.data and merged.data$x2 with x and one incidence of merged.data$y2 with y
# I assume I have lost some functionality but needed to get a working version for Ian to demonstrate
#         y2 <- y
#         x2 <- as.POSIXct(strptime(x,format="%Y-%m-%d"),
#                          format="%Y-%m-%d %H:%M:%S",tz="UTC")
#         data2 <- data.frame(x2,y2)
#         
#         full <- seq.POSIXt(x2[1],x2[length(x2)],by="day")
#         all.dates.frame <- data.frame(list(x2=full))
#         
#         merged.data <- merge(all.dates.frame,data2,all=T)
        
        #####################################################
        # GET THE MAX & MIN LIMITS
        n.std <- 4
        std.dev <- sd(y,na.rm=TRUE)
        mean <- mean(y,na.rm=TRUE)
        yaxis.max <- round(mean+(n.std*std.dev),digits=2)
        yaxis.min <- round(mean-(n.std*std.dev),digits=2)
        if (yaxis.min<0 & grepl("temp",var_col)==FALSE){
          yaxis.min=0
        }
        
        #####################################################
        # TITLES AND LABELS
        # title
        tit<- paste("Station id: ",station_id," - ",
                    var_col)
        # subtitle
        subtit <- paste(first.date,"-",last.date)
        # x and y labels
        xlabel <- paste("Time")
        ylabel <- var_col
        
        
        #######################################################
        # LEGEND
        # Total number of cases for the given time inverval
        tt.total <- paste("Total Cases: ",nrow(y))
        # Number of cases above the maximum limit
        above.max <- which(y>yaxis.max)
        above.percent <- round((length(above.max)/nrow(y))*100,2)
        tt.above <- paste("Cases > ",
                          yaxis.max,": ",
                          length(above.max),
                          " (",above.percent,"%)",     
                          sep="")
        
        # Number of cases below the minimum limit
        below.min <- which(y<yaxis.min)
        below.percent <- round((length(below.min)/nrow(y))*100,2)
        tt.below <- paste("Cases < ",
                          yaxis.min,": ",
                          length(below.min),
                          " (",below.percent,"%)",
                          sep="")
        
        # Number of NA cases 
        na.cases <- which(is.na(y))
        na.percent <- round((length(na.cases)/nrow(y))*100,2)
        tt.na <- paste("'NA'-Cases: ",
                       length(na.cases)," (",na.percent,"%)",sep="")
        
        # Check if the element is precipitation or not
        if (var_col=="rain"){
          plot.type <- "h"
          plot.color <- "blue"
        }else{
          plot.type <- "o"
          plot.color <- "red"
        }
        
        ######################################################################
        # Plot
        Sys.sleep(0.1) # this is to avoid an error that pops up when trying 
        # to plot two plots at a time (something to do with
        # rversion in grDevices)
        
        plot(x,y,type=plot.type,col=plot.color,
             #main=paste(tit,"\n",subtit),
             cex.main=.8,
             xlab=xlabel,
             ylim=c(min(y,na.rm=TRUE),max(y,na.rm=TRUE)),
             ylab=ylabel,
             cex.lab=.8,
             cex.axis=.8,
             xaxt='n',# to remove the numbering on x-axis
             yaxt="n")
        
        # Title
        title(main=paste(tit," (",subtit,")"),cex.main=0.8,line =3)
        # Change the xticks according to the date
        x3 <- x
        y3 <- y
        seq.begin <- as.POSIXct(x3[1],tz ='UTC')
        seq.end <- x3[length(x3)]

        date.length <- as.numeric(round((year(seq.end)-year(seq.begin)),0))
        if (date.length>50){
          seq.tick.int <- 366 #"year"
        }
        if (date.length>25 & date.length<=50){
          seq.tick.int <-  183 #"6 month"
        }
        if (date.length>10 & date.length<=25){
          seq.tick.int <- 91 #"3 month"
        }
        
        if (date.length>1 & date.length<=10){
          seq.tick.int <- 30 #"month"
        }
        
        if (date.length>0 & date.length<=1){
          seq.tick.int <- 1 #"day"
        }
        
        if (date.length==0){
          seq.tick.int <- 1/24 #"hour"
        }
        
        # X ticks

        ticks <- seq.Date(from = as.Date(seq.begin),to = as.Date(seq.end),
                            by=seq.tick.int) 
        xticks <- pretty(ticks,n=15)

        axis(1,xticks,tck=-.01,lwd=.2,#las=2,
             labels = xticks,
             cex.axis=.8,
             mgp=c(0,0.15,0)
        )
        abline(v=xticks,lty=3,lwd=.4)
        
        # Y Ticks
        yaxis.min <- min(y3,na.rm=TRUE)
        yaxis.max <- max(y3,na.rm=TRUE)
#        print(yaxis.min)
#        print(yaxis.max)
        yticks <- pretty(c(yaxis.min,yaxis.max),n=15)
        axis(2,yticks,
             labels = yticks,tck=-.01,lwd=.2,# lwd=.2,
             cex.axis=.8,
             mgp=c(0,0.15,0)
        )
        abline(h=yticks,lty=3,lwd=.4)
        
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