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
  
  # date time period is "daily" todo check it works for all time periods
  # Removing this should allow the function to work with all types of data
  
  # This is required, because the "get_climate_data_objects" needs to know the
  # type of time period of the data
  data_list=add_to_data_info_time_period(data_list, daily_label)
  
  climate_data_objs = get_climate_data_objects(data_list)
#  print(climate_data_objs)
  for(data_obj in climate_data_objs) {
    data_name = data_obj$get_meta(data_name_label)    
    
    #####################################################################
    # FIND OUT WHICH VARIABLE TO PLOT
    # Rain
    var_label=list()
    if (data_obj$is_present(rain_label)){
      var_label[[rain_label]]=rain_label
    }
    # Minimum Temperature
    if (data_obj$is_present(temp_min_label)) {
      var_label[[temp_min_label]]=temp_min_label
    }
    # Maximum Temperature
    if (data_obj$is_present(temp_max_label)){
      var_label[[temp_max_label]]=temp_max_label
    }
    # Wind speed 
    if (data_obj$is_present(wind_speed_label)){
      var_label[[wind_speed_label]]=wind_speed_label
    }
    # Wind direction
    if (data_obj$is_present(wind_direction_label)) {
      var_label[[wind_direction_label]]=wind_direction_label
    }
    
    # Create one plot for each variable
    date_col = data_obj$getvname(date_label)
    station_id_col = data_obj$getvname(station_label)
    # Get the data frames for analysis
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    for (i in var_label){
      # Get the variable 
      var_col = data_obj$getvname(as.character(i))
      # loop for computing 
      for( curr_data in curr_data_list ) {
        
        # Begin and end dates of dataset
        first.date <- curr_data[[date_col]][1]
        last.date <- curr_data[[date_col]][nrow(curr_data)]
        
        #define graphic working directory
        mainDir<-getwd()
        
        tperiod <- paste(first.date,"-",last.date, sep="")
        station_id <- unique(curr_data[[station_id_col]])

        y <- curr_data[[var_col]]
        x <- curr_data[[date_col]]
        
        ####################################################################
        # CREATE TIMESERIES DATAFRAME
        # a) Using "as.Date"
        y2 <- y
        x2 <- as.Date(x,format="%Y-%m-%d")
        data2 <- data.frame(x2,y2)
        full <- seq.Date(x2[1],x2[length(x2)],by="day")
        all.dates.frame <- data.frame(list(x2=full))
        merged.data <- merge(all.dates.frame,data2,all=T)     
        
#         # b) Using "as.POSIXct"
#         y2 <- y
#         x2 <- as.POSIXct(strptime(x,format="%Y-%m-%d"),
#                          format="%Y-%m-%d %H:%M:%S",tz="UTC")
#         data2 <- data.frame(x2,y2)
#         full <- seq.POSIXt(x2[1],x2[length(x2)],by="day")
#         all.dates.frame <- data.frame(list(x2=full))
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
        x3 <- merged.data$x2
        y3 <- merged.data$y2
        
        plot(x3,y3,type=plot.type,col=plot.color,
             cex.main=.8,
             xlab= paste("Time"),
             ylim=c(min(y,na.rm=TRUE),max(y,na.rm=TRUE)),
             ylab=var_col,
             cex.lab=.8,
             cex.axis=.8,
             xaxt='n',# to remove the numbering on x-axis
             yaxt="n")
        
        ##############################################################
        # XTICKS        
        
        # Sequence - Change the xticks according to the date
        # a) as.Date
        seq.begin <- as.Date(x3[1])
        # b) as. POSIXct
        # seq.begin <- as.POSIXct(x3[1],tz ='UTC')
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
        
        # a) With "as.Date"
        ticks <- seq.Date(from = as.Date(seq.begin),to = as.Date(seq.end),
                            by=seq.tick.int) 
        xticks <- pretty(ticks,n=15)

        axis(1,xticks,tck=-.01,lwd=.2,#las=2,
             labels = xticks,
             cex.axis=.8,
             mgp=c(0,-0.15,0)
        )
        abline(v=xticks,lty=3,lwd=.4)
        
        # b) With "POSIXt"
#         ticks <- seq.POSIXt(from = seq.begin,to = seq.end,
#                             by=seq.tick.int) 
#         xticks <- pretty(ticks,n=15)
#         
#         axis(1,xticks,tck=-.01,lwd=.2,#las=2,
#              labels = xticks,
#              cex.axis=.4,mgp=c(0,-0.35,0))
#         abline(v=xticks,lty=3,lwd=.4)
        
        ##############################################################
        # YTICKS
        yaxis.min <- min(y3,na.rm=TRUE)
        yaxis.max <- max(y3,na.rm=TRUE)
        yticks <- pretty(c(yaxis.min,yaxis.max),n=15)
        axis(2,yticks,
             labels = yticks,
             tck=-.01,lwd=.2,# lwd=.2,
             cex.axis=.8,
             mgp=c(0,0.15,0)
        )
        abline(h=yticks,lty=3,lwd=.4)
        
        ###########################################################
        # TITLE & SUBTITLE
        tit<- paste("Station id: ",station_id," - ",var_col)
        subtit <- paste(first.date,"-",last.date)
        title(main=paste(tit," (",subtit,")"),cex.main=0.8,line =3)
        
        ###########################################################
        # LEGEND
        # Total number of cases for the given time inverval
        tt.total <- paste("Total Cases: ",length(y))
        
        # Number of cases above the maximum limit
        above.max <- which(y>yaxis.max)
        above.percent <- round((length(above.max)/length(y))*100,2)
        tt.above <- paste("Cases > ",yaxis.max,": ",length(above.max),
                          " (",above.percent,"%)",sep="")
        
        # Number of cases below the minimum limit
        below.min <- which(y<yaxis.min)
        below.percent <- round((length(below.min)/length(y))*100,2)
        tt.below <- paste("Cases < ",yaxis.min,": ",length(below.min),
                          " (",below.percent,"%)",sep="")
        
        # Number of NA cases 
        na.cases <- which(is.na(y))
        na.percent <- round((length(na.cases)/length(y))*100,2)
        tt.na <- paste("'NA'-Cases: ",length(na.cases),
                       " (",na.percent,"%)",sep="")
        
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