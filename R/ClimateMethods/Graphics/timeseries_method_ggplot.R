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
climate$methods(gsub2 = function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
})

climate$methods(timeseries_ggplot = function(data_list = list(),plot_type="normal"){
  #######################################################################
  # CLIMATE DATA OBJS
  climate_data_objs = get_climate_data_objects(data_list)
  
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
    
    # Name of station_id column
    station_id_col = data_obj$getvname(station_label)
    # Get date_time_period ("daily","subdaily",etc.)
    data_time_period = data_obj$data_time_period
    # Get the data frame for analysis
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    # loop for computing 
    for( curr_data in curr_data_list ) {
      # Begin and end dates of dataset
      
      if(data_time_period!="subdaily"){
        date_col = data_obj$getvname(date_label)
      }else{
        date_col = data_obj$getvname(date_time_label)
      }
      
      first.date <- curr_data[[date_col]][1]
      last.date <- curr_data[[date_col]][nrow(curr_data)]
      
      # define graphic working directory
      mainDir<-getwd()
      
      tperiod <- paste(first.date,"-",last.date, sep="")
      station_id <- unique(curr_data[[station_id_col]])
      
      # Get the data of each variable
      for (i in var_label){
        # Get the variable 
        var_col = data_obj$getvname(as.character(i))
        
        y <- curr_data[[var_col]]
        x <- curr_data[[date_col]]
        
        ####################################################################
        # CREATE TIMESERIES DATAFRAME
        # a) Using "as.Date"
        y2 <- y
        x2 <- x 
        
        if (data_time_period=="daily"){
          interval <- "day"
          plot_style <- "normal"
        }
        
        if (data_time_period=="subdaily"){
          # Make sure to use always the same time format:
          x2 <- as.POSIXct(as.character(x2),tz="UTC")
          # Get the time period stamp of the dataset
          time_diff <- diff(x2)
          # Count cases with the same time stamp
          time_stamp <- table(diff(x2))
          
          # a) get the time difference units (minutes, hours, etc.)
          time_units <- units(time_diff)
          if(time_units=="hours"){
            id <- which(time_stamp==max(time_stamp))
            time_interval <- names(time_stamp)[id]
            interval <- paste(time_interval,"hour")
            if (time_interval>1){
              plot_style <- "subplots" # One plot with subplots, each for each
              # time stamp
            }else{
              plot_style <- "normal"  
            }
          }
        }
        
        if (date_time=="yearly"){
          interval <- "year"
        }
        if (date_time=="subyearly"){
          interval <- "month"
        }
        
        data2 <- data.frame(x2,y2)
        if (class(data2$x2)[1]=="Date"){        
          full <- seq.Date(x2[1],x2[length(x2)],by=interval)
        }
        if (class(data2$x2)[1]=="POSIXct"){
          full <- seq.POSIXt(x2[1],x2[length(x2)],by=interval)
        }
        all.dates.frame <- data.frame(list(x2=full))
        merged.data <- merge(all.dates.frame,data2,all=T)     
        
        ######################################################################
        # GET THE MAX & MIN LIMITS
        n.std <- 4
        std.dev <- sd(y,na.rm=TRUE)
        mean <- mean(y,na.rm=TRUE)
        yaxis.max <- round(mean+(n.std*std.dev),digits=2)
        yaxis.min <- round(mean-(n.std*std.dev),digits=2)
        if (yaxis.min<0 & grepl("temp",var_col)==FALSE){
          yaxis.min=0
        }
        
        ######################################################################
        # CHECK IF THE ELEMENT IS RAIN OR NOT
        if (var_col=="rain"){
          plot.type <- "h"
          plot.color <- "blue"
        }else{
          plot.type <- "o"
          plot.color <- "red"
        }
        
        Sys.sleep(0.1) # this is to avoid an error that pops up when trying 
        # to plot two plots at a time (something to do with
        # rversion in grDevices)
        x3 <- merged.data$x2
        y3 <- merged.data$y2
        
        #######################################################
        # COMMON VARIABLES
        # xticks and yticks
        xticks <- pretty(x3,20)
        yaxis.min <- min(y3,na.rm=TRUE)
        yaxis.max <- max(y3,na.rm=TRUE)
        yticks <- pretty(c(yaxis.min,yaxis.max),n=15)
        
        # Title and subtitle
        tit<- paste("Station id: ",station_id," - ",var_col)
        subtit <- paste(first.date,"-",last.date)
        
        # Legend
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
        
        #######################################################
        # NORMAL PLOT
        if (plot_style=="normal"){
          plot(x3,y3,type=plot.type,col=plot.color,
               cex.main=.8,
               xlab= paste("Time"),
               ylim=c(min(y,na.rm=TRUE),max(y,na.rm=TRUE)),
               ylab=var_col,
               cex.lab=.8,
               cex.axis=.8,
               xaxt='n',# to remove the numbering on x-axis
               yaxt="n")
          
          # Place grid
          abline(v=xticks,lty=3,lwd=.4) # xaxis
          abline(h=yticks,lty=3,lwd=.4) # yaxis
        }
        
        #######################################################
        # SUBPLOTS WITHIN THE PLOT 
        if (plot_style=="subplots"){
          
          cars <- data.frame(x3,y3,time=format(x3,"%H:%M:%S"))
          cars$identifier <- cars$time
          identifiers <- c(1:length(unique(cars$time)))
          # Replace the time of the data.frame with the identification number
          cars$identifier <- gsub2(unique(cars$time),identifiers,cars$identifier)
          
          # Remove those dataset which have no data
          for (ii in c(1:length(identifiers))){
            id <- which(cars$identifier==identifiers[ii])
            if(length(which(is.na(cars$y3[id])))==length(id)){
              cars <- cars[-id,]
            }
          }    
          
          # Plot
          k <- ggplot(cars, aes(x3,y3,group=time)) + 
            geom_point(colour=plot.color,na.rm=TRUE) + # Points
            theme_bw() + # white background
            facet_wrap(~ time, nrow=length(unique(cars$time))) # Number of subplots
          # Creating xticks
          k <- k + scale_x_datetime(breaks = xticks)
          # Creating title and subtitle
          k <- k + ggtitle(bquote(atop(.(tit), atop(italic(.(subtit)), ""))))
          # Adding labels
          k <- k + xlab(date_col)
          k <- k +ylab(var_col)
          print(k)
        }
      }
    }
  }
})