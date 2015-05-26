# Output data for CDT
#' @title Output data in the form needed for CDT.
#' @name output_for_CDT
#' @author David Stern 2015

#' @description \code{output_for_CDT} 
#' creates a file with data in the format required for CDT (IRI format needed for Tufa's quality control functions). 



climate$methods(output_for_CDT = function(data_list = list(), filename, interested_variables=list(rain_label))
{  
  if (!is.list(interested_variables)) interested_variables=list(interested_variables)
  # interested variables required
  data_list = add_to_data_info_required_variable_list(data_list, interested_variables)
  # date time period is "daily"
  data_list = add_to_data_info_time_period(data_list, daily_label)
  # a list of climate data objects
  climate_data_objs = get_climate_data_objects(data_list)

  
  for(interested_var in interested_variables) {
    outmetadata<-data.frame(labels=c("Station","Lat","Lon"))
    first=TRUE
    for(data_obj in climate_data_objs) {
      
      date_col  = data_obj$getvname(date_label) 
      var_col  = data_obj$getvname(interested_var) 
      curr_data_list = data_obj$get_data_for_analysis(data_list)
      
      for( curr_data in curr_data_list ) {
        outmetadata[[data_obj$Get_Station_Data(curr_data, station_label)]]=c(data_obj$Get_Station_Data(curr_data, station_label),data_obj$Get_Station_Data(curr_data, lat_label),data_obj$Get_Station_Data(curr_data, lon_label))
        temp<-subset(curr_data , select=c(date_col, var_col))		
        names(temp)<-c("Date",data_obj$Get_Station_Data(curr_data, station_label))
        if (first) {
          outdata<-temp
          first=FALSE
        } else {
          outdata<-merge(outdata,temp,by="Date",all=TRUE)
        }
      }
    }
    if (length(interested_variables)>1) filenametemp=paste0(filename,interested_var)
    else filenametemp=filename
    write.table(outmetadata , file=filenametemp,sep="\t", quote=F, row.names=FALSE, col.names=FALSE)
    write.table(outdata, file=filenametemp, sep="\t", quote=F, row.names=FALSE, append=T)  }
}
)