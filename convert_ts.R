#Function ---
#convert_ts ---
#
#Output: ts object with passenger/flight traffic for
#the desired airport/route/company.

convert_ts <- function(ts_name,
                       type.1 = c("route", "airport", "airport group"),
                       type.2 = c("international", "domestic", "consolidated"),
                       type.3 = c("passengers", "flights"),
                       start.date = as.Date("2001-01-01", format = "%Y-%m-%d"),
                       end.date = as.Date("2019-01-01", format = "%Y-%m-%d"),
                       departing = F){
  
  #Prepare the  desired data
  if(type.3 == "passengers"){
    data.aux.d <- pax_domesc
    data.aux.i <- pax_int
  } else if(type.3 == "flights") {
    data.aux.d <- flights_domesc
    data.aux.i <- flights_int
  }
  
  
  if(type.2 == "consolidated"){
    data.aux <- rbind(data.aux.d, data.aux.i)
  } else if (type.2 == "international"){
    data.aux <- data.aux.i
  } else if(type.2 == "domestic"){
    data.aux <- data.aux.d
  }
  
  if(departing == F){
  
    if(type.1 == "route"){
      ts_name.f <- strsplit(ts_name, split = "-")[[1]][1]
      ts_name.t <- strsplit(ts_name, split = "-")[[1]][2]
      aux.1 <- min(ts_name.f, ts_name.t)
      aux.2 <- max(ts_name.f, ts_name.t)
      ts_name <- paste0(aux.1,"-",aux.2)
      if(ts_name.t < ts_name.t){
        print(paste("Warning: variable name has been renamed to", ts_name))
      }
      ts <- data.aux %>% filter((Origin_IATA == aux.1 & Destination_IATA == aux.2) |
                                (Origin_IATA == aux.2 & Destination_IATA == aux.1))
    
    } else if(type.1 == "airport"){
      ts <- data.aux %>% filter(Origin_IATA == ts_name | Destination_IATA == ts_name)
    
    } else if (type.1 == "airport group"){
      ts <- data.aux %>% filter(Origin_Airport_Group == ts_name | Destination_Airport_Group == ts_name)
      ts.aux <- data.aux %>% filter(Origin_Airport_Group == ts_name & Destination_Airport_Group == ts_name)
      ts <- rbind(ts, ts.aux)
    }
  } else{
    
    if(type.1 == "route"){
      ts_name.f <- strsplit(ts_name, split = "-")[[1]][1]
      ts_name.t <- strsplit(ts_name, split = "-")[[1]][2]
      aux.1 <- min(ts_name.f, ts_name.t)
      aux.2 <- max(ts_name.f, ts_name.t)
      ts_name <- paste0(aux.1,"-",aux.2)
      if(ts_name.t < ts_name.t){
        print(paste("Warning: variable name has been renamed to", ts_name))
      }
      ts <- data.aux %>% filter((Origin_IATA == aux.1 & Destination_IATA == aux.2) |
                                  (Origin_IATA == aux.2 & Destination_IATA == aux.1))
      
    } else if(type.1 == "airport"){
      ts <- data.aux %>% filter(Origin_IATA == ts_name)
      
    } else if (type.1 == "airport group"){
      ts <- data.aux %>% filter(Origin_Airport_Group == ts_name)
      #ts.aux <- data.aux %>% filter(Origin_Airport_Group == ts_name & Destination_Airport_Group == ts_name)
      #ts <- rbind(ts, ts.aux)
    }
  }
  
    #create the series
    ts <- ts %>% group_by(Year = Year) %>%
      summarise_at(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                   "Oct", "Nov", "Dec"), .funs = sum) %>%
      melt(id.vars = "Year")
    #Structure the dates
    start.date <- as.Date(start.date, format = "%Y-%m-%d")
    end.date <- as.Date(end.date, format = "%Y-%m-%d")
    ts$date <- as.factor(paste0(as.character(ts$Year), "-", as.character(ts$variable), "-01"))
    ts$date <- as.Date(ts$date, format = "%Y-%b-%d")
  
    #Remove NA values
    ts <- na.omit(ts)
  
    #Create time series object
    ts <- xts(ts[,3], order.by = ts$date)
    aux.txt <- paste0(as.character(start.date),"/",as.character(end.date))
    ts <- ts[aux.txt]

    return(ts)
  
}
