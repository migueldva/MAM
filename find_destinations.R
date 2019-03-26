find_destinations <- function(airport, start_date = "2018-01-01", 
                              end_date = "2018-12-01", 
                              type.2 = c("international", "domestic", "consolidated"), 
                              type.3 = c("passengers", "flights", "passengers per flight"),
                              mkt_share = T){
  
  #Prepare the  desired data
  if(type.3 == "passengers"){
    data.aux.d <- pax_domesc
    data.aux.i <- pax_int
  } else if(type.3 == "flights" | type.3 == "passengers per flight") {
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
  
  data <- data.aux %>%
    filter(Origin_IATA == airport | Destination_IATA == airport) %>%
    filter(Year >= format(as.Date(start_date, format="%Y-%m-%d"),"%Y") & Year <= format(as.Date(end_date, format="%Y-%m-%d"),"%Y"))
    
  destinations <<- unique(c(data$Origin_IATA[data$Origin_IATA != airport], data$Destination_IATA[data$Destination_IATA != airport]))
  
  if(mkt_share == T){
    if(type.3 == "passengers per flight"){
      flights <- sapply(destinations, 
                              function(x) return(convert_ts(paste0(airport,"-",x),
                                                               "route",
                                                               type.2,
                                                               "flights",
                                                               start_date,
                                                               end_date)))
      passengers <- sapply(destinations, 
                                function(x) return(convert_ts(paste0(airport,"-",x),
                                                               "route",
                                                               type.2,
                                                               "passengers",
                                                               start_date,
                                                               end_date)))
      if(class(passengers) == "list"){
        passengers <- do.call(cbind, passengers)
        passengers[is.na(passengers)] <- 0
      }
      
      if(class(flights) == "list"){
        flights <- do.call(cbind, flights)
        flights[is.na(flights)] <- 0
      }
      
      variable <- colMeans(passengers/flights)
      names(variable) <- NULL
      destinations <- data.frame(destinations, mkt_share = round(variable, digits = 1))
      return(destinations)
      
    } else if (type.3 == "passengers" | type.3 == "flights"){
      variable <- sapply(destinations, 
                                 function(x) return(convert_ts(paste0(airport,"-",x),
                                                               "route",
                                                               type.2,
                                                               type.3,
                                                               start_date,
                                                               end_date)))
      if(class(variable) == "list"){
        variable <- do.call(cbind, variable)
        variable[is.na(variable)] <- 0
      }
      variable <- colSums(variable)
      
      names(variable) <- NULL
      destinations <- data.frame(destinations, mkt_share = round(variable/sum(variable), digits = 3))
      return(destinations)
    }

  } else{
    variable <- sapply(destinations, 
                      function(x) return(convert_ts(paste0(airport,"-",x),
                                                              "route",
                                                              type.2,
                                                              type.3,
                                                              start_date,
                                                              end_date)))
    if(class(variable) == "list"){
      variable <- do.call(cbind, variable)
      variable[is.na(variable)] <- 0
    }
    
    #destinations <- data.frame(destinations, type.3 = variable)
    return(variable)
  }
}

