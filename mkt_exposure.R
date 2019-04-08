####Function which returns data frame with market share for top15 destinations


market_exposure <- function(airport, start.date, end.date, type.2, type.3, n){
  
  airport_colors <- data.frame(Origin_Airport_Group = c("ASUR", "GAP", "OMA", "Mexico City", "International", "Other"),
                               coloract = c("#376B7F", "#58436E", "#AB503C", "#538059", "#D3C771", "#948F8F"))
  
  market <- find_destinations(airport, start_date = start.date, end_date = end.date, type.2 = type.2, type.3 = type.3)
  market <- top_n(market,n,mkt_share)
  
  market <- rbind(market, data.frame(destinations = "Others", mkt_share = 1 - sum(market$mkt_share)))
  market <- unique(merge(market, pax_cons[,c(1,5)],by.x = "destinations", by.y = "Origin_IATA", all.x = F, all.y = F))
  market <- merge(market, airport_colors)
  
  if(type.3 != "passengers per flight"){
    market$mkt_share <- round(market$mkt_share*100, digits = 1)
  }
  
  market <- market %>% filter(mkt_share > 1)
  
  market <- market[order(market$mkt_share, decreasing = T),]
  
  return(market)
  
}



