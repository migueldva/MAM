#Function which finds increasing/decreasing trends:

test <- find_destinations("MTY", start_date = "2017-01-01", 
                           end_date = "2019-01-01", 
                           type.2 = "consolidated", 
                           type.3 = "passengers",
                           mkt_share = F)


test2 <- find_destinations("MTY", start_date = "2016-01-01", 
                           end_date = "2018-01-01", 
                           type.2 = "consolidated", 
                           type.3 = "passengers",
                           mkt_share = F)


test <- as.data.frame(test)
test2 <- as.data.frame(test2)


growth <- data.frame()

for(name in union(names(test), names(test2))){
  for(i in 1:nrow(test)){
    if(length(test[i,name]) == 0){a <- 0} else{a <- test[i,name]}
    if(length(test2[i,name]) == 0){b <- 0} else{b <- test2[i,name]}
    growth[i, name] <- a - b
  }
}


growth[is.na(growth)] <- 0

air_g <- (rowSums(test)/rowSums(test2))-1

growth <- (growth/rowSums(growth))*100


growth$date <- seq.Date(as.Date("2017-01-01"), as.Date("2019-01-01"), by = "month")

growth <- melt(growth, id.vars = "date")
#growth$relevance <- (growth$value > 25 | growth$value < -25)

growth <- unique(merge(growth, pax_cons[,c(1,5)],by.x = "variable", by.y = "Origin_IATA", all.x = F, all.y = F))


growth_chart <- growth %>%
  group_by(Origin_Airport_Group, date) %>%
  summarise(value = sum(value)) 


growth_chart$variable <- "Others"


growth_chart_2 <- growth %>% filter(relevance == T)
growth_chart <- growth_chart[,c(2,4,3,1)]

growth_2 <- rbind(as.data.frame(growth_chart),as.data.frame(growth_chart_2))


final <- dcast(growth_chart, date ~ Origin_Airport_Group)
final[is.na(final)] <- 0
final[,-1] <- final[,-1]*air_g
final <- melt(final, id.vars = "date")

final <- final %>% 
  group_by(name = variable) %>%
  do(data = round(.$value, digits = 1))


colors <- c("#376B7F", "#58436E", "#D3C771", "#538059","#AB503C" , "#948F8F")

highchart() %>%
  hc_chart(type = "column") %>%
  hc_colors(colors) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_add_series_list(final) %>%
  hc_tooltip(pointFormat = paste0("{series.name}: {point.y} %")) %>%
  hc_xAxis(categories = seq.Date(from = as.Date("2017-01-01"), to = as.Date("2019-01-01"), by = "month")) %>%
  hc_yAxis(stackLabels = list(enabled = T, format = "{total} %"))
  


colors <- colorize(final$name, colors = c("#FAD2D2","#CA0000"))
colors <- viridis_pal(option = "D")(length(final$name))


hchart(growth_2, "column", hcaes(x = airport, y = growth, color = coloract),
       tooltip = list(pointFormat = paste0("Airport: {point.airport} <br>",
                                           "Operated by: {point.Origin_Airport_Group} <br>",
                                           "Growth in selected dates: {point.growth}"))) %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "")) %>%
  hc_subtitle(text = "Source: SCT")
