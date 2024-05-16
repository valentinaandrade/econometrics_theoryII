# Identificar los nombres de países únicos
nombres_unicos <- unique(data$country)

# Crear una nueva variable en data que contenga los nombres de países únicos
data$country_unico <- data$country

# Asignar los nombres de países únicos a las filas correspondientes
data$country_unico[duplicated(data$country)] <- NA

data <- data %>% 
  group_by(country) %>%
  mutate(num_universidades = n_distinct(university))


# Fusionar los datos con el objeto geo
map2 <- merge(states, data, by.x = "sovereignt", by.y = "country_unico")


mybins <- c(0,0.5,0.7,0.9, 1, 1.3,1.4, Inf)
mypalette <- colorBin( palette=c("#0099CC", "white","magenta4"), domain=map2@data$num_universidades, na.color="transparent", bins=mybins)
map2@data$NAME <- map2@data$sovereignt


# Prepare the text for tooltips:
mytext <- paste(
  "<b>", map2@data$NAME,"</b>","<br/>", 
  "U: ", map2@data$university,"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(map2) %>% 
  addTiles(providers$Stamen.TonerLite)  %>% 
  setView(lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(num_universidades), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="black", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto")) %>%
  addLegend( pal=mypalette, values=~num_universidades, opacity=0.9, title = "Institutions participants", position = "bottomleft" )



here
states <- geojsonio::geojson_read("custom.geo.json", what = "sp")

class(states)
names(states)
