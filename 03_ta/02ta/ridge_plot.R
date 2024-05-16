#1.1 Manipulacion general
pacman::p_load("tidyverse","dplyr","ggplot2",
               "haven","forcats","ggsci",
               "hrbrthemes",
               "gganimate", "broom", "plotly", "magick",
               "png","gifski", "dygraphs", "wourdcloud2") #interactive 

#1.2 Manipulacion de mapas
pacman::p_load("sf","raster","spData","spDataLarge",
               "rgdal","leaflet", "htmlwidgets", "tmap", "mapview", "shiny", "maps")


universities <- c("Harvard", "MIT", "UC Berkeley", "Northwestern", "Chicago", "Stanford", "Cornell", 
                  "UPenn-Wharton", "IADB", "Toulouse", "UBC", "Brown", "UC Santa Barbara", 
                  "Vanderbilt", "Carlos III", "NYU Abu Dhabi", "Georgetown", "Queen Mary", 
                  "Virginia", "Los Andes", "PUC", "Uchile", "Bolzano", "Potsdam", "Stockholm", 
                  "LSE", "São Paulo", "Insper", "UDP", "Penn State", "Fed Chicago", "ITAM", 
                  "Central Bank Colombia", "Central Bank Chile", "UAndes Chile", "World Bank", 
                  "John Hopkins", "KU Leuven", "London Business School", "Wisconsin Madison", 
                  "Microsoft", "U Pacifico", "CAF", "3IE-USM", "George Mason", "UAI", "U del Rosario", 
                  "EAFIT", "Di Tella", "SMU", "UC Santa Cruz", "UC San Diego", "UC Brasilia", 
                  "Georgia Tech", "Illinois", "Banco de la República", "Wilfrid Laurier")

# Crear un vector con los países correspondientes a cada universidad
countries <- c("USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "France", "Canada", "USA", 
               "USA", "USA", "Spain", "UAE", "USA", "UK", "USA", "Colombia", "Chile", "Chile", "Italy", 
               "Germany", "Sweden", "UK", "Brazil", "Brazil", "Chile", "USA", "USA", "Mexico", "Colombia", 
               "Chile", "Chile", "Italy", "USA", "Belgium", "UK", "USA", "USA", "USA", "Peru", 
               "US", "Chile", "USA", "Chile", "Colombia", "Argentina", "Singapore", 
               "USA", "USA", "Brazil", "USA", "USA", "Colombia", "Canada")
# Corregir los países correspondientes a U del Rosario y UAI
countries[which(universities == "U del Rosario")] <- "Colombia"
countries[which(universities == "UAI")] <- "Chile"

# Crear el data frame
universities_df <- data.frame(university = universities, country = countries)

# Agregar la columna de continente
universities_df$continent <- countrycode(universities_df$country, "country.name", "continent")

db <- universities_df %>% 
  mutate(continent  = if_else(is.na(continent), "Americas", continent))


# 4. Manipular base de datos

#2. Cargar mapa
#Make a Spdb object (spatial polygon data frame).
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="../input/data/world_shape_file.zip")
unzip(zipfile = "world_shape_file.zip")
geo <- shapefile("TM_WORLD_BORDERS_SIMPL-0.3.shp")

#Rename Korea, Republic of
geo$NAME <- gsub("Korea, Republic of", "South Korea", geo$NAME)
geo$NAME <- gsub("United States Minor Outlying Islands", "United States", geo$NAME)
geo$NAME <- gsub("United States Virgin Islands", "United States", geo$NAME)

db$country <- gsub("Russian Federation", "Russia", db$country)
# Agrupar el data frame por país y combinar todas las universidades
universities_by_country <- db %>%
  group_by(country, continent) %>%
  summarize(universities = paste(university, collapse = ", "),
            num_universities = n())

#Join map and data
map2 <- merge(geo, data, by.x = "NAME", by.y = "country")

# 3. Create a color palette with handmade bins (RColorBrewer)

mybins <- c(0,0.5,0.7,0.9, 1, 1.3,1.4, Inf)
mypalette <- colorBin( palette=c("#0099CC", "white","magenta4"), domain=map@data$num_universities, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "<b>", map@data$NAME,"</b>","<br/>", 
  "U: ", map@data$universities,"%", "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(map) %>% 
  addTiles(providers$Stamen.TonerLite)  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(num_universities), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="black", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto")) %>%
  addLegend( pal=mypalette, values=~num_universities, opacity=0.9, title = "Institutions participants", position = "bottomleft" )



set.seed(191247329)

before <- data %>% select(1:number_reporte) %>% 
  pivot_longer(cols =c(beforer_1:beforer_7), names_to = "number_mention", values_to = "beforer") %>%
  filter(!is.na(beforer)) %>% 
  group_by(beforer) %>% 
  summarise(n = n()) %>%
  arrange(-n) %>% 
  ungroup() %>%
  mutate(beforer1 = beforer) %>% 
  column_to_rownames(var = "beforer1")

wordcloud2::wordcloud2(before)


# Contar la frecuencia de cada país en el data frame
country_freq <- table(db$university)

# Convertir la frecuencia a un data frame
country_df <- as.data.frame(country_freq)

# Cambiar los nombres de las columnas
names(country_df) <- c("country", "freq")

# Crear el Wordcloud
wordcloud2::wordcloud2(country_df)





library(leaflet)

# Create 20 markers (Random points)
data = data.frame(
  long=sample(seq(-150,150),20),
  lat=sample(seq(-50,50),20),
  val=round(rnorm(20),2),
  name=paste("point",letters[1:20],sep="_")
) 

data <- data %>% mutate(name = probando, long = 71, lat = 43)

# Show a circle at each position
m = leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(~long, ~lat , popup = ~as.character(name))
m


if (!require(maps)) {
  install.packages("maps")
}
library(maps)

# Obtener las coordenadas de longitud y latitud para los países
country_coords <- map("world", plot = FALSE, fill = TRUE)$names

# Crear un data frame con las coordenadas de longitud y latitud para cada país
country_coords_df <- data.frame(country = country_coords,
                                long = sapply(country_coords, function(x) map(x, plot = FALSE)$range[1]),
                                lat = sapply(country_coords, function(x) map(x, plot = FALSE)$range[3]))

# Mostrar el resultado
print(country_coords_df)
