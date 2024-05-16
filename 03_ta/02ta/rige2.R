university <- c("Harvard", "MIT", "UC Berkeley", "Northwestern", "Chicago", "Stanford", "Cornell", 
                 "UPenn-Wharton", "IADB", "Toulouse", "UBC", "Brown", "UC Santa Barbara", 
                 "Vanderbilt", "Carlos III", "NYU Abu Dhabi", "Georgetown", "Queen Mary", 
                 "Virginia", "Los Andes (Colombia)", "Pontificia Universidad Católica de Chile", "Universidad de Chile", 
                 "Free University of Bozen-Bolzano", "University of Potsdam", "Stockholm University", 
                 "London School of Economics and Political Science (LSE)", "University of São Paulo (USP)", 
                 "Insper", "Universidad Diego Portales", "Pennsylvania State University (Penn State)", 
                 "Federal University of Paraná (UFPR)", "University of Illinois at Chicago (UIC)", 
                 "Instituto Tecnológico Autónomo de México (ITAM)", "Central Bank of Colombia", 
                 "Central Bank of Chile", "Universidad de los Andes Chile", "World Bank", 
                 "Johns Hopkins University", "KU Leuven (Katholieke Universiteit Leuven)", 
                 "London Business School", "University of Wisconsin-Madison", "Microsoft Corporation", 
                 "Universidad del Pacífico (Peru)", "Development Bank of Latin America (CAF)", 
                 "3IE - USM", "George Mason University", "Universidad Adolfo Ibáñez (UAI)", 
                 "Universidad del Rosario (Colombia)", "Universidad EAFIT (Colombia)", 
                 "Universidad Torcuato Di Tella (Argentina)", "Singapore Management University (SMU)", 
                 "University of California, Santa Cruz (UC Santa Cruz)", 
                 "University of California, San Diego (UC San Diego)", "University of Brasília (UnB)", 
                 "Georgia Institute of Technology (Georgia Tech)", 
                 "University of Illinois at Urbana-Champaign (UIUC)", "Wilfrid Laurier")


# Crear un vector con los países correspondientes a cada universidad
countries <- c("United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "France", "Canada", "United States", 
               "United States", "United States", "Spain", "UAE", "United States", "United Kingdom", "United States", "Colombia", "Chile", "Chile", "Italy", 
               "Germany", "Sweden", "United Kingdom", "Brazil", "Brazil", "Chile", "United States", "United States", "Mexico", "Colombia", 
               "Chile", "Chile", "Italy", "United States", "Belgium", "United Kingdom", "United States", "United States", "United States", "Peru", 
               "United States", "Chile", "United States", "Chile", "Colombia", "Argentina", "Singapore", 
               "United States", "United States", "Brazil", "United States", "United States", "Colombia", "Canada")
# Corregir los países correspondientes a U del Rosario y UAI
countries[which(university == "Universidad del Rosario (Colombia)")] <- "Colombia"
countries[which(university == "Universidad Adolfo Ibáñez (UAI)")] <- "Chile"

# Crear un nuevo vector de países corregido
# Crear un nuevo vector de países corregido
# Crear un nuevo vector de países corregido
countries_corrected <- c("United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "France", "Canada", "United States", 
                         "United States", "United States", "Spain", "UAE", "United States", "United Kingdom", "United States", "Colombia", "Chile", "Chile", "Italy", 
                         "Germany", "Sweden", "United Kingdom", "Brazil", "Brazil", "Chile", "United States", "United States", "Mexico", "Colombia", 
                         "Chile", "Chile", "Italy", "United States", "Belgium", "United Kingdom", "United States", "United States", "United States", "Peru", 
                         "United States", "Chile", "United States", "Chile", "Colombia", "Argentina", "Singapore", 
                         "United States", "United States", "Brazil", "United States", "United States", "Colombia", "Canada")

# Verificar la longitud del nuevo vector de países
length(countries_corrected)




# Crear el data frame
universities_df <- data.frame(university = university, country = countries, countries_corrected)

# Agregar la columna de continente
universities_df$continent <- countrycode(universities_df$country, "country.name", "continent")

db <- universities_df %>% 
  mutate(continent  = if_else(is.na(continent), "Americas", continent))

# Definir los datos de la tabla como vectores
university <- c("Harvard", "MIT", "UC Berkeley", "Northwestern", "Chicago", "Stanford", "Cornell", 
                "UPenn-Wharton", "IADB", "Toulouse", "UBC", "Brown", "UC Santa Barbara", 
                "Vanderbilt", "Carlos III", "NYU Abu Dhabi", "Georgetown", "Queen Mary", 
                "Virginia", "Pontificia Universidad Católica de Chile", "Pontificia Universidad Católica de Chile", "Universidad de Chile", 
                "Free University of Bozen-Bolzano", "University of Potsdam", "Stockholm University", 
                "London School of Economics and Political Science (LSE)", "University of São Paulo (USP)", 
                "Insper", "Universidad Diego Portales", "Pennsylvania State University", 
                "Federal University of Paraná (UFPR)", "University of Illinois at Chicago (UIC)", 
                "Instituto Tecnológico Autónomo de México (ITAM)", "Universidad de los Andes (Colombia)", 
                "Pontificia Universidad Católica de Chile",  # Aquí eliminamos la duplicación
                "Central Bank of Colombia", "Central Bank of Chile", "Los Andes (Colombia)", 
                "World Bank", "Johns Hopkins University", "KU Leuven (Katholieke Universiteit Leuven)", 
                "London Business School", "University of Wisconsin-Madison", "Microsoft Corporation", 
                "Universidad del Pacífico (Peru)", "Development Bank of Latin America (CAF)", 
                "3IE - USM", "George Mason University", 
                "Universidad Adolfo Ibáñez (UAI)", "Universidad del Rosario (Colombia)", 
                "Universidad EAFIT (Colombia)", "Universidad Torcuato Di Tella (Argentina)", 
                "Singapore Management University (SMU)", "University of California, Santa Cruz (UC Santa Cruz)", 
                "University of California, San Diego (UC San Diego)", "University of Brasília (UnB)", 
                "Georgia Institute of Technology (Georgia Tech)", "University of Illinois at Urbana-Champaign (UIUC)", 
                "Wilfrid Laurier", "Universidad de los Andes Chile")




latitud <- c(42.3736, 42.3601, 37.8719, 42.0555, 41.7897, 37.4275, 42.4470, 39.9526, 38.9059, 43.5624, 49.2606, 
             41.8262, 34.4133, 36.1447, 40.3327, 24.4709, 38.9076, 51.5246, 38.0356, -4.6021, -33.4496, -33.4542, 
             46.4983, 52.3914, 59.3650, 51.5142, -23.5619, -23.5842, -33.4394, 40.7982, -25.4275, 41.8696, 19.3600, 
             -4.6021, -33.4542, 4.6368, -33.4418, -4.6020, 38.9072, 39.3299, 50.8798, 51.5194, 43.0761, 47.6435, 
             -12.0810, 10.4880, -33.4504, 38.8318, -33.4458, -4.5981, -6.2004, -34.5328, 1.2966, 36.9915, 32.8801, 
             -15.7644, 33.7756, 40.1020, 43.4723, -33.4040)

longitud <- c(-71.1097, -71.0942, -122.2585, -87.6752, -87.5997, -122.1697, -76.4835, -75.1652, -77.0424, 1.4683, 
              -123.2458, -71.4032, -119.8489, -86.8027, 3.7682, 54.3735, -77.0723, 0.0399, -78.5031, -74.0650, 
              -70.6646, -70.6342, 11.3543, 13.1287, 18.0697, 0.1165, -46.7300, -46.6721, -70.6505, -77.8599, 
              -49.2733, -87.6496, -99.1836, -74.0650, -70.6342, -74.0825, -70.6541, -74.0650, -77.0369, -76.6205, 
              4.7005, 0.1270, -89.4125, -122.1306, -77.0244, -66.8792, -70.6565, -77.3110, -70.6250, -74.0760, 
              -75.5788, -58.5039, 103.8522, -122.0609, -117.2340, -47.8681, -84.3963, -88.2272, -80.5449, -70.5075)

# Crear el dataframe
df <- data.frame(university = university,
                 latitud = latitud,
                 longitud = longitud)

# Mostrar el dataframe
print(df)

data <- merge(db, df, by = "university", all = T)

data <- data %>% mutate_at(vars(starts_with("l")), ~ as.numeric(.))

mymap <- leaflet() %>% addTiles() 

mymap <- mymap %>%
  addTiles(
    'http://otile{s}.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpeg',
    attribution = 'Institutions <a href="https://economia.uc.cl/2024/04/08/xix-ridge-forum-2024-13-al-17-de-mayo-2024/">RIDGE 2024 - Instituto Economía PUC</a> &mdash; by; <a href="https://valentinaandrade.netlify.app">Valentina Andrade</a>'
  ) %>% setView(-97, 40, zoom = 4)


mymap %>% 
  setView( lat=10, lng=0 , zoom=2) %>% 
  addTiles() %>%
  addMarkers(data = data, lng = ~longitud, lat = ~latitud, popup = ~as.character(university),
             options = popupOptions(openPopup = TRUE),
             icon = list(
               iconUrl = 'https://icons.iconarchive.com/icons/icons-land/vista-map-markers/128/Map-Marker-Ball-Pink-icon.png',
               iconSize = c(40, 40)
             ))

library(leaflet)
library(dplyr)
library(maptools)
library(maps)

# Obtener los límites del mapa
# Crear un dataframe con las coordenadas de los países
world <- map("world", plot = FALSE, fill = TRUE)
world_df <- map2SpatialPolygons(world, IDs = world$names, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Convertir el dataframe a un objeto SpatialPolygonsDataFrame
world_spdf <- SpatialPolygonsDataFrame(world_df, data.frame(region = world$names, stringsAsFactors = FALSE))

# Unir los datos de los países con tus datos
data_with_countries <- left_join(data, world_spdf, by = c("country" = "region"))

# Definir colores basados en las observaciones
pal <- colorFactor("YlOrRd", domain = data$university)

# Agregar polígonos de los países con colores basados en las observaciones
mymap <- mymap %>%
  addPolygons(data = data_with_countries, fillColor = ~pal(university), fillOpacity = 0.7, stroke = FALSE)

# Mostrar el mapa
mymap