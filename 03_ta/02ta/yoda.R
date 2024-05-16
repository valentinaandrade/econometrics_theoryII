##### Location Names
Location <- c("Atlanta ","Los Angeles","Chicago","New York","Dallas","Baltimore","Phoenix","Charlotte","Houston","San Antonio", "Seattle" )

#### Latitude and Longitude values for each of the above location
Lat <- c(33.74401,33.82377,41.78798,40.767309,32.88153,39.148492,33.45444,35.2406,29.935842,29.44838,47.714965 )
Lon <- c(-84.56032,-118.2668,-87.7738,-73.978308,-96.64601,-76.796211,-112.32401,-81.04028,-95.398436,-98.39908,-122.127166 )

#### Some hypothetical number of orders shipped out of each location
Orders <- c(1992,2500,3600,2252,3650,3450,4145,3945,5050,4300,1987)

#### Let us create some hypothetical class flags for the cities
Type <- c(rep("Yoda",5),rep("Vader",6))
### Create data set from the above vectors
df <- data.frame(Location, Lat,Lon,Orders,Type)

mymap <- leaflet() %>% addTiles() 

mymap <- mymap %>%
  addTiles(
    'http://otile{s}.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpeg',
    attribution = 'Tiles Courtesy of <a href="http://www.mapquest.com/">MapQuest</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
  ) %>% setView(-97, 40, zoom = 4)


mymap %>% 
  addMarkers(data = df, lng = ~Lon, lat = ~Lat,
             icon = list(
               iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
               iconSize = c(75, 75)
             ))
