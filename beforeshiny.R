library(ggmap)
library(mapproj)
test = data201801[1:100,]
map <- get_map(location = 'New York',zoom = 7)
ggmap(map)
ggmap(map) + geom_point(aes(x = end.station.longitude, y = end.station.latitude), data = test)
qmplot(start.station.longitude, start.station.latitude, data=test, source='google', maptype = 'roadmap',zoom = 13)
get_map("New York City", maptype = 'roadmap', source = 'google')
