library(ggmap)
library(tidyverse)
library(data.table)
library(shinyjs)
timetable = fread("timetable.csv") %>% as.tbl()
stat = fread("stat.txt")
hot = fread("hot.txt")
stop = fread("stop.csv")
map = get_googlemap("New York City",zoom = 13)
map1 = get_googlemap("New York City",zoom = 12)
work = c(rep(c(rep(0,5),rep(1,2)),4),1,1,1)
work = rep(work,each=5)
holiday = rep(0,31)
holiday[c(5,14,29)] = 1
holiday = rep(holiday,each = 5)
hot$work = work
hot$holiday = holiday
hot = hot %>% mutate(free = work + holiday)
mostpopular = stat[stat$n>5,]
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    now = timetable %>% filter(startday == input$day & stophour == input$hour)
    now = now %>% unite(stop,longitude,latitude)
    counts = now %>% count(stop)
    counts = counts %>% separate(stop,into = c("longitude","latitude"),sep = "_")
    counts$longitude = as.numeric(counts$longitude)
    counts$latitude = as.numeric(counts$latitude)
    counts$nc = as.character(ifelse(as.integer(counts$n/10)>10,10,as.integer(counts$n/10)))
    ggmap(map)+
      geom_point(mapping = aes(x = longitude , y = latitude,colour = nc),data = counts)+
      geom_text(aes(x = longitude , y = latitude,label=nc),data = counts)
    
  })
  output$popularplot = renderPlot({
    shinyjs::hide(id = "hour")
    daysite = hot %>% filter(day == input$day)
    ggmap(map1)+geom_point(mapping = aes(x = longitude,y = latitude),shape = 2*daysite$free,data = daysite)+
      geom_point(mapping = aes(x = longitude,y = latitude),data=mostpopular,color = "red")
  })
  output$plot1 = renderPlot({
    site1 = get_googlemap(center = c(lon = as.numeric(mostpopular[1,1]),lat = as.numeric(mostpopular[1,2])),zoom = 19)
    ggmap(site1)+geom_point(mapping = aes(x = longitude,y = latitude),data = stop)
  })
  output$plot2 = renderPlot({
    site2 = get_googlemap(center = c(lon = as.numeric(mostpopular[2,1]),lat = as.numeric(mostpopular[2,2])),zoom = 19)
    ggmap(site2)+geom_point(mapping = aes(x = longitude,y = latitude),data = stop)
    
  })
  output$plot3 = renderPlot({
    site3 = get_googlemap(center = c(lon = as.numeric(mostpopular[3,1]),lat = as.numeric(mostpopular[3,2])),zoom = 19)
    ggmap(site3)+geom_point(mapping = aes(x = longitude,y = latitude),data = stop)
  })
  output$plot4 = renderPlot({
    site4 = get_googlemap(center = c(lon = as.numeric(mostpopular[4,1]),lat = as.numeric(mostpopular[4,2])),zoom = 19)
    ggmap(site4)+geom_point(mapping = aes(x = longitude,y = latitude),data = stop)
  })
  output$plot5 = renderPlot({
    site5 = get_googlemap(center = c(lon = as.numeric(mostpopular[5,1]),lat = as.numeric(mostpopular[5,2])),zoom = 19)
    ggmap(site5)+geom_point(mapping = aes(x = longitude,y = latitude),data = stop)
  })
  
}
