library(data.table)
library(dplyr)
library(tidyverse)

for(i in 1:12){
  if(i == 1){
    file = paste("20180",as.character(i),".csv",sep = "")
    data = fread(file) %>% as.tbl()
    data[,13:16] = NULL
    data$starttime = gsub(x = data$starttime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(starttime,into = c("startyear","startmonth","startday","starthour","startmin","startsec"))
    data$stoptime = gsub(x = data$stoptime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    dat = data %>% separate(stoptime,into = c("stopyear","stopmonth","stopday","stophour","stopmin","stopsec"))
    dat$stopsec = NULL
    dat$startsec = NULL
  } else if(i %in% c(2,3)){
    file = paste("20180",as.character(i),".csv",sep = "")
    data = fread(file) %>% as.tbl()
    data[,13:16] = NULL
    data$starttime = gsub(x = data$starttime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(starttime,into = c("startyear","startmonth","startday","starthour","startmin","startsec"))
    data$stoptime = gsub(x = data$stoptime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(stoptime,into = c("stopyear","stopmonth","stopday","stophour","stopmin","stopsec"))
    data$stopsec = NULL
    data$startsec = NULL
    dat = rbind(dat,data)
  }else if(i < 10){
    file = paste("20170",as.character(i),".csv",sep = "")
    data = fread(file) %>% as.tbl()
    data[,13:15] = NULL
    data$starttime = gsub(x = data$starttime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(starttime,into = c("startyear","startmonth","startday","starthour","startmin","startsec"))
    data$stoptime = gsub(x = data$stoptime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(stoptime,into = c("stopyear","stopmonth","stopday","stophour","stopmin","stopsec"))
    data$stopsec = NULL
    data$startsec = NULL
    dat = rbind(dat,data)
  }else{
    file = paste("2017",as.character(i),".csv",sep = "")
    data = fread(file) %>% as.tbl()
    data[,13:15] = NULL
    data$starttime = gsub(x = data$starttime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(starttime,into = c("startyear","startmonth","startday","starthour","startmin","startsec"))
    data$stoptime = gsub(x = data$stoptime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(stoptime,into = c("stopyear","stopmonth","stopday","stophour","stopmin","stopsec"))
    data$stopsec = NULL
    data$startsec = NULL
    dat = rbind(dat,data)
  }
}

dat$startmonth = as.numeric(dat$startmonth)
dat$startday = as.numeric(dat$startday)
dat$startyear = as.numeric(dat$startyear)
dat$starthour = as.numeric(dat$starthour)
dat$startmin = NULL
dat$stopmonth = as.numeric(dat$stopmonth)
dat$stopday = as.numeric(dat$stopday)
dat$stopyear = as.numeric(dat$stopyear)
dat$stophour = as.numeric(dat$stophour)
dat$stopmin = NULL

data <- dat
names(data)
names(data)[11] <- "start_station_name"
names(data)[12] <- "start_station_latitude"
names(data)[13] <- "start_station_longitude"
names(data)[15] <- "stop_station_name"
names(data)[16] <- "stop_station_latitude"
names(data)[17] <- "stop_station_longitude"
by_hour <- group_by(data, startyear, startmonth, startday,starthour)
byhour = summarise(by_hour, num = n(), duration = mean(tripduration))
by_day <- group_by(data, startyear, startmonth, startday)
byday = summarise(by_day, num = n(), duration = mean(tripduration))

by_hour_startstation <- group_by(data, startyear, startmonth, startday,starthour,start_station_name)
byhourstartstation = summarise(by_hour_startstation, num = n(), duration = mean(tripduration))

by_hour_stopstation <- group_by(data, stopyear, stopmonth, stopday,stophour,stop_station_name)
byhourstopstation = summarise(by_hour_stopstation, num = n(), duration = mean(tripduration))

by_day_startstation <- group_by(data, startyear, startmonth, startday,start_station_name)
bydaystartstation = summarise(by_day_startstation, num = n(), duration = mean(tripduration))

by_day_stopstation <- group_by(data, stopyear, stopmonth, stopday,stop_station_name)
bydaystopstation = summarise(by_day_stopstation, num = n(), duration = mean(tripduration))

stations <- select(data, start_station_name,start_station_latitude,start_station_longitude)
stations <- unique(stations)
names(stations)[1] <- "station_name"
names(stations)[2] <- "station_latitude"
names(stations)[3] <- "station_longitude"

by_hour_start <- left_join(byhourstartstation, stations, by = c("start_station_name" = "station_name"))
by_hour_stop <- left_join(byhourstopstation, stations, by = c("stop_station_name" = "station_name"))

by_day_start <- left_join(bydaystartstation, stations, by = c("start_station_name" = "station_name"))
by_day_stop <- left_join(bydaystopstation, stations, by = c("stop_station_name" = "station_name"))


#write.table(byhour, "byhour.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(byday, "byday.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(by_hour_start, "byhourstartstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(by_hour_stop, "byhourstopstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(by_day_start, "bydaystartstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(by_day_stop, "bydaystopstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)


weatherbyhour <- fread("weatherbyhour.csv")
weatherbyhour$year <- c(rep(2017, 6600),rep(2018,2160))
weatherbyhour$month <- c(rep(4,720), rep(5,744),rep(6,720),rep(7,744),rep(8,744),rep(9,720),rep(10,744),rep(11,720),rep(12,744),rep(1,744),rep(2,672),rep(3,744))
weatherbyhour$day <- c(rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),rep(31,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),rep(31,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),rep(31,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),rep(31,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),rep(31,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),rep(31,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),
                       rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24),rep(9,24),rep(10,24),rep(11,24),rep(12,24),rep(13,24),rep(14,24),rep(15,24),rep(16,24),rep(17,24),rep(18,24),rep(19,24),rep(20,24),rep(21,24),rep(22,24),rep(23,24),rep(24,24),rep(25,24),rep(26,24),rep(27,24),rep(28,24),rep(29,24),rep(30,24),rep(31,24))
a = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
weatherbyhour$hour = rep(a,365)

weatherbyhour$season[weatherbyhour$month %in% c(3,4,5)] = rep("spring",sum(weatherbyhour$month %in% c(3,4,5)))
weatherbyhour$season[weatherbyhour$month %in% c(6,7,8)] = rep("summer",sum(weatherbyhour$month %in% c(6,7,8)))
weatherbyhour$season[weatherbyhour$month %in% c(9,10,11)] = rep("fall",sum(weatherbyhour$month %in% c(9,10,11)))
weatherbyhour$season[weatherbyhour$month %in% c(12,1,2)] = rep("winter",sum(weatherbyhour$month %in% c(12,1,2)))

a = c(rep(6,24),rep(7,24),rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24))
weatherbyhour$dayofweek = c(rep(a,52),rep(6,24))

f_byhour = left_join(byhour,weatherbyhour,by=c("startyear"="year", "startmonth" = "month", "startday" = "day","starthour" = "hour"))
f_byhourstartstation = left_join(byhourstartstation,weatherbyhour,by=c("startyear"="year", "startmonth" = "month", "startday" = "day","starthour" = "hour") )
f_byhourstopstation = left_join(byhourstopstation,weatherbyhour,by=c("stopyear"="year", "stopmonth" = "month", "stopday" = "day","stophour" = "hour") )

weatherbyday <- fread("weatherbyday.csv")
weatherbyday$year <- c(rep(2017, 275),rep(2018,90))
weatherbyday$month <- c(rep(4,30), rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31),rep(1,31),rep(2,28),rep(3,31))
weatherbyday$day <- c(1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31,1:31,1:28,1:31)


weatherbyday$season[weatherbyday$month %in% c(3,4,5)] = rep("spring",sum(weatherbyday$month %in% c(3,4,5)))
weatherbyday$season[weatherbyday$month %in% c(6,7,8)] = rep("summer",sum(weatherbyday$month %in% c(6,7,8)))
weatherbyday$season[weatherbyday$month %in% c(9,10,11)] = rep("fall",sum(weatherbyday$month %in% c(9,10,11)))
weatherbyday$season[weatherbyday$month %in% c(12,1,2)] = rep("winter",sum(weatherbyday$month %in% c(12,1,2)))

a = c(6,7,1,2,3,4,5)
weatherbyday$dayofweek = c(rep(a,52),6)

f_byday = left_join(byday,weatherbyday,by=c("startyear"="year", "startmonth" = "month", "startday" = "day"))
f_bydaystartstation = left_join(bydaystartstation,weatherbyday,by=c("startyear"="year", "startmonth" = "month", "startday" = "day"))
f_bydaystopstation = left_join(bydaystopstation,weatherbyday,by=c("stopyear"="year", "stopmonth" = "month", "stopday" = "day") )

#write.table(f_byhour, "f_byhour.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(f_byhourstartstation, "f_byhourstartstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(f_byhourstopstation, "f_byhourstopstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(f_byday, "f_byday.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(f_bydaystartstation, "f_bydaystartstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
#write.table(f_bydaystopstation, "f_bydaystopstation.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)


write.csv(f_byhour, "f_byhour.csv")
write.csv(f_byhourstartstation, "f_byhourstartstation.csv")
write.csv(f_byhourstopstation, "f_byhourstopstation.csv")
write.csv(f_byday, "f_byday.csv")
write.csv(f_bydaystartstation, "f_bydaystartstation.csv")
write.csv(f_bydaystopstation, "f_bydaystopstation.csv")




