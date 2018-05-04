setwd("~/Desktop/R/479 project")
library(data.table)
library(tidyverse)

for(i in 1:12){
  if(i == 1){
    file = paste("20180",as.character(i),"_citibikenyc_tripdata.csv",sep = "")
    data = fread(file) %>% as.tbl()
    data[,13:16] = NULL
    data$starttime = gsub(x = data$starttime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    data = data %>% separate(starttime,into = c("startyear","startmonth","startday","starthour","startmin","startsec"))
    data$stoptime = gsub(x = data$stoptime,pattern = "(\\d+)-(\\d+)-(\\d+) +(\\d+):(\\d+):(\\d+)",replacement = "\\1/\\2/\\3/\\4/\\5/\\6")
    dat = data %>% separate(stoptime,into = c("stopyear","stopmonth","stopday","stophour","stopmin","stopsec"))
    dat$stopsec = NULL
    dat$startsec = NULL
  } else if(i %in% c(2,3)){
    file = paste("20180",as.character(i),"_citibikenyc_tripdata.csv",sep = "")
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
    file = paste("20170",as.character(i),"-citibike-tripdata.csv",sep = "")
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
    file = paste("2017",as.character(i),"-citibike-tripdata.csv",sep = "")
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
dat$season = rep("spring",nrow(dat))

dat$season[dat$startmonth %in% c(6,7,8)] = rep("summer",sum(dat$startmonth %in% c(6,7,8)))
dat$season[dat$startmonth %in% c(9,10,11)] = rep("fall",sum(dat$startmonth %in% c(9,10,11)))
dat$season[dat$startmonth %in% c(12,1,2)] = rep("winter",sum(dat$startmonth %in% c(12,1,2)))
dat$season = as.factor(dat$season)

march = dat %>% filter(startmonth == 5)
for(day in 1:31){
  if(day == 1){
    temp = march %>% filter(startday == day)
    bike.first = temp[match(unique(temp$bikeid),temp$bikeid),]
    bike.first = bike.first[,c(3,4,12,13,18)]
    startpoint = bike.first
  }else{
    temp = march %>% filter(startday == day)
    bike.first = temp[match(unique(temp$bikeid),temp$bikeid),]
    bike.first = bike.first[,c(3,4,12,13,18)]
    startpoint = rbind(startpoint,bike.first)
  }
  for(j in 1:23){
    if(day == 1 & j == 1){
      mintemp = temp %>% filter(stophour == j-1)
      bike.last = mintemp[tapply(seq_along(mintemp$bikeid) ,mintemp$bikeid,FUN = max),c(3,4,9,16,17,18)]
      stoppoint = bike.last
    }else{
      mintemp = temp %>% filter(stophour == j-1)
      bike.last = mintemp[tapply(seq_along(mintemp$bikeid) ,mintemp$bikeid,FUN = max),c(3,4,9,16,17,18)]
      stoppoint = rbind(stoppoint,bike.last)
    }
  }
}


stoppoint$stophour = stoppoint$stophour + 1
names(startpoint)[c(3,4)] = c("latitude","longitude")
names(stoppoint)[c(4,5)] = c("latitude","longitude")
nbikes = startpoint %>% count(bikeid) %>% nrow()

for(day in 1:31){
  for(hour in 1:23){
    if(day == 1 & hour == 1){
      tempstop = stoppoint %>% filter(stophour == hour & startday == day)
      tempstart = startpoint %>% filter(startday == day)
      notusing = which(!tempstart$bikeid %in% tempstop$bikeid)
      tempstart = tempstart[notusing,]
      tempstart$stophour = rep(hour,nrow(tempstart))
      tempstart = tempstart %>% select(startmonth,startday,stophour,latitude,longitude,bikeid)
      timetable = rbind(tempstop,tempstart)
      timetable1 = timetable
    }else if(hour == 1 & day != 1){
      tempstop = stoppoint %>% filter(stophour == hour & startday == day)
      tempstart = startpoint %>% filter(startday == day)
      notusing = which(!tempstart$bikeid %in% tempstop$bikeid)
      tempstart = tempstart[notusing,]
      tempstart$stophour = rep(hour,nrow(tempstart))
      tempstart = tempstart %>% select(startmonth,startday,stophour,latitude,longitude,bikeid)
      timetable1 = rbind(tempstop,tempstart)
      timetable = rbind(timetable,timetable1)
    }else{
      tempstop = stoppoint %>% filter(stophour == hour & startday == day)
      tempstart = timetable1
      notusing = which(!tempstart$bikeid %in% tempstop$bikeid)
      tempstart = tempstart[notusing,]
      tempstart$stophour = rep(hour,nrow(tempstart))
      timetable1 = rbind(tempstop,tempstart)
      timetable = rbind(timetable,timetable1)
    }
  }
}
write.table(timetable,"timetable.csv",col.names = T,row.names = F)
