library(data.table)
library(tidyverse)
data201801 = fread("201801.csv") %>% as.tbl()
names(data201801)
data201801$starttime = gsub(x = data201801$starttime,pattern = "(\\d+)/(\\d+)/(\\d+) +(\\d+):(\\d+)",replacement = "\\1-\\2-\\3-\\4-\\5")
data201801 = data201801 %>% separate(starttime,into = c("startyear","startmonth","startday","starthour","startmin"))
data201801$stoptime = gsub(x = data201801$stoptime,pattern = "(\\d+)/(\\d+)/(\\d+) +(\\d+):(\\d+)",replacement = "\\1-\\2-\\3-\\4-\\5")
data201801 = data201801 %>% separate(stoptime,into = c("stopyear","stopmonth","stopday","stophour","stopmin"))

data201801$startmonth = as.numeric(data201801$startmonth)
data201801$startday = as.numeric(data201801$startday)
data201801$startyear = as.numeric(data201801$startyear)
data201801$starthour = as.numeric(data201801$starthour)
data201801$startmin = as.numeric(data201801$startmin)
data201801$stopmonth = as.numeric(data201801$stopmonth)
data201801$stopday = as.numeric(data201801$stopday)
data201801$stopyear = as.numeric(data201801$stopyear)
data201801$stophour = as.numeric(data201801$stophour)
data201801$stopmin = as.numeric(data201801$stopmin)

for(i in 1:range(data201801$startday)[2]){
  #first determine every bike's start point everyday
  if(i == 1){
    temp = data201801 %>% filter(startday == i)
    bike.first = temp[match(unique(temp$bikeid),temp$bikeid),]
    bike.first = bike.first[,c(4,14,15,20)]
    startpoint = bike.first
  }
  temp = data201801 %>% filter(startday == i)
  bike.first = temp[match(unique(temp$bikeid),temp$bikeid),]
  bike.first = bike.first[,c(4,14,15,20)]
  startpoint = rbind(startpoint,bike.first)
  for(j in 1:23){
    if(i == 1 & j == 1){
      mintemp = temp %>% filter(stophour == j-1)
      bike.last = mintemp[tapply(seq_along(mintemp$bikeid) ,mintemp$bikeid,FUN = max),c(4,10,18,19,20)]
      stoppoint = bike.last
    }
    mintemp = temp %>% filter(stophour == j-1)
    bike.last = mintemp[tapply(seq_along(mintemp$bikeid) ,mintemp$bikeid,FUN = max),c(4,10,18,19,20)]
    stoppoint = rbind(stoppoint,bike.last)
  }
}

stoppoint$stophour = stoppoint$stophour + 1

names(startpoint)[c(2,3)] = c("latitude","longitude")
names(stoppoint)[c(3,4)] = c("latitude","longitude")
for(i in 1: range(startpoint$startday)[2]){
  for(j in 1:23){
    if(j == 1 & i == 1){
      tempstop = stoppoint %>% filter(stophour == j & startday == i)
      tempstart = startpoint %>% filter(startday == i)
      unused = which(!tempstart$bikeid %in% tempstop$bikeid)
      temp = tempstart[unused,]
      temp$stophour = rep(j,length(unused))
      temp = temp %>% select(startday,stophour,latitude,longitude,bikeid)
      timetable = rbind(tempstop,temp)
    }
    tempstop = stoppoint %>% filter(stophour == j & startday == i)
    tempstart = startpoint %>% filter(startday == i)
    unused = which(!tempstart$bikeid %in% tempstop$bikeid)
    temp = tempstart[unused,]
    temp$stophour = rep(j,length(unused))
    temp = temp %>% select(startday,stophour,latitude,longitude,bikeid)
    timetable1 = rbind(tempstop,temp)
    timetable = rbind(timetable,timetable1)
  }
}







#formal all year
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



march = dat %>% filter(startmonth == 3)
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