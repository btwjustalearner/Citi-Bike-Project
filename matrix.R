library(Matrix)
library(tidytext)
library(tidyverse)
library(data.table)

dat = fread("201801.csv") %>% as.tbl()
names(dat)
dat$starttime = gsub(x = dat$starttime,pattern = "(\\d+)/(\\d+)/(\\d+) +(\\d+):(\\d+)",replacement = "\\1-\\2-\\3-\\4-\\5")
dat = dat %>% separate(starttime,into = c("startyear","startmonth","startday","starthour","startmin"))
dat$stoptime = gsub(x = dat$stoptime,pattern = "(\\d+)/(\\d+)/(\\d+) +(\\d+):(\\d+)",replacement = "\\1-\\2-\\3-\\4-\\5")
dat = dat %>% separate(stoptime,into = c("stopyear","stopmonth","stopday","stophour","stopmin"))

dat$startmonth = as.numeric(dat$startmonth)
dat$startday = as.numeric(dat$startday)
dat$startyear = as.numeric(dat$startyear)
dat$starthour = as.numeric(dat$starthour)
dat$startmin = as.numeric(dat$startmin)
dat$stopmonth = as.numeric(dat$stopmonth)
dat$stopday = as.numeric(dat$stopday)
dat$stopyear = as.numeric(dat$stopyear)
dat$stophour = as.numeric(dat$stophour)
dat$stopmin = as.numeric(dat$stopmin)

data = dat %>% unite(startpt,`start station longitude`,`start station latitude`)
data = data %>% unite(endpt,`end station longitude`,`end station latitude`)
stops = data$endpt %>% as.factor() %>% levels()
temp = data %>% filter(startmonth == 1 & startday == 1)
temp = temp %>% select(startpt,endpt)
ad = matrix(rep(0,858*858),nrow = 858)
for(i in 1:858){
  temp1 = temp %>% filter(startpt == stops[i])
  if(temp1 %>% nrow() == 0){
    next
  }else{
    for(j in 1:858){
      temp2 = temp1 %>% filter(endpt == stops[j])
      num = temp2 %>% nrow()
      if(num == 0){
        next
      }else{
        ad[i,j] = num
      }
    }
  }
}

nan = which(apply(ad,1,sum) == 0)
ad = ad[-nan,-nan]
D = Diagonal(n = nrow(ad),apply(ad,1,sum))
DI = solve(D)
DI[is.infinite(DI)] = 0
Da = DI%*%ad
L = Diagonal(n = nrow(ad),rep(1,nrow(ad))) - Da
decomp = svd(L)
ev = decomp$u[,length(decomp$d)-1]
top10 = order(ev,decreasing = T)[1:10]
popular = data.frame(site = stops[-nan][top10])
popular = popular %>% separate(site,into = c("longitude","latitude"),"_")
