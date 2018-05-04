library(Matrix)
library(tidytext)
library(tidyverse)
data = dat %>% unite(startpt,`start station longitude`,`start station latitude`)
data = data %>% unite(endpt,`end station longitude`,`end station latitude`)
stops = data$endpt %>% as.factor() %>% levels()
stop = data.frame(stops) %>% separate(stops,into = c("longitude","latitude"),"_")
stop$latitude = as.numeric(stop$latitude)
stop$longitude = as.numeric(stop$longitude)
write.table(stop,"stop.csv",col.names = T,row.names = F)
for(j in 1:31){
  if(j == 1){
    temp = data %>% filter(startmonth == 5 & startday == 1)
    temp = temp %>% select(startpt,endpt)
    ad = matrix(rep(0,858*858),nrow = 858)
    for(i in 1:858){
      temp1 = temp %>% filter(startpt == stops[i])
      if(temp1 %>% nrow() == 0){
        next
      }else{
        destinations = temp1 %>% count(endpt)
        ad[i,match(destinations$endpt,stops)]=destinations$n
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
    top5 = order(ev,decreasing = T)[1:5]
    popular = data.frame(site = stops[-nan][top5],day = rep(1,5))
    
  }else{
    temp = data %>% filter(startmonth == 5 & startday == j)
    temp = temp %>% select(startpt,endpt)
    ad = matrix(rep(0,858*858),nrow = 858)
    for(i in 1:858){
      temp1 = temp %>% filter(startpt == stops[i])
      if(temp1 %>% nrow() == 0){
        next
      }else{
        destinations = temp1 %>% count(endpt)
        ad[i,match(destinations$endpt,stops)]=destinations$n
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
    top5 = order(ev,decreasing = T)[1:5]
    popular1 = data.frame(site = stops[-nan][top5],day = rep(j,5))
    popular = rbind(popular,popular1)
  }
}
stat = popular %>% count(site)
stat = stat %>% separate(site,into = c("longitude","latitude"),"_")
stat$longitude = as.numeric(stat$longitude)
stat$latitude = as.numeric(stat$latitude)
write.table(stat,"stat.txt",col.names = T,row.names = F)
hot = popular %>% separate(site,into = c("longitude","latitude"),"_")
hot$longitude = as.numeric(hot$longitude)
hot$latitude = as.numeric(hot$latitude)
write.table(hot,"hot.txt",col.names= T,row.names = F)
library(ggmap)


