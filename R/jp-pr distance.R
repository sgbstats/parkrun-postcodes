library(XML)
library(tidyverse)
library(RCurl)
library(RJSONIO)
library(tictoc)
library(geosphere)
`%notin%`=Negate(`%in%`)


for(i in 1:nevents)
{
  short[i]=parkrunsall$events$features[[i]]$properties$EventShortName
  long[i]=parkrunsall$events$features[[i]]$properties$EventLongName
  countrycode[i]=parkrunsall$events$features[[i]]$properties$countrycode
  coords[i,]=parkrunsall$events$features[[i]]$geometry$coordinates
}



parkruns5k=cbind.data.frame(short,long,countrycode, coords) %>% 
  rename("lat"="2",
         "lon"="1") %>% 
  mutate(long=iconv(long, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  # filter(countrycode==97) %>% 
  filter(!grepl("junior",long),
         short %notin% c("Cape Pembroke Lighthouse", "Jersey", "Guernsey", "Douglas", "Nobles")) %>% 
  arrange(short) %>% 
  mutate(foo=1) 

startdates=read.csv("Data/jpstartdates.csv") %>% janitor::clean_names()  %>%
  mutate(first_run=as.Date(first_run, format="%d/%m/%Y"),
         jpage=as.numeric(difftime(as.Date("2024-04-10"),first_run, units="days"))/365.25)%>% 
  select(event, jpage)


parkruns2k=cbind.data.frame(short,long,countrycode, coords) %>% 
  rename("lat"="2",
         "lon"="1") %>% 
  mutate(long=iconv(long, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  # filter(countrycode==97) %>% 
  filter(grepl("junior",long),
         short %notin% c("Cape Pembroke Lighthouse", "Jersey", "Guernsey", "Douglas", "Nobles")) %>% 
  arrange(short)%>% 
  mutate(foo=1) %>% 
  select(-countrycode) %>% 
  merge(startdates, by.x="long", by.y="event", all.x=T)

parkruns=merge(parkruns5k %>% dplyr::select(-long) ,
               parkruns2k%>% dplyr::select(-long) %>% rename("shortjunior"="short"), by="foo") %>% 
  mutate(rn=row_number()) %>% 
  mutate(euclid=(lon.x-lon.y)^2+(lat.x-lat.y)^2) %>% 
  slice_min(euclid, n=1, by=short) %>%
  rowwise() %>% 
  mutate(dist=distm(cbind(lon.x,lat.x), cbind(lon.y, lat.y), fun = distHaversine)) %>% 
  select(-euclid, -lat.x,-lon.x, -lat.y, -lon.y, -foo, -rn) %>% 
  arrange(dist) %>% 
  filter(dist<1000) %>% 
  mutate(match=case_when(gsub(" juniors", "",shortjunior)==short~"Perfect",
                         stringr::word(short)==stringr::word(shortjunior)~"Partial",
                         T~"Different"))
write.csv(parkruns, "Data/jp-pr-distance.csv")

parkruns %>% mutate(out=match=="Perfect") %>% glm(out~dist, data=.) %>% summary()
parkruns %>% mutate(out=match!="Different") %>% glm(out~dist, data=.) %>% summary()

parkruns %>% mutate(out=match=="Perfect") %>% glm(out~jpage, data=.) %>% summary()
parkruns %>% mutate(out=match!="Different") %>% glm(out~jpage, data=.) %>% summary()


parkruns_more=merge(parkruns5k %>% dplyr::select(-long) ,
                    parkruns2k%>% dplyr::select(-long) %>% rename("shortjunior"="short"), by="foo") %>% 
  mutate(rn=row_number()) %>% 
  mutate(euclid=(lon.x-lon.y)^2+(lat.x-lat.y)^2) %>% 
  slice_min(euclid, n=1, by=short) %>%
  rowwise() %>% 
  mutate(dist=distm(cbind(lon.x,lat.x), cbind(lon.y, lat.y), fun = distHaversine)) %>% 
  select(-euclid, -lat.x,-lon.x, -lat.y, -lon.y, -foo, -rn) %>% 
  arrange(-dist) %>% 
  # filter(dist<1000) %>% 
  mutate(match=case_when(gsub(" juniors", "",shortjunior)==short~"Perfect",
                         stringr::word(short)==stringr::word(shortjunior)~"Partial",
                         T~"Different")) %>% 
  filter(match=="Perfect")



parkruns_evenmore=merge(parkruns5k %>% dplyr::select(-long) ,
                        parkruns2k%>% dplyr::select(-long) %>% rename("shortjunior"="short"), by="foo") %>% 
  mutate(rn=row_number()) %>% 
  mutate(match=case_when(gsub(" juniors", "",shortjunior)==short~"Perfect",
                         stringr::word(short)==stringr::word(shortjunior)~"Partial",
                         T~"Different")) %>% 
  filter(match=="Perfect") %>% 
mutate(euclid=(lon.x-lon.y)^2+(lat.x-lat.y)^2) %>% 
  # slice_min(euclid, n=1, by=short) %>%
  rowwise() %>% 
  mutate(dist=distm(cbind(lon.x,lat.x), cbind(lon.y, lat.y), fun = distHaversine)) %>% 
  select(-euclid, -lat.x,-lon.x, -lat.y, -lon.y, -foo, -rn) %>% 
  arrange(-dist) 
# filter(dist<1000) %>% 

startdates=read.csv("Data/jpstartdates.csv") %>% janitor::clean_names()
