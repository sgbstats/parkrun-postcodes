library(tidyverse)


files <- list.files("py/cr/data", pattern = ".csv", full.names = TRUE)

df=tribble(~"Runner",~"localparkruns",~"totalparkruns", ~"parkrun")
totalattend=tribble(~"parkrun", ~"n")


for(i in files){
  name=substr(i, 12,str_length(i)) %>%  gsub("_cr_members.csv", "",.)
  x=read.csv(i) %>% 
    mutate(parkrun=name)
  
  df=df %>% rbind.data.frame(x)
  
  n=tribble(~"parkrun", ~"n",
            name, nrow(x))
  totalattend =totalattend %>% rbind.data.frame(n)
  
}


df2=df %>% merge(totalattend) %>% 
  filter(n==1,
         grepl("bate|carlson|burnett|buckley|charlotte turner|bale|o'donnell|guntrip|luke donald|moyle|rachel brown|mooney|suzy hill|almond|letchfield|culshaw|nat harper|natalie harper|michael peter", Runner,ignore.case = T))



parkrunsall=RJSONIO::fromJSON("https://images.parkrun.com/events.json")


nevents=length(parkrunsall$events$features)

short=character(nevents)
long=character(nevents)
location=character(nevents)
countrycode=numeric(nevents)
name=numeric(nevents)
coords=matrix(0,ncol=2, nrow=nevents)

for(i in 1:nevents)
{
  name[i]=parkrunsall$events$features[[i]]$properties$eventname
  short[i]=parkrunsall$events$features[[i]]$properties$EventShortName
  long[i]=parkrunsall$events$features[[i]]$properties$EventLongName
  countrycode[i]=parkrunsall$events$features[[i]]$properties$countrycode
  coords[i,]=parkrunsall$events$features[[i]]$geometry$coordinates
  location[i]=parkrunsall$events$features[[i]]$properties$EventLocation
}


parkrunsuk=cbind.data.frame(name,short,long,countrycode, coords, location) %>% 
  rename("lat"="2",
         "lon"="1") %>% 
  filter(countrycode==97) %>% 
  filter(!grepl("junior",long),
         short %notin% c("Cape Pembroke Lighthouse", "Jersey", "Guernsey", "Douglas", "Nobles")) %>% 
  arrange(short)

library(geosphere)
nil=parkrunsuk %>% filter(name %in% (totalattend %>% filter(n==0))$parkrun) %>% 
  cross_join(parkrunsuk %>% filter(name=="southmanchester") %>% dplyr::select(lat,lon)) %>% 
  mutate(dist=distm(cbind(lon.x,lat.x), cbind(lon.y, lat.y), fun = distHaversine)/1000, .by="name") %>% 
  arrange(dist) %>% 
  dplyr::select(name, dist)


df %>% filter(totalparkruns>=50) %>% 
  slice_max(localparkruns, by="Runner") %>% 
  mutate(pc=localparkruns/totalparkruns) %>% 
  arrange(-pc)


df3=df %>% filter(totalparkruns>=50) %>% 
  mutate(events=n(), .by = Runner) %>% 
  mutate(tq=events/sum(localparkruns), .by=Runner) %>% 
  arrange(-tq) %>% 
  slice_max(localparkruns, by="Runner", with_ties = F) %>% 
  mutate(tq=sprintf("%.1f", 100*tq)) %>% 
  filter(grepl("bate|carlson|burnett|Alex buckley|charlotte turner|bale|o'donnell|guntrip|luke donald|moyle|rachel brown|mooney|suzy hill|almond|letchfield|culshaw|nat harper|natalie harper|michael peter", Runner,ignore.case = T)) %>% 
  select(Runner, "UK Toursism Quotient"=tq, "Total different UK prs"=totalparkruns, "Total UK events"=events, "UK most done"=parkrun, "Total at most done"=localparkruns)
