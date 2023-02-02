library(XML)
library(tidyverse)
library(RCurl)
library(RJSONIO)
library(tictoc)
`%notin%`=Negate(`%in%`)


parkrunsall=fromJSON("https://images.parkrun.com/events.json?fbclid=IwAR0PP6LCoKah55PCl2sr1JLcHlEPCPdbWLYL2KXpq6V2Kk9aOO67IadRLkE")



nevents=length(parkrunsall$events$features)

short=character(nevents)
long=character(nevents)
countrycode=numeric(nevents)
coords=matrix(0,ncol=2, nrow=nevents)

for(i in 1:2303)
{
  short[i]=parkrunsall$events$features[[i]]$properties$EventShortName
  long[i]=parkrunsall$events$features[[i]]$properties$EventLongName
  countrycode[i]=parkrunsall$events$features[[i]]$properties$countrycode
  coords[i,]=parkrunsall$events$features[[i]]$geometry$coordinates
}

parkrunsuk=cbind.data.frame(short,long,countrycode, coords) %>% 
  rename("lat"="2",
         "lon"="1") %>% 
  filter(countrycode==97,
         !grepl("junior",long),
         short %notin% c("Cape Pembroke Lighthouse", "Jersey", "Guernsey", "Douglas", "Nobles")) %>% 
  arrange(short)


postcode_finder=function(lat,lon)
{
  url=paste("https://findthatpostcode.uk/points/", lat,",",lon, ".json", sep="")
  json=RJSONIO::fromJSON(url)
  postcode=json$data$relationships$nearest_postcode$data[1]
  return(postcode)

}

postcode=character(nrow(parkrunsuk))
tic()
for(i in 1:nrow(parkrunsuk))
{
  postcode[i]=postcode_finder(lat=parkrunsuk$lat[i], lon=parkrunsuk$lon[i])
  print(i)
}

toc()

areaslist=readHTMLTable(getURL("https://en.wikipedia.org/wiki/List_of_postcode_areas_in_the_United_Kingdom"))[1]$`NULL`[-1,] %>% 
  select(V1, V2) %>% 
  rename(area=V1,
         areaname=V2) %>% 
  mutate(areaname=stringr::str_replace_all(areaname, "\\[[0-9]\\]", ""))

parkrunsuk_postcodes=cbind.data.frame(parkrunsuk, postcode) %>% 
  group_by(short) %>% 
  mutate(area = substr(postcode, 1,gregexpr("[0-9]", postcode)[[1]][1]-1),
         sector=substr(postcode, 1,gregexpr(" ", postcode)[[1]][1]-1),) %>% 
  ungroup() %>% 
  merge(areaslist, by="area") %>% 
  dplyr::select(short, long, postcode, area, areaname, sector, lat,lon) %>% 
  arrange(short)
write.csv(parkrunsuk_postcodes, "Data/uk_parkruns_postcodes.csv", row.names = F)



