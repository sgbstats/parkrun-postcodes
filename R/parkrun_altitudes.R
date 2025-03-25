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

for(i in 1:nevents)
{
  short[i]=parkrunsall$events$features[[i]]$properties$EventShortName
  long[i]=parkrunsall$events$features[[i]]$properties$EventLongName
  countrycode[i]=parkrunsall$events$features[[i]]$properties$countrycode
  coords[i,]=parkrunsall$events$features[[i]]$geometry$coordinates
}

parkruns=cbind.data.frame(short,long,countrycode, coords) %>% 
  rename("lat"="2",
         "lon"="1") %>% 
  filter(countrycode==97,
         !grepl("junior",long),
        short %notin% c("Cape Pembroke Lighthouse", "Jersey", "Guernsey", "Douglas", "Nobles")
        ) %>% 
  arrange(short)

alt_finder=function(lat,lon)
{
  url=paste("https://api.opentopodata.org/v1/eudem25m?locations=", lat,",",lon, sep="")
  json=RJSONIO::fromJSON(url)
  alt=json$results[[1]]$elevation
  if(is.null(alt))
  {
    alt=NA_real_
  }
  return(alt)
  
}

alt=numeric(nrow(parkruns))
tic()
for(i in 1:nrow(parkruns))
{
  skip_to_next <- FALSE
  tryCatch({
    alt[i]=alt_finder(lat=parkruns$lat[i], lon=parkruns$lon[i])
    print(i)
    Sys.sleep(1)
  },
  error = function(e) { skip_to_next <<- TRUE})
  
}

toc()

parkrun_alt=cbind.data.frame(parkruns, alt)
write.csv(parkrun_alt,"Data/parkrun_uk_altitude.csv", row.names = F)


parkruns_foreign=cbind.data.frame(short,long,countrycode, coords) %>% 
  rename("lat"="2",
         "lon"="1") %>% 
  filter(countrycode!=97,
         !grepl("junior",long),
         short %notin% c("Cape Pembroke Lighthouse", "Jersey", "Guernsey", "Douglas", "Nobles")
  ) %>% 
  arrange(short)

alt=numeric(nrow(parkruns_foreign))
tic()
# 
for(i in 1:nrow(parkruns_foreign))
{
  Sys.sleep(1)
  print(paste(i, parkruns_foreign$short[i], alt[i]))
  skip_to_next <- FALSE
  tryCatch({
    
    alt[i]=alt_finder(lat=parkruns_foreign$lat[i], lon=parkruns_foreign$lon[i])
    
    
  },
  error = function(e) { skip_to_next <<- TRUE})
  
}
toc()

