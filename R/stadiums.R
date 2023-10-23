library(tidyverse)
load("Data/postcodes.RDa")

teams=readxl::read_excel("Data/leagueteams.xlsx")
stadiums=read.csv("Data/stadiums.csv")

s2=stadiums %>% merge(teams, by="Team", all=T) %>% filter(!is.na(League)) %>% 
  select(-Also.known.as, -Grid.Reference, -Easting, -Northing)

s3=s2 %>% mutate(id=1) %>%  merge(parkrunsuk_postcodes %>% mutate(id=1), by="id")

stadium_parkruns=s3%>% 
  rowwise() %>% 
  mutate(dist=geosphere::distHaversine(cbind(Longitude, Latitude), cbind(lon, lat))/1e3) %>% 
  group_by(Team, Name) %>% 
  slice_min(dist) %>% 
  select(Team, Name, short) %>% 
  ungroup()

write.csv(stadium_parkruns, "Data/stadium_pakruns.csv")
  
  