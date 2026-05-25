library(XML)
library(tidyverse)
library(RCurl)
library(RJSONIO)
library(httr)
library(rvest)
library(tictoc)
library(googlesheets4)
# gs4_auth(
#   cache = ".secrets",
#   email = gargle::gargle_oauth_email(),
#   scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
# )

tictoc::tic()
parkrunsall = RJSONIO::fromJSON("https://images.parkrun.com/events.json")


nevents = length(parkrunsall$events$features)

short = character(nevents)
long = character(nevents)
location = character(nevents)
countrycode = numeric(nevents)
name = numeric(nevents)
coords = matrix(0, ncol = 2, nrow = nevents)

for (i in 1:nevents) {
       name[i] = parkrunsall$events$features[[i]]$properties$eventname
       short[i] = parkrunsall$events$features[[i]]$properties$EventShortName
       long[i] = parkrunsall$events$features[[i]]$properties$EventLongName
       countrycode[i] = parkrunsall$events$features[[i]]$properties$countrycode
       coords[i, ] = parkrunsall$events$features[[i]]$geometry$coordinates
       location[i] = parkrunsall$events$features[[i]]$properties$EventLocation
}


parkrunsuk = cbind.data.frame(short, long, countrycode, coords, location) |>
       rename("lat" = "2", "lon" = "1") |>
       filter(countrycode == 97) |>
       filter(
              !grepl("junior", long),
              !short %in%
                     c(
                            "Cape Pembroke Lighthouse",
                            "Jersey",
                            "Guernsey",
                            "Douglas",
                            "Nobles",
                            "Gibraltar Botanical Gardens"
                     )
       ) |>
       arrange(short)


postcode_finder = function(lat, lon) {
       url = paste(
              "https://findthatpostcode.uk/points/",
              lat,
              ",",
              lon,
              ".json",
              sep = ""
       )
       json = RJSONIO::fromJSON(url)
       postcode = json$data$relationships$nearest_postcode$data[1]
       return(postcode)
}

load("Data/postcodes.RDa")

postcode = character(nrow(parkrunsuk))

newparkruns = parkrunsuk |> filter(!short %in% parkrun_postcodes$short)

tic()
for (i in 1:nrow(parkrunsuk)) {
       tryCatch(
              {
                     postcode[i] = postcode_finder(
                            lat = parkrunsuk$lat[i],
                            lon = parkrunsuk$lon[i]
                     )
              },
              error = function(e) {
                     postcode[i] = NA_character_
              }
       )
       # Sys.sleep(1)
       cat(paste0(parkrunsuk$short[i], "\n"))
}

toc()
url = "https://en.wikipedia.org/wiki/List_of_postcode_areas_in_the_United_Kingdom"
headers = c(
       `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
       `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
       `Accept-Language` = "en-US,en;q=0.9",
       `Connection` = "keep-alive"
)

response = GET(url, add_headers(.headers = headers), timeout(15))

html <- content(response, as = "text", encoding = "UTF-8") |> read_html()
tables <- html |> html_elements("table")


areaslist = tables[[1]] |>
       html_table() |>
       dplyr::select(c(1, 2)) |>
       rename(area = 1, areaname = 2) |>
       mutate(areaname = stringr::str_replace_all(areaname, "\\[[0-9]\\]", ""))

parkrunsuk_postcodes = cbind.data.frame(parkrunsuk, postcode) |>
       group_by(short) |>
       mutate(
              area = substr(
                     postcode,
                     1,
                     gregexpr("[0-9]", postcode)[[1]][1] - 1
              ),
              sector = substr(postcode, 1, gregexpr(" ", postcode)[[1]][1] - 1),
       ) |>
       ungroup() |>
       merge(areaslist, by = "area") |>
       dplyr::select(
              short,
              long,
              postcode,
              area,
              areaname,
              sector,
              lat,
              lon
       ) |>
       arrange(short)
# write.csv(parkrunsuk_postcodes, "Data/uk_parkruns_postcodes.csv", row.names = F)

###

parkrunscd = cbind.data.frame(short, long, countrycode, coords) |>
       rename("lat" = "2", "lon" = "1") |>
       filter(countrycode == 97) |>
       filter(
              !grepl("junior", long),
              short %in%
                     c(
                            "Cape Pembroke Lighthouse",
                            "Jersey",
                            "Guernsey",
                            "Douglas",
                            "Nobles",
                            "Gibraltar Botanical Gardens"
                     )
       ) |>
       arrange(short) |>
       cbind.data.frame(
              "postcode" = c("FIQQ 1ZZ", "JE3 8LZ", "GY3 5BY", "IM2 4BD", ""),
              "area" = c(NA_character_, "JE", "GY", "IM", NA_character_),
              "areaname" = c(
                     "Falkland Islands",
                     "Jersey",
                     "Guernsey",
                     "Isle of Man",
                     "Gibraltar"
              )
       ) |>
       dplyr::select(short, long, postcode, area, areaname, lat, lon) |>
       mutate(sector = NA_character_)
# write.csv(parkrunscd, "Data/cd_ot_parkruns_postcodes.csv", row.names = F)

closed = googlesheets4::read_sheet(
       "https://docs.google.com/spreadsheets/d/1f3R2XuHb0QevoGbWaUFGchttf_mr53PtVT1Q3cXn6HE/edit?#gid=0",
       col_names = T
)

closedlatlong = closed |>
       filter(Country == "UK", Region != "Overseas Military Bases") |>
       select(`Short Name`, `Long Name`, Latitude, Longitude) |>
       rename(
              "short" = "Short Name",
              "long" = "Long Name",
              "lat" = "Latitude",
              "lon" = "Longitude"
       ) |>
       mutate(lat = as.numeric(lat), lon = as.numeric(lon)) |>
       filter(!is.na(lat)) |>
       group_by(short) |>
       mutate(postcode = postcode_finder(lat = lat, lon = lon)) |>
       ungroup()

bastion = closed |>
       filter(Country == "UK", Region == "Overseas Military Bases") |>
       select(`Short Name`, `Long Name`, Latitude, Longitude) |>
       rename(
              "short" = "Short Name",
              "long" = "Long Name",
              "lat" = "Latitude",
              "lon" = "Longitude"
       ) |>
       mutate(
              postcode = NA_character_,
              areaname = NA_character_,
              area = NA_character_,
              sector = NA_character_
       )


parkruns_closed_postcodes = closedlatlong |>
       # rbind.data.frame(closed|> filter(!is.na(postcode))) |>
       group_by(short) |>
       mutate(
              area = substr(
                     postcode,
                     1,
                     gregexpr("[0-9]", postcode)[[1]][1] - 1
              ),
              sector = substr(postcode, 1, gregexpr(" ", postcode)[[1]][1] - 1),
       ) |>
       ungroup() |>
       # select(-areaname) |>
       merge(areaslist, by = "area") |>
       dplyr::select(
              short,
              long,
              postcode,
              area,
              areaname,
              sector,
              lat,
              lon
       ) |>
       arrange(short)

parkrun_postcodes = rbind.data.frame(
       parkrunsuk_postcodes |> mutate(open = T, overseas = F),
       parkruns_closed_postcodes |> mutate(open = F, overseas = F),
       parkrunscd |> mutate(open = T, overseas = T),
       bastion |> mutate(open = F, overseas = T)
) |>
       mutate(lat = as.numeric(lat), lon = as.numeric(lon))

write.csv(parkrun_postcodes, "Data/parkrun_postcodes.csv", row.names = F)
save(parkrun_postcodes, file = "Data/parkrun_postcodes.RDa")
tictoc::toc()
