
if(!require("readr")) install.packages("readr")
if(!require("plyr")) install.packages("plyr")
if(!require("dply")) install.packages("dplyr")
if(!require("tidyr")) install.packages("tidyr")
if(!require("ggmap")) install.packages("ggmap")

library("readr")
library("plyr")
library("dplyr")
library("tidyr")
library("ggmap")

getwd()
setwd("Desktop")

url.listings = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/data/listings.csv.gz"
df.listings = read_csv(url.listings)  #Downloads listings into data frame

df.location = df.listings %>%
  mutate(
    lat = as.numeric(latitude),
    lon = as.numeric(longitude),
    zip = as.numeric(zipcode)
  ) %>% 
  select(lon, lat, zip, id) %>% 
  arrange(-zip)

df.location = df.location[1:2500, ]

result = do.call(rbind,
                 lapply(1:nrow(df.location),
                        function(i)revgeocode(as.numeric(df.location[i,1:2]))))

df.location_missing = cbind(df.location,result)

library(stringr)
df.location_missing$zipcode <- substring(str_extract(df.location_missing$result,", [0-9]{4} "),2,8)
df.zipcode = cbind(df.location,df.location_missing$zipcode)

write.csv(file = "zipcodes1.csv", x = df.zipcode)
