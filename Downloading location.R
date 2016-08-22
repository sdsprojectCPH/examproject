
if(!require("readr")) install.packages("readr")
if(!require("plyr")) install.packages("plyr")
if(!require("dply")) install.packages("dplyr")
if(!require("tidyr")) install.packages("tidyr")
if(!require("ggmap")) install.packages("ggmap")
if(!require("stringr")) install.packages("stringr")

library("readr")
library("plyr")
library("dplyr")
library("tidyr")
library("e1071")
library("ggplot2")
library("scales")

setwd("C:/Users/Janek Eskildsen/Dropbox/Økonomi/Kandidat/Social Data Science/Examination project/Output.R/")

## -----------------------------     Create url for downloading     -----------------------------------------

url.listings = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/data/listings.csv.gz"
url.reviews = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/data/reviews.csv.gz"
url.calendar = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/data/calendar.csv.gz"

## -----------------------------     Download data     ------------------------------------------------------

df.listings = read_csv(url.listings)  #Downloads listings into data frame
df.reviews = read_csv(url.reviews)    #Downloads reviews into data frame
df.calendar = read_csv(url.calendar)  #Downloads availability into data frame

########################################################################################################################
############################          Getting the correct location                 #####################################
########################################################################################################################
#
# In this section, we download the address of each listing based on longitude and lattitude. 
#
#df.location = df.listings %>% # Create data.frame with lat, lon, zip and id. 
#  mutate(
#    lat = as.numeric(latitude),
#    lon = as.numeric(longitude),
#    zip = as.numeric(zipcode)
#  ) %>% 
#  select(lon, lat, zip, id) %>% 
#  arrange(-zip)
#
# Google only lets you request 2500 times per day, thus we had to limit the number of requests to 2500:
#df.location = df.location[1:2500, ] # This is changed to 2501:5000 and so forth on each computer.
#
# This function downloads the relevant address for each listing. 
#library("ggmap")
#result = do.call(rbind,
#                 lapply(1:nrow(df.location),
#                        function(i)revgeocode(as.numeric(df.location[i,1:2]))))
#
#df.location_missing = cbind(df.location,result)
#
# Extract the zipcode from the address
library("stringr") 
#df.location_missing$zipcode <- substring(str_extract(df.location_missing$result,", [0-9]{4} "),2,8)
#df.zipcode = cbind(df.location,df.location_missing$zipcode)
#
#write.csv(file = "zipcodes1.csv", x = df.location_missing) #this was changed to "zipcodes2.csv and so forth on each computer.
#
# Each .csv file was uploaded to github, so every member of the group could download the datasets.
#
########################################################################################################################
############################          Downloading dataset from github              ####################################
########################################################################################################################

gh.link = "https://raw.githubusercontent.com/"
user.repo = "sdsprojectCPH/examproject/"
branch = "gh-pages/"
link.url = paste("zipcodes","1", ".csv", sep = "")

scrape_git = function(link.url){
  data.link = paste0(gh.link, user.repo, branch, link.url)
  df.zipcode = data.frame(
    read_csv(data.link), 
    stringsAsFactors = F
  )
  return(df.zipcode)
}

zipcodes <- list() 
seq = seq(1, 7, by = 1) 

for (i in 1:length(seq)){
  message(i, '/', length(seq))  # informative message about progress of loop
  link.url <- paste("zipcodes", seq[i], ".csv", sep="") # prepare URL
  zipcodes[[i]] <- scrape_git(link.url) # use scrape function
  Sys.sleep(2) # wait a couple of seconds between URL calls
}
df.address = do.call(rbind, zipcodes) #append data by column.
df.address = df.address %>% 
  mutate(
    zipcode_clean = substring(str_extract(result,"[0-9]{4} "),0,8)
  ) %>% 
  select(id, result, zipcode_clean) 

df.listing = full_join(df.listings, df.address, by = "id")
