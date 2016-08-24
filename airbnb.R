remove(list = ls())

library("readr")
library("plyr")
library("dplyr")
library("tidyr")
library("e1071")
library("ggplot2")
library("scales")
library("jsonlite")
library("maptools")
library("geojsonio")
library("rgdal")
"GeoJSON" %in% ogrDrivers()$name

library("sp")
#library("GISTools")


setwd("C:/Users/Janek Eskildsen/Dropbox/Økonomi/Kandidat/Social Data Science/Examination project/Output.R/")

## -----------------------------     Create url for downloading     -----------------------------------------

url.listings = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/data/listings.csv.gz"
url.reviews = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/data/reviews.csv.gz"
url.calendar = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/data/calendar.csv.gz"
url.neighbourhood = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/visualisations/neighbourhoods.csv"
url.jsonmap = "http://data.insideairbnb.com/denmark/hovedstaden/copenhagen/2016-06-28/visualisations/neighbourhoods.geojson"

## -----------------------------     Download data     ------------------------------------------------------

df.listings = read_csv(url.listings)  #Downloads listings into data frame
#df.reviews = read_csv(url.reviews)    #Downloads reviews into data frame
#df.calendar = read_csv(url.calendar)  #Downloads availability into data frame
#df.neighbourhood = read_csv(url.neighbourhood)
jsonmap = geojson_read("C:/Users/Janek Eskildsen/Dropbox/Økonomi/Kandidat/Social Data Science/airbnb/neighbourhoods.geojson", what = "sp")




########################################################################################################################
############################          Getting the correct location                 #####################################
########################################################################################################################


# In this section, we download the address of each listing based on longitude and lattitude. 

df.location = df.listings %>% # Create data.frame with lat, lon, zip and id. 
  mutate(
    lat = as.numeric(latitude),
    lon = as.numeric(longitude),
    zip = as.numeric(zipcode)
  ) %>% 
  select(lon, lat, zip, id) %>% 
  arrange(-zip)

# Google only lets you request 2500 times per day, thus we had to limit the number of requests to 2500:
df.location = df.location[1:2500, ] # This is changed to 2501:5000 and so forth on each computer.

# This function downloads the relevant address for each listing. 
library("ggmap")
result = do.call(rbind,
                 lapply(1:nrow(df.location),
                        function(i)revgeocode(as.numeric(df.location[i,1:2]))))

df.location_missing = cbind(df.location,result)

# Extract the zipcode from the address
library(stringr) 
df.location_missing$zipcode <- substring(str_extract(df.location_missing$result,", [0-9]{4} "),2,8)
#df.zipcode = cbind(df.location,df.location_missing$zipcode)

write.csv(file = "zipcodes1.csv", x = df.location_missing) #this was changed to "zipcodes2.csv and so forth on each computer.

# Each .csv file was uploaded to github, so every member of the group could download the datasets.

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

df.zips = df.address %>% 
  mutate(
    zipcode1 = ifelse(zipcode <= 1499, 1499, ifelse(zipcode <= 1799, 1799, ifelse(zipcode <= 1999, 1999, zipcode)))
  ) %>% 
  count(zipcode1)

postal_codes = df.zips$zipcode1

neighborhood.function = function(zipcode){
  ifelse(
    zipcode <= 1499, "Koebenhavn K", ifelse(
      zipcode <= 1799, "Koebenhavn V", ifelse(
        zipcode <= 1999, "Frederiksberg", ifelse(
          zipcode == 2000, "Frederiksberg", ifelse(
            zipcode == 2100, "Koebenhavn Ø", ifelse(
              zipcode == 2200, "Koebenhavn N", ifelse(
                zipcode == 2300, "Kobenhavn S", ifelse(
                  zipcode == 2400, "Koebenhavn NV", ifelse(
                    zipcode == 2450, "Koebenhavn SV", ifelse(
                      zipcode == 2500, "Valby", ifelse(
                        zipcode == 2610, "Roedovre", ifelse(
                          zipcode == 2650, "Hvidovre", ifelse(
                            zipcode == 2700, "Broenshøj", ifelse(
                              zipcode == 2720, "Vanloese", ifelse(
                                zipcode == 2730, "Herlev", ifelse(
                                  zipcode == 2770, "Kastrup", ifelse(
                                    zipcode == 2860, "Soeborg", ifelse(
                                      zipcode == 2870, "Dyssegaard", "Hellerup"
                                    ))))))))))))))))))
}

library("stringr")

df.address = df.address %>% 
  mutate(
    zipcode_new = as.numeric(
      ifelse(
        is.na(
          substring(str_extract(result,"[0-9]{4} "),0,8))==TRUE, 0, substring(str_extract(result,"[0-9]{4} "),0,4)
      )),
    postal_district = neighborhood.function(zipcode)
  ) %>% 
  select(-X1, -lat, -lon, -zipcode)

df.listings_full = full_join(df.listings, df.address, by = "id") #%>% 
 # mutate(
 #  zipcode_new = ifelse(is.na(zipcode_new)==TRUE, zipcode, zipcode_new)
 # )


n_address=nrow(df.address)-sum(df.address$zipcode == 0)
df.address = df.address[1:n_address, ] # We are excluding 19 observations, which do not have zipcodes for

########################################################################################################################
############################          Manipulating the dataset                     #####################################
########################################################################################################################

#Step 1:
df.listings_tidy = df.listings_full[
  complete.cases(
    df.listings[,c("accommodates", "bedrooms", "price", "beds", "host_id")]
    ),
  ] 

df.listings_tidy = df.listings_tidy[!(df.listings_tidy$accommodates==0|
                                      df.listings_tidy$bedrooms==0|
                                      df.listings_tidy$price==0|
                                      df.listings_tidy$beds==0),]
#Step 2:
df.listings_tidy = df.listings_tidy[
  complete.cases(
    df.listings_tidy[, c("host_listings_count")]
  ),
]
df.listings_tidy = df.listings_tidy[!(df.listings_tidy$host_listings_count==0),]

#Selecting listings with description variable
df.listings_tidy = df.listings_tidy %>% 
  mutate(
    description_dummy = ifelse(is.na(neighborhood_overview) == TRUE, 0, 1),
    sample_size = sum(description_dummy),
    description_rate = sum(description_dummy) / nrow(df.listings)
  ) %>%
  arrange(-description_dummy)


df.location = df.listings_tidy[1:df.listings_tidy$sample_size, ] %>%
  select(id, neighborhood_overview, street, 
         neighbourhood, neighbourhood_cleansed, 
         city, zipcode, market, smart_location,
         longitude, latitude, is_location_exact, result, zipcode_new, postal_district) 

df.location_grouped = df.location %>% 
  mutate(
    neighbourhood = ifelse(is.na(neighbourhood)==TRUE, "NA", neighbourhood)
  )

df.location_grouped = df.location_grouped %>% 
  group_by(neighbourhood) %>% 
  mutate(
    n = n(),
    freq = n / nrow(df.location_grouped)
  ) %>% 
  arrange(n)


df.locations = df.location %>% 
  count(neighbourhood) %>% 
  mutate(
    neighbourhood.total = sum(n),
    frequency = round(n / neighbourhood.total * 100, digits = 2)
  ) %>% 
  arrange(frequency)

#library("sjPlot")
#sjt.df(df.locations[,c(1, 2, 4)], altr.row.col = TRUE, 
#       describe = FALSE, big.mark = TRUE, file = "Tables/Airbnb neighbourhoods.excel",
#       show.rownames = FALSE)
#
#---------------------------------- Making histogram ---------------------------------------------------------------------

p = ggplot(data = df.location_grouped, aes(reorder(neighbourhood, freq), fill = "orange")) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) 

p = p + labs(x = NULL, 
             y = "Number of listings")

p = p + theme_minimal(base_size = 14) + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )
p = p + scale_y_continuous(breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25), labels = percent_format()) 

n.neighbourhod = df.location_grouped %>% 
  ungroup() %>% 
  select(neighbourhood) %>% 
  count(neighbourhood) %>% 
  mutate(
    sum = sum(n),
    freq = n/sum,
    number = sprintf("n==%.0f", n)
  ) 

p = p + coord_flip() +
  geom_text(
    data = n.neighbourhod, aes(x = neighbourhood, y = freq + 0.02 , label = number),
    size=4, parse=T, color = "black")
p

ggsave(plot = p, file = "Figures/Airbnb neighbourhoods.jpeg",
       width = 8, height = 6)

# --------------------------------------------- Triangulating locations ---------------------------------------

df.loc.compare = df.location %>% 
  mutate(
    exact_location = ifelse(is_location_exact == "t", 1, 0),
    exact_rate = sum(exact_location) / nrow(df.location),
    zipcode = ifelse(is.na(zipcode)==TRUE, 0, ifelse(zipcode<=1499, 1499, 
                      ifelse(zipcode<=1799, 1799, 
                             ifelse(zipcode<=2000, 2000, zipcode)))),
    zipcode_new = ifelse(is.na(zipcode_new)==TRUE, 0, ifelse(zipcode_new<=1499, 1499, 
                        ifelse(zipcode_new<=1799, 1799, 
                               ifelse(zipcode_new<=2000, 2000, zipcode_new)))),
    exact_zip = ifelse(zipcode == zipcode_new, 1, 0)
  ) %>% 
  select(id, neighborhood_overview, latitude, longitude, street, result, city, neighbourhood, neighbourhood_cleansed,
         postal_district, zipcode, zipcode_new, is_location_exact, exact_location, exact_rate, exact_zip) %>% 
  arrange(exact_location)

df.final = df.loc.compare[!(df.loc.compare$exact_location==0&df.loc.compare$exact_zip==0),]


########################################################################################################################
############################          Tidy data for Map             ####################################################
########################################################################################################################

if(!require("devtools")) install.packages("devtools")
devtools::install_github("sebastianbarfort/mapDK", force = TRUE) 
if(!require("ggmap")) install.packages("ggmap")

library("mapDK")


df.address1 = df.final %>% 
  group_by(neighbourhood_cleansed) %>% 
  mutate(
    n = n(),
    freq = n / nrow(df.location) * 100,
    lon = longitude,
    lat = latitude
  ) %>%
  select(lon, lat, zipcode, freq, id, neighbourhood_cleansed) %>% 
  arrange(neighbourhood_cleansed)

df.address2 = df.final %>% 
  group_by(postal_district) %>% 
  mutate(
    n = n(),
    freq = n / nrow(df.location) * 100,
    lon = longitude,
    lat = latitude
  ) %>%
  ungroup() %>% 
  select(lon, lat, zipcode, freq, id) %>% 
  arrange(freq)

map1 = pointDK(df.address1, values = 'freq', detail = "polling", sub.plot = "koebenhavn",
        aesthetic = "colour")

ggsave(plot = map1, file = "Figures/map1.png",
       width = 8, height = 6)

map2 = pointDK(df.address2, values = 'freq', detail = "polling", sub.plot = "koebenhavn",
               aesthetic = "colour")
ggsave(plot = map2, file = "Figures/map2.png",
       width = 8, height = 6)


### ------------------------------------ Making Choropeth map -------------------------------------------------------

df.group1 = df.final %>% 
  count(neighbourhood_cleansed) %>%
  mutate(
    sum1 = sum(n)
  ) %>% 
  arrange(n)

df.group2 = df.final %>% 
  count(postal_district) %>% 
  mutate(
    sum2 = sum(n)
  ) %>% 
  arrange(n)


library("ggmap")

#cph.map = ggmap(get_map(
#  location = c(12.57, 55.68),
#  maptype = "roadmap",
#  source = "google",
#  crop = TRUE,
#  zoom = 13
#))
cph.map = ggmap(get_map(
  location = "Copenhagen",
  zoom = 12
))

cph.map

dir.create("tempdir1")
writeOGR(obj=jsonmap, dsn="tempdir1", layer="jsonmap", driver="ESRI Shapefile")

Shape = readOGR("tempdir1", "jsonmap")
Shape = spTransform(Shape, CRS("+proj=longlat +datum=WGS84"))

plot(Shape)
names(Shape)
View(Shape)

Shape.f = Shape %>% fortify(region = "nghbrhd")
Neighbourhoods = merge(Shape.f, Shape@data, by.x = "id", by.y="nghbrhd")
head(Neighbourhoods)

hoods = Neighbourhoods %>% select(id) %>% distinct()

values = df.group1[, 1:2] %>% 
  mutate(
    id = neighbourhood_cleansed
  ) %>% 
  select(id, n)

NH = merge(Neighbourhoods, values, by.x="id")

NH %>% group_by(id) %>% do(head(., 1)) %>% head(10)

ggplot(NH, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = n))

choropeth = cph.map + geom_polygon(aes(fill = n, x = long, y = lat, group = group), 
             data = NH,
             alpha = 0.8, 
             color = "black",
             size = 0.2) 
ggsave(plot = choropeth, file = "Figures/choropeth.png",
       width = 8, height = 6)

overview = cph.map + geom_polygon(data = NH, aes(x = long, y = lat, group = group), fill = "red", color = "black", size = 1, alpha = 0.5) +
  geom_point(data = df.address1, aes(x = lon, y = lat), color = "blue", size = 1.2) 
overview


## -----------------------------     Tidy data     ----------------------------------------------------------

View(df.listings)

df.small = df.listings %>%
  select(price, bedrooms, bathrooms, number_of_reviews, review_scores_rating, review_scores_location)

df.small = df.small %>%
  separate(price, c("Currency", "Price"), 1) %>%
  mutate(
    Price = as.numeric(gsub(",", "", Price))
  ) %>%
  select(Price, Currency, bedrooms, bathrooms, number_of_reviews, review_scores_rating, review_scores_location)

summary(df.small)

df.small = df.small[complete.cases(df.small),]

lin.model = lm(Price ~ bedrooms + bathrooms + number_of_reviews + review_scores_rating + review_scores_location,
               df.small)


## Expanding Amenites ----------------------------------------------------------------------------------------------

df.1 = df[1:5, 1:length(df)]
df.1 = df.1  %>%
  mutate(
    TV = ifelse(str_detect(df.1$amenities, "TV") == TRUE, 1, 0),
    Cable = ifelse(str_detect(df.1$amenities, "Cable") == TRUE, 1, 0),
    Internet = ifelse(str_detect(df.1$amenities, "Internet") == TRUE, 1, 0),
    Wireless.Internet = ifelse(str_detect(df.1$amenities, "Wireless Internet") == TRUE, 1, 0),
    Kitchen = ifelse(str_detect(df.1$amenities, "Kitchen") == TRUE, 1, 0),
    Smoke.Detector = ifelse(str_detect(df.1$amenities, "Smoke Detector") == TRUE, 1, 0),
    Heating = ifelse(str_detect(df.1$amenities, "Heating") == TRUE, 1, 0),
    Elevator = ifelse(str_detect(df.1$amenities, "Elevator in Building") == TRUE, 1, 0),
    Washer = ifelse(str_detect(df.1$amenities, "Washer") == TRUE, 1, 0),
    Fireplace = ifelse(str_detect(df.1$amenities, "Indoor Fireplace") == TRUE, 1, 0)
  )


## -----------------------------     Price plot     ----------------------------------------------------------

# Tidy data for plot
df.price = df.listings %>%
  separate(price, c("currency", "Price"), 1) %>%
  mutate(
    price = as.numeric(gsub(",", "", Price))
  ) %>%
  select(price, room_type) 

# Histogram plot of price per room type
p_room = ggplot(data = df.price, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "orange", color = "orange") +
  coord_cartesian(xlim = c(0, 2100)) +
  scale_x_continuous(
    expand = c(0,0), 
    breaks = c(0, min(df.price$price), 250, 500, 750, 1000, 1250, 1500, 1750, 2000)
  ) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~ room_type, ncol = 1, 
             scales = "free_y")

# Addding median to the plot
df.median = df.price %>%  #Here we are just calculating the median for each group
  group_by(room_type) %>%
  summarise_each(funs(median.price = median(price)))

p_room = p_room + geom_vline(
  data = df.median,
  mapping = aes(xintercept=median.price, linetype='Median'),
  color = "black", size = 1, show.legend = TRUE
)

# Applying minimal theme and addind labels
p_room = p_room + labs(x = "Price per night (in DKK)", 
                       y = "Number of listings")
p_room = p_room + 
  theme_minimal(base_size = 14) + 
  theme(
    legend.title = element_blank(), legend.position = "right",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

# Including label with count
library("dplyr")
n.room_type = df.price %>% 
  group_by(room_type) %>% 
  summarise(n = n()) %>%
  select(room_type, n)
n.room_type = n.room_type %>% 
  mutate(
    number = sprintf("n==%.0f", n)
  ) 
head(n.room_type)

p_room = p_room + 
  geom_text(
    data = n.room_type, aes(x = 1500, y = n / 14, label = number),
    size=4, parse=T, color = "black"
  )
p_room

# Saving the picture
getwd()
setwd("C:/Users/Janek Eskildsen/Dropbox/Økonomi/Kandidat/Social Data Science/Examination project/Output.R")
ggsave(plot = p_room, file = "Figures/price distribution.tex",
       width = 8, height = 6)