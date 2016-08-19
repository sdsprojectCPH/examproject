library("readr")
library("dplyr")
library("stringr")
library("NLP")
library("tm")

df.small = read_csv("C:/Users/Dose/Google Drev/Polit/SDS/airbnb/listings1.csv")
df.det = read_csv("C:/Users/Dose/Google Drev/Polit/SDS/airbnb/listings.csv")
df.cal = read_csv("C:/Users/Dose/Google Drev/Polit/SDS/airbnb/calendar.csv")
df.rev = read_csv("C:/Users/Dose/Google Drev/Polit/SDS/airbnb/reviews.csv")


overview.data <-   cbind(df.det$id, df.det$neighborhood_overview, df.det$neighbourhood_cleansed)
df.overview <- data.frame(overview.data)
df.overview <- data.frame("id" = df.overview$X1, "overview" = df.overview$X2, "neighbourhood" = df.overview$X3)
df.overview <- df.overview %>%
  mutate("overview" = as.character(overview))
df.overview <- na.omit(df.overview)


df.overview.count <- df.overview %>%
  group_by(neighbourhood) %>%
  summarise(n=n())

#overview data to tidy data
library("tidytext")
overview_words = df.overview %>%
  select(id, overview, neighbourhood) %>%
  unnest_tokens(word, overview)

#Cleaning (removing english stop words)
overview_words <- overview_words %>%
  filter(!word %in% stop_words$word) %>%
  filter(str_detect(word, "[a-z']+$"))


#Removing danish stop words
overview_words <- overview_words %>%
  mutate( word = tm_map(overview_words$word, removeWords, stopwords("danish")) )

#Working with "tm"
Corpus.Overview <- c(Corpus(VectorSource(overview_words$id)),
                     Corpus(VectorSource(overview_words$neighbourhood)),
                     Corpus(VectorSource(overview_words$word)))
Corpus.Overview <- tm_map(Corpus.Overview, removeWords, stopwords(kind = "danish"))
inspect(Corpus.Overview)

OverviewDTM <- DocumentTermMatrix(Corpus.Overview)
Overview <- as.data.frame(as.matrix(Corpus.Overview))