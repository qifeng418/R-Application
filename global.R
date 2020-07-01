
#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("HH")
#install.packages("shinydashboard")
#install.packages("topicmodels")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("lubridate")
#install.packages("memoise")
#install.packages("igraph")
#install.packages("ggraph")
#install.packages("reshape2")
#install.packages("scales")
#install.package("fmsb")

# packages
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(HH)
library(maps)
library(ggplot2)
library(lubridate)
library(memoise)
library(igraph)
library(ggraph)
library(reshape2)
library(scales)
library(fmsb)
library(plotly)


#---------- import the data into RStudio
AirlineData <- read.csv(file = "data/Airline_with_country.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)
AirlineData <- as.tibble(AirlineData)
AirlineData$date <- year(as.Date(AirlineData$date,format("%d/%m/%Y")))
AirlineData$content <- as.character(AirlineData$content)
AirlineData$star <- as.character(AirlineData$star)
AirlineData$airline_country <- as.character(AirlineData$airline_country)
#tidy_review <- read.table(file = "tidy_review")
#tidy_review <- as.tibble(tidy_review)
#tidy_review_iter <- tidy_review


#-------------------------------- Data Preparation (likert) -------------------------------


#--------------------------------  Data Preparation (sentiment)  -------------------------------------

#--------------------custom stop words

custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add http, win, and t.co as custom stop words
  "the", "CUSTOM",
  "was", "CUSTOM",
  "flight", "CUSTOM",
  "trip", "CUSTOM",
  "time", "CUSTOM",
  "seat", "CUSTOM",
  "seats", "CUSTOM",
  "service", "CUSTOM",
  "flights", "CUSTOM",
  "staff", "CUSTOM",
  "class", "CUSTOM",
  "2", "CUSTOM",
  "airline", "CUSTOM",
  "airlines", "CUSTOM",
  "cabin", "CUSTOM",
  "aircraft", "CUSTOM",
  "business", "CUSTOM",
  "plane", "CUSTOM",
  "fly", "CUSTOM",
  "economy", "CUSTOM",
  "told", "CUSTOM",
  "minutes", "CUSTOM",
  "hours", "CUSTOM",
  "hour", "CUSTOM",
  "flew", "CUSTOM",
  "experience", "CUSTOM",
  "check in", "CUSTOM",
  "check", "CUSTOM",
  "boarding", "CUSTOM",
  "air", "CUSTOM",
  "3", "CUSTOM",
  "4", "CUSTOM",
  "1", "CUSTOM",
  "5", "CUSTOM",
  "ife", "CUSTOM",
  "bit", "CUSTOM",
  "served", "CUSTOM",
  "return", "CUSTOM",
  "passengers", "CUSTOM",
  "people", "CUSTOM",
  "offered", "CUSTOM",
  "luggage", "CUSTOM",
  "lounge", "CUSTOM",
  "lhr", "CUSTOM",
  "left", "CUSTOM",
  "gate", "CUSTOM",
  "flying", "CUSTOM",
  "extra", "CUSTOM",
  "due", "CUSTOM",
  "board", "CUSTOM",
  "arrived", "CUSTOM",
  "airport", "CUSTOM",
  "booked", "CUSTOM",
  "day", "CUSTOM",
  "pay", "CUSTOM",
  "baggage", "CUSTOM",
  "times", "CUSTOM",
  "inflight", "CUSTOM",
  "paid", "CUSTOM",
  "ticket", "CUSTOM",
  "this airline", "CUSTOM",
  "bag", "CUSTOM",
  "finally", "CUSTOM",
  "the cabin", "CUSTOM",
  "with the", "CUSTOM",
  "we were", "CUSTOM",
  "we had", "CUSTOM",
  "was a", "CUSTOM",
  "to the", "CUSTOM",
  "to be", "CUSTOM",
  "there was", "CUSTOM",
  "the plane", "CUSTOM",
  "the food", "CUSTOM",
  "the flight", "CUSTOM",
  "flight to", "CUSTOM",
  "flight from", "CUSTOM",
  "on the", "CUSTOM",
  "on a", "CUSTOM",
  "of the", "CUSTOM",
  "it was", "CUSTOM",
  "in the", "CUSTOM",
  "i was", "CUSTOM",
  "i have", "CUSTOM",
  "i had", "CUSTOM",
  "had to", "CUSTOM",
  "for the", "CUSTOM",
  "for a", "CUSTOM",
  "food was", "CUSTOM",
  "flight was", "CUSTOM",
  "at the", "CUSTOM",
  "and the", "CUSTOM",
  "and i", "CUSTOM",
  "with a", "CUSTOM",
  "was very", "CUSTOM",
  "was the", "CUSTOM",
  "was not", "CUSTOM",
  "to get", "CUSTOM",
  "the same", "CUSTOM",
  "that the", "CUSTOM",
  "is a", "CUSTOM",
  "i would", "CUSTOM",
  "had a", "CUSTOM",
  "from the", "CUSTOM",
  "due to", "CUSTOM",
  "did not", "CUSTOM",
  "but the", "CUSTOM",
  "and a", "CUSTOM",
  "which was", "CUSTOM",
  "out of", "CUSTOM",
  "it is", "CUSTOM",
  "in a", "CUSTOM",
  "if you", "CUSTOM",
  "i will", "CUSTOM",
  "have to", "CUSTOM",
  "delayed","CUSTOM"
)

# Bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)

stop_words_iter <- tribble()


tidy_review <- AirlineData %>% 
  #  # Tokenize the twitter data
  #  ##      unnest_tokens(word, content)  %>% 
  unnest_tokens(word, content, token = "ngrams", n = 2, n_min = 2)  %>% 
  distinct() %>%
  #     unnest_tokens(word, content, token = "ngrams", n = 2)  %>% 
  anti_join(stop_words2)

tidy_review_single <- AirlineData %>%
  unnest_tokens(word, content)  %>%
  anti_join(stop_words2)


