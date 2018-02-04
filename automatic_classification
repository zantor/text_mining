####### TEXTE MINING WITH NOVELS FROM THE GUTENBERG PROJETC

'
install.packages("Hmisc")
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("stringr")
install.packages("rjson")
install.packages("geonames")
install.packages("rgdal")
install.packages("magrittr")
install.packages("ggmap")
install.packages("gutenbergr")
install.packages("tokenizers")
'


library(dplyr)
library(stringr)
library(leaflet)
library(tokenizers)
library(Hmisc)
library(gutenbergr)


### Import book and switch raw into sentences 
link <- "http://www.gutenberg.org/cache/epub/17489/pg17489.txt"
link_download = "http://www.gutenberg.org/ebooks/17489?msg=welcome_stranger"
import <- readLines(link)
book <- import
book <- book[book != ""] 
book <- paste(book, collapse = "")
book <- tokenize_sentences(book)
book_author <- "Victor HUGO"
book_title <- "Les misérables Tome I: Fantine"


### Data places from INSEE
places_france <- read.csv(file = "data_insee.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
places_france$nom_commune <- capitalize(places_france$nom_commune)
places_vec <-  as.vector(places_france$nom_commune)

### Get the sentence 
# unnlist book
book_sentences <- book[[1]]

# paste logical element "|" between each places
places_vectorized <- paste0("(", paste(places_vec, collapse="|"), ")")

# count the number of character of the places_vectorized element
num_char_places_vectorized <- nchar(places_vectorized)

# substrings of the places_vectorized element
#substr1 <- substr(x = places_vectorized, start = 1, stop = num_char_places_vectorized/16)
substr1 <- substr(x = places_vectorized, start = 1, stop = num_char_places_vectorized/18)
substr1 <- paste0(substr1, ")")

# return TRUE if a place is mentioned in the sentence for substr1
sentence_bool1 <- grepl(book_sentences, pattern = substr1) # fixed = TRUE doesn't went well
sentence_place1 <- book_sentences[sentence_bool1]

# number of places mentioned
nb_places <- length(sentence_bool1[sentence_bool1])

# get the n° of the sentence 
position_sentence <- which(sentence_bool1 %in% 'TRUE')

# number of sentences with a location inside
nb_sentence <- length(position_sentence)
  
# find the place(ies) mentioned into the sentence
place_mentioned <- data.frame(str_extract(sentence_place1, pattern = places_vectorized), sentence_place1, position_sentence, book_author, book_title) %>%
  na.omit()
place_mentioned$link_download <- paste("<a href=",link_download,">") 
names(place_mentioned) <- c('place', 'sentence_associated', 'position_sentence', 'author', 'title', 'link_download')

# get the frequency 
df_frequency <- data.frame(table(place_mentioned$place))
names(df_frequency) <- c('place', 'freq')
place_mentioned <- inner_join(place_mentioned, df_frequency, by = c("place" = "place"))
place_mentioned$place <- as.character(place_mentioned$place)

# merge with the data cities
place_mentioned_location <- inner_join(place_mentioned, places_france, by = c("place" = "nom_commune"))

# leaflet options
getColor <- function(place_mentioned_location) {
  sapply(place_mentioned_location$freq, function(freq) {
    if(freq <= 1) {
      "green"
    } else if(freq <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(place_mentioned_location)
)

### show the map with the places mentioned and other informations 

# Turn lat/long into numeric
place_mentioned_location$latitude <- as.numeric(place_mentioned_location$latitude)
place_mentioned_location$longitude <- as.numeric(place_mentioned_location$longitude)

# show location map 
map_place <- leaflet(data = place_mentioned_location) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addMarkers(~longitude,
             ~latitude, 
             popup = paste( "</u>", "</br>", place_mentioned_location$author, " ,", place_mentioned_location$title, "<br>",
                            "<u>", "Place:", "</u>", "<br>", place_mentioned_location$place, "<br>", "<br>", "<u>", 
                            "Sentence:", "</u>", "</br>", place_mentioned_location$sentence_associated, "<br>", 
                            " Sentence n° ", place_mentioned_location$position_sentence, "<br>", "<u>",
                            "</u>", "</br>", place_mentioned_location$link_download, " Download the whole book here !", "<br>"),
             clusterOptions = markerClusterOptions()
  )

map_place

### show frequency map
map_occurence <- leaflet(data = place_mentioned_location) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addAwesomeMarkers(~longitude,
                    ~latitude,
                    popup = paste( "</u>", "</br>", place_mentioned_location$author, " ,", place_mentioned_location$title, "<br>",
                                   "<u>", "Place:", "</u>", "<br>", place_mentioned_location$place, "<br>", "<br>", "<u>", 
                                   "Sentence:", "</u>", "</br>", place_mentioned_location$sentence_associated, "<br>", 
                                   " Sentence n° ", place_mentioned_location$position_sentence, "<br>", "<u>",
                                   "</u>", "</br>", place_mentioned_location$link_download, " Download the whole book here !", "<br>"),
                    
                    icon=icons)

map_occurence

### Import book and switch raw into sentences 
link_2 <- "http://www.gutenberg.org/cache/epub/41211/pg41211.txt"
link_download_2 <- "http://www.gutenberg.org/ebooks/41211"
import_2 <- readLines(link_2)
book_2 <- import_2
book_2 <- book_2[book_2 != ""] 
book_2 <- paste(book_2, collapse = "")
book_2 <- tokenize_sentences(book_2)
book_author_2 <- "Honoré de Balzac"
book_title_2 <- "La comédie humaine"

### Get the sentence 
# unnlist book
book_sentences_2 <- book_2[[1]]

# paste logical element "|" between each places
places_vectorized_2 <- paste0("(", paste(places_vec, collapse="|"), ")")

# count the number of character of the places_vectorized element
num_char_places_vectorized_2 <- nchar(places_vectorized)


# get the places mentioned
# Start_stop_2 <- str_locate_all(book_2, pattern = places_vec)
# bibi_2 <- lapply(Start_stop_2, nrow)
# bibi_2 <- sapply(Start_stop_2, nrow)
#  
# 
# cities_mentioned <- places_vec[bibi_2>0] %>%
#   unique()


# return TRUE if a place is mentioned in the sentence for substr1
sentence_bool1_2 <- grepl(book_sentences_2, pattern = substr1)
sentence_place1_2 <- book_sentences_2[sentence_bool1_2] 

# get the n° of the sentence 
position_sentence_2 <- which(sentence_bool1_2 %in% 'TRUE')


# find the place(ies) mentioned into the sentence
place_mentioned_2 <- data.frame(str_extract(sentence_place1_2, pattern = places_vectorized), sentence_place1_2, position_sentence_2, book_author_2, book_title_2) 
place_mentioned_2 <- na.omit(place_mentioned_2)
place_mentioned_2$link_download <- paste("<a href=",link_download_2,">") 
names(place_mentioned_2) <- c('place', 'sentence_associated', 'position_sentence', 'author', 'title', 'link_download')

# get the frequency 
df_frequency_2 <- data.frame(table(place_mentioned_2$place))
names(df_frequency_2) <- c('place', 'freq')
place_mentioned_2 <- inner_join(place_mentioned_2, df_frequency_2, by = c("place" = "place"))
place_mentioned_2$place <- as.character(place_mentioned_2$place)

# merge with the data cities
place_mentioned_location_2 <- inner_join(place_mentioned_2, places_france, by = c("place" = "nom_commune"))
 
# leaflet options 
getColor <- function(place_mentioned_location_2) {
  sapply(place_mentioned_location_2$freq, function(freq) {
    if(freq <= 1) {
      "green"
    } else if(freq <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(place_mentioned_location_2)
)

### Affichage des places citées sur une carte
place_mentioned_location_2$latitude <- as.numeric(place_mentioned_location_2$latitude)
place_mentioned_location_2$longitude <- as.numeric(place_mentioned_location_2$longitude)

# show location map 
map_place_2 <- leaflet(data = place_mentioned_location_2) %>% # Attention, Montreuil est positionné deux fois (à Dreux et à Montreuil)
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addMarkers(~longitude,
             ~latitude, 
             popup = paste( "</u>", "</br>", place_mentioned_location_2$author, " ,", place_mentioned_location_2$title, "<br>",
                            "<u>", "Place:", "</u>", "<br>", place_mentioned_location_2$place, "<br>", "<br>", "<u>", 
                            "Sentence:", "</u>", "</br>", place_mentioned_location_2$sentence_associated, "<br>", 
                            " Sentence n° ", place_mentioned_location_2$position_sentence, "<br>", "<u>",
                            "</u>", "</br>", place_mentioned_location_2$link_download, " Download the whole book here !", "<br>"),
             clusterOptions = markerClusterOptions()
  )

map_place_2





############### MERGE TWO BOOKS

# bind places mentioned from book 1 & 2
df_rbind <- rbind(place_mentioned$place, place_mentioned_2)
place_mentioned_both<- place_mentioned[place_mentioned$place %in% place_mentioned_2$place,]
# merge with data places 
place_mentioned_location_3 <- inner_join(df_rbind, places_france, by = c("place" = "nom_commune"))

# turn lat/long into numeric
place_mentioned_location_3$latitude <- as.numeric(place_mentioned_location_3$latitude)
place_mentioned_location_3$longitude <- as.numeric(place_mentioned_location_3$longitude)

# the location map
map_place_3 <- leaflet(data = place_mentioned_location_3) %>% # Attention, Montreuil est positionné deux fois (à Dreux et à Montreuil)
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addMarkers(~longitude,
             ~latitude, 
             popup = paste( "</u>", "</br>", place_mentioned_location_3$author, " ,", place_mentioned_location_3$title, "<br>",
                            "<u>", "Place:", "</u>", "<br>", place_mentioned_location_3$place, "<br>", "<br>", "<u>", 
                            "Sentence:", "</u>", "</br>", place_mentioned_location_3$sentence_associated, "<br>", 
                            " Sentence n° ", place_mentioned_location_3$position_sentence, "<br>", "<u>",
                            "</u>", "</br>", place_mentioned_location_3$link_download, " Download the whole book here !", "<br>"),
             clusterOptions = markerClusterOptions()
  )

map_place_3


map_occurence # attention aux markers <- symboles proportionnels




