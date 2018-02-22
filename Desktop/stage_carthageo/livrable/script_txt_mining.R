####### TEXTE MINING WITH NOVELS FROM THE GUTENBERG PROJECT

### Uncomment to install packages
# install.packages("Hmisc")
# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("rjson")
# install.packages("geonames")
# install.packages("rgdal")
# install.packages("magrittr")
# install.packages("ggmap")
# install.packages("gutenbergr")
# install.packages("tokenizers")
#install.packages("leaflet.extras")
#install.packages("igraph")
#install.packages("maptools")
#install.packages("shinydashboard")


library(dplyr)
library(stringr)
library(leaflet)
library(tokenizers)
library(Hmisc)
library(gutenbergr)
library(leaflet.extras)
library(igraph)
library(maptools)
library(shiny)
library(shinydashboard)
library(shiny)

### Import book and switch raw into sentences 
link <- "http://www.gutenberg.org/cache/epub/17489/pg17489.txt"
link_download = "http://www.gutenberg.org/ebooks/17489?msg=welcome_stranger"
import <- readLines(link)
book <- import
book <- book[book != ""] 
book <- paste(book, collapse = "")
#book <- tokenize_sentences(book)
book_author <- "Victor HUGO"
book_title <- "Les misérables Tome I: Fantine"
# remove head of the text

### Data places from INSEE
places_france <- read.csv(file = "/Users/Bastien/Desktop/stage_carthageo/data_insee.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
places_france <- places_france[,c("nom_commune", "latitude", "longitude")]
places_france$nom_commune <- capitalize(places_france$nom_commune)
#places_france$nom_commune <- paste0(" ", places_france$nom_commune) # good solution to avoid the association between a word and a place but it forgets also many places
places_vec <-  as.vector(places_france$nom_commune)


### Get the sentence 
# unlist book
book_sentences <- book[[1]]

# keep only place_mentioned
## regular_expression <- paste0(paste0( " " , places_vec, "[:punct:]", "|", " ", places_vec, "[:blank:]"))
## regular_expression <- places_vec
# #Start_stop <- str_locate_all(book, regular_expression)
# #Start_stop <- str_locate_all(book, places_vec)
# Start_stop <- str_extract(book, regular_expression)
# Start_stop <- na.omit(Start_stop)
regular_expression <- paste0("[:blank:]" , places_vec, "[:punct:]", "[:blank:]", "|", "[:blank:]", places_vec, "[:blank:][0-9a-zA-Z]")

ptm <- proc.time()
cities <- str_extract_all(book, regular_expression)
proc.time() - ptm

cities_mentioned <- cities[lapply(cities,length)>0]

cities_mentioned <- lapply(cities_mentioned, `[[`, 1) %>%
  unique(cities_mentioned)

cities_mentioned_exac <- substr(cities_mentioned, 2, nchar(cities_mentioned)-2)

## appearance <- lapply(Start_stop, nrow)
#appearance <- sapply(Start_stop, nrow)

#cities_mentioned <- places_vec[appearance>0] %>%
#  unique()
cities_mentioned_exac <- unique(cities_mentioned_exac)
# number of different places mentioned
nb_places <- length(cities_mentioned_exac)

#number of mentions 
nb_mentions <- length(cities_mentioned)

# paste logical operator "|" between each places in order to use easily the "grepl" function
places_vectorized <- paste0("(", paste(cities_mentioned_exac, collapse="|"), ")")

book <- tokenize_sentences(book)
book_sentences <- book[[1]]
# return TRUE if a place is mentioned in the sentence for substr1
sentence_bool <- grepl(book_sentences, pattern = places_vectorized) # fixed = TRUE doesn't went well
sentence_place <- book_sentences[sentence_bool]

# get the n° of the sentence 
position_sentence <- grep(book_sentences, pattern = places_vectorized)

# number of sentences with a location inside
nb_sentence_location <- length(position_sentence)

# % of sentence with a location inside
nb_sentence_total <- length(book_sentences)
percent_sentence_location <- (nb_sentence_location/nb_sentence_total) * 100

# find the place(ies) mentioned into the sentence
place_mentioned <- data.frame(str_extract(sentence_place, pattern = places_vectorized), sentence_place, position_sentence, book_author, book_title) %>%
  na.omit() 
place_mentioned$link_download <- paste("<a href=",link_download,">") 
names(place_mentioned) <- c('place', 'sentence_associated', 'position_sentence', 'author', 'title', 'link_download')

# get the frequency 
df_frequency <- data.frame(table(place_mentioned$place))
names(df_frequency) <- c('place', 'freq')
place_mentioned <- inner_join(place_mentioned, df_frequency, by = c("place" = "place"))
place_mentioned$place <- as.character(place_mentioned$place)


# merge with the data cities
place_mentioned_location <- inner_join(place_mentioned, places_france, by = c("place" = "nom_commune")) %>%
  unique()

# leaflet options

icons_book1 <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)




#################################
### Position map with clusters option and popup informations 
### Occurrence map with proportional circles
### Association map between places mentioned in the same sentence

# Turn lat/long into numeric
place_mentioned_location$latitude <- as.numeric(place_mentioned_location$latitude)
place_mentioned_location$longitude <- as.numeric(place_mentioned_location$longitude)




##################################################################################################################
###############################        BOOK N°2      ###########################################################################
##################################################################################################################


link_2 <-  "http://www.gutenberg.org/cache/epub/41211/pg41211.txt"
link_download_2 = "http://www.gutenberg.org/ebooks/41211"
import_2 <- readLines(link_2)
book_2 <- import_2
book_2 <- book_2[book_2 != ""] 
book_2 <- paste(book_2, collapse = "")
#book <- tokenize_sentences(book)
book_author_2 <- "Honoré de Balzac"
book_title_2 <- "La Comédie Humaine"
# remove head of the text

### Data places from INSEE
places_france_2 <- read.csv(file = "/Users/Bastien/Desktop/stage_carthageo/data_insee.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
places_france <- places_france[,c("nom_commune", "latitude", "longitude")]
places_france$nom_commune <- capitalize(places_france$nom_commune)
#places_france$nom_commune <- paste0(" ", places_france$nom_commune) # good solution to avoid the association between a word and a place but it forgets also many places
places_vec_2 <-  as.vector(places_france$nom_commune)


### Get the sentence 
# unlist book
book_sentences_2 <- book_2[[1]]

# keep only place_mentioned
## regular_expression <- paste0(paste0( " " , places_vec, "[:punct:]", "|", " ", places_vec, "[:blank:]"))
## regular_expression <- places_vec
# #Start_stop <- str_locate_all(book, regular_expression)
# #Start_stop <- str_locate_all(book, places_vec)
# Start_stop <- str_extract(book, regular_expression)
# Start_stop <- na.omit(Start_stop)
regular_expression_2 <- paste0("[:blank:]" , places_vec, "[:punct:]", "[:blank:]", "|", "[:blank:]", places_vec, "[:blank:][0-9a-zA-Z]")

ptm <- proc.time()
cities_2 <- str_extract_all(book_2, regular_expression_2)
proc.time() - ptm

cities_mentioned_2 <- cities_2[lapply(cities_2,length)>0]

cities_mentioned_2 <- lapply(cities_mentioned_2, `[[`, 1) %>%
  unique(cities_mentioned_2)

cities_mentioned_exac_2 <- substr(cities_mentioned_2, 2, nchar(cities_mentioned_2)-2)

## appearance <- lapply(Start_stop, nrow)
#appearance <- sapply(Start_stop, nrow)

#cities_mentioned <- places_vec[appearance>0] %>%
#  unique()
cities_mentioned_exac_2 <- unique(cities_mentioned_exac_2)
# number of different places mentioned
nb_places_2 <- length(cities_mentioned_exac_2)

#number of mentions 
nb_mentions_2 <- length(cities_mentioned_2)

# paste logical operator "|" between each places in order to use easily the "grepl" function
places_vectorized_2 <- paste0("(", paste(cities_mentioned_exac_2, collapse="|"), ")")

book_2 <- tokenize_sentences(book_2)
book_sentences_2 <- book_2[[1]]
# return TRUE if a place is mentioned in the sentence for substr1
sentence_bool_2 <- grepl(book_sentences_2, pattern = places_vectorized_2) # fixed = TRUE doesn't went well
sentence_place_2 <- book_sentences_2[sentence_bool_2]

# get the n° of the sentence 
position_sentence_2 <- grep(book_sentences_2, pattern = places_vectorized_2)

# number of sentences with a location inside
nb_sentence_location_2 <- length(position_sentence_2)

# % of sentence with a location inside
nb_sentence_total_2 <- length(book_sentences_2)
percent_sentence_location_2 <- (nb_sentence_location_2/nb_sentence_total_2) * 100

# find the place(ies) mentioned into the sentence
place_mentioned_2 <- data.frame(str_extract(sentence_place_2, pattern = places_vectorized_2), sentence_place_2, position_sentence_2, book_author_2, book_title_2) %>%
  na.omit() 
place_mentioned_2$link_download <- paste("<a href=",link_download_2,">") 
names(place_mentioned_2) <- c('place', 'sentence_associated', 'position_sentence', 'author', 'title', 'link_download')

# get the frequency 
df_frequency_2 <- data.frame(table(place_mentioned_2$place))
names(df_frequency_2) <- c('place', 'freq')
place_mentioned_2 <- inner_join(place_mentioned_2, df_frequency_2, by = c("place" = "place"))
place_mentioned_2$place <- as.character(place_mentioned_2$place)


# merge with the data cities
place_mentioned_location_2 <- inner_join(place_mentioned_2, places_france, by = c("place" = "nom_commune")) 
place_mentioned_location_2 <- unique(place_mentioned_location_2)




# leaflet options


icons_book2 <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "purple"
)

place_mentioned_location_2$latitude <- as.numeric(place_mentioned_location_2$latitude)
place_mentioned_location_2$longitude <- as.numeric(place_mentioned_location_2$longitude)

write.table()

map_place <- leaflet() %>%
  # Base groups
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Black") %>%
  addTiles(group = "OSM") %>%
  # Overlay groups
  addAwesomeMarkers(data = place_mentioned_location,
                    ~longitude,
                    ~latitude, 
                    popup = ~place, group = "Place name - Book 1",
                    icon = icons_book1) %>%
  addAwesomeMarkers(data = place_mentioned_location_2,
                    ~longitude,
                    ~latitude, 
                    popup = ~place, group = "Place name - Book 2",
                    icon = icons_book2) %>%
  addCircles(data = place_mentioned_location,
             lng = ~ longitude, lat = ~ latitude, weight = 6,
             radius = ~freq * 1000, 
             opacity = 1,
             color = "orange",
             fill = TRUE,
             fillOpacity = 0.1,
             popup = paste( "</u>", "</br>", place_mentioned_location$author, " ,", place_mentioned_location$title, "<br>", "<br>",
                            "<u>", "Place:", "</u>", "<br>", place_mentioned_location$place, "<br>", "<br>", "<u>", 
                            "Frequency:", "</u>", "</br>", place_mentioned_location$freq, " time(s)" , "<br>", 
                            "</u>", "</br>", place_mentioned_location$link_download, " Download the whole book here !", "<br>") , group = "Frequency - Book 1") %>%
  addCircles(data = place_mentioned_location_2,
             lng = ~ longitude, lat = ~ latitude, weight = 6,
             radius = ~freq * 1000, 
             opacity = 1,
             color = "purple",
             fill = TRUE,
             fillOpacity = 0.1,
             popup = paste( "</br>", place_mentioned_location_2$author, " ,", place_mentioned_location_2$title, "<br>", "<br>",
                            "<u>", "Place:", "</u>", "<br>", place_mentioned_location_2$place, "<br>", "<br>", "<u>", 
                            "Frequency:", "</u>", "</br>", place_mentioned_location_2$freq, " time(s)" , "<br>", 
                            "</u>", "</br>", place_mentioned_location_2$link_download, " Download the whole book here !", "<br>") , group = "Frequency - Book 2") %>%
  addAwesomeMarkers(data = place_mentioned_location,
                    ~longitude,
                    ~latitude, 
                    popup = paste( "</br>", place_mentioned_location$author, " ,", place_mentioned_location$title, "<br>",
                                   "<u>", "Place:", "</u>", "<br>", place_mentioned_location$place, "<br>", "<br>", "<u>", 
                                   "Sentence:", "</u>", "</br>", place_mentioned_location$sentence_associated, "<br>", "</br>",
                                   " Sentence n° ", place_mentioned_location$position_sentence, "<br>", 
                                   "Frequency:", "</u>", "</br>", place_mentioned_location$freq, " time(s)" , "<br>", 
                                   "<u>","</u>", "</br>", place_mentioned_location$link_download, " Download the whole book here !", "<br>"),
                    icon = icons_book1,
                    clusterOptions = markerClusterOptions(), group = "More informations - Book 1") %>%
  addAwesomeMarkers(data = place_mentioned_location_2,
                    ~longitude,
                    ~latitude, 
                    popup = paste( "</br>", place_mentioned_location_2$author, " ,", place_mentioned_location_2$title, "<br>",
                                   "<u>", "Place:", "</u>", "<br>", place_mentioned_location_2$place, "<br>", "<br>", 
                                   "<u>", "Sentence:", "</u>", "</br>", place_mentioned_location_2$sentence_associated, "<br>", "</br>",
                                   " Sentence n° ", place_mentioned_location_2$position_sentence, "<br>", 
                                   "Frequency:", "</u>", "</br>", place_mentioned_location_2$freq, " time(s)" , "<br>", 
                                   "</u>", "</br>", place_mentioned_location_2$link_download, " Download the whole book here !", "<br>"),
                    icon = icons_book2,
                    clusterOptions = markerClusterOptions(), group = "More informations - Book 2") %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("Black", "OSM"),
    overlayGroups = c("Frequency - Book 1", "Place name - Book 1", "More informations - Book 1", "Frequency - Book 2", "Place name - Book 2", "More informations - Book 2"),
    options = layersControlOptions(collapsed = TRUE)
    
  )
map_place
