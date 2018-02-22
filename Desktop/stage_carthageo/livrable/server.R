

## app.R ##
library(shiny)
library(shinydashboard)
places_shiny <- read.csv(file = "/Users/Bastien/Desktop/livrable/data/place_mentioned_location.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
places_shiny_2 <- read.csv(file = "/Users/Bastien/Desktop/livrable/data/place_mentioned_location_2.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)



ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "My app"),
                    dashboardSidebar(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))),
                    dashboardBody(
                      fluidRow(
                        tags$h1("Where does the scene take place ? "),
                        box(width = 12, title = "Which book(s) do you want to explore ? ", 
                            splitLayout(
                              selectInput("inputA", "Select book n°1", c("Les Misérables : Tome 1 - V. HUGO", "La Comédie Humaine - H. de BALZAC")),
                              checkboxInput("checkbox", "Add an other book ?", value = TRUE),
                              selectInput("inputB", "Select book n°2", c("Les Misérables : Tome 1 - V. HUGO", "La Comédie Humaine - H. de BALZAC"))
                            )
                        )
                      ),         
                      tags$h2("Map of the town(s) mentioned inside the book(s)"),
                      leafletOutput("mymap"),
                      tags$h2("Data table"),
                      dataTableOutput('table')
                        )
                        
                      )
                    


server <- function(input, output)   {
  
  output$value <- renderText({ input$somevalue })
  output$table <- renderDataTable(place_mentioned_location)
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
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
  }
  
  )
  
  
}






shinyApp(ui, server)
