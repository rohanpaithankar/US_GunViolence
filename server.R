library(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(plotly)
library(shinydashboard)
library(leaflet)
library(maps)
library(maptools)
library(sp)



function(input, output) {
  gunviolence <- read.csv("gunviolence_wrangled.csv",head=T,na.strings = c("","NA"))
  gunsales <- read.csv("gunsales_wrangled.csv")
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = gunviolence, { 
    showModal(modalDialog(
      title = tags$h1("FIT 5147 Visualization Project"), 
      tags$h3("Name : Rohan Paithankar"),
      tags$h3("Student ID : 30131502"),
      h2('Guide - '),
      p('This project aims to show a few aspects of Gun Violence in the United States. The visualization has the following components-'),
        p('- Choropleth Map that shows the absolute total number of incidents in every state.'),
        p('- Line Graph comparing the sales of HandGuns and LongGuns within the chosen year. The bars represent the number of HandGuns or LongGuns used in the crime.'),
        p('- Pie Chart to show the share of HandGuns and LongGuns used in the gun shooting crimes.'),
        p('- WordCloud showing words related to the locations of the crime.'),
        p('- Localised Map of the selected state which shows clusters of the number of incidents per area. 
          Clicking the circle marker gives information about the address, date, number killed and number injured in the incident.'),
        h2('Instructions -'),
        p('1.) Click on a State from the choloropleth map to view the visualisations.'),
        p('2.) Select Year with the help of the slider. Default value has been set to 2013.'),
        p('3.) Select the Incident Type. Visualizations can be viewed for \'All\' incidents or \'Mass Shooting\' incidents.')
    ))
  })
  
  
  
  #Main Leaflet Chloropleth Map.The shades correspond to the Total Number of shooting incidents. 
  output$mymap <- renderLeaflet({
  if(input$radio == "all"){                                                               #Selecting All or Mass Shooting incidents
    incident_by_state <- gunviolence %>% group_by(state) %>% summarize(num_incident = n())
  }
  else if(input$radio == "mass"){
    incident_by_state <- gunviolence %>% filter(MassShooting == 1) %>%group_by(state) %>% summarize(num_incident = sum(MassShooting))
  }
  incident_by_state$state <- tolower(incident_by_state$state)   #Converting all state names to lowercase
  mapStates <- maps::map("state", fill = TRUE, plot = FALSE)
  spliteNames <- strsplit(mapStates$names, ":")
  firstPartNames <- lapply(spliteNames, function(x) x[1])
  incidents <- incident_by_state$num_incident[match(firstPartNames, incident_by_state$state)]
  cpal <- colorNumeric("Oranges", incidents)
    leaflet(mapStates,options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('pk.eyJ1Ijoicm9oYW5wYWl0aGFua2FyIiwiYSI6ImNqd2gzZjMzbjI5YjU0ZHFubjI2b3lyeWYifQ.KLPLoaidNNjBHCuxeCYn-g'))) %>% 
      addPolygons(weight = 2,stroke = TRUE,color = "white", smoothFactor = 0.2,fillOpacity = 1,fillColor =  ~cpal(incidents), dashArray = "3",highlightOptions = highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7,bringToFront = TRUE))%>%
      addLegend(pal = cpal, values = ~incidents, opacity = 0.7, title = "No. of Incidents",
                position = "bottomright" )
  })
  
  # Function to get State name from longitude and latitude
  get_state <- function(point) {                   # Referred from -  https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965#8751965
    states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_spatial <- map2SpatialPolygons(states, IDs=IDs,proj4string=CRS("+proj=longlat +datum=WGS84"))
    point_spatial <- SpatialPoints(point, proj4string=CRS("+proj=longlat +datum=WGS84"))  # Converting to SpatialPoint Object
    indices <- over(point_spatial, states_spatial)    # To retreive the indices of the clicked point.
    state <- sapply(states_spatial@polygons, function(x) x@ID)  # Return state name of the polygon containing the clicked point.
    state[indices]
  }
  
  observeEvent(input$mymap_shape_click,{              # Observe click event on the 'mymap' in leaflet.
    click <- input$mymap_shape_click
    point_clicked <- data.frame(x = c(click$lng),y=c(click$lat))  # Retreive Longitude and Latitude
    
    if(is.null(click)) return()
    state_clicked <- get_state(point_clicked)
   output$selected_state <- renderText({toupper(state_clicked)})

  
   output$statemap <- renderLeaflet({           # Leaflet for localised view of the clicked state
     if(input$radio == "all"){
       statewise <- gunviolence %>% filter(tolower(state) == state_clicked)%>%filter(grepl(toString(input$slideryear),date))%>% filter(!is.na(address))
       statewise$injured[statewise$injured == 0 & statewise$killed == 0] <- NA                     #For those incidents for which the number of fatalities are unknown
       statewise$killed[is.na(statewise$injured) & statewise$killed == 0] <- NA
       statewise <- statewise %>% filter(!is.na(killed) & !is.na(injured))
     }
     else if(input$radio == "mass"){
       statewise <- gunviolence %>% filter(MassShooting == 1) %>%filter(tolower(state) == state_clicked)%>%filter(grepl(toString(input$slideryear),date))%>% filter(!is.na(address))
       statewise$injured[statewise$injured == 0 & statewise$killed == 0] <- NA                     #For those incidents for which the number of fatalities are unknown
       statewise$killed[is.na(statewise$injured) & statewise$killed == 0] <- NA
       statewise <- statewise %>% filter(!is.na(killed) & !is.na(injured)) %>% filter((killed + injured) >= 4)
     }
     mapStates <- maps::map("state", fill = TRUE, plot = FALSE)
     spliteNames <- strsplit(mapStates$names, ":")
     leaflet(mapStates) %>%
       setView(click$lng, click$lat, 7) %>% addTiles() %>% addCircleMarkers(lng = ~statewise$longitude, lat=~statewise$latitude,weight = 2,clusterOptions = markerClusterOptions() ,radius = ~2*10,fillColor = "red",popup = ~paste(statewise$address,"<br>Date : ",format(as.Date(statewise$date),"%d/%m/%Y"),"<br>Number Injured = ",statewise$injured,"<br>Number Killed = ",statewise$killed))
   })
   
  # Function for WordCloud of words related to the incident location in the clicked state.
  output$words <- renderPlot({ wordcloud_rep <- repeatable(wordcloud)
  if(input$radio == "all"){
    location <- gunviolence %>% filter(tolower(state) == state_clicked) %>% filter(grepl(toString(input$slideryear),date))
  }
  else if(input$radio == "mass"){
    location <- gunviolence %>%filter(MassShooting == 1) %>% filter(tolower(state) == state_clicked) %>% filter(grepl(toString(input$slideryear),date))
  }
  text <- Corpus(VectorSource(location$location_description))
  text <- tm_map(text, tolower) # might simplify the vector
  text <- tm_map(text, removeWords, stopwords('english'))
  words_to_remove <- c('(dfw)','/ft','mci','stl','high','den','phx','del','boi','fca',                           # To remove certain words that don't make sense.
                       'slc','agc','kum','dsm','ict','intl','iah','hou','aus','maf','tpa',
                       'mco','fll','mia','lax','smf','smo','msy','btr')
  text <- tm_map(text, removeWords,words_to_remove)
  text <- tm_map(text,removePunctuation)
  text <- tm_map(text,removeNumbers)
  tdm <- TermDocumentMatrix(text) # turn it into a term document matrix
  m <- as.matrix(tdm)
  freq <- sort(rowSums(m), decreasing = TRUE)
    wordcloud(words = names(freq), freq = freq,min.freq = 5, random.order = FALSE, 
              colors=brewer.pal(12, "Dark2"), scale=c(9,.4), max.words=200, rot.per=.05)
  })
  
  
  
  
  # Function for PieChart to the show the number of HandGuns and LongGuns that were used.
  output$pie_guns  <- renderPlotly({
    if(input$radio == "all"){
      statewise <- gunviolence %>% filter(tolower(state) == state_clicked) %>% filter(grepl(toString(input$slideryear),date))
    }
    else if(input$radio == "mass"){
      statewise <- gunviolence %>% filter(MassShooting == 1) %>% filter(tolower(state) == state_clicked) %>% filter(grepl(toString(input$slideryear),date))
      }
  x_guns = mean(statewise$Handgun)
  y_guns = mean(statewise$Longgun)
  dataframe_guns<- data.frame("typeofgun" = c("Handgun","Longgun"),"count"=c(x_guns,y_guns))
  colors <- c('#022B3A','#FE5F55')

   plot_ly(dataframe_guns, labels = ~typeofgun, values = ~count,
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(count),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 2)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>% add_pie(hole = 0.6)%>%
     layout(title = NULL,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # Function for Line Graph to show trend of HandGun and LongGun sales in the select state and year.
  output$sale_guns <- renderPlotly({
  gunsales_by_year <- gunsales %>% filter(grepl(toString(input$slideryear),month)) %>% filter(tolower(state) == state_clicked)
  gunviolence_by_year <- gunviolence %>% filter(grepl(toString(input$slideryear),date))%>% filter(tolower(state) == state_clicked)
  gunviolence_by_year <- gunviolence_by_year %>% group_by(new_date) %>% summarize(hand = sum(Handgun),long = sum(Longgun))
  plot_ly(gunsales_by_year, x = ~month, y = ~handgun, name = 'HandGuns Sold', type = 'scatter', mode = 'lines',
                       line = list(color = '#159598', width = 4),hoverinfo = 'text',text = ~paste(handgun,'HandGuns Sold')) %>% 
    add_trace(y = ~long_gun, line = list(color = '#d74a86'),name = 'LongGuns Sold',hoverinfo = 'text',text = ~paste(long_gun,'LongGuns Sold'))%>%
    add_trace(x= ~gunviolence_by_year$new_date, y=~gunviolence_by_year$hand,type = 'bar',name = "HandGuns Involved in Crime",yaxis = 'y2', marker = list(color = '#159598'), hoverinfo = 'text',text = ~paste(gunviolence_by_year$hand,'HandGuns'),opacity = c(0.3))%>%
    add_trace(x= ~gunviolence_by_year$new_date, y=~gunviolence_by_year$long,type = 'bar',name = "LongGuns Involved in Crime",yaxis = 'y2', marker = list(color = '#d74a86'),hoverinfo = 'text',text = ~paste(gunviolence_by_year$long,'LongGuns'),opacity = c(0.3))%>%
    layout(title = NULL,
           xaxis = list(title = "Months"),
           yaxis = list(side = 'left', title = 'Guns Sold',overlaying = "y2",showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = 'right', title = 'Number of Incidents', showgrid = FALSE, zeroline = FALSE)
           )
          
  })
  })
  
}