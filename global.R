### Libraries
library(gargle)
library(googlesheets4)
library(htmltools)
library(leafem)
library(leaflet)
library(leaflet.extras) # NEW
library(leafgl)
library(mongolite)
library(plyr)
library(purrr)
library(raster)
library(rdrop2)
library(rgdal)
library(rgeos)
library(rrapply)
library(sf)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus) # NEW
library(shinydisconnect)
library(shinyjs)
library(shinysurveys)
library(shinyFeedback)
library(shinyTree)
library(shinyWidgets)
library(shinyvalidate) # NEW
library(stringr)
library(tidyr)
library(tibble)
library(rrapply)
library(stringi)
library(ggplot2)
library(dplyr) # load last to stop issues with plyr 

gs4_auth(cache = "secrets", email = TRUE)

# Mongo database ----
load("secrets/host.rda")
load("secrets/username.rda")
load("secrets/password.rda")

options(mongodb = list(
  "host" = host,
  "username" = username,
  "password" = password
))

databaseName <- "culturalresponses"

## Themes ----
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Responses directory ----
answersdir <- "responses" 
polygonsdir <- "responses" 
valuesdir <- "responses" 

## Leaflet spinner ----
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

## Read in cultural list (downloaded from Googledrive) ----
cultural_values <- read.csv("data/Activity list - cultural-values.csv", na.strings=c("","NA",NA)) %>%
  select(-c(Comments))

# Create a list of inputs for each value selected ----
cultural_list <- cultural_values %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_","__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Activity = stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  tidyr::replace_na(list(c(Category = "NA"))) %>%
  dplyr::mutate(input.suffix = if_else(is.na(Activity), paste(tolower(Category), tolower(Sub.category), "NA", sep = "__"), paste(tolower(Category), tolower(Sub.category), tolower(Activity), sep = "__"))) %>%
  dplyr::mutate(days = paste("days__values",input.suffix, sep = "__")) %>%
  dplyr::mutate(season = paste("season__values",input.suffix, sep = "__")) %>%
  dplyr::mutate(description = paste("description__values",input.suffix, sep = "__"))

days_inputs <- (unique(cultural_list$days))
season_inputs <- (unique(cultural_list$season))
description_inputs <- (unique(cultural_list$description))

cultural_input_list <- c(days_inputs, season_inputs, description_inputs)

# Create a list of the subcategories for the accordion ----
cultural_acc <- cultural_values %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category,Activity, sep = " - "),
                nice.cat = Category,
                nice.sub = Sub.category,
                nice.act = Activity) %>%
  dplyr::filter(!Category %in% c("Pressures and threats", "Other")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Activity = stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(checkbox = if_else(is.na(Activity), paste("checkbox_", Category, sep = ""), paste("checkbox_", Category, "__", Sub.category, sep = ""))) %>%
  dplyr::mutate(nice.act = if_else(is.na(nice.act), nice.sub, nice.act)) %>%
  # dplyr::mutate(checkbox = paste("checkbox_", Category, "__", Sub.category, sep = "")) %>%
  glimpse()

other_acc <- cultural_values %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category, sep = " - "),
                nice.cat = Category,
                nice.act = Sub.category) %>%
  dplyr::filter(Category %in% c("Other")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(checkbox = paste("checkbox_", Category, sep = "")) %>%
  glimpse()

pressures_acc <- cultural_values %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category, sep = " - "),
                nice.cat = Category,
                nice.sub = Sub.category,
                nice.act = Activity) %>%
  dplyr::filter(Category %in% c("Pressures and threats")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Activity = stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(checkbox = paste("checkbox_", Category, "__", Sub.category, sep = "")) %>%
  glimpse()

unique(cultural_acc$checkbox)

## Read in response scales ----
response.scales <- read.csv("data/Activity list - Response scales.csv", na.strings=c("","NA"))

# grid.2.5 <- readOGR(dsn="spatial/intersections/2.5 km_whole hexagons_regions.shp", layer="2.5 km_whole hexagons_regions")
# save(grid.2.5, file = "spatial/intersections/grid.2.5.rda")
# bathy <- readOGR(dsn="spatial/bathyMatt4.shp", layer="bathyMatt4")
# save(bathy, file = "spatial/intersections/bathy.rda")
# regions <- readOGR(dsn="spatial/extended_regions.shp", layer="extended_regions")
# save(regions, file = "spatial/intersections/regions.rda")
# regions <- readOGR(dsn="spatial/regions.shp", layer="regions")
# save(regions, file = "spatial/regions.rda")

# bathy_test <- st_read("./spatial/Bath_matt4.shp") #Matt's approach
# bathy_test <-sf::st_cast(bathy_test, "POLYGON")
# save(bathy_test, file = "spatial/intersections/bathy.rda")

# # extra grids for cultural
# cultural.grid <- readOGR(dsn="spatial/intersections/2.5 km_whole hexagons_state_to_200m_with_regions.shp", layer="2.5 km_whole hexagons_state_to_200m_with_regions")
# cultural.grid <- readOGR(dsn="spatial/intersections/2.5 km_whole hexagons_state_to_islands_with_regions.shp", layer="2.5 km_whole hexagons_state_to_islands_with_regions")
# save(cultural.grid, file = "spatial/intersections/cultural.grid.rda")
# # estuaries grids
# estuaries.grid <- readOGR(dsn="spatial/estuaries.shp", layer="estuaries")
# save(estuaries.grid, file = "spatial/intersections/estuaries.grid.rda")

placenames <- st_read(dsn = "spatial/Esp_PlaceNames.shp")

labels <-  bind_cols(data.frame(st_coordinates(placenames[,1])), placenames) %>%
  filter(!X%in%c("NaN"))

# Zoom on at 8 and off at 10
# Zoom on at 14
# Zoom on at 12
# Zoom on at 10

on.10 <- labels %>%
  filter(Zoom.on %in% c(10))

on.12 <- labels %>%
  filter(Zoom.on %in% c(12))

on.14 <- labels %>%
  filter(Zoom.on %in% c(14))

on.8 <- labels %>%
  filter(Zoom.on %in% c(8)) %>%
  filter(!Feature %in% c("Town"))

towns <- labels %>%
  filter(Feature %in% c("Town"))

# towns <- labels %>% dplyr::filter(Feature %in% c("Town"))
# islands <- labels %>% dplyr::filter(!Feature %in% c("Town"))

# Read in spatial files ----
load("spatial/intersections/grid.2.5.rda")
load("spatial/intersections/bathy.rda")
load("spatial/regions.rda")
load("spatial/intersections/cultural.grid.rda")
load("spatial/intersections/estuaries.grid.rda")

regionbounds <- regions %>% 
  st_bbox() %>% 
  as.character()

bathy.pal <- colorFactor(palette = "Blues", 
                         levels = c("0-10m","10-20m","20-50m","50-100m","100-200m","200-500m","500-1000m"))

bathy$LABEL <- factor(bathy$LABEL, 
                      levels = c("0-10m","10-20m","20-50m","50-100m","100-200m","200-500m","500-1000m"))

# Grids for spatial questions ----
state <- SpatialPolygons(grid.2.5@polygons)
commonwealth <- SpatialPolygons(cultural.grid@polygons)
estuaries <- SpatialPolygons(estuaries.grid@polygons)

SpP.state = SpatialPolygonsDataFrame(
  state,
  data = data.frame(ID = paste("state",as.character(c(1:(
    length(state@polygons))
  ))),
  reg = grid.2.5$Reg,
  display = c(1:(
    length(state@polygons)
  ))),
  match.ID = FALSE
)

SpP.commonwealth = SpatialPolygonsDataFrame(
  commonwealth,
  data = data.frame(ID = paste("commonwealth",as.character(c(1:(
    length(commonwealth@polygons))
  ))),
  reg = cultural.grid$Reg,
  display = c(1:(
    length(commonwealth@polygons)
  ))),
  match.ID = FALSE
)

SpP.estuaries = SpatialPolygonsDataFrame(
  estuaries,
  data = data.frame(ID = paste("estuaries",as.character(c(1:(
    length(estuaries@polygons))
  ))),
  reg = estuaries.grid$Reg,
  display = c(1:(
    length(estuaries@polygons)
  ))),
  match.ID = FALSE
)

nrow(SpP.commonwealth) + nrow(SpP.state) + nrow(SpP.estuaries)

SpP <- rbind(SpP.state, SpP.commonwealth, SpP.estuaries)

# regions pallete
regpal <- colorFactor(palette = c("#F83D41","#FD5E53","#FFAE41","#FF9506","#FF5E01"), levels = regions@data$Reg) # oranges and reds

# which fields get saved ----
fieldsAll <- c("name", "email", "phone", "postcode", "family", "gender", cultural_input_list)

# which fields are mandatory ----
fieldsMandatory <- c("name", "email", "phone", "family","gender")

# CSS to use in the app ----
appCSS <-
  ".mandatory_star { color: red; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
  "

# FUNCTIONS ----
saveData <- function(data, collection) {
  # Connect to the database
  db <- mongo(collection = collection,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  # Insert the data into the mongo collection as a data.frame
  # data <- as.data.frame(t(data))
  db$insert(data)
}

# Function to add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Function to get a formatted string of the timestamp
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# Function to change the colour of polygons on leaflet map
change_color <- function(map, id_to_remove, data, colour, new_group){
  leafletProxy(map) %>%
    removeShape(id_to_remove) %>% # remove previous occurrence
    addPolygons(
      data = data,
      layerId = data$ID,
      group = new_group, # change group
      fillColor = colour,
      fillOpacity  = 0.5,
      weight = 1,
      color = "red",
      options = pathOptions(pane = "polygons"))
}

# Function to create a unique random ID to match datasets back up without using the users name ----
randomID <- function(n = 1000000) {
  a <- do.call(paste0, replicate(10, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%09d", sample(999999999, n, TRUE)), sample(LETTERS, n, TRUE))
}

# Functions to create mouseover lat and lon for leaflet maps ----
clipboardDependency = function() {
  list(
    htmltools::htmlDependency(
      name = "clipboard",
      version = "0.0.1",
      src = system.file("htmlwidgets/lib/clipboard", package = "leafem"),
      script = "setClipboardText.js"
    )
  )
}

addmouselatlon <- function(map,
                                epsg = NULL,
                                proj4string = NULL,
                                native.crs = FALSE) {
  
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, c("leaflet", "leaflet_proxy", "mapdeck")))
  
  if (native.crs) { 
    txt_detailed <- paste0("
                           ' x: ' + (e.latlng.lng).toFixed(5) +
                           ' | y: ' + (e.latlng.lat).toFixed(5) +
                           ' | epsg: ", epsg, " ' +
                           ' | proj4: ", proj4string, " ' +
                           ' | zoom: ' + map.getZoom() + ' '")
  } else {
    txt_detailed <- paste0("
                            'Latitude: ' + ((e.latlng.lat).toFixed(5)) + 
                            ' | Longitude: ' + (e.latlng.lng).toFixed(5) +
                           ' (Decimal Degrees)'")
  }
  
  
  txt_basic <- paste0("
                      'Latitude: ' + [0|(e.latlng.lat)] + 
                      '° ' + 
                      [0|((e.latlng.lat)<0?(e.latlng.lat)=-(e.latlng.lat):(e.latlng.lat))%1*60] +
                      ' ' + 
                      ((e.latlng.lat)*60%1*60).toFixed(3) +
                      ' S | ' +
                      
                      'Longitude: ' + [0|(e.latlng.lng)] + 
                      '° ' + 
                      [0|((e.latlng.lng)<0?(e.latlng.lng)=-(e.latlng.lng):(e.latlng.lng))%1*60] +
                      ' ' + 
                      ((e.latlng.lng)*60%1*60).toFixed(3) +
                      ' E (Degrees Minutes Seconds)'
                      
")
  
  map$dependencies = c(
    map$dependencies,
    clipboardDependency()
  )
  
  map <- htmlwidgets::onRender(
    map,
    paste0(
      "
      function(el, x, data) {
      // get the leaflet map
      var map = this; //HTMLWidgets.find('#' + el.id);
      // we need a new div element because we have to handle
      // the mouseover output separately
      // debugger;
      function addElement () {
      // generate new div Element
      var newDiv = $(document.createElement('div'));
      // append at end of leaflet htmlwidget container
      $(el).append(newDiv);
      //provide ID and style
      newDiv.addClass('lnlt');
      newDiv.css({
      'position': 'relative',
      'bottomleft':  '0px',
      'background-color': 'rgba(255, 255, 255, 0.7)',
      'box-shadow': '0 0 2px #bbb',
      'background-clip': 'padding-box',
      'margin': '0',
      'padding-left': '5px',
      'color': '#333',
      'font': '12px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',
      'z-index': '700',
      });
      return newDiv;
      }
      // check for already existing lnlt class to not duplicate
      var lnlt = $(el).find('.lnlt');
      if(!lnlt.length) {
      lnlt = addElement();
      // grab the special div we generated in the beginning
      // and put the mousmove output there
      map.on('mousemove', function (e) {
      if (e.originalEvent.ctrlKey) {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_detailed, ");
      } else {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_basic, ");
      }
      });
      // remove the lnlt div when mouse leaves map
      map.on('mouseout', function (e) {
      var strip = document.querySelector('.lnlt');
      if( strip !==null) strip.remove();
      });
      };
      //$(el).keypress(67, function(e) {
      map.on('preclick', function(e) {
      if (e.originalEvent.ctrlKey) {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_basic, ");
      var txt = document.querySelector('.lnlt').textContent;
      console.log(txt);
      //txt.innerText.focus();
      //txt.select();
      setClipboardText('\"' + txt + '\"');
      }
      });
      }
      "
    )
  )
  map
}

# Function to validate email address ----
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
        ignore.case=TRUE)
}