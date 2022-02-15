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
library(shinyRadioMatrix)
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
library(profvis)

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
  # dplyr::mutate(Activity = if_else(is.na(Activity), "NA", (Activity))) %>%
  dplyr::mutate(input.suffix = if_else(is.na(Activity), paste(tolower(Category), tolower(Sub.category), "NA", sep = "__"), paste(tolower(Category), tolower(Sub.category), tolower(Activity), sep = "__"))) %>%
  dplyr::mutate(days = paste("days__values",input.suffix, sep = "__")) %>%
  dplyr::mutate(time = paste("time__values",input.suffix, sep = "__")) %>%
  dplyr::mutate(description = paste("description__values",input.suffix, sep = "__"))


test<- cultural_list %>% filter(is.na(Activity))

days_inputs <- (unique(cultural_list$days))
time_inputs <- (unique(cultural_list$time))
description_inputs <- (unique(cultural_list$description))

cultural_input_list <- c(days_inputs, time_inputs, description_inputs)

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

## Read in questions (only using this in the matrix style so far) ----
questions <- read.csv("data/Activity list - questions.csv", na.strings=c("","NA")) %>%
  dplyr::select(section, question, type, response.items.for.matrix.only, scale.name, question.number)%>%
  mutate(question.number = as.character(question.number))

matrix.data <- questions %>%
  filter(type%in%c("matrix")) %>%
  dplyr::select(question.number,response.items.for.matrix.only)

# Group responses into a list for matrix
matrix.questions <- rrapply(cbind(matrix.data), how = "unmelt")

# grid.2.5 <- readOGR(dsn="spatial/intersections/2.5 km_whole hexagons_regions.shp", layer="2.5 km_whole hexagons_regions")
# save(grid.2.5, file = "spatial/intersections/grid.2.5.rda")
# bathy <- readOGR(dsn="spatial/bathyMatt4.shp", layer="bathyMatt4")
# save(bathy, file = "spatial/intersections/bathy.rda")
# regions <- readOGR(dsn="spatial/regions_outline.shp", layer="regions_outline")
# save(regions, file = "spatial/intersections/regions.rda")

# bathy_test <- st_read("./spatial/Bath_matt4.shp") #Matt's approach
# bathy_test <-sf::st_cast(bathy_test, "POLYGON")
# save(bathy_test, file = "spatial/intersections/bathy.rda")

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
load("spatial/intersections/regions.rda")

regionbounds <- regions %>% 
  st_bbox() %>% 
  as.character()

# grid.2.5$area_sqkm <- area(grid.2.5) / 1000000
# summary(grid.2.5$area_sqkm)

unique(bathy@data$LABEL)

# bathy <- bathy[!bathy@data$LABEL %in% c("ISLAND"),] # , "1000-2000m", "2000-5000m", "> 5000m"

bathy.pal <- colorFactor(palette = "Blues", 
                         levels = c("0-10m","10-20m","20-50m","50-100m","100-200m","200-500m","500-1000m"))

bathy$LABEL <- factor(bathy$LABEL, 
                      levels = c("0-10m","10-20m","20-50m","50-100m","100-200m","200-500m","500-1000m"))

# Grids for spatial questions ----
SpP <- SpatialPolygons(grid.2.5@polygons)

SpP1 <- SpP[1:1181,]
SpP2 <- SpP[1182:2362,]
SpP3 <- SpP[2363:3544,]

SpP = SpatialPolygonsDataFrame(
  SpP,
  data = data.frame(ID = as.character(c(1:(
    length(SpP@polygons)
  ))),
  reg = grid.2.5$Reg,
  display = c(1:(
    length(SpP@polygons)
  ))),
  match.ID = FALSE
)


# regions pallete


# regpal <- colorFactor(palette = c("#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c"), levels = regions@data$Reg)

regpal <- colorFactor(palette = c("#F83D41","#FD5E53","#FFAE41","#FF9506","#FF5E01"), levels = regions@data$Reg) # oranges and reds

# regpal <- colorFactor(palette = c("#F83D41", "#FF9506", "#FD5E53", "#FF5E01", "#FFAE41"), levels = regions@data$Reg) # oranges and reds, ordered

# regpal <- colorFactor(palette = c("#4DE88B","#E8D94D","#E8D94D","#4DE88B","#4DE88B"), levels = regions@data$Reg) # yellow and green

# regpal <- colorFactor(palette = c("#F5B859","#F5B859","#F5B859","#F5B859","#F5B859"), levels = regions@data$Reg) # all orange
#4DE88B - green
#E8D94D - yellow

# "BRE" - green - 1
# "CLG" - yellow - 4
# "STO" - yellow - 2
# "EAS" - green - 5
# "ESP" - green - 3

# which fields get saved ----
fieldsAll <- c("name", "email", "phone", "residence","postcode", "gender", "age", "frequency", cultural_input_list, "visited")

# which fields are mandatory ----
fieldsMandatory <- c("name", "email", "phone", "residence", "gender", "age")

# CSS to use in the app ----
appCSS <-
  ".mandatory_star { color: red; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
  "

# FUNCTIONS ----
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

