

# Load the tiles in working_folder/mapTiles/OSM/
library(RgoogleMaps)
library(leaflet)
library(shiny)

(center=getGeoCode("Washington Square Park;NY"))

for (zoom in 10:16)
  GetMapTiles(center, zoom = zoom,
              nTiles = round(c(20,20)/(17-zoom)),
              type = "osm", tileDir = "C:/GitHub/culturalmapper/mapTiles/OSM/", returnTiles = TRUE)

# For ESPERANCE ----
for (zoom in 8:14)
  GetMapTiles(lonR = c(119, 130),
              latR = c(-35, -31),
              zoom = zoom,
              #nTiles = round(c(20,20)/(17-zoom)),
              type = "osm", tileDir = "C:/GitHub/culturalmapper/mapTiles/OSM/", returnTiles = TRUE)

# Start serving working folder on port 8000 in demon mode
deamon_id <- servr::httd(dir = "C:/GitHub/culturalmapper/mapTiles/OSM", port = 8000, daemon = TRUE)

# Plot with leaflet

#shiny ui 
ui = fluidPage(leafletOutput("map"))

#create basic map, load tiles from directory and set view to centre of downloaded tiles
server = function(input, output, server){

addResourcePath("mytiles", "C:/GitHub/culturalmapper/mapTiles/OSM")

# m = leaflet() %>% 
#   addTiles( urlTemplate = "http:/localhost:8000/C:/GitHub/culturalmapper/mapTiles/OSM/{z}_{x}_{y}.png")
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "/mytiles/{z}_{x}_{y}.png") %>%
      setView(125, -33, zoom = 10)
})
}

shinyApp(ui, server)

# Stop serving
servr::daemon_stop(1)
