library(leaflet)

map <- leaflet(width = 400, height = 400)
map <- addTiles(map)
map <- setView(map, lng = -80.098171,
               lat = 26.372827,
               zoom = 6)
map


map <- leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addMarkers(lng = -80.098171,
             lat = 26.372827, 
             popup = "You are here.",
             options = markerOptions(draggable = TRUE, riseOnHover = TRUE)) %>% 
  addCircleMarkers(lng = -123.261,
                   lat = 49.273, 
                   popup = "You aren't here.",
                   fillColor= "red", opacity = 1,
                   options = markerOptions(draggable = FALSE, title = "Whoops")) %>% 
  setView(lng = -80.098171,
          lat = 26.372827,
          zoom = 13)

map