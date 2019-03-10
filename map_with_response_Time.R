library(geojsonio)
library(leaflet)

# From http://eric.clst.org/Stuff/USGeoJSON and
# https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents
bos <- geojson_read( "Boston_Neighborhoods.geojson",
                     what = "sp")
# Or use the rgdal equivalent:
# nycounties <- rgdal::readOGR("json/nycounties.geojson", "OGRGeoJSON")

pal <- colorNumeric(palette = c("#006838","#1a9850", "#66bd63", "#a6b69a", "#d9ef8b", "#fee08b", 
                                "#fdae61", "#f46d43","#d73027","#a50026"),
                    domain = seq(from = 25, to = 70, by = 5))

leaflet() %>%
  addTiles() %>%
  addPolygons(data = bos@polygons[[2]], stroke = TRUE, color = "#d9ef8b",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.11514, lat = 42.30974, label ="Jamaca Plain",
                      labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% 
  addPolygons(data = bos@polygons[[1]], stroke = TRUE, color = "#f46d43",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.12703, lat = 42.28321, label ="Roslindale", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # Roslindale
  addPolygons(data = bos@polygons[[3]], stroke = TRUE, color = "#fdae61",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.10623, lat = 42.32962, label ="Mission Hill", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # mission hill
  addPolygons(data = bos@polygons[[4]], stroke = TRUE, color = "#d9ef8b",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.10029, lat = 42.34287, label ="Fenway/Kenmore", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # longwood/fenway
  addPolygons(data = bos@polygons[[5]], stroke = TRUE, color = "#66bd63",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.08098, lat = 42.35026, label ="Back Bay", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # back bay
  addPolygons(data = bos@polygons[[6]], stroke = TRUE, color = "#fee08b",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.05721, lat = 42.35571, label = "Downtown/Financial District", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "top", textsize = "13px")) %>% # downtown bos
  addPolygons(data = bos@polygons[[7]], stroke = TRUE, color = "#fee08b",fillOpacity = 0.9) %>% # downtown bos
  addPolygons(data = bos@polygons[[8]], stroke = TRUE, color = "#fee08b",fillOpacity = 0.9) %>% # downtown bos
  addPolygons(data = bos@polygons[[9]], stroke = TRUE, color = "#a6d69a",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.09137, lat = 42.3152, label ="Roxbury", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # roxbury
  addPolygons(data = bos@polygons[[10]], stroke = TRUE, color = "#a6d69a",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.07652, lat = 42.33881, label ="South End", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # south end
  addPolygons(data = bos@polygons[[11]], stroke = TRUE, color = "#66bd63",fillOpacity = 0.9) %>%
  addPolygons(data = bos@polygons[[12]], stroke = TRUE, color = "#1a9850",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.03886, lat = 42.37021, label ="East Boston", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "right", textsize = "13px")) %>% # east bos
  addPolygons(data = bos@polygons[[13]], stroke = TRUE, color = "#a6d69a",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.06021, lat = 42.37821, label ="Charleston", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # charleston
  addPolygons(data = bos@polygons[[14]], stroke = TRUE, color = "#fee08b",fillOpacity = 0.9) %>%  # downtown
  addPolygons(data = bos@polygons[[15]], stroke = TRUE, color = "#fdae61",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.07074, lat = 42.3588, label ="Beacon Hill", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center",textsize = "13px")) %>% # beacon hill
  addPolygons(data = bos@polygons[[16]], stroke = TRUE, color = "#fee08b",fillOpacity = 0.9) %>% # downtown
  addPolygons(data = bos@polygons[[17]], stroke = TRUE, color = "#d9ef8b",fillOpacity = 0.9) %>% # fenway
  addPolygons(data = bos@polygons[[18]], stroke = TRUE, color = "#66bd63",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.16268, lat = 42.34635, label ="Allston/Brighton", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "right", textsize = "13px")) %>% # brighton
  addPolygons(data = bos@polygons[[19]], stroke = TRUE, color = "#d73027",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.16268, lat = 42.27976, label ="West Roxbury", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "top", textsize = "13px")) %>% # west roxbury
  addPolygons(data = bos@polygons[[20]], stroke = TRUE, color = "#a50026",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.12406, lat = 42.25653, label ="Hyde Park", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "top", textsize = "13px")) %>% # hyde park
  addPolygons(data = bos@polygons[[21]], stroke = TRUE, color = "#fee08b",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.09137, lat = 42.27714, label ="Mattapan", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "center", textsize = "13px")) %>% # mattapan
  addPolygons(data = bos@polygons[[22]], stroke = TRUE, color = "#66bd63",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.06761, lat = 42.30163, label ="Dorchester", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "top", textsize = "13px")) %>% # dorchester
  addPolygons(data = bos@polygons[[23]], stroke = TRUE, color = "#66bd63",fillOpacity = 0.9) %>%
  addLabelOnlyMarkers(lng = -71.04758, lat = 42.33814, label ="South Boston/South Boston Waterfront", labelOptions = labelOptions(noHide = T, textOnly = T, direction = "right", textsize = "13px")) %>% # south bos
  addPolygons(data = bos@polygons[[24]], stroke = TRUE, color = "#66bd63",fillOpacity = 0.9) %>% # south bost
  addPolygons(data = bos@polygons[[25]], stroke = TRUE, color = "#66bd63",fillOpacity = 0.9) %>%  # allston 
  addLegend(pal = pal, values = seq(from = 25, to = 70, by = 5), 
            position = "bottomright", opacity = 0.9, title = "Average<br>Response<br>Time (Day)")

