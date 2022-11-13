library(leaflet)
library(geojsonio)
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

cd <- read.csv(file = 'D:/SLU/DATA/time_series_covid19_confirmed_USA_20210618.csv')
x1 = aggregate(cd$X6.18.21, list(cd$Province_State), FUN=sum)
row.names(x1) = x1$Group.1
states$covid = x1[states$name,'x']

bins <- c(0, 100, 200, 500, 1000, 2000, 3000, Inf)*1000
pal <- colorBin("YlOrRd", domain = states$covid, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g covid cases",
  states$name, states$covid
) %>% lapply(htmltools::HTML)


m = leaflet(states)
m = setView(m,-96, 37.8, 4)
m = addProviderTiles(m, "MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m = addPolygons(m,
    fillColor = ~pal(covid),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))

m = addLegend(m, pal = pal, values = ~covid, opacity = 0.7, title = NULL,
    position = "bottomright")
m

