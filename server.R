library(shiny)
library(leaflet)
library(geojsonio)

covidData <- read.csv(file = './Data/time_series_covid19_confirmed_USA_20210618.csv')
statesNames = unique(covidData$Province_State)

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

getDataByState <- function(state) 
{
	data = covidData[covidData$Province_State==state,12:525]
	dataSum = colSums(data)
	return(dataSum)
}

getDataByStateAndCounty <- function(state, county) 
{
	if (county == " WHOLE STATE"){
	data = covidData[covidData$Province_State==state,12:525]
	dataSum = colSums(data)

}	else{
	data = covidData[covidData$Province_State==state & covidData$Admin2==county,12:525]
	dataSum = colSums(data)
}
	return(dataSum)
}

countyNamesByState <- function(state)
{
	data = covidData[covidData$Province_State==state,6]
	return(data)
}




####
function(input, output, session) {
  
output$countySelect <- renderUI({
	selectInput("county", "County:", 
                  choices = c(" WHOLE STATE",countyNamesByState(input$state)))
})

#plot start
  output$covidPlot <- renderPlot({
    
    # Render a barplot
	plot(
		getDataByStateAndCounty(input$state, input$county), 
            main = paste(input$county, input$state, sep=", "),
            ylab = "Number of Covid Cases",
            xlab = "Date"
		)
	})
#plot end

#map start
	output$map <- renderLeaflet({

x1 = aggregate(covidData[,input$mapSlider + 11], list(covidData$Province_State), FUN=sum)
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

	})
#map end

	output$table <- renderTable({
   	 covidData[1:10,]
  	})

}#server end
