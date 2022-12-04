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
fluidPage(    
  

  titlePanel("Covid Data by US State and County"),
  

  sidebarLayout(      
    

    sidebarPanel(
      selectInput("state", "State:", 
                  choices = statesNames),

      hr(),
	uiOutput("countySelect"),

	hr(),
      helpText("Data from SLU (6/18/22).")
    ),
    

    mainPanel(

	tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("covidPlot")),
                  tabPanel("Map", leafletOutput("map"),
	sliderInput("mapSlider", label = h3("Day"), min = 1, 
        max = 513, value = 250)),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
			)

		)#mainPanel end
  )#sidebarLayout end
)#ui end
