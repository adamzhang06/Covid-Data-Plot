library(shiny)
#library(datasets)
covidData <- read.csv(file = 'D:/SLU/DATA/time_series_covid19_confirmed_USA_20210618.csv')
statesNames = unique(covidData$Province_State)

####
getDataByState <- function(state) 
{
	data = covidData[covidData$Province_State==state,12:525]
	dataSum = colSums(data)
	return(dataSum)
}
####getDataByState("Alabama")

####
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
####getDataByStateAndCounty("Alabama", "Autauga")

####
countyNamesByState <- function(state)
{
	data = covidData[covidData$Province_State==state,6]
	return(data)
}
####countyNamesByState("Alabama")

####
####

ui <- fluidPage(    
  

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
      plotOutput("covidPlot")  
    )
    
  )
)



server <- function(input, output, session) {
  
output$countySelect <- renderUI({
	selectInput("county", "County:", 
                  choices = c(" WHOLE STATE",countyNamesByState(input$state)))
})

  output$covidPlot <- renderPlot({
    
    # Render a barplot
	plot(
		getDataByStateAndCounty(input$state, input$county), 
            main = paste(input$county, input$state, sep=", "),
            ylab = "Number of Covid Cases",
            xlab = "Date"
		)
	})

}



shinyApp(ui = ui, server = server)