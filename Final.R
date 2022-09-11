library(ggplot2)
library(shiny)
library(shinythemes)
library(randomForest)
library(dplyr)
library(stats)
car <- read.csv("C:/Users/simon/Documents/ML/archive/data.csv")
cardata<- na.omit(car)
View(cardata)
#index = sample(2, nrow(cardata), replace=T, prob=(c(0.80,0.20)))
#Training = cardata[index==1,]
#Testing = cardata[index==2,]
#View(Training)
#RFM = randomForest(MSRP~., data=Training)
ui<- fluidPage(theme= shinytheme("united"),
               headerPanel('Predict car MSRP'),
               sidebarPanel(
                 h2("Put in Features"),
                 sliderInput("Horsepower", "Horse Power:", min=min(cardata$Engine.HP), max=max(cardata$Engine.HP),value=1),
                 sliderInput("enginecylinders", "# of Engine Cylinders:", min=min(cardata$Engine.Cylinders), max=max(cardata$Engine.Cylinders),value=1),
                 selectInput("vehiclestyle", label = "Vehicle Style", choices =rownames(table(cardata$Vehicle.Style)), selected="Coupe"),
                 selectInput("make", label = "Make", choices =rownames(table(cardata$Make)), selected="BMW"),
                 selectInput("model", label = "Model", choices =rownames(table(cardata$Model)), selected="300"),
                 sliderInput("year", "Year", min=min(cardata$Year), max=max(cardata$Year),value=1),
                 selectInput("fueltype", label = "Fuel Type", choices =rownames(table(cardata$Engine.Fuel.Type)), selected="regular unleaded"),
                 selectInput("transmission", label = "Transmission", choices =rownames(table(cardata$Transmission.Type)), selected="MANUAL"),
                 selectInput("wheelsdriving", label = "Wheels Driving", choices =rownames(table(cardata$Driven_Wheels)), selected="front wheel drive"),
                 selectInput("numberofdoors", label = "# of Doors", choices =rownames(table(cardata$Number.of.Doors)), selected="front wheel drive"),
                 selectInput("marketcat", label = "Market Category", choices =rownames(table(cardata$Market.Category)), selected="front wheel drive"),
                 selectInput("vehiclesize", label = "Size", choices =rownames(table(cardata$Vehicle.Size)), selected="front wheel drive"),
                 sliderInput("highwaympg", "Highway MPG", min=min(cardata$highway.MPG), max=max(cardata$highway.MPG),value=1),
                 sliderInput("citympg", "City MPG", min=min(cardata$city.mpg), max=max(cardata$city.mpg),value=1),
                 sliderInput("popularity", "Popularity", min=min(cardata$Popularity), max=max(cardata$Popularity),value=1),
                 actionButton("predict", "Predict")
               )
               , 
               mainPanel(
                 tags$label(h3('MSRP prediction')),
                 verbatimTextOutput('contents'),
                 tableOutput('tabledata')
               ), sidebarPanel("City MPG",
               tabPanel("Correlations",
                           mainPanel(
                             h1("City MPG vs MSRP"),
                        
                             plotOutput(outputId = "Correlation")
                           )    
               ) ), sidebarPanel("Highway MPG",
                 tabPanel("Correlations2",
                          mainPanel(
                            h1("Highway MPG vs MSRP"),
                            
                            plotOutput(outputId = "Correlation2")
                          )    
                 ) )
               
)
server<-function(input, output, session){
  index = sample(3, nrow(cardata), replace=T, prob=(c(0.9, 0.08, 0.02)))

  Training = cardata[index==2,]
  Testing = cardata[index==3,]
  
  RFM = randomForest(MSRP~., data=Training)
  output$Correlation <- renderPlot({
    ggplot(data = cardata, aes(x=cardata$city.mpg,y=cardata$MSRP))+geom_point(color="red")
  })
  output$Correlation2 <- renderPlot({
    ggplot(data = cardata, aes(x=cardata$highay.MPG,y=cardata$MSRP))+geom_point(color="red")
  })
  output$Correlation2 <- renderPlot({
    ggplot(data = cardata, aes(x=cardata$highway.MPG,y=cardata$MSRP))+geom_point(color="red")
  })
  output$tabledata <- renderPrint({
    if(input$predict>0){
      
      inputData <- data.frame(Make = input$make,
                              Engine.HP = input$Horsepower, 
                              Engine.Cylinders = input$enginecylinders, 
                              Vehicle.Style = input$vehiclestyle, 
                              Model = input$model, 
                              Year = input$year, 
                              Engine.Fuel.Type = input$fueltype, 
                              Transmission.Type = input$transmission, 
                              Driven_Wheels = input$wheelsdriving, 
                              Number.of.Doors = input$numberofdoors, 
                              Market.Category = input$marketcat, 
                              Vehicle.Size = input$vehiclesize, 
                              highway.MPG = input$highwaympg, 
                              city.mpg = input$citympg, 
                              Popularity = input$popularity
                              )

      print(predict(RFM, inputData))
    }
  })
}
shinyApp(ui=ui, server=server)
