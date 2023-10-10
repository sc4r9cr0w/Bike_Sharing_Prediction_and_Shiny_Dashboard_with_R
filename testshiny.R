# Import library
library(tidyverse)
library(rio)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidymodels)
library(ggpubr)
library(glmnet)
library(shiny)
library(leaflet)
library(httr)
library(rsconnect)

# Get weather forecast data by cities
get_weather_forecaset_by_cities <- function(city_names){
  # Create some empty vectors to hold data temporarily
  city <- c()
  weather <- c()
  temperature <- c()
  visibility <- c()
  humidity <- c()
  wind_speed <- c()
  seasons <- c()
  hours <- c()
  forecast_date <-c()
  weather_labels<-c()
  weather_details_labels<-c()
  
  # Get 5-days forecast data for each city
  for (city_name in city_names){
    url_get='https://api.openweathermap.org/data/2.5/forecast'
    api_key <- "1ca9b941326ee08030b9c15c50653eec"
    forecast_query <- list(q = city_name, appid = api_key, units="metric")
    response <- GET(url_get, query=forecast_query)
    json_list <-content(response, as="parsed")
    results <- json_list$list
    
    for(result in results) {
      # Get weather data and append them to vectors
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      
      # Get predictor variables
      temperature <- c(temperature, result$main$temp)
      visibility <- c(visibility, result$visibility)
      humidity <- c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      
      forecast_datetime <- result$dt_txt
      hour <- as.numeric(strftime(forecast_datetime, format="%H"))
      month <- as.numeric(strftime(forecast_datetime, format="%m"))
      forecast_date <-c(forecast_date, forecast_datetime)
      season <- "Spring"
      # Simple rule to determine season
      if (month >= 3 && month <= 5)
        season <- "SPRING"
      else if(month >= 6  &&  month <= 8)
        season <- "SUMMER"
      else if (month >= 9  && month <= 11)
        season <- "AUTUMN"
      else
        season <- "WINTER"
      # Add a HTML label to be shown on Leaflet
      weather_label <- paste(sep = "",
                             "<b><a href=''>",
                             city_name, 
                             "</a></b>", "</br>", 
                             "<b>", result$weather[[1]]$main, "</b></br>")
      # Add a detailed HTML label to be shown on Leaflet
      weather_detail_label <- paste(sep = "",
                                    "<b><a href=''>",
                                    city_name, 
                                    "</a></b>", "</br>", 
                                    "<b>", result$weather[[1]]$main, "</b></br>",
                                    "Temperature: ", result$main$temp, " C </br>",
                                    "Visibility: ", result$visibility, " m </br>",
                                    "Humidity: ", result$main$humidity, " % </br>", 
                                    "Wind Speed: ", result$wind$speed, " m/s </br>", 
                                    "Datetime: ", forecast_datetime, " </br>")
      weather_labels <- c(weather_labels, weather_label)
      weather_details_labels <- c(weather_details_labels, weather_detail_label)
      
      seasons <- c(seasons, season)
      hours <- c(hours, hour)
    }
  }
  # Create and return a tibble
  weather_df <- tibble(CITY_ASCII=city, WEATHER=weather, 
                       TEMPERATURE=temperature,
                       VISIBILITY=visibility, 
                       HUMIDITY=humidity, 
                       WIND_SPEED=wind_speed, SEASONS=season, HOURS=hours, FORECASTDATETIME=forecast_date, 
                       LABEL=weather_labels, DETAILED_LABEL=weather_details_labels)
  
  return(weather_df)
  
}

# Load a saved regression model (variables and coefficients) from csv
load_saved_model <- function(model_name){
  model <- read_csv(model_name)
  model <- model %>% 
    mutate(Variable = gsub('"', '', Variable))
  coefs <- setNames(model$Coef, as.list(model$Variable))
  return(coefs)
}


# Predict bike-sharing demand using a saved regression model
predict_bike_demand <- function(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS){
  model <- load_saved_model("model.xls")
  weather_terms <- model['Intercept'] + TEMPERATURE*model['TEMPERATURE'] + HUMIDITY*model['HUMIDITY'] +
    WIND_SPEED*model['WIND_SPEED'] + VISIBILITY*model['VISIBILITY'] 
  season_terms <- c()
  hour_terms <- c()
  # Calculate season related regression terms
  for(season in SEASONS) {
    season_term <- switch(season, 'SPRING'=model['SPRING'],'SUMMER'=model['SUMMER'],
                          'AUTUMN'=model['AUTUMN'], 'WINTER'=model['WINTER'])
    season_terms <- c(season_terms, season_term)
  }
  # Calculate hour related regression terms
  for(hour in HOURS){
    hour_term<- switch(as.character(hour),'0'=model['0'],'1'=model['1'],'2'=model['2'],'3'=model['3'],
                       '4'=model['4'],'5'=model['5'],'6'=model['6'],'7'=model['7'],
                       '8'=model['8'],'9'=model['9'],'10'=model['10'],'11'=model['11'],
                       '12'=model['12'],'13'=model['13'],'14'=model['14'],'15'=model['15'],'16'=model['16'],
                       '17'=model['17'],'18'=model['18'],'19'=model['19'],'20'=model['20'],
                       '21'=model['21'],'22'=model['22'],'23'=model['23'])
    hour_terms <- c(hour_terms, hour_term)
    
  }
  
  return(as.integer(weather_terms + season_terms + hour_terms))     
  
}

# Define a bike-sharing demand level, used for leaflet visualization
calculate_bike_prediction_level<- function(predictions) {
  levels <- c()
  for(prediction in predictions){
    if(prediction <= 1000 && prediction > 0)
      levels <- c(levels, 'small')
    else if (prediction > 1000 && prediction < 3000)
      levels <- c(levels, 'medium')
    else
      levels <- c(levels, 'large')
  }
  return(levels)
}

# Generate a data frame containing weather forecasting and bike prediction data
# Generate a data frame containg weather forecasting and bike prediction data
generate_city_weather_bike_data <- function (){
  cities_df <- read_csv("selected_cities.xls")
  weather_df <- get_weather_forecaset_by_cities(cities_df$CITY_ASCII)
  results <- weather_df %>% 
    mutate(BIKE_PREDICTION=predict_bike_demand(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS)) %>%
    mutate(BIKE_PREDICTION_LEVEL=calculate_bike_prediction_level(BIKE_PREDICTION))
  
  cities_bike_pred <- cities_df %>% left_join(results) %>% 
    select(CITY_ASCII, LNG, LAT, TEMPERATURE, HUMIDITY, BIKE_PREDICTION, BIKE_PREDICTION_LEVEL, LABEL, DETAILED_LABEL, FORECASTDATETIME)
  return(cities_bike_pred)
}

# source("model_prediction.R") 
test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}



# <------------------------------------------------------------------------------------------------->

# Dataset wrangling before apply to dashboard

city_weather_bike_extracted=test_weather_data_generation()
city_weather_bike_df=city_weather_bike_extracted
city_weather_bike_df$FORECASTDATETIME=ymd_hms(city_weather_bike_df$FORECASTDATETIME)
city_weather_bike_df$DATE=date(city_weather_bike_df$FORECASTDATETIME)
cities_max_bike=city_weather_bike_df %>% group_by(CITY_ASCII,DATE) %>% arrange(desc(BIKE_PREDICTION)) %>% 
  slice(1) 
cities_today_status=cities_max_bike %>% filter(DATE==min(cities_max_bike$DATE))
cities_today_status=cities_today_status %>% mutate(CIRCLE=case_when(BIKE_PREDICTION_LEVEL=="large"~12,
                                                                    BIKE_PREDICTION_LEVEL=="medium"~10,
                                                                    TRUE~6))
cities_today_status=cities_today_status %>% mutate(COLOR=case_when(BIKE_PREDICTION_LEVEL=="large"~"red",
                                                                   BIKE_PREDICTION_LEVEL=="medium"~"yellow",
                                                                   
                                                                   
                                                                   TRUE~"green"))
# Define UI for application that draws a histogram
library(shiny)

ui=shinyUI(
  fluidPage(padding=5,
            titlePanel(paste("Demand Prediction of Bike-Sharing: From",min(city_weather_bike_df$DATE),"to",max(city_weather_bike_df$DATE))), 
            # Create a side-bar layout
            sidebarLayout(
              # Create a main panel to show cities on a leaflet map
              mainPanel(
                leafletOutput("city_bike_map", height=600)
              ),
              sidebarPanel(selectInput("city_dropdown","Cities",choices=c("All",city_weather_bike_df$CITY_ASCII)),
                           plotOutput("temp_line", height=200,width=400),
                           br(),
                           plotOutput("bike_prediction",height=200,width=400,click = "plot_click"),
                           verbatimTextOutput("bike_date_output"),
                           
                           plotOutput("humidity_pred_chart", height=200,width=400)
              )
            )
  ))

# Create a RShiny server
server=shinyServer(function(input, output){
  
  
  observeEvent (input$city_dropdown,{
    if(input$city_dropdown=="All"){
      output$city_bike_map=renderLeaflet ({
        leaflet() %>% addTiles() %>% addMarkers(label=cities_today_status$CITY_ASCII,lng=cities_today_status$LNG, lat=cities_today_status$LAT, 
                                                popup=cities_today_status$LABEL,options = popupOptions(closeButton = FALSE)) %>% 
          addCircleMarkers(lng=cities_today_status$LNG,lat=cities_today_status$LAT,color=cities_today_status$COLOR, radius=cities_today_status$CIRCLE )
      })
    }else{
      selected_city=reactive({ cities_today_status %>% filter(CITY_ASCII==input$city_dropdown) }) # reactive give a function output not df. So need to
      # add () at the end.
      
      selected_city_5_day=reactive({city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown)})
      
      output$city_bike_map=renderLeaflet ({
        leaflet() %>% addTiles() %>% setView(lng=selected_city()$LNG, lat=selected_city()$LAT, zoom=15) %>% 
          addMarkers(lng=selected_city()$LNG, lat=selected_city()$LAT, 
                     popup=selected_city()$DETAILED_LABEL)
      })
      output$temp_line=renderPlot({
        ggplot(selected_city_5_day(),aes(x=FORECASTDATETIME,y=TEMPERATURE))+geom_line(color="brown")+
          geom_point(color="red")+geom_text(aes(label=TEMPERATURE),size=3)+
          labs(title=paste("Temperature forcast of next 5 days in",input$city_dropdown))+xlab('Date (3hrs interval)')+
          ylab("Temperature in C")+theme_clean() })
      
      output$bike_prediction=renderPlot({
        ggplot(selected_city_5_day(),aes(x=FORECASTDATETIME,y=BIKE_PREDICTION))+geom_line(color="blue")+
          geom_point(color="blue")+geom_text(aes(label=BIKE_PREDICTION),size=3)+
          labs(title=paste("Bike prediction of next 5 days in",input$city_dropdown))+xlab('Date (3hrs interval)')+
          ylab("Bike No.")+theme_clean() })
      
      output$bike_date_output = renderText({
        click_time <- as_datetime(input$plot_click$x, tz = "America/Los_Angeles")
        paste0("Time = ", click_time, "\nBike Count Prediction = ", input$plot_click$y)
      })
      
      output$humidity_pred_chart=renderPlot({
        ggplot(selected_city_5_day(),aes(x=HUMIDITY,y=BIKE_PREDICTION))+
          geom_smooth(method=lm,formula=y~poly(x,4),color="green")+
          geom_point(color="brown")+
          labs(title=paste("Relationship between Humidity & Bike prediction in",input$city_dropdown))+xlab('Humidity')+
          ylab("Bike No.")+theme_clean() })
      
    }})})

# Run the application 

shinyApp(ui = ui, server = server)


