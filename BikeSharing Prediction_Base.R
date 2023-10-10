pacman::p_load(rvest, tidyverse, httr, stringr, magrittr)
url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems" 
data <- read_html(url) 

#Get all the the child table nodes under the root HTML node
table_nodes <- html_nodes(data, "table")

#Convert data to data frame
bike_df <- data %>% 
  html_element("table") %>% 
  html_table()
print(df)

# Export the dataframe into a csv file
write.csv(bike_df, file = "raw_bike_sharing_systems.csv", row.names = FALSE)

#URL for current weather API
current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'

# OpenWeatherAPI

api_key <- "1ca9b941326ee08030b9c15c50653eec"

#List to hold URL parameter
current_query <- list(q="Seoul",appid=api_key,units="metric")

#Make a HTTP request to the current weather API
response <- GET(current_weather_url, query=current_query)
http_type(response)

#Read json http data
json_result <- content(response, as="parsed")
json_result

# Create some empty vectors to hold data temporarily
city <- c()
weather <- c()
visibility <- c()
temp <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
wind_speed <- c()
wind_deg <- c()

#Assign values in json_result into different vector
city <- c(city, json_result$name)
weather <- c(weather, json_result$weather[[1]]$main)
visibility <- c(visibility, json_result$visibility)
temp <- c(temp, json_result$main$temp)
temp_min <-c(temp_min, json_result$main$temp_min)
temp_max <- c(temp_max, json_result$main$temp_max)
pressure <- c(pressure, json_result$main$pressure)
humidity <- c(humidity, json_result$main$humidity)
wind_speed <- c(wind_speed, json_result$wind$speed)
wind_deg <-c(wind_deg, json_result$wind$deg)

#Combine all vector into data frame
weather_df <- data.frame(city = city,
                         weather=weather, 
                         visibility=visibility, 
                         temp=temp, 
                         temp_min=temp_min, 
                         temp_max=temp_max, 
                         pressure=pressure, 
                         humidity=humidity, 
                         wind_speed=wind_speed, 
                         wind_deg=wind_deg)
print(weather_df)

# Get 5 -day weather forecast for a list of cities
weather_forecast_by_cities <- function(city_names) {
  df <- data.frame()
  for (city_name in city_names) {
    #forecast API URL
    forecast_url <-'https://api.openweathermap.org/data/2.5/weather' 
    #create query parameter
    forecast_query <- list(q=city_name,appid=api_key, units="metric")
    #make HTTP GET call for the given city
    response <- GET(forecast_url, query=forecast_query)
    json_result <- content(response, as="parsed")
    results <- json_result$list
    #Loop the json result
    for(result in results) {
      city <- c(city, city_name)
    }
    # Add R lists into a data frame
    city <- c(city, json_result$name)
    weather <- c(weather, json_result$weather[[1]]$main)
    visibility <- c(visibility, json_result$visibility)
    temp <- c(temp, json_result$main$temp)
    temp_min <-c(temp_min, json_result$main$temp_min)
    temp_max <- c(temp_max, json_result$main$temp_max)
    pressure <- c(pressure, json_result$main$pressure)
    humidity <- c(humidity, json_result$main$humidity)
    wind_speed <- c(wind_speed, json_result$wind$speed)
    wind_deg <-c(wind_deg, json_result$wind$deg)
    
    #Combine all vector into data frame
    df <- data.frame(city = city,
                     weather=weather, 
                     visibility=visibility, 
                     temp=temp, 
                     temp_min=temp_min, 
                     temp_max=temp_max, 
                     pressure=pressure, 
                     humidity=humidity, 
                     wind_speed=wind_speed, 
                     wind_deg=wind_deg)
  }
  return(df)
}

cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- weather_forecast_by_cities(cities)
print(cities_weather_df)

write.csv(cities_weather_df,file= "cities_weather_forecast.csv", row.names = FALSE)

# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "raw_worldcities.csv")

# Download a specific hourly Seoul bike sharing demand dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# download the file
download.file(url, destfile = "raw_seoul_bike_sharing.csv")

dataset_list <- c("raw_bike_sharing_system.csv", "raw_seoul_bike_sharing.csv", "cities_weather_forecast.csv", "raw_worldcities.csv")

#Convert iterate over the above datasets and convert their column names 
for (dataset_name in dataset_list) {
  dataset <- read.csv(dataset_name)
  names(dataset) <- toupper(names(dataset))
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  write.csv(dataset, dataset_name, row.names = FALSE)
}

bike_sharing_df <- read.csv("raw_bike_sharing_systems.csv")
head(bike_sharing_df)

sub_bike_sharing_df <- bike_sharing_df %>% 
  select(Country, City, System, Bicycles)

library(dplyr)
#Check the type of data in those columns
sub_bike_sharing_df %>% 
  summarize_all(class) %>% 
  gather(variable, class)

find_character <- function(strings) grepl("[^0-9]", strings) #Create Function

sub_bike_sharing_df %>% 
  select(Bicycles) %>% #Use the function to check BICYCLES column
  filter(find_character(Bicycles)) %>% 
  slice(0:10)

#Create a function to check if there is any reference link in the values
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)

sub_bike_sharing_df %>% 
  select(Country) %>% 
  filter(find_reference_pattern(Country)) %>% 
  slice(1:10)

sub_bike_sharing_df %>% 
  select(City) %>% 
  filter(find_reference_pattern(City)) %>% 
  slice(1:10)

sub_bike_sharing_df %>% 
  select(System) %>% 
  filter(find_reference_pattern(System)) %>% 
  slice(1:10)

#Create a function to remove reference links
remove_ref <- function(strings) {
  ref_pattern <- "\\[[A-z0-9]+\\]" # Define a pattern matching a reference link such as [1]
  result <- stringr::str_replace_all(strings,ref_pattern,"")  # Replace all matched substrings with a white space
  result <-  trimws(result) 
  return(result)
}

# Use the function to remove the reference links
sub_bike_sharing_df %<>% #use mutate and remove_ref fcn to remove ref in CITY and SYSTEM
  mutate(System=remove_ref(System),
         City=remove_ref(City))

# Check whether all reference links are removed
sub_bike_sharing_df %>% 
  select(Country, City, System, Bicycles) %>% 
  filter(find_reference_pattern(Country) | find_reference_pattern(City) | find_reference_pattern(City) |find_reference_pattern(Bicycles) )

extract_num <- function(columns) {
  digitals_pattern <- "\\d+" #define a pattern matching digital substring
  str_extract(columns,digitals_pattern) %>% 
    as.numeric()
}

sub_bike_sharing_df %<>% #use mutate and to apply function to BICYLCLES
  mutate(Bicycles=extract_num(Bicycles))

write.csv(sub_bike_sharing_df,file="bike_sharing_system.csv")


dataset_list <- c('bike_sharing_system.csv','raw_seoul_bike_sharing.csv')
for (dataset_name in dataset_list) {
  dataset <- read.csv(dataset_name)
  names(dataset) <- toupper(names(dataset))
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  write.csv(dataset, dataset_name, row.names = FALSE)
}

# Load the dataset
bike_sharing_df <- read.csv("raw_seoul_bike_sharing.csv")
summary(bike_sharing_df)

dim(bike_sharing_df)
bike_sharing_df <- drop_na(bike_sharing_df, RENTED_BIKE_COUNT)
na_rows <- bike_sharing_df[is.na(bike_sharing_df$TEMPERATURE), ]
print(na_rows)

#calculate the average temperature in summer
summer_temp <- bike_sharing_df[bike_sharing_df$SEASONS == "Summer", ]
summer_avg_temp <- mean(summer_temp$TEMPERATURE, na.rm=TRUE)
print(summer_avg_temp)

bike_sharing_df["TEMPERATURE"][is.na(bike_sharing_df["TEMPERATURE"])] <- summer_avg_temp

write.csv(bike_sharing_df,"seoul_bike_sharing.csv")

library(fastDummies)
bike_sharing_df %>% 
  mutate(HOUR=as.character(HOUR)) %>%  #convert HOUR to character because it's from 0 to 23
  head(10)

# load the package
pacman::p_load(fastDummies)

bike_sharing_df <- dummy_cols(bike_sharing_df, select_columns = "HOUR")
bike_sharing_df <- dummy_cols(bike_sharing_df, select_columns = "HOLIDAY")
bike_sharing_df <- dummy_cols(bike_sharing_df, select_columns = "SEASONS")


#Change the colnames for shorterning
colnames(bike_sharing_df)[c(40,41,42,43,44,45)] <- c("HOLIDAY", "NO HOLIDAY", "AUTUMN", "SPRING", "SUMMER","WINTER")

write.csv(bike_sharing_df,"seoul_bike_sharing_converted.csv")


#Create a function for min-max normalization
minmax_norm <- function(x){       
  (x-min(x))/(max(x)-min(x))}
bike_sharing_df <- read.csv("seoul_bike_sharing_converted.csv")

#Apply min-max normalization function to numerical columns in df
bike_sharing_df %<>%              
  mutate(TEMPERATURE = minmax_norm(TEMPERATURE),
         HUMIDITY = minmax_norm(HUMIDITY),
         WIND_SPEED = minmax_norm(WIND_SPEED),
         VISIBILITY = minmax_norm(VISIBILITY),
         DEW_POINT_TEMPERATURE = minmax_norm(DEW_POINT_TEMPERATURE),
         SOLAR_RADIATION = minmax_norm(SOLAR_RADIATION),
         RAINFALL = minmax_norm(RAINFALL),
         SNOWFALL = minmax_norm(SNOWFALL)) 
head(bike_sharing_df)

write.csv(bike_sharing_df,"seoul_bike_sharing_converted_normalized.csv")

dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')

for (dataset_name in dataset_list) {
  dataset <- read.csv(dataset_name)
  names(dataset) <- toupper(names(dataset))
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  write.csv(dataset, dataset_name, row.names = FALSE)
}


seoul_bike_sharing <- read.csv("seoul_bike_sharing.csv")
str(seoul_bike_sharing)


seoul_bike_sharing$DATE = as.Date(seoul_bike_sharing$DATE,format="%d/%m/%Y") #recast date as a date format
seoul_bike_sharing$HOUR <- factor(seoul_bike_sharing$HOUR, levels = 0:23, ordered = TRUE) #cast the HOUR as categorical variables
seoul_bike_sharing$SEASONS <- factor(seoul_bike_sharing$SEASONS, levels=c("Winter", "Spring", "Summer","Autumn"))
class(seoul_bike_sharing$HOUR)
class(seoul_bike_sharing$DATE)
class(seoul_bike_sharing$SEASONS)
sum(is.na(seoul_bike_sharing))
summary(seoul_bike_sharing)
#calculate how many holiday there are
holiday_count <- table(seoul_bike_sharing$HOLIDAY)
num_holiday <- holiday_count['Holiday']
num_holiday
#% if records fall on a holiday
num_holiday/(num_holiday +holiday_count['No Holiday']) 
#calculate the number of rainfall and snowfall by seasons
seasonal_total <- seoul_bike_sharing %>% 
  group_by(SEASONS) %>% 
  summarize(total_rainfall=sum(RAINFALL), total_snowfall=sum(SNOWFALL))
seasonal_total

seoul_bike_sharing %>%
  mutate(DATE = as.Date(DATE,format="%d/%m/%Y"))%>%
  ggplot(aes(x = as.Date(DATE), y = RENTED_BIKE_COUNT, color = HOUR, alpha=0.5)) +
  geom_point() +
  scale_x_date(date_labels = "%d/%m/%Y") +
  labs(x= "Date")

ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribution of Rented Bike Counts",
       x = "Rented Bike Count",
       y = "Density")
#Load the packages:
pacman::p_load(rlang, tidymodels, stringr, broom, ggplot2)

#Load the dataset
bike_sharing_df <- read.csv("seoul_bike_sharing_converted_normalized.csv")

bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY, -X.2, -X.1, -X, -HOUR, -SEASONS, -HOLIDAY)
colnames(bike_sharing_df)[c(34,35)] <- c("HOLIDAY", "NO_HOLIDAY")

library(rsample)

set.seed(1234)

data_split <- initial_split(bike_sharing_df, prop = 3/4) #set the training dataset with 75% of the original dataset

bike_train <- training(data_split)

bike_test <- testing(data_split)

# Task: Build a linear regression model using weather variables only
lm_model_weather <- lm(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL,
                       data=bike_train)
summary(lm_model_weather)

#Task: Build a linear regression model using all variables
lm_model_all <- lm(RENTED_BIKE_COUNT ~ .,
                   data=bike_train)
summary(lm_model_all)

# Use model to make prediction
lm_model_weather_pred <- predict(lm_model_weather, newdata = bike_test)
test_results_weather <- data.frame(PREDICTION=lm_model_weather_pred, TRUTH = bike_test$RENTED_BIKE_COUNT)

lm_model_all_pred <- predict(lm_model_all, newdata = bike_test)

test_results_all <- data.frame(PREDICTION = lm_model_all_pred, TRUTH = bike_test$RENTED_BIKE_COUNT)

summary(lm_model_weather)$r.squared #0.4303

summary(lm_model_all)$r.squared #0.6589

rmse_weather <- sqrt(mean((test_results_weather$TRUTH-test_results_weather$PREDICTION)^2))

rmse_all <- sqrt(mean((test_results_all$TRUTH-test_results_all$PREDICTION)^2))

print(rmse_weather) #474.6247

print(rmse_all) #361.9543


# create a data frame of coefficients
library("broom")
coef_df <- tidy(lm_model_all)
# plot the coefficients in a bar chart (coef plot.png)
ggplot(coef_df, aes(x = reorder(term, desc(abs(estimate))), y = abs(estimate))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  xlab("Predictor") +
  ylab("Coefficient") +
  ggtitle("Coefficients of Linear Model") +
  theme(plot.title = element_text(hjust = 0.5))

#load the packages:
pacman::p_load(tidymodels, tidyverse, stringr)

#define a linear regression model specification.
lm_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")
lm_spec

#split training and test data
set.seed(1234)
data.split <- initial_split(bike_sharing_df, prop=4/5)
bike_train <- training(data.split)
bike_test <- testing(data.split)

#(poly1.png)
ggplot(data=bike_train, aes(RENTED_BIKE_COUNT, TEMPERATURE)) +
  geom_point() #nonlinearity -> polynomial regression 

# (poly2.png)
ggplot(data=bike_train, aes(RENTED_BIKE_COUNT, TEMPERATURE)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, color="red") +
  geom_smooth(method="lm", formula = y~poly(x,2), color="yellow") +
  geom_smooth(method="lm", formula = y~poly(x,2), color="green") +
  geom_smooth(method="lm", formula = y~poly(x,2), color="blue")


# Fit a linear model with higher order polynomial on some important variables 
lm_poly <- lm(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) +
                poly(HUMIDITY, 4)+
                poly(RAINFALL,2), data = bike_train)
summary(lm_poly$fit)

lm_poly_pred <- predict(lm_poly, newdata = bike_test) #predict
test_results_poly = data.frame(PREDICTION = lm_poly_pred, TRUTH = bike_test$RENTED_BIKE_COUNT) #create df for test results

#convert all negative prediction to 0 (RENTED_BIKE_COUNT can't be negative)
test_results_poly <- test_results_poly %>% 
  mutate(PREDICTION = ifelse(PREDICTION <0, 0, PREDICTION))

#calculate R_squared and RMSE (better than lm_weather but worse than lm_all)
summary(lm_poly)$r.squared #0.4861

rmse_poly <- sqrt(mean ( (test_results_poly$TRUTH - test_results_poly$PREDICTION)^2) )
rmse_poly #451.7091

#Task: Add Interaction Terms
lm_poly_interaction <- lm(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(HUMIDITY, 4)+poly(RAINFALL,2)+
                            RAINFALL*HUMIDITY + TEMPERATURE*HUMIDITY,
                          data = bike_train)
summary(lm_poly_interaction)

library(car)
lm_poly_interaction_pred <- predict(lm_poly_interaction, newdata = bike_test)

test_results_poly_interaction <- data.frame(PREDICTION = lm_poly_interaction_pred, TRUTH=bike_test$RENTED_BIKE_COUNT)

#model performance (improved model)
summary(lm_poly_interaction)$r.squared #0.5086

rmse_poly_interaction <- rmse(test_results_poly_interaction, TRUTH, PREDICTION )
rmse_poly_interaction #442

pacman::p_load(glmnet, yardstick)

#prediction function
model_prediction <- function(lm_model, test_data) {
  results <- lm_model %>% 
    predict(new_data=test_data) %>% 
    mutate(TRUTH=test_data$RENTED_BIKE_COUNT)
  results[results<0] <-0
  return(results)
}

#model evaluation function
model_evaluation <- function(results) {
  rmse = rmse(results, truth=TRUTH, estimate=.pred)
  rsq = rsq(results, truth=TRUTH, estimate=.pred)
  print(rmse)
  print(rsq)
}

#Use grid to define the best penalty (lambda)
penalty_value <- 10^seq(-4,4, by = 0.5) #penalty values ranging from 10^-4 to 10^4
x = as.matrix(bike_train[,-1]) #define a matrix for CV
y= bike_train$RENTED_BIKE_COUNT

cv_ridge <- cv.glmnet(x,y, alpha = 0, lambda = penalty_value, nfolds = 10)
cv_lasso <- cv.glmnet(x,y, alpha = 1, lambda = penalty_value, nfolds = 10)
cv_elasticnet <- cv.glmnet(x,y, alpha = 0.5, lambda = penalty_value, nfolds = 10)

#glmnet spec (using CV above, best optimal is 0.3 and 0.5)
glmnet_spec <- linear_reg(penalty = 0.3, mixture=0.5) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

#Fit the model (best model)
glmnet_best <- glmnet_spec %>% 
  fit(RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY*TEMPERATURE + SPRING*SUMMER*HOLIDAY*HOUR_18* HOUR_19* HOUR_8* HOUR_21* HOUR_20* HOUR_4 + 
        poly(RAINFALL, 8) + poly(HUMIDITY, 5) +  poly(TEMPERATURE, 5) + poly(DEW_POINT_TEMPERATURE, 5) + poly(SOLAR_RADIATION, 5) + poly(SNOWFALL,5) + 
        SPRING + SUMMER  + HOLIDAY + WIND_SPEED + VISIBILITY + 
        HOUR_18+ HOUR_4 + HOUR_5 + HOUR_3 + HOUR_19 + HOUR_11 + HOUR_8 + HOUR_21 + HOUR_10 + HOUR_2 + HOUR_20,
      data = bike_train)

glmnet_best_pred <- model_prediction(glmnet_best, bike_test)
model_evaluation(glmnet_best_pred) #rsq = 0.783, rmse = 296


glmnet_best_rsq = rsq(glmnet_best_pred, truth = TRUTH, estimate = .pred)
glmnet_best_rmse = rmse(glmnet_best_pred, truth = TRUTH, estimate = .pred)

# Fit the model (with top 10 coeficients)
glmnet_top10 <- glmnet_spec %>% 
  fit(RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY*TEMPERATURE + SPRING*SUMMER + SUMMER +
        poly(RAINFALL, 6) + poly(HUMIDITY, 5) +  poly(TEMPERATURE, 5) + poly(DEW_POINT_TEMPERATURE,5) + 
        HOUR_18 + HOUR_4 + HOUR_5 +HOUR_3,
      data=bike_train
  )
glmnet_top10_pred <- model_prediction(glmnet_top10, bike_test)
model_evaluation(glmnet_top10_pred) #rsq = 0.640, rmse = 381 (not good)

glmnet_top10_rsq = rsq(glmnet_top10_pred, truth = TRUTH, estimate = .pred)
glmnet_top10_rmse = rmse(glmnet_top10_pred, truth = TRUTH, estimate = .pred)

# Fit Ridge Regression
glmnet_ridge <- glmnet(x,y, alpha=0)
glmnet_ridge_pred <- predict(glmnet_ridge, s=cv_ridge$lambda.min,
                             newx = as.matrix(bike_test[,-1]))

ridge_rmse = sqrt(mean( (bike_test[,1] - glmnet_ridge_pred)^2))
ridge_rmse #365.06

ridge_mse = mean( (bike_test[,1] - glmnet_ridge_pred)^2)
ridge_rsq = 1 - ridge_mse / var(bike_test[,1])
ridge_rsq #0.667

# Fit Lasso
glmnet_lasso <- glmnet(x,y,alpha=1)
glm_lasso_pred <- predict(glmnet_lasso, s=cv_lasso$lambda.min,
                          newx=as.matrix(bike_test[,-1]))

lasso_rmse = sqrt(mean( (bike_test[,1] - glm_lasso_pred)^2))
lasso_rmse #364.0492

lasso_mse = mean( (bike_test[,1] - glm_lasso_pred)^2)
lasso_rsq = 1 - lasso_mse/var(bike_test[,1])
lasso_rsq #0.6693


# Fit Elastic Net
glmnet_elasticnet <- glmnet(x,y,alpha=0.7)
glm_elasticnet_pred <- predict(glmnet_elasticnet, s=cv_elasticnet$lambda.min,
                               newx=as.matrix(bike_test[,-1]))

elasticnet_rmse = sqrt(mean( (bike_test[,1] - glm_elasticnet_pred)^2))
elasticnet_rmse #364.0468


elasticnet_mse = mean( (bike_test[,1] - glm_elasticnet_pred)^2)
elasticnet_rsq = 1 - elasticnet_mse/var(bike_test[,1])
elasticnet_rsq #0.6693


#Create data frame for group bar chart 
model <- c(rep("glmnet_best",2), rep("glmnet_top10",2), 
           rep("glmnet_ridge",2), rep("glmnet_lasso",2), rep("glmnet_elasticnet",2))
stat <- rep(c("RSQ", "RMSE"),5)
value <- c(glmnet_best_rsq$.estimate, glmnet_best_rmse$.estimate,
           glmnet_top10_rsq$.estimate, glmnet_top10_rmse$.estimate,
           ridge_rsq, ridge_rmse,
           lasso_rsq,lasso_rmse,
           elasticnet_rsq, elasticnet_rmse)
model_df <-  data.frame(model, stat, value)
print(model_df)


# Create group bar chart for rsq and rmse (model evaluation.png)
model_df %>% 
  ggplot(aes(fill=stat, x=model, y=value)) +
  geom_bar(position="dodge", stat="identity")


# Create a Q-Q chart for best model: glmnet_best (Q-Q chart.png)
glmnet_best_pred %>% 
  ggplot() +
  stat_qq(aes(sample=TRUTH), color='green') +
  stat_qq(aes(sample=.pred), color='red')

