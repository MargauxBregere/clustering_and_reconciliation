####################################################################
#                   Data Pre Processing 
####################################################################

# electricity demand data access: https://www.aeso.ca/market/market-and-system-reporting/data-requests/hourly-load-by-area-and-region/
data <- readRDS('data/data_canada.rds')

# weather data access: https://www.noaa.gov/weather
# read load and weather data 
stations <- readRDS('data/nearest_station.rds') %>% as.data.frame()
colnames(stations) <- c('area','station')

# FORT MCMURRAY AIRPORT not available - > COLD LAKE
stations$station[stations$station == "FORT MCMURRAY AIRPORT"] <- "COLD LAKE"

meteo <- readRDS('data/canada_weather.rds') %>%
  rename(date = DATETIME,
         station = STATION,
         temperature = TEMPERATURE) %>%
  dplyr::select(date,station, temperature)  %>%
  spread( key = 'station', value = 'temperature')

d_min = max(min(meteo$date), min(data$date))
d_max = min(max(meteo$date), max(data$date))

dates = data.frame(date = seq(d_min,d_max, by = 'hour'))

meteo <- meteo %>%
  right_join(dates, by = 'date') %>%
  arrange(date) %>%
  as.data.frame()
for(s in 2:ncol(meteo)){
  meteo[,s] <- zoo::na.approx(meteo[,s])
}

# exponential smoothing of temperature (to model inertia)
exp_smooth <- function(vec, alpha){
  if(alpha > 1 | alpha < 0){
    return('alpha has to be between 0 and 1')
  }else{
    smth <- rep(NA,length(vec))
    if( length(vec) > 0){
      smth[1] <- vec[1]
      if( length(vec) > 1){
        for (i in 2:length(vec)){
          smth[i] <- alpha*smth[i-1] + (1-alpha)*vec[i]
        }
      }
    }
    return(smth)
  }
}
meteo_smooth <- meteo
meteo_smooth_long <- meteo
meteo_smooth_short <- meteo

for(s in 2:ncol(meteo)){
  meteo_smooth[,s] <- exp_smooth(meteo[,s], alpha = 0.98)
  meteo_smooth_long[,s] <- exp_smooth(meteo[,s], alpha = 0.999)
  meteo_smooth_short[,s] <- exp_smooth(meteo[,s], alpha = 0.95)
}


data <- data  %>%
  right_join(dates, by = 'date') %>% 
  arrange(date)
for(s in 2:ncol(data)){
  data[,s] <- zoo::na.approx(data[,s], na.rm = FALSE)
}

data <-  data %>%
  gather(-date, key = 'area', value = 'load') %>%
  left_join(stations, by = 'area') 

data <- data %>%
  left_join(
    meteo %>% gather(-date, key = 'station', value = 'temperature'), by = c('date', 'station')) %>%
  left_join(
    meteo_smooth %>% gather(-date, key = 'station', value = 'temperature_smooth'), by = c('date', 'station'))%>%
  left_join(
    meteo_smooth_long %>% gather(-date, key = 'station', value = 'temperature_smooth_long'), by = c('date', 'station'))%>%
  left_join(
    meteo_smooth_short %>% gather(-date, key = 'station', value = 'temperature_smooth_short'), by = c('date', 'station'))
