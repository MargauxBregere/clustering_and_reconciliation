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

alpha = 0.98
for(s in 2:ncol(meteo)){
  meteo_smooth[,s] <- exp_smooth(meteo[,s], alpha = alpha )
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
    meteo_smooth %>% gather(-date, key = 'station', value = 'temperature_smooth'), by = c('date', 'station'))

print(summary(data %>% filter(lubridate::year(date) > 2018)))
print(paste0("Number of AREA: ", length(unique(data$area))))
print(paste0("Number of weather stations: ", length(unique(data$station))))



graph_temp <- data %>% filter(area  == 'AREA60') %>% filter(lubridate::year(date) == 2019) %>% 
  dplyr::select(date, station, temperature, temperature_smooth) %>% 
  gather(-c(date,station), key = 'type', value = 'temperature')
 

g <- graph_temp %>% 
  ggplot(aes(x = date, y = temperature, colour =  type)) + geom_line()  + 
  theme_classic() + 
  xlab('') + ylab('Temperature (°C)') +
  theme(
    # axis.title.x=element_blank(),
    # axis.text.x=element_blank(),
    # axis.ticks.x=element_blank(),
    legend.title = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15) ,
    # legend.position = c(0.8,0.05),
    legend.direction = "horizontal",
    legend.position = 'bottom',
    #legend.justification = c("right", "bottom"),
    #legend.box.just = "right",
    #legend.margin = margin(6, 6, 6, 6),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = my_colors,
                     labels = c("Observed temperature", "Smoothed temperature")) 

pdf(paste0("graphs/temperature",unique(graph_temp$station),"_2019.pdf"), width = 10, height = 5) 
print(g)
dev.off()

data_graph <- data
data_graph$period <- NA 
data_graph$period[lubridate::year(data_graph$date) > 2018] <- '0Training data'
data_graph$period[lubridate::year(data_graph$date) > 2020] <- '1Calibration data'
data_graph$period[lubridate::year(data_graph$date) > 2021] <- '2Testing data'

data_graph$area <- stringr::str_remove(data_graph$area, "AREA")
data_graph <- data_graph %>% na.omit()
box_plot <- data_graph %>% ggplot(aes( x = area, y = load, colour = period)) + geom_boxplot() + 
  theme_classic() + 
  xlab('Area number') + ylab('Electrical demand (MW)') +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12) ,
    # legend.position = c(0.8,0.05),
    legend.direction = "horizontal",
    legend.position = 'bottom',
    #legend.justification = c("right", "bottom"),
    #legend.box.just = "right",
    #legend.margin = margin(6, 6, 6, 6),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = my_colors,
                     labels = c("Training data", "Calibration data", "Testing data")) 

pdf(paste0("graphs/boxplot.pdf"), width = 15, height = 5) 
print(box_plot)
dev.off()