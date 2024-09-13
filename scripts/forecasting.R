####################################################################
#                   Forecasting model estimation
####################################################################

# get hierarchy matrix 
Utemp <- readRDS('results/U_temp.rds')
Ust <- readRDS('results/U_st.rds')

# create aggregated load and weather data
nb_clust_temp = ncol(Utemp)
nb_clust_st = ncol(Ust)
Utemp$area    <- unique(data_train$area)
Utemp$cluster_temp <- 0
for(i in 1:nb_clust_temp){
  Utemp$cluster_temp <- Utemp[,i]*i + Utemp$cluster_temp
}
Ust$area    <- unique(data_train$area)
Ust$cluster_st <- 0
for(i in 1:nb_clust_st){
  Ust$cluster_st <- Ust[,i]*i + Ust$cluster_st
}

U <- Utemp %>% dplyr::select(area,cluster_temp) %>%
  left_join(Ust %>% dplyr::select(area,cluster_st), by = 'area')
U$cluster_st <- paste0('cluster_st_',U$cluster_st)
U$cluster_temp <- paste0('cluster_temp_',U$cluster_temp)
saveRDS(U, 'results/U.rds')

data_train <- data_train %>%
  left_join(U, by = 'area')
data_calib <- data_calib %>%
  left_join(U, by = 'area')
data_test <- data_test %>%
  left_join(U, by = 'area')


data_clust_temp <- data_train %>% group_by(date,cluster_temp) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  rename(area = cluster_temp)

data_clust_st <- data_train %>% group_by(date,cluster_st) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  rename(area = cluster_st)

data_all <- data_train %>% group_by(date) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE),, .groups = 'drop') %>%
  mutate(area = 'ALL')

data_train <- data_train %>% dplyr::select(-c(cluster_temp, cluster_st, station)) %>%
  bind_rows(data_clust_temp) %>%
  bind_rows(data_clust_st)%>%
  bind_rows(data_all) %>%
  as.data.frame()


data_clust_temp <- data_calib %>% group_by(date,cluster_temp) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  rename(area = cluster_temp)

data_clust_st <- data_calib %>% group_by(date,cluster_st) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  rename(area = cluster_st)
data_all <- data_calib %>% group_by(date) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  mutate(area = 'ALL')
data_calib <- data_calib %>% dplyr::select(-c(cluster_temp, cluster_st, station)) %>%
  bind_rows(data_clust_temp) %>%
  bind_rows(data_clust_st)%>%
  bind_rows(data_all) %>%
  as.data.frame()


data_clust_temp <- data_test %>% group_by(date,cluster_temp) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  rename(area = cluster_temp)

data_clust_st <- data_test %>% group_by(date,cluster_st) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  rename(area = cluster_st)

data_all <- data_test %>% group_by(date) %>%
  summarise(load = sum(load),
            temperature = mean(temperature, na.rm = TRUE),
            temperature_smooth = mean(temperature_smooth, na.rm = TRUE), .groups = 'drop') %>%
  mutate(area = 'ALL')

data_test <- data_test %>% dplyr::select(-c(cluster_temp, cluster_st, station)) %>%
  bind_rows(data_clust_temp) %>%
  bind_rows(data_clust_st)%>%
  bind_rows(data_all) %>%
  as.data.frame()

##########################
### FORECASTING MODELS ###
##########################
levels = unique(data_train$area)

data_train <- data_train %>% 
  mutate(hour = as.factor(lubridate::hour(date)),
         week_day = as.factor(lubridate::wday(date)),
         covid = date >= as.POSIXct(strptime("2020-05-01 00:00:00", "%Y-%m-%d %H:%M:%S"))) %>%
  as.data.frame()

data_calib <- data_calib %>% 
  mutate(hour = as.factor(lubridate::hour(date)),
         week_day = as.factor(lubridate::wday(date)),
         covid = date >= as.POSIXct(strptime("2020-05-01 00:00:00", "%Y-%m-%d %H:%M:%S"))) %>%
  as.data.frame()

data_test <- data_test %>% 
  mutate(hour = as.factor(lubridate::hour(date)),
         week_day = as.factor(lubridate::wday(date)),
         covid = date >= as.POSIXct(strptime("2020-05-01 00:00:00", "%Y-%m-%d %H:%M:%S"))) %>%
  as.data.frame()


prev_gam_train <- data.frame()
prev_gam_calib <- data.frame()
prev_gam_test <- data.frame()

for(l in levels){

  if(str_detect(l, 'AREA', negate = FALSE)){
    # GAM formula for bottom-level loads
    mod_l <- gam(load ~   as.factor(week_day):as.factor(hour) + as.factor(covid) - 1, data = data_train %>% filter(area == l))

  }else if(str_detect(l, 'cluster', negate = FALSE)){
    # GAM formula for middle-level loads
    mod_l <- gam(load ~  s(temperature_smooth, k = 5) + 
                   as.factor(week_day):as.factor(hour) + as.factor(covid)  - 1, data = data_train %>% filter(area == l))

  }else{
    # GAM formula for top-level load
    mod_l <- gam(load ~  s(temperature, k = 5)  + 
                   as.factor(week_day):as.factor(hour) + s(temperature_smooth, k = 5) + as.factor(covid) - 1, data = data_train %>% filter(area == l))
  }

  effects_train <- data_train %>% filter(area == l)
  effects_train$estimated_load <- predict(mod_l, newdata = effects_train)
  
  effects_calib <- data_calib %>% filter(area == l)
  effects_calib$estimated_load <- predict(mod_l, newdata = effects_calib)
  
  effects_test <- data_test %>% filter(area == l)
  effects_test$estimated_load <- predict(mod_l, newdata = effects_test)
  
  prev_gam_train <- prev_gam_train %>%
    bind_rows(effects_train)
  
  prev_gam_calib <- prev_gam_calib %>%
    bind_rows(effects_calib)
  
  prev_gam_test <- prev_gam_test %>%
    bind_rows(effects_test)
  
  print(paste0(l,' - MAPE Train = ', 100*mean(abs((effects_train$load - effects_train$estimated_load)/effects_train$load))))
  print(paste0(l,' - MAPE Calibration = ', 100*mean(abs((effects_calib$load - effects_calib$estimated_load)/effects_calib$load))))
  print(paste0(l,' - MAPE Test = ', 100*mean(abs((effects_test$load - effects_test$estimated_load)/effects_test$load))))
  print(paste0(l,' - RMSE Train = ', sqrt(mean((effects_train$load - effects_train$estimated_load)^2))))
  print(paste0(l,' - RMSE Calibration= ', sqrt(mean((effects_calib$load - effects_calib$estimated_load)^2))))
  print(paste0(l,' - RMSE Test = ', sqrt(mean((effects_test$load - effects_test$estimated_load)^2))))
}

saveRDS(prev_gam_train,'results/prev_gam_train.rds')
saveRDS(prev_gam_calib,'results/prev_gam_calib.rds')
saveRDS(prev_gam_test,'results/prev_gam_test.rds')



