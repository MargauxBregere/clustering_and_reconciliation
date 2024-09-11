####################################################################
#                       Reconciliation
####################################################################

prev <- readRDS('results/prev_gam_calib.rds')
prev_test <- readRDS('results/prev_gam_test.rds')
U <- readRDS('results/U.rds')

# Define S based on the various clustering approaches
n = nrow(U)
clusters_temp = unique(U$cluster_temp)
clusters_st = unique(U$cluster_st)
S_no_clust = rbind(rep(1,n),diag(n))
colnames(S_no_clust) <-  c("AREA13", "AREA17", "AREA18", "AREA19", "AREA20", "AREA21", 
                           "AREA22", "AREA23", "AREA24", "AREA25", "AREA26", "AREA27", "AREA28", 
                           "AREA29", "AREA30", "AREA31", "AREA32", "AREA33", "AREA34", "AREA35", 
                           "AREA36", "AREA37", "AREA38", "AREA39", "AREA4", "AREA40", "AREA42", 
                           "AREA43", "AREA44", "AREA45", "AREA46", "AREA47", "AREA48", "AREA49", 
                           "AREA52", "AREA53", "AREA54", "AREA55", "AREA56", "AREA57", "AREA6", 
                           "AREA60")

names_no_clust <- c('ALL',colnames(S_no_clust))

S_all = S_no_clust
S_st = S_no_clust
for(i in 1:length(clusters_st)){
  S_st = rbind(S_st, 1*(U$cluster_st == clusters_st[i]))
  S_all = rbind(S_all, 1*(U$cluster_st == clusters_st[i]))
}
names_st <- c(names_no_clust, clusters_st)
names_all <- c(names_no_clust, clusters_st)


S_temp = S_no_clust
for(i in 1:length(clusters_temp)){
  S_temp = rbind(S_temp, 1*(U$cluster_temp == clusters_temp[i]))
  S_all = rbind(S_all, 1*(U$cluster_temp == clusters_temp[i]))
}
names_temp <- c(names_no_clust, clusters_temp)
names_all <- c(names_all, clusters_temp)


# reconciliate predictions using OLS and MinT
for(OLS in c(TRUE,FALSE)){
  
  # express prediction and observation in matrices
  train_load <-  prev %>% dplyr::filter(area %in% names_all) %>% 
    dplyr::select(date, area, load) %>% 
    spread(key = 'area', value = 'load')

  train_estimated_load <-  prev %>% dplyr::filter(area %in% names_all) %>% 
    dplyr::select(date, area, estimated_load) %>% 
    spread(key = 'area', value = 'estimated_load')
  
  train_estimated_load_rec  = train_estimated_load
  
  
  test_load <-  prev_test %>% dplyr::filter(area %in% names_all) %>% 
    dplyr::select(date, area, load) %>% 
    spread(key = 'area', value = 'load')
  
  test_estimated_load <-  prev_test %>% dplyr::filter(area %in% names_all) %>% 
    dplyr::select(date, area, estimated_load) %>% 
    spread(key = 'area', value = 'estimated_load')
  
  test_estimated_load_rec = test_estimated_load
  
  # reconciliate for the different clustering approaches
  type = c('all', 'st','temp', 'no_clust')
  for(t in type){
    eval(expr = parse(text=paste0('names = names_',t)))
    if(OLS){
      # OLS : G = Id 
      G = diag(length(names))
    }else{
      # MinT : estimation of covariance matrix inverse
      G = matrix(NA, nrow = length(names), ncol = length(names))
      for(r in 1:length(names)){
        for(rr in r:length(names)){
          G[r,rr] <- cov(train_estimated_load[names[r]] - train_load[names[r]],train_estimated_load[names[rr]] - train_load[names[rr]])
          G[rr,r] <- G[r,rr] 
        }
      }
      G = ginv(G)
    }
    
    eval(expr = parse(text=paste0('P_',t,' = S_',t,'%*%ginv(t(S_',t,')%*%G%*%S_',t,')%*%t(S_',t,')%*%G')))
    eval(expr = parse(text=paste0('test_estimated_load_rec[,names_',t,'] = t(P_',t,'%*%t(as.matrix(test_estimated_load[,names_',t,'])))')))
    eval(expr = parse(text=paste0('train_estimated_load_rec[,names_',t,'] = t(P_',t,'%*%t(as.matrix(train_estimated_load[,names_',t,'])))')))
    eval(expr = parse(text=paste0('prev_rec_t <-  test_estimated_load_rec[,c(\'date\',  names_',t,')] %>%', 
                                  'gather(-date, key = \'area\', value = \'estimated_load_rec\') %>%',
                                  'mutate(reconciliation = t)')))
    if(t == "all"){
      prev_rec = prev_rec_t
    }else{
      prev_rec <- prev_rec %>% bind_rows(prev_rec_t)
    }
  }
  
  prev_rec <- prev_rec %>% left_join(prev_test %>% dplyr::select(date,area,load,estimated_load), by = c('date','area')) 
  prev_rec <- prev_rec[,c("date", "area", 
                            "load",  
                            "estimated_load",
                            "estimated_load_rec", 
                            "reconciliation")]
  
  if(OLS==FALSE){
    rec = 'MinT'
  }else{
    rec = 'OLS'
  }
  saveRDS(prev_rec, paste0('results/prev_rec_',rec,'.rds'))
}



names_area <- c("AREA13", "AREA17", "AREA18", "AREA19", "AREA20", "AREA21", "AREA22",
                "AREA23", "AREA24", "AREA25", "AREA26", "AREA27", "AREA28", "AREA29",
                "AREA30", "AREA31", "AREA32", "AREA33", "AREA34", "AREA35", "AREA36",
                "AREA37", "AREA38", "AREA39", "AREA4",  "AREA40", "AREA42", "AREA43",
                "AREA44", "AREA45", "AREA46", "AREA47", "AREA48", "AREA49", "AREA52",
                "AREA53", "AREA54", "AREA55", "AREA56", "AREA57", "AREA6",  "AREA60")

prev_MinT <- readRDS('results/prev_rec_MinT.rds')
prev_OLS <- readRDS('results/prev_rec_OLS.rds')

# Compute bottom predictions 
prev_BU <- prev_MinT %>% dplyr::filter(area %in% names_area) %>%
  dplyr::filter(reconciliation == 'no_clust') %>%
  group_by(date) %>%
  summarise(estimated_load_rec = sum(estimated_load),
            load = sum(load)) %>%
  mutate(area = 'ALL', reconciliation = 'no_clust')%>% 
  bind_rows(prev_MinT  %>% dplyr::filter(area %in% c("cluster_st_1", "cluster_st_2", "cluster_st_3",
                                                     "cluster_temp_1", "cluster_temp_2", "cluster_temp_3","cluster_temp_4", "cluster_temp_5")) %>%
              dplyr::filter(reconciliation %in% c( "st",  "temp")) %>%
              group_by(date, reconciliation) %>%
              summarise(estimated_load_rec = sum(estimated_load),
                        load = sum(load)) %>%
              mutate(area = 'ALL'))

results <- prev_MinT %>% dplyr::select(date, area,estimated_load_rec, reconciliation) %>% rename(MinT = estimated_load_rec) %>% 
  left_join(prev_OLS %>% dplyr::select(date, area, estimated_load_rec, reconciliation) %>% rename(OLS = estimated_load_rec), by = c('date', 'area', 'reconciliation')) %>% na.omit() %>% 
  gather(-c(date,area, reconciliation), value = 'estimated_load_rec', key = 'type') %>% 
  left_join(prev_MinT %>% dplyr::select(-estimated_load_rec), by = c('date', 'area', 'reconciliation'))

q_0001 = quantile(results$load,0.0001)

results <- results%>% 
  dplyr::filter(year(date) == 2022) %>%
  group_by(area, reconciliation, type) %>% 
  summarise(
    rmse = sqrt(mean((estimated_load - load)^2,na.rm = TRUE)),
    rmse_rec = sqrt(mean((estimated_load_rec - load)^2,na.rm = TRUE)),
    mape = 100*(mean(abs(estimated_load - load)/abs(pmax(load,q_0001)),na.rm = TRUE)),
    mape_rec = 100*(mean(abs(estimated_load_rec - load)/abs(pmax(load,q_0001)),na.rm = TRUE)))  %>% 
  mutate(better = (rmse > rmse_rec),
         better_percent = 100*(rmse - rmse_rec)/rmse,
         better_percent_p = 100*pmax(rmse - rmse_rec,0)/rmse,
         diff_rmse = (rmse - rmse_rec), 
         diff_mape = mape-mape_rec)

res <- results %>% 
  dplyr::filter(area %in% names_area) %>%
  group_by(reconciliation, type) %>%
  summarise(better = mean(better, na.rm = TRUE), 
            better_percent = mean(better_percent, na.rm = TRUE),
            better_percent_p = mean(better_percent_p, na.rm = TRUE),
            min_rmse = min(rmse),
            min_rmse_rec = min(rmse_rec),
            min_mape = min(mape),
            min_mape_rec = min(mape_rec),
            mean_rmse = mean(rmse),
            mean_rmse_rec = mean(rmse_rec),
            med_mape = median(mape),
            med_mape_rec = median(mape_rec),
            max_rmse = max(rmse),
            max_rmse_rec = max(rmse_rec),
            max_mape = max(mape),
            max_mape_rec = max(mape_rec), .groups = 'drop')


res_all <- prev_MinT %>% dplyr::select(date, area, estimated_load, estimated_load_rec, reconciliation, load) %>% 
  rename(MinT = estimated_load_rec) %>% 
  left_join(prev_OLS %>% dplyr::select(date, area, estimated_load_rec, reconciliation) %>% rename(OLS = estimated_load_rec), by = c('date', 'area', 'reconciliation'))%>% 
  left_join(prev_BU %>% dplyr::select(date, area, estimated_load_rec, reconciliation) %>% rename(BU = estimated_load_rec), by = c('date', 'area', 'reconciliation'))  %>% 
  na.omit()  %>% 
  dplyr::filter(area == 'ALL') %>% 
  gather(-c(date,area, reconciliation,estimated_load, load), value = 'estimated_load_rec', key = 'type')  %>%
  group_by(area, reconciliation, type) %>% 
  summarise(
    rmse = sqrt(mean((estimated_load - load)^2,na.rm = TRUE)),
    rmse_rec = sqrt(mean((estimated_load_rec - load)^2,na.rm = TRUE)),
    mape = 100*(mean(abs(estimated_load - load)/load,na.rm = TRUE)),
    mape_rec = 100*(mean(abs(estimated_load_rec - load)/load,na.rm = TRUE)), .groups = 'drop')  %>% 
  mutate(better = (rmse > rmse_rec),
         better_percent = 100*(rmse - rmse_rec)/rmse) 

print(res)
print(res_all)



for(l in c( 'AREA60', 'AREA36', 'ALL')){
  graph <- prev_MinT %>% dplyr::select(date, area, estimated_load, estimated_load_rec, reconciliation, load) %>% 
    rename(MinT = estimated_load_rec, AAload = load, ABestimated_load = estimated_load) %>% 
    left_join(prev_OLS %>% dplyr::select(date, area, estimated_load_rec, reconciliation) %>% rename(OLS = estimated_load_rec), by = c('date', 'area', 'reconciliation'))%>% 
    na.omit()
  if(l =='ALL'){
    graph <- graph %>% 
      left_join(prev_BU %>% dplyr::select(date, area, estimated_load_rec, reconciliation) %>% rename(ZBU = estimated_load_rec), by = c('date', 'area', 'reconciliation'))  %>% 
      na.omit()
    graph <- graph %>% filter(area == l) %>%
      filter(reconciliation == 'st') %>% 
      dplyr::select(date, ABestimated_load, MinT, OLS, ZBU, AAload) %>% 
      gather(-date, key = 'type', value = 'load')
  }else{
    graph <- graph %>% filter(area == l) %>%
      filter(reconciliation == 'st') %>% 
      dplyr::select(date, ABestimated_load, MinT, OLS, AAload) %>% 
      gather(-date, key = 'type', value = 'load')
  }
  
  
  g1 <- graph %>% filter(date <= as.POSIXct(strptime("2022-01-07 00:00:00", "%Y-%m-%d %H:%M:%S"))) %>%
    ggplot(aes(x = date, y = load, colour =  type)) + geom_line()  + 
    theme_classic() + 
    xlab('') + ylab('Load (MW)') +
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
      legend.background = element_rect(fill = "transparent"))
  
  g2 <- graph %>% filter(date >= as.POSIXct(strptime("2023-10-15 00:00:00", "%Y-%m-%d %H:%M:%S"))) %>%
    ggplot(aes(x = date, y = load, colour =  type)) + geom_line()  + 
    theme_classic() + 
    xlab('') + ylab('Load (MW)') +
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
      legend.background = element_rect(fill = "transparent"))
  
  if(l =='ALL'){
    g1 <- g1 +
      scale_color_manual(values = my_colors,
                         labels = c("Observation", "Benchmark", "MinT", "OLS", "Bottom-up")) 
    g2 <- g2 +
      scale_color_manual(values = my_colors,
                         labels = c("Observation", "Benchmark", "MinT", "OLS", "Bottom-up")) 
  }else{
    g1 <- g1 +
      scale_color_manual(values = my_colors,
                         labels = c("Observation", "Benchmark", "MinT", "OLS")) 
    g2 <- g2 +
      scale_color_manual(values = my_colors,
                         labels = c("Observation", "Benchmark", "MinT", "OLS")) 
  }
  
  pdf(paste0("graphs/prediction_",l,"_deb.pdf"), width = 10, height = 5) 
  print(g1)
  dev.off()
  pdf(paste0("graphs/prediction_",l,"_end.pdf"), width = 10, height = 5) 
  print(g2)
  dev.off()
  
}
