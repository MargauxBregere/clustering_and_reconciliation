####################################################################
#                      Space-Time Series Clustering
####################################################################

# Define the number of clusters:
k = 3  #(Reasonable elbow)

data_clust = data_train %>% 
  dplyr::select(date, area, load) %>% 
  spread(key = 'area', value = 'load')

shpprov <- st_read("data/AESO_Planning_Areas.shp")
shpprov <- as_Spatial(shpprov)
rswm_q <- poly2nb(shpprov, queen = TRUE)
set.ZeroPolicyOption(TRUE)
set.ZeroPolicyOption(TRUE)
Sp <- sp::coordinates(shpprov)
rownames(Sp) <- shpprov$Area_ID

canada_stations <- rio::import("data/canada_stations.rds")
canada_stations <- canada_stations[-c(10,28),] # 10 and 11 and 28 and 29 are almost the same

# Spatial distance based on geographical coordinates
D1 <- as.matrix(dist(Sp)) # 42 areas in Canada
data_clust <- data_clust[,c('date',paste0('AREA',shpprov$Area_ID))] # Data has same ordering as shape file


################# Computing time series distance with hourly data
# Define an appropriate distance for hourly time series
hourly_matrix <- data_clust

# Define the number of hours in a day
hours_per_day <- 24

data_clust <- data_clust %>% 
  mutate(hour = lubridate::hour(date), day = lubridate::date(date)) 

# Define the number of days
days <- nrow(data_clust)/hours_per_day
areas = paste0('AREA',shpprov$Area_ID)

# Initialize a list to store the results for each area
area_time_series <- vector("list", length = length(areas))
# Loop through each area
for (area_index in 1:length(areas)) {
  area_demand = array(data_clust[,areas[area_index]])[[1]]  
  area_time_series[[area_index]] <-  matrix(area_demand , nrow = days, ncol = hours_per_day, byrow = TRUE)
}

# area_time_series now contains a list of length 42, where each element is a matrix of dimension 1279 x 24,
# representing the daily time series for each of the 24 hours of the day for each of the 42 areas

names(area_time_series) <- areas

# Step 2: compute compute 24 pairwise distance matrices, one for each daily series

Dmatrices <- array(0, dim=c(length(areas),length(areas),24))

for (t in 1:24) {
  for (i in 1:length(areas)) {
    for (j in 1:length(areas)) {
      Dmatrices[i,j,t] <- sqrt(2 * (1 - cor(area_time_series[[i]][,t],area_time_series[[j]][,t])))
    }
  }
}


# Step 3: compute the compromise of each of the 24 distances

library(DistatisR)

model0 <- distatis(Dmatrices)
D0 <- model0$res4Splus$Splus

################### Clustering task
######## Time series distance A (hourly DISTATIS)
### Clustering areas with temporal information only

HCTemp <- hclust(as.dist(D0), method="ward.D2")
plot(seq(1:15),sort(HCTemp$height,decreasing = T)[1:15], type="b")
plot(HCTemp) 
rect.hclust(HCTemp,k=k)
temp_id <- cutree(HCTemp, k)
U_temp <- mclust::unmap(classification=temp_id)
rio::export(U_temp, "results/U_temp.rds")

# Plot the clusters and stations
shpprov$temp_id <- temp_id
pdf("graphs/temporal_clustering.pdf", width = 10, height = 15) 
sp::plot(shpprov, 
     col = my_colors[as.integer(shpprov$temp_id)],#shpprov$st_id,   
     main = "Temporal clustering",
     cex.main=2)
points(canada_stations$LONGITUDE, 
       canada_stations$LATITUDE, 
       pch = 16,   # Change to a different plotting symbol if needed
       col = "black")  # Change point color if needed
dev.off()

### Clustering areas with spatio-temporal information

choicealpha(as.dist(D0), as.dist(D1), seq(0,1,0.1), 3)

# Optimal alpha is...
alphastar <- 0.1

HCST <- hclustgeo(as.dist(D0), as.dist(D1), alpha=alphastar)
plot(seq(1:15),sort(HCST$height,decreasing = T)[1:15], type="b")
plot(HCST) 
rect.hclust(HCST,k=k)
st_id <- cutree(HCST, k)
table(st_id)
U_st <- mclust::unmap(classification=st_id)
rio::export(U_st, "results/U_st.rds")

# Plot the clusters and stations
shpprov@data$st_id <- as.factor(st_id)

pdf("graphs/spacial_temporal_clustering.pdf", width = 10, height = 15) 
sp::plot(shpprov, 
     col = my_colors[as.integer(shpprov$st_id)+3],#shpprov$st_id,   
     main = "Spatio-temporal clustering",
     cex.main=3)
points(canada_stations$LONGITUDE, 
       canada_stations$LATITUDE, 
       pch = 16,   # Change to a different plotting symbol if needed
       col = "black")  # Change point color if needed
dev.off()



