outl <- 0
run <- run + 1

if(run == 1){
  
  # Exclude segment speeds above threshold
  index_outl <- which(data$seg.speed_2D > seg.speed.threshold)
  length(index_outl)
  
  if(length(index_outl) >= 1){
    outl <- 1
    print(paste("Outlier index:", paste(index_outl, collapse = ", ")))
    
    data <- data[-index_outl,]
  }
}

if(run > 1){
  
  # Recalculate 2D delta distance (meters)
  longlat <- cbind(data$location.long[2:nrow(data)], data$location.lat[2:nrow(data)])
  coord <- SpatialPoints(longlat, proj4string=CRS("EPSG:4326"))
  longlat_min1 <- cbind(data$location.long[1:(nrow(data)-1)], data$location.lat[1:(nrow(data)-1)])
  coord_min1 <- SpatialPoints(longlat_min1, proj4string=CRS("EPSG:4326"))
  
  data$ddist_2D <- c(NA, distGeo(p1 = coord, p2 = coord_min1)) # in m
  index_ID.switch <- which(data$individual.global.identifier[2:nrow(data)] != data$individual.global.identifier[1:(nrow(data)-1)]) + 1
  if(length(index_ID.switch) != 0){data$ddist_2D[index_ID.switch] <- NA}
  
  # Recalculate delta time (minutes)
  data$dtime_new <- NA
  data$dtime_new[2:nrow(data)] <- (as.numeric(data$timestamp[2:nrow(data)]) - 
                                     as.numeric(data$timestamp[1:(nrow(data)-1)])) / 60
  if(length(index_ID.switch) != 0){data$dtime_new[index_ID.switch] <- NA}
  
  # Recalculate segment speed
  data$seg.speed_2D <- data$ddist_2D / (data$dtime_new * 60)
  
  # Histograms
  hist(data$seg.speed_2D); data$seg.speed_2D[order(data$seg.speed_2D, decreasing = T)][1:50]
  hist(data$seg.speed_2D[which(data$seg.speed_2D > 10 & data$seg.speed_2D < 80)])
  
  # Exclude segment speeds above threshold
  index_outl <- which(data$seg.speed_2D > seg.speed.threshold)
  length(index_outl)
  
  if(length(index_outl) >= 1){
    outl <- 1
    print(paste("Outlier index:", paste(index_outl, collapse = ", ")))
    
    data <- data[-index_outl,]
  }
}

n_outl_collect <- c(n_outl_collect, length(index_outl))