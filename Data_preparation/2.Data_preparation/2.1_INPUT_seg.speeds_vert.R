outl <- 0
run <- run + 1

if(run == 1){
  
  # Exclude segment speeds above threshold
  index_outl <- which(data$seg.speed_vert > seg.speed.threshold)
  length(index_outl)
  
  if(length(index_outl) >= 1){
    outl <- 1
    print(paste("Outlier index:", paste(index_outl, collapse = ", ")))
    
    data <- data[-index_outl,]
  }
}

if(run > 1){
  
  # Recalculate delta height (meters)
  data$dheight <- c(NA, abs(data$height.above.msl_GPS[2:nrow(data)] - data$height.above.msl_GPS[1:(nrow(data)-1)])) # in m
  index_ID.switch <- which(data$individual.global.identifier[2:nrow(data)] != data$individual.global.identifier[1:(nrow(data)-1)]) + 1
  if(length(index_ID.switch) != 0){data$dheight[index_ID.switch] <- NA}

  # Recalculate delta time (minutes)
  data$dtime_new <- NA
  data$dtime_new[2:nrow(data)] <- (as.numeric(data$timestamp[2:nrow(data)]) - 
                                     as.numeric(data$timestamp[1:(nrow(data)-1)])) / 60
  if(length(index_ID.switch) != 0){data$dtime_new[index_ID.switch] <- NA}
  
  # Recalculate vertical segment speed
  data$seg.speed_vert <- data$dheight / (data$dtime_new * 60) # m/s
  
  # Histograms
  hist(data$seg.speed_vert); data$seg.speed_vert[order(data$seg.speed_vert, decreasing = T)][1:50]
  hist(data$seg.speed_vert[which(data$seg.speed_vert > 10 & data$seg.speed_vert < 80)])
  
  # Exclude segment speeds above threshold
  index_outl <- which(data$seg.speed_vert > seg.speed.threshold)
  length(index_outl)
  
  if(length(index_outl) >= 1){
    outl <- 1
    print(paste("Outlier index:", paste(index_outl, collapse = ", ")))
    
    data <- data[-index_outl,]
  }
}

n_outl_collect <- c(n_outl_collect, length(index_outl))
