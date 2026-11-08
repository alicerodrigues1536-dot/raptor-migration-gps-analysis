#######################################
##               RDATA
##       OUTPUT 2.1 ALL SUB5 & HF
#######################################
rm(list=ls())    # effacement de l'historique R

# Libraries

library(here)

library(sp)
library(geosphere) # For distance calculations
library(suntools)

load(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_all_A_2026-11-08.RData"))
load(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_sub5_A_2026-11-08.RData"))
load(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_HF_A_2026-11-08.RData"))

# 2.1_OUTPUT_STE_France_processed_all_A_2026-11-08.RData

# Prepare day-table ####

days_collect <- NULL
bird_collect <- NULL
start.time <- Sys.time()
for(i in unique(data$individual.local.identifier)){
  
  range(data$timestamp[which(data$individual.local.identifier == i)])
  days <- as.POSIXct(strptime(unique(substr(data$timestamp[which(data$individual.local.identifier == i)],1,10)),
                              format="%Y-%m-%d", tz="GMT"))
  
  # Sequence from first to last day (gap-proof)
  days_by <- substr(seq(days[1], days[length(days)], 24*60*60), 1, 10)
  
  days_collect <- c(days_collect, days_by)
  bird_collect <- c(bird_collect, rep(i, length(days_by)))
}
delta.time <- Sys.time() - start.time; delta.time # 2 min -> 4.5min

ov_days <- data.frame(individual.local.identifier = bird_collect, date = days_collect)
nrow(ov_days) # 4866 -> 7258

ov_days$bird_day <- paste(ov_days$individual.local.identifier, ov_days$date)

data$bird_day <- paste(data$individual.local.identifier, substr(data$timestamp, 1, 10))

n_fixes_tab <- summary(factor(data$bird_day), maxsum = 5000)
n_fixes_tab <- data.frame(bird_day = names(n_fixes_tab), n_fixes = n_fixes_tab)
ov_days <- merge(ov_days, n_fixes_tab, "bird_day", all.x = T)
ov_days$n_fixes[which(is.na(ov_days$n_fixes))] <- 0
length(which(ov_days$n_fixes == 0)) # 39 days without positions -> 2259??
length(which(ov_days$n_fixes > 0)) # 4827 days with positions -> 4999 



# Check:
load(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_additional_tables_2026-11-08.RData"))
sum(ov_ind$n_days) # 4825 -> 8567 : -> Where does the difference come from?! -> ov_ind is on day-positions only!

rm(n_fixes_tab, bird_collect, days, days_by, days_collect, delta.time, i, start.time)

# Original order of bird-days to check if maintained through merge()
bird_day_order_orig <- ov_days$bird_day


# Variant 1: Distance between night roosts ####

# Define new day variable for matching:
# For tracking data: Timestamp rounded to nearest start of day

sec_p.day <- 24*60*60
ov_days$date_num_rnd <- as.numeric(as.POSIXct(ov_days$date, "GMT"))
ov_days$bird_day_2 <- paste(ov_days$individual.local.identifier,
                            as.numeric(as.POSIXct(ov_days$date, "GMT")))

data$date_num_rnd <- round(as.numeric(data$timestamp) / sec_p.day) * sec_p.day
data$bird_day_2 <- paste(data$individual.local.identifier, data$date_num_rnd)

# Check:
index_rand <- sample(1:nrow(data),5)
data$bird_day_2[index_rand]
data$timestamp[index_rand]
as.POSIXct(data$date_num_rnd[index_rand], "GMT")
# Works fine

# Retrieve information on "night before the given day"

data_night <- data[which(data$daynight_6 == "night"),]

ov_days$night_bf_lon_avg <- ov_days$night_bf_lat_avg <- ov_days$night_bf_n_fixes <- NA
for(i in 1:nrow(ov_days)){
  index <- which(data_night$bird_day_2 == ov_days$bird_day_2[i])
  ov_days$night_bf_n_fixes[i] <- length(index)
  ov_days$night_bf_lat_avg[i] <- median(data_night$location.lat[index], na.rm = T)
  ov_days$night_bf_lon_avg[i] <- median(data_night$location.lon[index], na.rm = T)
}
length(which(ov_days$night_bf_n_fixes == 0)) # 130 -> OK! -> 186

ov_days$n_fixes[which(ov_days$night_bf_n_fixes == 0)]
# Quite some days with positions, but no night positions! -> Problem!? -> 0

summary(ov_days$night_bf_lat_avg)
summary(ov_days$night_bf_lon_avg)

# Copy information to "night after the given day"

ov_days$night_af_n_fixes <- c(ov_days$night_bf_n_fixes[2:nrow(ov_days)], NA)
ov_days$night_af_lat_avg <- c(ov_days$night_bf_lat_avg[2:nrow(ov_days)], NA)
ov_days$night_af_lon_avg <- c(ov_days$night_bf_lon_avg[2:nrow(ov_days)], NA)

# Treat last days per individual separately:
# First set night after to NA
index_ID.switch <- which(ov_days$individual.local.identifier[2:nrow(ov_days)] != ov_days$individual.local.identifier[1:(nrow(ov_days)-1)]) + 1
if(length(index_ID.switch) != 0){
  ov_days$night_af_n_fixes[index_ID.switch-1] <- NA
  ov_days$night_af_lat_avg[index_ID.switch-1] <- NA
  ov_days$night_af_lon_avg[index_ID.switch-1] <- NA
}

# Do new calculation for these last days
for(i in index_ID.switch-1){
  index <- which(data_night$bird_day_2 == paste(ov_days$individual.local.identifier[i],
                                                as.numeric(as.POSIXct(ov_days$date[i], "GMT")) + sec_p.day))
  print(length(index))
  ov_days$night_af_n_fixes[i] <- length(index)
  ov_days$night_af_lat_avg[i] <- median(data_night$location.lat[index], na.rm = T)
  ov_days$night_af_lon_avg[i] <- median(data_night$location.lon[index], na.rm = T)
}
# View(ov_days[c(index_ID.switch-1)[1],])

ov_days$n_fixes[which(ov_days$night_af_n_fixes == 0)]
# Quite some days with positions, but no night positions! -> Problem!?

# Check on map
ID <- unique(data$individual.local.identifier)[4]
index <- which(ov_days$individual.local.identifier == ID)
plot(data$location.lat[index] ~ data$location.long[index], type = "l", xlab = "Longitude", ylab = "Latitude", main = unique(data$individual.local.identifier)[4])
index <- which(ov_days$individual.local.identifier == unique(data$individual.local.identifier)[4])
lines(ov_days$night_bf_lat_avg[index] ~ ov_days$night_bf_lon_avg[index], col = "red")
# -> Works!

rm(data_night, i, index, index_ID.switch, index_rand, sec_p.day)

# Delta distance between night before and night after
index_no.na <- which(ov_days$night_bf_n_fixes > 0 & ov_days$night_af_n_fixes > 0)
longlat_bf <- cbind(ov_days$night_bf_lon_avg[index_no.na], ov_days$night_bf_lat_avg[index_no.na])
coord_bf <- SpatialPoints(longlat_bf, proj4string=CRS("EPSG:4326"))
longlat_af <- cbind(ov_days$night_af_lon_avg[index_no.na], ov_days$night_af_lat_avg[index_no.na])
coord_af <- SpatialPoints(longlat_af, proj4string=CRS("EPSG:4326"))

ov_days$day_dist_1 <- NA
ov_days$day_dist_1[index_no.na] <- distGeo(p1 = coord_bf, p2 = coord_af)/1000 # in m
index_ID.switch <- which(ov_days$individual.local.identifier[2:nrow(ov_days)] != ov_days$individual.local.identifier[1:(nrow(ov_days)-1)]) + 1
if(length(index_ID.switch) != 0){ov_days$day_dist_1[index_ID.switch] <- NA}

hist(ov_days$day_dist_1)
summary(ov_days$day_dist_1) # Max: 604 km, 220 NA's
# View(ov_days[which(ov_days$day_dist_1 > 600),])

# Check for example:
# 4: Charline
# 12: Aupsie
# 6: Diva
ID <- unique(data$individual.local.identifier)[6]
index <- which(ov_days$individual.local.identifier == ID)
plot(ov_days$day_dist_1[index] ~ as.POSIXct(ov_days$date[index], "GMT"),
     type = "l", xlab = "Time", ylab = "Daily distance (km)", main = ID)

rm(ID, index, index_ID.switch, index_no.na, longlat_af, longlat_bf, coord_af, coord_bf)


# Variant 2: Distance between first and last position of day ####

index_first <- c(1, which(data$bird_day[2:nrow(data)] != data$bird_day[1:(nrow(data)-1)]) + 1)
index_last <- c(index_first[-1]-1, nrow(data))
table_prelim <- data.frame(
  bird_day_2 = data$bird_day_2[index_first],  # ← bird_day_2 !
  first_lat = data$location.lat[index_first],
  first_lon = data$location.long[index_first],
  last_lat = data$location.lat[index_last],
  last_lon = data$location.long[index_last]
)

ov_days <- merge(ov_days, table_prelim, "bird_day_2", all.x = T)
str(ov_days)

length(which(is.na(ov_days$first_lat))) # 39 days without positions -> 64 jours sans positions


# Add dtime between 1st and last position
dtime_first_last <- 
  (as.numeric(data$timestamp[index_last]) - as.numeric(data$timestamp[index_first])) / 60
summary(dtime_first_last)
sort(dtime_first_last)[1:100]

table_prelim <- data.frame(bird_day = data$bird_day[index_first], dtime_first_last = dtime_first_last)
ov_days <- merge(ov_days, table_prelim, "bird_day", all.x = T)

ov_days$n_fixes[which(ov_days$dtime_first_last == 0)] # Happens when there is only 1 position

# Transform into hours
if(median(ov_days$dtime_first_last, na.rm = T) > 1000){
  ov_days$dtime_first_last <- ov_days$dtime_first_last/60
}

# Distance between first and last fix

index_no.na <- which(ov_days$n_fixes > 0)
longlat_first <- cbind(ov_days$first_lon[index_no.na], ov_days$first_lat[index_no.na])
str(longlat_first)
coord_first <- SpatialPoints(longlat_first, proj4string=CRS("EPSG:4326"))
longlat_last <- cbind(ov_days$last_lon[index_no.na], ov_days$last_lat[index_no.na])
coord_last <- SpatialPoints(longlat_last, proj4string=CRS("EPSG:4326"))

ov_days$day_dist_2 <- NA
ov_days$day_dist_2[index_no.na] <- distGeo(p1 = coord_first, p2 = coord_last)/1000 # in m
index_ID.switch <- which(ov_days$individual.local.identifier[2:nrow(ov_days)] != ov_days$individual.local.identifier[1:(nrow(ov_days)-1)]) + 1
if(length(index_ID.switch) != 0){ov_days$day_dist_2[index_ID.switch] <- NA}

hist(ov_days$day_dist_2)
summary(ov_days$day_dist_2) # Max: 604 km, 51 NA's, 150 less than day_dist_1
# View(ov_days[which(ov_days$day_dist_2 > 600),])

# Check on map
ID <- unique(data$individual.local.identifier)[4]
index <- which(data$individual.local.identifier == ID)
dev.off()
plot(data$location.lat[index] ~ data$location.long[index], type = "l", xlab = "Longitude", ylab = "Latitude", main = unique(data$individual.local.identifier)[4])
index <- which(ov_days$individual.local.identifier == ID)
lines(ov_days$first_lat[index] ~ ov_days$first_lon[index], col = "red")
# -> Works!

plot(ov_days$day_dist_2 ~ ov_days$day_dist_1) # Virtually the same thing!
abline(0,1)

summary(abs(ov_days$day_dist_2 - ov_days$day_dist_1)) # Max. difference is 3.5 km!

# Check for example:
# 4: Charline
# 12: Aupsie
# 6: Diva
ID <- unique(data$individual.local.identifier)[6]
index <- which(ov_days$individual.local.identifier == ID)
plot(ov_days$day_dist_2[index] ~ as.POSIXct(ov_days$date[index], "GMT"),
     type = "l", xlab = "Time", ylab = "Daily distance (km)", main = ID)

rm(ID, index, index_ID.switch, index_no.na, longlat_first, longlat_last, coord_first, coord_last,
   index_first, index_last, dtime_first_last, table_prelim)


# Variant 3: Sum of segment distances ####

# Re-calculate 2D delta distance for subsampled data (meters)
longlat <- cbind(data_sub5$location.long[2:nrow(data_sub5)], data_sub5$location.lat[2:nrow(data_sub5)])
coord <- SpatialPoints(longlat, proj4string=CRS("EPSG:4326"))
longlat_min1 <- cbind(data_sub5$location.long[1:(nrow(data_sub5)-1)], data_sub5$location.lat[1:(nrow(data_sub5)-1)])
coord_min1 <- SpatialPoints(longlat_min1, proj4string=CRS("EPSG:4326"))

data_sub5$ddist_2D <- c(NA, distGeo(p1 = coord, p2 = coord_min1)) # in m
index_ID.switch <- which(data_sub5$individual.global.identifier[2:nrow(data_sub5)] != data_sub5$individual.global.identifier[1:(nrow(data_sub5)-1)]) + 1
if(length(index_ID.switch) != 0){data_sub5$ddist_2D[index_ID.switch] <- NA}

data_sub5$bird_day <- paste(data_sub5$individual.local.identifier, substr(data_sub5$timestamp, 1, 10))
data_sub5$bird_day

data_sub5_day <- data_sub5[which(data_sub5$daynight_6 == "day"),]
data_sub5_day

# Set ddist for first position of day to NA
index_day.switch <- which(data_sub5_day$bird_day[2:nrow(data_sub5_day)] != data_sub5_day$bird_day[1:(nrow(data_sub5_day)-1)]) + 1
ddist_2D_for_day_dist_3 <- data_sub5_day$ddist_2D
ddist_2D_for_day_dist_3[index_day.switch] <- NA

day_dist_3_tab <- tapply(ddist_2D_for_day_dist_3, factor(data_sub5_day$bird_day), sum, na.rm = T) / 1000
day_dist_3_tab <- data.frame(bird_day = names(day_dist_3_tab), day_dist_3 = day_dist_3_tab)
ov_days <- merge(ov_days, day_dist_3_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$day_dist_3))) # 40 days

# Add n_fixes_sub5_day
n_fixes_sub5_day_tab <- summary(factor(data_sub5_day$bird_day), maxsum = 5000)
n_fixes_sub5_day_tab <- data.frame(bird_day = names(n_fixes_sub5_day_tab), n_fixes_sub5_day = n_fixes_sub5_day_tab)
ov_days <- merge(ov_days, n_fixes_sub5_day_tab, "bird_day", all.x = T)
ov_days$n_fixes_sub5_day[which(is.na(ov_days$n_fixes_sub5_day))] <- 0
length(which(ov_days$n_fixes_sub5_day == 0)) # 40 days without positions
summary(ov_days$n_fixes_sub5_day) # Up to 200

# Check
summary(ov_days$day_dist_3)
hist(ov_days$day_dist_3)
sort(ov_days$day_dist_3, decreasing = T)[1:10]

plot(ov_days$day_dist_3 ~ ov_days$day_dist_1)
abline(0,1)

summary(ov_days$day_dist_3 - ov_days$day_dist_1)
index_neg <- which(ov_days$day_dist_3 - ov_days$day_dist_1 < 0) # 7 cases where dist_3 < dist_1.
# View(ov_days[index_neg,])
# Not a big problem, don't dive into this further.

rm(longlat, coord, longlat_min1, coord_min1,
   index_day.switch, ddist_2D_for_day_dist_3, day_dist_3_tab, index_ID.switch,
   n_fixes_sub5_day_tab, index_neg)


# Add country names ####

ov_days$countries <- NA
start.time <- Sys.time()
for(i in 1:nrow(ov_days)){
  index <- which(data$bird_day == ov_days$bird_day[i])
  ov_days$countries[i] <- paste(unique(data$country[index]), collapse = ", ")
}
delta.time <- Sys.time() - start.time; delta.time # 1.5 min
summary(factor(ov_days$countries))

rm(start.time, delta.time, i, index)


# Add latitudinal/longitudinal difference ####
# Based on first and last coordinates

ov_days$lat_diff <- ov_days$last_lat - ov_days$first_lat
hist(ov_days$lat_diff)
summary(ov_days$lat_diff)
ov_days$lon_diff <- ov_days$last_lon - ov_days$first_lon
hist(ov_days$lon_diff)
summary(ov_days$lon_diff)

# Mean lat/lon
ov_days$lat_mean <- (ov_days$last_lat + ov_days$first_lat)/2
hist(ov_days$lat_mean)
summary(ov_days$lat_mean)
ov_days$lon_mean <- (ov_days$last_lon + ov_days$first_lon)/2
hist(ov_days$lon_mean)
summary(ov_days$lon_mean)

# Check on map
ID <- unique(data$individual.local.identifier)[4]
index <- which(data$individual.local.identifier == ID)
plot(data$location.lat[index] ~ data$location.long[index], type = "l", xlab = "Longitude", ylab = "Latitude", main = unique(data$individual.local.identifier)[4])
index <- which(ov_days$individual.local.identifier == ID)
points(ov_days$lat_mean[index] ~ ov_days$lon_mean[index], col = "red")
# -> Works!

rm(ID, index)


# Average speed ####

# First variant: GPS speed
data_sub5_day_move <- data_sub5_day[which(data_sub5_day$move_2 == "move"),]
speed_GPS_median_tab <- tapply(data_sub5_day_move$ground.speed, factor(data_sub5_day_move$bird_day), median)
speed_GPS_median_tab <- data.frame(bird_day = names(speed_GPS_median_tab), speed_GPS_median = speed_GPS_median_tab)
ov_days <- merge(ov_days, speed_GPS_median_tab, "bird_day", all.x = T)
summary(ov_days$speed_GPS_median)
hist(ov_days$speed_GPS_median)
length(which(is.na(ov_days$speed_GPS_median))) # 192 days

# Add n_fixes_sub5_day_move
n_fixes_sub5_day_move_tab <- summary(factor(data_sub5_day_move$bird_day), maxsum = 5000)
n_fixes_sub5_day_move_tab <- data.frame(bird_day = names(n_fixes_sub5_day_move_tab), n_fixes_sub5_day_move = n_fixes_sub5_day_move_tab)
ov_days <- merge(ov_days, n_fixes_sub5_day_move_tab, "bird_day", all.x = T)
ov_days$n_fixes_sub5_day_move[which(is.na(ov_days$n_fixes_sub5_day_move))] <- 0
length(which(ov_days$n_fixes_sub5_day_move == 0)) # 192 days without positions
summary(ov_days$n_fixes_sub5_day_move) # Up to 149

rm(n_fixes_sub5_day_move_tab)

# Second variant: Segment speed

# Recalculate delta time for subsampled data (minutes)
data_sub5$dtime_new <- NA
data_sub5$dtime_new[2:nrow(data_sub5)] <- (as.numeric(data_sub5$timestamp[2:nrow(data_sub5)]) - 
                                             as.numeric(data_sub5$timestamp[1:(nrow(data_sub5)-1)])) / 60
index_ID.switch <- which(data_sub5$individual.global.identifier[2:nrow(data_sub5)] != data_sub5$individual.global.identifier[1:(nrow(data_sub5)-1)]) + 1
if(length(index_ID.switch) != 0){data_sub5$dtime_new[index_ID.switch] <- NA}

data_sub5$seg.speed_2D <- data_sub5$ddist_2D / (data_sub5$dtime_new * 60)
hist(data_sub5$seg.speed_2D)

# Set ddist for first position of day to NA
data_sub5_day <- data_sub5[which(data_sub5$daynight_6 == "day"),]
data_sub5_day

########################
nrow(data_sub5)  # Combien avant filtrage ?
table(data_sub5$daynight_6, useNA = "always")  # Répartition jour/nuit ?
nrow(data_sub5[which(data_sub5$daynight_6 == "day"),])  # Après filtrage ?
########################



index_day.switch <- which(data_sub5_day$bird_day[2:nrow(data_sub5_day)] != data_sub5_day$bird_day[1:(nrow(data_sub5_day)-1)]) + 1
seg.speed_2D_for_daily_stats <- data_sub5_day$seg.speed_2D
seg.speed_2D_for_daily_stats[index_day.switch] <- NA
hist(seg.speed_2D_for_daily_stats, breaks = 100)

speed_seg_median_tab <- tapply(seg.speed_2D_for_daily_stats, factor(data_sub5_day$bird_day), sum, na.rm = T) / 1000
speed_seg_median_tab <- data.frame(bird_day = names(speed_seg_median_tab), speed_seg_median = speed_seg_median_tab)
ov_days <- merge(ov_days, speed_seg_median_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$speed_seg_median))) # 40 days -> 56 jours

summary(ov_days$speed_seg_median)
hist(ov_days$speed_seg_median)
sort(ov_days$speed_seg_median, decreasing = T)[1:10]

# -> Is not really working, as I cannot identify "flight" segments really.
ov_days <- ov_days[,-which(colnames(ov_days) == "speed_seg_median")]

rm(index_ID.switch, index_day.switch, speed_seg_median_tab, seg.speed_2D_for_daily_stats,
   speed_GPS_median_tab)


# Proportion in flight ####

prop_move <- function(vec){
  x <- length(which(vec == "move")) / length(vec)
  return(x)
}

prop_move_tab <- tapply(data_sub5_day$move_2, factor(data_sub5_day$bird_day), prop_move) * 100
prop_move_tab <- data.frame(bird_day = names(prop_move_tab), prop_move = prop_move_tab)
ov_days <- merge(ov_days, prop_move_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$prop_move))) # 40 days -> 56 jours

summary(ov_days$prop_move)
hist(ov_days$prop_move)

plot(ov_days$prop_move ~ ov_days$day_dist_3)

rm(prop_move, prop_move_tab)


# Flight height parameters ####

# Remove very bad outliers?
# No, leave for now.
summary(data_HF$height.above.ground_GPS)
# sort(data_HF$height.above.ground_GPS)[1:100]
# sort(data_HF$height.above.ground_GPS, decreasing = T)[1:100]
# length(which(data_HF$height.above.ground_GPS < -100)) # 29
# 
# data_HF$height.above.ground_GPS_clean <-
#   ifelse(data_HF$height.above.ground_GPS > -100, data_HF$height.above.ground_GPS, NA)
# summary(data_HF$height.above.ground_GPS_clean)
# data_HF$height.above.msl_GPS_clean <-
#   ifelse(data_HF$height.above.ground_GPS > -100, data_HF$height.above.ground_GPS, NA)
# summary(data_HF$height.above.msl_GPS)

data_HF$bird_day <- paste(data_HF$individual.local.identifier, substr(data_HF$timestamp, 1, 10))
data_HF_move <- data_HF[which(data_HF$move_2 == "move"),]

height.above.ground_GPS_median_tab <- tapply(data_HF_move$height.above.ground_GPS, factor(data_HF_move$bird_day), median, na.rm = T)
height.above.ground_GPS_median_tab <- data.frame(bird_day = names(height.above.ground_GPS_median_tab), height.above.ground_GPS_median = height.above.ground_GPS_median_tab)
ov_days <- merge(ov_days, height.above.ground_GPS_median_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$height.above.ground_GPS_median))) # 4179 days -> 6257
summary(ov_days$height.above.ground_GPS_median)
hist(ov_days$height.above.ground_GPS_median)

height.above.ground_GPS_min_tab <- tapply(data_HF_move$height.above.ground_GPS, factor(data_HF_move$bird_day), min, na.rm = T)
height.above.ground_GPS_min_tab <- data.frame(bird_day = names(height.above.ground_GPS_min_tab), height.above.ground_GPS_min = height.above.ground_GPS_min_tab)
ov_days <- merge(ov_days, height.above.ground_GPS_min_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$height.above.ground_GPS_min))) # 4179 days
summary(ov_days$height.above.ground_GPS_min)
hist(ov_days$height.above.ground_GPS_min)

height.above.ground_GPS_max_tab <- tapply(data_HF_move$height.above.ground_GPS, factor(data_HF_move$bird_day), max, na.rm = T)
height.above.ground_GPS_max_tab <- data.frame(bird_day = names(height.above.ground_GPS_max_tab), height.above.ground_GPS_max = height.above.ground_GPS_max_tab)
ov_days <- merge(ov_days, height.above.ground_GPS_max_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$height.above.ground_GPS_max))) # 4179 days
summary(ov_days$height.above.ground_GPS_max)
hist(ov_days$height.above.ground_GPS_max)

height.above.ground_GPS_q1_tab <- tapply(data_HF_move$height.above.ground_GPS, factor(data_HF_move$bird_day), quantile, 0.01, na.rm = T)
height.above.ground_GPS_q1_tab <- data.frame(bird_day = names(height.above.ground_GPS_q1_tab), height.above.ground_GPS_q1 = height.above.ground_GPS_q1_tab)
ov_days <- merge(ov_days, height.above.ground_GPS_q1_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$height.above.ground_GPS_q1))) # 4179 days
summary(ov_days$height.above.ground_GPS_q1)
hist(ov_days$height.above.ground_GPS_q1)

height.above.ground_GPS_q99_tab <- tapply(data_HF_move$height.above.ground_GPS, factor(data_HF_move$bird_day), quantile, 0.99, na.rm = T)
height.above.ground_GPS_q99_tab <- data.frame(bird_day = names(height.above.ground_GPS_q99_tab), height.above.ground_GPS_q99 = height.above.ground_GPS_q99_tab)
ov_days <- merge(ov_days, height.above.ground_GPS_q99_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$height.above.ground_GPS_q99))) # 4179 days
summary(ov_days$height.above.ground_GPS_q99)
hist(ov_days$height.above.ground_GPS_q99)

rm(height.above.ground_GPS_median_tab, height.above.ground_GPS_min_tab,
   height.above.ground_GPS_max_tab, height.above.ground_GPS_q1_tab,
   height.above.ground_GPS_q99_tab)

# Altitude gain
alt_gain_tab <- tapply(data_HF_move$alt_gain, factor(data_HF_move$bird_day), sum, na.rm = T)
alt_gain_tab <- data.frame(bird_day = names(alt_gain_tab), alt_gain = alt_gain_tab)
ov_days <- merge(ov_days, alt_gain_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$alt_gain))) # 4179 days -> 6257

summary(ov_days$alt_gain) # Max. 17.8 km!
hist(ov_days$alt_gain)

# Add n_fixes_HF_move
data_HF_move_no.na <- data_HF_move[which(!is.na(data_HF_move$height.above.msl_GPS)),]
n_fixes_HF_move_tab <- summary(factor(data_HF_move_no.na$bird_day), maxsum = 5000)
n_fixes_HF_move_tab <- data.frame(bird_day = names(n_fixes_HF_move_tab), n_fixes_HF_move = n_fixes_HF_move_tab)
ov_days <- merge(ov_days, n_fixes_HF_move_tab, "bird_day", all.x = T)
ov_days$n_fixes_HF_move[which(is.na(ov_days$n_fixes_HF_move))] <- 0
length(which(ov_days$n_fixes_HF_move == 0)) # 4179 days without positions -> 6257
summary(ov_days$n_fixes_HF_move) # Up to 10900 -> 12077

# Add timespan HF_move
TS_HF_move_tab <- tapply(data_HF_move_no.na$dtime_orig, factor(data_HF_move_no.na$bird_day), sum, na.rm = T)
TS_HF_move_tab <- data.frame(bird_day = names(TS_HF_move_tab), TS_HF_move = TS_HF_move_tab)
ov_days <- merge(ov_days, TS_HF_move_tab, "bird_day", all.x = T)
ov_days$TS_HF_move[which(is.na(ov_days$TS_HF_move))] <- 0
length(which(ov_days$TS_HF_move == 0)) # 4179 days without positions
summary(ov_days$TS_HF_move) # Up to 307
summary(ov_days$TS_HF_move[ov_days$TS_HF_move > 0]) # Median 58 min -> 65

hist(ov_days$alt_gain/ov_days$n_fixes_HF_move)
hist(ov_days$alt_gain/ov_days$TS_HF_move)
plot(ov_days$alt_gain ~ ov_days$TS_HF_move, xlab = "Timespan of in-flight HF data (min)",
     ylab = "Altitude gain within HF data (m)", xaxs = "i", yaxs = "i", xpd = NA,
     main = "STE altitude gain")

# View(ov_days[which(ov_days$alt_gain/ov_days$TS_HF_move > 100),])
# All with quite short HF periods (6-7 min)

ID <- 4
index <- which(data_HF_move$bird_day ==
                 ov_days$bird_day[which(ov_days$alt_gain/ov_days$TS_HF_move > 100)][ID])
plot(data_HF_move$height.above.msl_GPS[index] ~ data_HF_move$timestamp[index])
sum(data_HF_move$dheight_pm[index][which(data_HF_move$dheight_pm[index] > 0)])

# -> Unrelated to height outliers! Instead, HF mainly collected during ascents!
# Solution for analysis: Remove days with unsufficient TS of HF data...

rm(ID, index, alt_gain_tab, alt_gain, TS_HF_move_tab, index_ID.switch,
   n_fixes_HF_move_tab, TS_HF_move_tab)


# Add general variables to table ####

colnames(data)
ov_days$age.1st.depl <- ov_days$year.1st.depl <-
  ov_days$sex <- ov_days$tag.local.identifier <- NA
for(i in 1:nrow(ov_days)){
  index <- which(ov_ind$individual.local.identifier == ov_days$individual.local.identifier[i])[1]
  ov_days$age.1st.depl[i] <- ov_ind$age.1st.depl[index]
  ov_days$year.1st.depl[i] <- substr(ov_ind$period[index],1,4)
  ov_days$sex[i] <- ov_ind$sex[index]
  ov_days$tag.local.identifier[i] <- ov_ind$tag.local.identifier[index]
}

summary(factor(ov_days$age.1st.depl))
summary(factor(ov_days$year.1st.depl))
summary(factor(ov_days$sex))
summary(factor(ov_days$tag.local.identifier))


# Calendar year
ov_days$age_CY <- NA
ov_days$age_CY[which(ov_days$age.1st.depl == "adult")] <- "adult"
ov_days$age_CY[which(ov_days$age.1st.depl == "1CY")] <-
  as.numeric(substr(ov_days$date[which(ov_days$age.1st.depl == "1CY")],1,4)) -
  as.numeric(ov_days$year.1st.depl[which(ov_days$age.1st.depl == "1CY")]) + 1
ov_days$age_CY[which(ov_days$age.1st.depl == "2CY")] <-
  as.numeric(substr(ov_days$date[which(ov_days$age.1st.depl == "2CY")],1,4)) -
  as.numeric(ov_days$year.1st.depl[which(ov_days$age.1st.depl == "2CY")]) + 2
summary(factor(ov_days$age_CY))
summary(factor(ov_days$age_CY[which(ov_days$individual.local.identifier == "Henri de la Trejasse")]))
# Works fine.

# Year
ov_days$DOY <- as.POSIXlt(ov_days$date)$yday + 1 # Plus 1 because 1st January has value 0 otherwise.
summary(ov_days$DOY)
hist(ov_days$DOY)

ov_days$age_year <- NA
# Use July 1st as threshold
ov_days$age_year[which(ov_days$DOY >= 182 & ov_days$age_CY != "adult")] <- ov_days$age_CY[which(ov_days$DOY >= 182 & ov_days$age_CY != "adult")]
ov_days$age_year[which(ov_days$DOY < 182 & ov_days$age_CY != "adult")] <- as.numeric(ov_days$age_CY[which(ov_days$DOY < 182 & ov_days$age_CY != "adult")]) - 1
ov_days$age_year[which(ov_days$age_CY == "adult")] <- ov_days$age_CY[which(ov_days$age_CY == "adult")]
summary(factor(ov_days$age_year))

rm(i, index)


# Ground level parameters ####

summary(data_sub5$ground.level_srtm)

# data_sub5$bird_day <- paste(data_sub5$individual.local.identifier, substr(data_sub5$timestamp, 1, 10))

ground.level_srtm_median_tab <- tapply(data_sub5$ground.level_srtm, factor(data_sub5$bird_day), median, na.rm = T)
ground.level_srtm_median_tab <- data.frame(bird_day = names(ground.level_srtm_median_tab), ground.level_srtm_median = ground.level_srtm_median_tab)
ov_days <- merge(ov_days, ground.level_srtm_median_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$ground.level_srtm_median)))
summary(ov_days$ground.level_srtm_median)
hist(ov_days$ground.level_srtm_median)

ground.level_srtm_min_tab <- tapply(data_sub5$ground.level_srtm, factor(data_sub5$bird_day), min, na.rm = T)
ground.level_srtm_min_tab <- data.frame(bird_day = names(ground.level_srtm_min_tab), ground.level_srtm_min = ground.level_srtm_min_tab)
ov_days <- merge(ov_days, ground.level_srtm_min_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$ground.level_srtm_min)))
summary(ov_days$ground.level_srtm_min)
hist(ov_days$ground.level_srtm_min)

ground.level_srtm_max_tab <- tapply(data_sub5$ground.level_srtm, factor(data_sub5$bird_day), max, na.rm = T)
ground.level_srtm_max_tab <- data.frame(bird_day = names(ground.level_srtm_max_tab), ground.level_srtm_max = ground.level_srtm_max_tab)
ov_days <- merge(ov_days, ground.level_srtm_max_tab, "bird_day", all.x = T)
length(which(is.na(ov_days$ground.level_srtm_max)))
summary(ov_days$ground.level_srtm_max)
hist(ov_days$ground.level_srtm_max)

rm(ground.level_srtm_median_tab, ground.level_srtm_min_tab, ground.level_srtm_max_tab)


# Proportion over sea ####

summary(factor(data_sub5$over_sea))

prop_1 <- function(vec){
  x <- length(which(vec == "1")) / length(vec)
  return(x)
}

prop_over_sea_tab <- tapply(data_sub5$over_sea, factor(data_sub5$bird_day), prop_1)
prop_over_sea_tab <- data.frame(bird_day = names(prop_over_sea_tab), prop_over_sea = prop_over_sea_tab)
ov_days <- merge(ov_days, prop_over_sea_tab, "bird_day", all.x = T)

summary(ov_days$prop_over_sea)
hist(ov_days$prop_over_sea)
length(which(ov_days$prop_over_sea > 0)) # 36 days with >= 1 positions over sea.
# View(ov_days[which(ov_days$prop_over_sea > 0),])

rm(prop_1, prop_over_sea_tab)


# Check order ####

all(bird_day_order_orig == ov_days$bird_day)
# OK, order is maintained.


# Re-arrange columns in daytable ####

colnames(ov_days)
colnames_rm <- c("date_num_rnd", "bird_day_2", "year.1st.depl", "age.1st.depl")
ov_days <- ov_days[,-which(colnames(ov_days) %in% colnames_rm)]

colnames(ov_days)
col_index_sort <- c(1:3,39,36:38,40,4,20,5:17,19,18,21:24,
                    26,25,27,34,35,28:33,41:44)
length(col_index_sort); length(unique(col_index_sort))
which(!(1:ncol(ov_days) %in% col_index_sort))
ov_days <- ov_days[,col_index_sort]

summary(ov_days)


# Export output ####

save(ov_days, file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.2_OUTPUT_STE_France_Daytable_"), Sys.Date(), ".RData", sep = ""))

write.table(ov_days,
            file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.2_OUTPUT_STE_France_Daytable_"), Sys.Date(), ".csv", sep = ""),
            row.names = F, sep = ",")
