###################################
##              DATA
##         OUTPUT 2.1 2.2 
###################################
rm(list=ls())    # effacement de l'historique R


library(here)

library(sp)
library(geosphere) # For distance calculations

load(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_HF_A_2026-11-08.RData"))

# Daytable For comparison:
load(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.2_OUTPUT_STE_France_Daytable_2026-11-08.RData"))

# ===============================================================================================
# Découpe des segments HF

# Créer les segment_globals à partir du temps
# Etape 1
max_TS <- 15 # en minutes

data_HF$segment <- NA

for(id in unique(data_HF$HF.ID)){
  
  index <- which(data_HF$HF.ID == id)
  
  cum_time <- cumsum(data_HF$dtime_orig[index])
  
  data_HF$segment[index] <- floor(cum_time / max_TS)
}

# Etape 2
data_HF$HF_ID_seg <- paste(data_HF$HF.ID, data_HF$segment, sep = "_")
str(data_HF)

# Déjà calculé segment local
data_HF$HF_ID_seg <- paste(data_HF$HF.ID, data_HF$segment, sep = "_")

# numéro global unique par segment découpé
segments_unique <- unique(data_HF$HF_ID_seg)
segment_global <- seq_along(segments_unique)

# créer une nouvelle colonne segment_global
data_HF$segment_global <- match(data_HF$HF_ID_seg, segments_unique)

# vérifier
head(data_HF[, c("HF.ID", "segment", "HF_ID_seg", "segment_global")])
length(unique(data_HF$segment_global)) # doit être égal au nombre total de segments découpés


# ===========================================================================================
# Etape 3 : Calculs des métriques sur les segments au lieu de sur les ID.HF


# Prepare HF track table ####

ID_switch <- c(1, which(data_HF$segment_global[2:nrow(data_HF)] != data_HF$segment_global[1:(nrow(data_HF)-1)]) + 1)

colnames(data_HF)
colnames(ov_days)
ov_HF_tracks <- data_HF[ID_switch, c("segment_global", "individual.local.identifier",               # Ici regroupement par tracks de HF à partir de segment_global
                                     "tag.local.identifier", "individual.sex",
                                     "age_CY", "age_year", "timestamp")]

colnames(ov_HF_tracks)[which(colnames(ov_HF_tracks) == "individual.sex")] <- "sex"
colnames(ov_HF_tracks)[which(colnames(ov_HF_tracks) == "timestamp")] <- "timestamp_start"


# Add basic information ####

ov_HF_tracks$timestamp_end <- data_HF$timestamp[c(ID_switch[-1]-1, nrow(data_HF))]
ov_HF_tracks$date <- substr(ov_HF_tracks$timestamp_start,1,10)

ov_HF_tracks$bird_day <- paste(ov_HF_tracks$individual.local.identifier, ov_HF_tracks$date)

# n fixes
n_fixes_tab <- summary(factor(data_HF$segment_global), maxsum = 5000)
n_fixes_tab <- data.frame(segment_global = names(n_fixes_tab), n_fixes = n_fixes_tab)
ov_HF_tracks <- merge(ov_HF_tracks, n_fixes_tab, "segment_global", all.x = T)
summary(ov_HF_tracks$n_fixes) # Up to 11k

data_HF_move <- data_HF[which(data_HF$move == "move"),]
n_fixes_move_tab <- summary(factor(data_HF_move$segment_global), maxsum = 5000)
n_fixes_move_tab <- data.frame(segment_global = names(n_fixes_move_tab), n_fixes_move = n_fixes_move_tab)
ov_HF_tracks <- merge(ov_HF_tracks, n_fixes_move_tab, "segment_global", all.x = T)
ov_HF_tracks$n_fixes_move[is.na(ov_HF_tracks$n_fixes_move)] <- 0
summary(ov_HF_tracks$n_fixes_move) # Up to 7.7k

data_HF_sit <- data_HF[which(data_HF$move == "sit"),]
n_fixes_sit_tab <- summary(factor(data_HF_sit$segment_global), maxsum = 5000)
n_fixes_sit_tab <- data.frame(segment_global = names(n_fixes_sit_tab), n_fixes_sit = n_fixes_sit_tab)
ov_HF_tracks <- merge(ov_HF_tracks, n_fixes_sit_tab, "segment_global", all.x = T)
ov_HF_tracks$n_fixes_sit[is.na(ov_HF_tracks$n_fixes_sit)] <- 0
summary(ov_HF_tracks$n_fixes_sit) # Up to 7.8k

data_HF_move_2 <- data_HF[which(data_HF$move_2 == "move"),]
n_fixes_move_2_tab <- summary(factor(data_HF_move_2$segment_global), maxsum = 5000)
n_fixes_move_2_tab <- data.frame(segment_global = names(n_fixes_move_2_tab), n_fixes_move_2 = n_fixes_move_2_tab)
ov_HF_tracks <- merge(ov_HF_tracks, n_fixes_move_2_tab, "segment_global", all.x = T)
ov_HF_tracks$n_fixes_move_2[is.na(ov_HF_tracks$n_fixes_move_2)] <- 0
summary(ov_HF_tracks$n_fixes_move_2) # Up to 8.4k

data_HF_sit_2 <- data_HF[which(data_HF$move_2 == "sit"),]
n_fixes_sit_2_tab <- summary(factor(data_HF_sit_2$segment_global), maxsum = 5000)
n_fixes_sit_2_tab <- data.frame(segment_global = names(n_fixes_sit_2_tab), n_fixes_sit_2 = n_fixes_sit_2_tab)
ov_HF_tracks <- merge(ov_HF_tracks, n_fixes_sit_2_tab, "segment_global", all.x = T)
ov_HF_tracks$n_fixes_sit_2[is.na(ov_HF_tracks$n_fixes_sit_2)] <- 0
summary(ov_HF_tracks$n_fixes_sit_2) # Up to 7.2k

all(ov_HF_tracks$n_fixes_move + ov_HF_tracks$n_fixes_sit == ov_HF_tracks$n_fixes)
all(ov_HF_tracks$n_fixes_move_2 + ov_HF_tracks$n_fixes_sit_2 == ov_HF_tracks$n_fixes)

# Proportion in flight
ov_HF_tracks$prop_move <- ov_HF_tracks$n_fixes_move_2/ov_HF_tracks$n_fixes

summary(ov_HF_tracks$prop_move)
hist(ov_HF_tracks$prop_move)
# Wow, many with only flight positions!
# View(ov_HF_tracks[ov_HF_tracks$prop_move == 1,])
# summary(factor(ov_HF_tracks$countries[ov_HF_tracks$prop_move == 1]))


# Time span
TS_tab <- tapply(data_HF$dtime_orig, factor(data_HF$segment_global), sum, na.rm = T)
TS_tab <- data.frame(segment_global = names(TS_tab), TS = TS_tab)
ov_HF_tracks <- merge(ov_HF_tracks, TS_tab, "segment_global", all.x = T)
summary(ov_HF_tracks$TS)
sum(ov_HF_tracks$TS) / 60 # 1441 h

TS_move_tab <- tapply(data_HF_move_2$dtime_orig, factor(data_HF_move_2$segment_global), sum, na.rm = T)
TS_move_tab <- data.frame(segment_global = names(TS_move_tab), TS_move = TS_move_tab)
ov_HF_tracks <- merge(ov_HF_tracks, TS_move_tab, "segment_global", all.x = T)
summary(ov_HF_tracks$TS_move)
ov_HF_tracks$TS_move[is.na(ov_HF_tracks$TS_move)] <- 0
summary(ov_HF_tracks$TS_move)
sum(ov_HF_tracks$TS_move) / 60 # 921 h                                          


# Distance between first and last position ####

ov_HF_tracks$first_lat <- data_HF$location.lat[ID_switch]
ov_HF_tracks$first_lon <- data_HF$location.lon[ID_switch]
ov_HF_tracks$last_lat <- data_HF$location.lat[c(ID_switch[-1]-1, nrow(data_HF))]
ov_HF_tracks$last_lon <- data_HF$location.lon[c(ID_switch[-1]-1, nrow(data_HF))]

ID <- "1"
index <- which(data_HF$segment_global == ID)
plot(data_HF$location.lat[index] ~ data_HF$location.long[index], type = "l", xlab = "Longitude", ylab = "Latitude",
     main = ID)
index <- which(ov_HF_tracks$segment_global == ID)
lines(c(ov_HF_tracks$first_lon[index], ov_HF_tracks$last_lon[index]),
      c(ov_HF_tracks$first_lat[index], ov_HF_tracks$last_lat[index]), col = "red")

# Distance between first and last fix

longlat_first <- cbind(ov_HF_tracks$first_lon, ov_HF_tracks$first_lat)
coord_first <- SpatialPoints(longlat_first, proj4string=CRS("EPSG:4326"))
longlat_last <- cbind(ov_HF_tracks$last_lon, ov_HF_tracks$last_lat)
coord_last <- SpatialPoints(longlat_last, proj4string=CRS("EPSG:4326"))

ov_HF_tracks$dist_first_last <- NA
ov_HF_tracks$dist_first_last <- distGeo(p1 = coord_first, p2 = coord_last)/1000 # in km
summary(ov_HF_tracks$dist_first_last) # Max. 135


# Cumulative distance ####
# Sum of segment_global distances

# Re-calculate 2D delta distance for subsampled data (meters)
longlat <- cbind(data_HF$location.long[2:nrow(data_HF)], data_HF$location.lat[2:nrow(data_HF)])
coord <- SpatialPoints(longlat, proj4string=CRS("EPSG:4326"))
longlat_min1 <- cbind(data_HF$location.long[1:(nrow(data_HF)-1)], data_HF$location.lat[1:(nrow(data_HF)-1)])
coord_min1 <- SpatialPoints(longlat_min1, proj4string=CRS("EPSG:4326"))

data_HF$ddist_2D <- c(NA, distGeo(p1 = coord, p2 = coord_min1)) # in m
index_ID.switch <- which(data_HF$segment_global[2:nrow(data_HF)] != data_HF$segment_global[1:(nrow(data_HF)-1)]) + 1
if(length(index_ID.switch) != 0){data_HF$ddist_2D[index_ID.switch] <- NA}
hist(data_HF$ddist_2D)
summary(data_HF$ddist_2D)

cum_dist_tab <- tapply(data_HF$ddist_2D, factor(data_HF$segment_global), sum, na.rm = T) / 1000 # In km
cum_dist_tab <- data.frame(segment_global = names(cum_dist_tab), cum_dist = cum_dist_tab)
ov_HF_tracks <- merge(ov_HF_tracks, cum_dist_tab, "segment_global", all.x = T)
summary(ov_HF_tracks$cum_dist) # Max. 185 km
hist(ov_HF_tracks$cum_dist)

plot(ov_HF_tracks$cum_dist ~ ov_HF_tracks$dist_first_last)
abline(0,1)

rm(longlat, coord, longlat_min1, coord_min1,
   index_ID.switch, cum_dist_tab, index_ID.switch)


# Add country names ####

ov_HF_tracks$countries <- NA
start.time <- Sys.time()
for(i in 1:nrow(ov_HF_tracks)){
  index <- which(data_HF$segment_global == ov_HF_tracks$segment_global[i])
  ov_HF_tracks$countries[i] <- paste(unique(data_HF$country[index]), collapse = ", ")
}
delta.time <- Sys.time() - start.time; delta.time # 20 s
summary(factor(ov_HF_tracks$countries))

rm(start.time, delta.time, i, index)


# Add latitudinal/longitudinal difference ####
# Based on first and last coordinates

ov_HF_tracks$lat_diff <- ov_HF_tracks$last_lat - ov_HF_tracks$first_lat
hist(ov_HF_tracks$lat_diff)
summary(ov_HF_tracks$lat_diff)
ov_HF_tracks$lon_diff <- ov_HF_tracks$last_lon - ov_HF_tracks$first_lon
hist(ov_HF_tracks$lon_diff)
summary(ov_HF_tracks$lon_diff)

# Mean lat/lon
ov_HF_tracks$lat_mean <- (ov_HF_tracks$last_lat + ov_HF_tracks$first_lat)/2
hist(ov_HF_tracks$lat_mean)
summary(ov_HF_tracks$lat_mean)
ov_HF_tracks$lon_mean <- (ov_HF_tracks$last_lon + ov_HF_tracks$first_lon)/2
hist(ov_HF_tracks$lon_mean)
summary(ov_HF_tracks$lon_mean)


# Proportion over sea ####

summary(factor(data_HF$over_sea))

prop_1 <- function(vec){
  x <- length(which(vec == "1")) / length(vec)
  return(x)
}

prop_over_sea_tab <- tapply(data_HF$over_sea, factor(data_HF$segment_global), prop_1)
prop_over_sea_tab <- data.frame(segment_global = names(prop_over_sea_tab), prop_over_sea = prop_over_sea_tab)
ov_HF_tracks <- merge(ov_HF_tracks, prop_over_sea_tab, "segment_global", all.x = T)

summary(ov_HF_tracks$prop_over_sea)
hist(ov_HF_tracks$prop_over_sea)
length(which(ov_HF_tracks$prop_over_sea > 0)) # 30 tracks only!
# View(ov_HF_tracks[which(ov_HF_tracks$prop_over_sea > 0),])

length(which(is.na(ov_HF_tracks$prop_over_sea))) # 40 days


# Average speed ####
# Based on GPS speed

summary(data_HF_move_2$ground.speed)

speed_GPS_median_tab <- tapply(data_HF_move_2$ground.speed, factor(data_HF_move_2$segment_global), median)
speed_GPS_median_tab <- data.frame(segment_global = names(speed_GPS_median_tab), speed_GPS_median = speed_GPS_median_tab)
ov_HF_tracks <- merge(ov_HF_tracks, speed_GPS_median_tab, "segment_global", all.x = T)
summary(ov_HF_tracks$speed_GPS_median)
hist(ov_HF_tracks$speed_GPS_median)
length(which(is.na(ov_HF_tracks$speed_GPS_median))) # 227 NA's
# Where do NA's come from?!
# View(ov_HF_tracks[is.na(ov_HF_tracks$speed_GPS_median),])
unique(ov_HF_tracks$n_fixes_move_2[is.na(ov_HF_tracks$speed_GPS_median)])
# Tracks without in-flight positions!

rm(speed_GPS_median_tab)


# Flight height parameters ####

# Remove very bad outliers?
# No, leave for now.
summary(data_HF$height.above.ground_GPS) # 66 NA's
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

# First: Number of in-flight fixes with height data
data_HF_move_2_no.na <- data_HF[which(data_HF$move_2 == "move" & !(is.na(data_HF$height.above.ground_GPS))),]
n_fixes_move_2_no.na_tab <- summary(factor(data_HF_move_2_no.na$segment_global), maxsum = 5000)
n_fixes_move_2_no.na_tab <- data.frame(segment_global = names(n_fixes_move_2_no.na_tab), n_fixes_move_2_no.na = n_fixes_move_2_no.na_tab)
ov_HF_tracks <- merge(ov_HF_tracks, n_fixes_move_2_no.na_tab, "segment_global", all.x = T)
ov_HF_tracks$n_fixes_move_2_no.na[is.na(ov_HF_tracks$n_fixes_move_2_no.na)] <- 0
summary(ov_HF_tracks$n_fixes_move_2_no.na) # Up to 8.4k
summary(ov_HF_tracks$n_fixes_move_2 - ov_HF_tracks$n_fixes_move_2_no.na)

height.above.ground_GPS_median_tab <- tapply(data_HF_move_2$height.above.ground_GPS, factor(data_HF_move_2$segment_global), median, na.rm = T)
height.above.ground_GPS_median_tab <- data.frame(segment_global = names(height.above.ground_GPS_median_tab), height.above.ground_GPS_median = height.above.ground_GPS_median_tab)
ov_HF_tracks <- merge(ov_HF_tracks, height.above.ground_GPS_median_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$height.above.ground_GPS_median))) # 2302
summary(ov_HF_tracks$height.above.ground_GPS_median)
hist(ov_HF_tracks$height.above.ground_GPS_median)

height.above.ground_GPS_min_tab <- tapply(data_HF_move_2$height.above.ground_GPS, factor(data_HF_move_2$segment_global), min, na.rm = T)
height.above.ground_GPS_min_tab <- data.frame(segment_global = names(height.above.ground_GPS_min_tab), height.above.ground_GPS_min = height.above.ground_GPS_min_tab)
ov_HF_tracks <- merge(ov_HF_tracks, height.above.ground_GPS_min_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$height.above.ground_GPS_min))) # 2302
summary(ov_HF_tracks$height.above.ground_GPS_min)
hist(ov_HF_tracks$height.above.ground_GPS_min)

height.above.ground_GPS_max_tab <- tapply(data_HF_move_2$height.above.ground_GPS, factor(data_HF_move_2$segment_global), max, na.rm = T)
height.above.ground_GPS_max_tab <- data.frame(segment_global = names(height.above.ground_GPS_max_tab), height.above.ground_GPS_max = height.above.ground_GPS_max_tab)
ov_HF_tracks <- merge(ov_HF_tracks, height.above.ground_GPS_max_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$height.above.ground_GPS_max))) # 2302
summary(ov_HF_tracks$height.above.ground_GPS_max)
hist(ov_HF_tracks$height.above.ground_GPS_max)

height.above.ground_GPS_q1_tab <- tapply(data_HF_move_2$height.above.ground_GPS, factor(data_HF_move_2$segment_global), quantile, 0.01, na.rm = T)
height.above.ground_GPS_q1_tab <- data.frame(segment_global = names(height.above.ground_GPS_q1_tab), height.above.ground_GPS_q1 = height.above.ground_GPS_q1_tab)
ov_HF_tracks <- merge(ov_HF_tracks, height.above.ground_GPS_q1_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$height.above.ground_GPS_q1))) # 2302
summary(ov_HF_tracks$height.above.ground_GPS_q1)
hist(ov_HF_tracks$height.above.ground_GPS_q1)

height.above.ground_GPS_q99_tab <- tapply(data_HF_move_2$height.above.ground_GPS, factor(data_HF_move_2$segment_global), quantile, 0.99, na.rm = T)
height.above.ground_GPS_q99_tab <- data.frame(segment_global = names(height.above.ground_GPS_q99_tab), height.above.ground_GPS_q99 = height.above.ground_GPS_q99_tab)
ov_HF_tracks <- merge(ov_HF_tracks, height.above.ground_GPS_q99_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$height.above.ground_GPS_q99))) # 2302
summary(ov_HF_tracks$height.above.ground_GPS_q99)
hist(ov_HF_tracks$height.above.ground_GPS_q99)

rm(height.above.ground_GPS_median_tab, height.above.ground_GPS_min_tab,
   height.above.ground_GPS_max_tab, height.above.ground_GPS_q1_tab,
   height.above.ground_GPS_q99_tab)

# Altitude gain
alt_gain_tab <- tapply(data_HF_move_2$alt_gain, factor(data_HF_move_2$segment_global), sum, na.rm = T)
alt_gain_tab <- data.frame(segment_global = names(alt_gain_tab), alt_gain = alt_gain_tab)
ov_HF_tracks <- merge(ov_HF_tracks, alt_gain_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$alt_gain))) # 2308.6

summary(ov_HF_tracks$alt_gain) # Max. 17.8 -> 2308.6 ??
hist(ov_HF_tracks$alt_gain)

hist(ov_HF_tracks$alt_gain/ov_HF_tracks$n_fixes_move_2)
hist(ov_HF_tracks$alt_gain/ov_HF_tracks$TS_move)
plot(ov_HF_tracks$alt_gain ~ ov_HF_tracks$TS_move, xlab = "Timespan of in-flight HF data (min)",
     ylab = "Altitude gain within HF data (m)", xaxs = "i", yaxs = "i", xpd = NA,
     main = "STE altitude gain")

# View(ov_HF_tracks[which(ov_HF_tracks$alt_gain/ov_HF_tracks$TS_move > 100),])
# All with quite short HF periods (6-7 min)

ID <- 1
index <- which(data_HF_move_2$segment_global ==
                 ov_HF_tracks$segment_global[which(ov_HF_tracks$alt_gain/ov_HF_tracks$TS_move > 50)][ID]) # Forcément pas beaucoup car découpés 
index
plot(data_HF_move_2$height.above.msl_GPS[index] ~ data_HF_move_2$timestamp[index])
sum(data_HF_move_2$dheight_pm[index][which(data_HF_move_2$dheight_pm[index] > 0)])

# -> Unrelated to height outliers! Instead, HF mainly collected during ascents!
# Solution for analysis: Remove days with unsufficient TS of HF data...

rm(ID, index, alt_gain_tab, alt_gain, index_ID.switch)


# Ground level parameters ####

summary(data_HF$ground.level_srtm) # Max. 3652 m!

ground.level_srtm_median_tab <- tapply(data_HF$ground.level_srtm, factor(data_HF$segment_global), median, na.rm = T)
ground.level_srtm_median_tab <- data.frame(segment_global = names(ground.level_srtm_median_tab), ground.level_srtm_median = ground.level_srtm_median_tab)
ov_HF_tracks <- merge(ov_HF_tracks, ground.level_srtm_median_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$ground.level_srtm_median)))
summary(ov_HF_tracks$ground.level_srtm_median)
hist(ov_HF_tracks$ground.level_srtm_median)

ground.level_srtm_min_tab <- tapply(data_HF$ground.level_srtm, factor(data_HF$segment_global), min, na.rm = T)
ground.level_srtm_min_tab <- data.frame(segment_global = names(ground.level_srtm_min_tab), ground.level_srtm_min = ground.level_srtm_min_tab)
ov_HF_tracks <- merge(ov_HF_tracks, ground.level_srtm_min_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$ground.level_srtm_min)))
summary(ov_HF_tracks$ground.level_srtm_min)
hist(ov_HF_tracks$ground.level_srtm_min)

ground.level_srtm_max_tab <- tapply(data_HF$ground.level_srtm, factor(data_HF$segment_global), max, na.rm = T)
ground.level_srtm_max_tab <- data.frame(segment_global = names(ground.level_srtm_max_tab), ground.level_srtm_max = ground.level_srtm_max_tab)
ov_HF_tracks <- merge(ov_HF_tracks, ground.level_srtm_max_tab, "segment_global", all.x = T)
length(which(is.na(ov_HF_tracks$ground.level_srtm_max)))
summary(ov_HF_tracks$ground.level_srtm_max)
hist(ov_HF_tracks$ground.level_srtm_max)


# Check sorting ####
# Modified through merge()?

all(rownames(ov_HF_tracks) == 1:nrow(ov_HF_tracks)) # T


# Re-arrange columns ####

colnames(ov_HF_tracks)
# colnames_rm <- c("date_num_rnd", "segment_global_2", "year.1st.depl", "age.1st.depl")
# ov_HF_tracks <- ov_HF_tracks[,-which(colnames(ov_HF_tracks) %in% colnames_rm)]
# 
# colnames(ov_HF_tracks)
# col_index_sort <- c(1:3,39,36:38,40,4,20,5:17,19,18,21:24,
#                     26,25,27,34,35,28:33)
# length(col_index_sort); length(unique(col_index_sort))
# which(!(1:ncol(ov_HF_tracks) %in% col_index_sort))
# ov_HF_tracks <- ov_HF_tracks[,col_index_sort]
# 
# summary(ov_HF_tracks)



### Vérification

hist(ov_HF_tracks$TS)
#table(ov_HF_tracks$TS)

# Vérif des latitudes pour chaque ligne de ov_HF_tracks
sum(is.na(ov_HF_tracks$first_lat))   # nombre de segments sans first_lat
sum(is.na(ov_HF_tracks$last_lat))    # nombre de segments sans last_lat

# Optionnel : voir les lignes qui posent problème
#ov_HF_tracks[is.na(ov_HF_tracks$first_lat) | is.na(ov_HF_tracks$last_lat), ]
# => Aucun NA dans les first_lat / last_lat ; first_lon / last_lon



# Export output ####

save(ov_HF_tracks, file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.5_OUTPUT_STE_France_HF_track_segement_table_"), Sys.Date(), ".RData", sep = ""))

write.table(ov_HF_tracks,
            file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.5_OUTPUT_STE_France_HF_track_segement_table_"), Sys.Date(), ".csv", sep = ""),
            row.names = F, sep = ",")


