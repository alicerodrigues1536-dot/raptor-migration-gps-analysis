####################
##       DATA
##     ORNITELA
####################
rm(list=ls())    # effacement de l'historique R

####
## Library

library(here)

###########################################
library(sp)
library(sf)
library(suntools) # For sun angle calculations, instead of maptools
library(geosphere) # For distance calculations
library(terra)
library(rnaturalearth) # To couple country names
library(conflicted)
###########################################


#################################################################################################################################
#############################################   2.1_Process_tracking_data  ######################################################
#################################################################################################################################

# Assemble raw dataset ####

filenames <- list.files(here("Alice/STE_data_preparation_June_2026/1.Raw_tracking_data"), pattern="*.csv", full.names=TRUE)
View(filenames)

start.time <- Sys.time()
for(i in 1:length(filenames)){
  
  data_temp <- read.csv(filenames[i], sep = ",", stringsAsFactors = F,
                        na.strings = c("", NA))
  
  if(i == 1){data <- data_temp}
  if(i > 1){data <- rbind(data, data_temp)}
  rm(data_temp)
}
comp.time <- Sys.time() - start.time; comp.time # 1.5 min

rm(filenames, start.time, comp.time, i)


# Translate column names ####

col_translation <- read.table(
  here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_INPUT_column_translation.txt"),
  sep = "\t",
  header = TRUE
)
colnames(data)
colnames(data)[which(!(colnames(data) %in% col_translation$Ornitela_V2))]

# Identify columns to drop I
index_col_rm <- which(!(colnames(data) %in% col_translation$Ornitela_V2))

for(j in which(colnames(data) %in% col_translation$Ornitela_V2)){
  trans_Movebank_dummy <- col_translation$Movebank[which(col_translation$Ornitela_V2 == colnames(data)[j])]
  if(trans_Movebank_dummy != "" & is.na(trans_Movebank_dummy) == F){
    colnames(data)[j] <- trans_Movebank_dummy
  } else { # Identify columns to drop II
    index_col_rm <- c(index_col_rm, j)
  }
}
colnames(data)

# Drop columns
colnames(data)[index_col_rm]
# [1] "UTC_date"           "UTC_time"           "int_temperature_C" 
# [4] "UTC_timestamp"      "milliseconds"       "light"             
# [7] "depth_m"            "conductivity_mS.cm" "ext_temperature_C" 
# [10] "solar_I_mA"  

data <- data[,-index_col_rm]
colnames(data)

rm(index_col_rm, j, trans_Movebank_dummy, col_translation)


# Format and check datetime ####

# Format datetime
cbind(head(data$timestamp),
      as.character(as.POSIXct(strptime(head(data$timestamp), format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))))
data$timestamp <- as.POSIXct(strptime(data$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
head(data$timestamp)

# Check for impossible dates
index_false_time <- which(data$timestamp > Sys.time()); length(index_false_time) # 0
if(length(index_false_time) > 0){data <- data[-index_false_time,]}

# Original delta time (in minutes; NA when Device.ID changes)
# Needs to be done BEFORE SORTING BY TIMESTAMP!
data$dtime_orig[2:nrow(data)] <- (as.numeric(data$timestamp[2:nrow(data)]) - 
                                    as.numeric(data$timestamp[1:(nrow(data)-1)])) / 60
index_ID.switch <- which(data$tag.local.identifier[2:nrow(data)] != data$tag.local.identifier[1:(nrow(data)-1)]) + 1
if(length(index_ID.switch) != 0){data$dtime_orig[index_ID.switch] <- NA}

# Check delta time
summary(data$dtime_orig)
index_neg_dtime <- which(data$dtime_orig < 0); index_neg_dtime
length(index_neg_dtime) # 138
summary(data$dtime_orig[index_neg_dtime])
summary(data$dtime_orig[index_neg_dtime]/60)
sort(data$dtime_orig[index_neg_dtime], decreasing = F)
sort(data$dtime_orig[index_neg_dtime[data$dtime_orig[index_neg_dtime] < -60]], decreasing = F)/60
which(data$dtime_orig[index_neg_dtime] == -1/60) # 89 are - 1 sec
which(data$dtime_orig[index_neg_dtime] < -1/60 & data$dtime_orig[index_neg_dtime] > -1) # 18 are between -1 sec and -1 min
which(data$dtime_orig[index_neg_dtime] < -1)

# View(data[(index_neg_dtime[50]-100):(index_neg_dtime[50]+100),])
# First 88 are from 1 HF sequence
# View(data[(index_neg_dtime[89]-5):(index_neg_dtime[89]+5),])

# View(data[(which(data$dtime_orig < -600*60)[1]-5):(which(data$dtime_orig < -600*60)[1]+5),])
# View(data[(which(data$dtime_orig < -600*60)[2]-5):(which(data$dtime_orig < -600*60)[2]+5),])
# View(data[(which(data$dtime_orig < -600*60)[3]-5):(which(data$dtime_orig < -600*60)[3]+5),])
# 08-30 interchanged with 08-03 in all 3 cases

# index_neg_dtime_view <- which(data$dtime_orig == sort(data$dtime_orig[index_neg_dtime[data$dtime_orig[index_neg_dtime] < -60]], decreasing = F)[4])
# View(data[(index_neg_dtime_view-5):(index_neg_dtime_view+5),])
# 04-16 instead of 04-23

# index_neg_dtime_view <- which(data$dtime_orig == sort(data$dtime_orig[index_neg_dtime[data$dtime_orig[index_neg_dtime] < -60]], decreasing = F)[5])
# View(data[(index_neg_dtime_view-5):(index_neg_dtime_view+5),])
# 00:00 instead of 17:00 (-> mistake in the hours!)

# index_neg_dtime_view <- which(data$dtime_orig == sort(data$dtime_orig[index_neg_dtime[data$dtime_orig[index_neg_dtime] < -60]], decreasing = F)[6])
# View(data[(index_neg_dtime_view-5):(index_neg_dtime_view+5),])
# 00:00 instead of 17:00 (-> mistake in the hours!)

# Problem: It's not necessarily the positions with negative dtime which are problematic.
# -> Faulty timestamp could be positive, only next normal one would be negative... 

ov_neg_dtime <- data.frame(index_neg_dtime = index_neg_dtime, tag.local.identifier = data$tag.local.identifier[index_neg_dtime],
                           timestamp = data$timestamp[index_neg_dtime])
ov_neg_dtime$dtime_m3 <- data$dtime_orig[index_neg_dtime-3]
ov_neg_dtime$dtime_m2 <- data$dtime_orig[index_neg_dtime-2]
ov_neg_dtime$dtime_m1 <- data$dtime_orig[index_neg_dtime-1]
ov_neg_dtime$dtime_0 <- data$dtime_orig[index_neg_dtime]
ov_neg_dtime$dtime_p1 <- data$dtime_orig[index_neg_dtime+1]
ov_neg_dtime$dtime_p2 <- data$dtime_orig[index_neg_dtime+2]
ov_neg_dtime$dtime_p3 <- data$dtime_orig[index_neg_dtime+3]

which(ov_neg_dtime$dtime_m1 > 10/60) # Only one where dtime of previous is larger than 10 s!

index_neg_dtime_view <- ov_neg_dtime$index_neg_dtime[138]
# View(data[(index_neg_dtime_view-5):(index_neg_dtime_view+5),])
# -> This is due to a double-negative timestamp sequence...

# -> Conclusion: Removing all positions with negative dtime is working!
length(index_neg_dtime) # 138
if(length(index_neg_dtime) > 0){data <- data[-index_neg_dtime,]}

# Sort after tag ID and datetime
data <- data[order(data$tag.local.identifier, data$timestamp),]
row.names(data) <- 1:nrow(data)

# Recalculate original delta time (in minutes; NA when Device.ID changes)
data$dtime_orig[2:nrow(data)] <- (as.numeric(data$timestamp[2:nrow(data)]) - 
                                    as.numeric(data$timestamp[1:(nrow(data)-1)])) / 60
index_ID.switch <- which(data$tag.local.identifier[2:nrow(data)] != data$tag.local.identifier[1:(nrow(data)-1)]) + 1
if(length(index_ID.switch) != 0){data$dtime_orig[index_ID.switch] <- NA}

# Check delta time
summary(data$dtime_orig)
min(data$dtime_orig, na.rm = T)

# Delta time == 0
index_double <- which(data$dtime_orig == 0)
length(index_double) # 126
# View(data[(index_double[2]-5):(index_double[2]+5),])
# Apparently HF data where timestamp got stuck... (A pity to remove, as HF sequences may get spli!)
index_double[2:length(index_double)] - index_double[1:(length(index_double)-1)]
which(index_double[2:length(index_double)] - index_double[1:(length(index_double)-1)] == 1)
# -> No longer sequences with same timestamp. Only 3 occasions with > 1 double timestamp in a row.
if(length(index_double) > 0){data <- data[-index_double,]}

# Check delta time < 0 again
if(length(which(data$dtime_orig < 0)) > 0){print("Interrupted due to negative dtime values."); break}

rm(index_false_time, index_ID.switch, index_double, index_neg_dtime, index_view, ov_neg_dtime,
   index_neg_dtime_view) # Ignorer?

# Add day of the year
data$DOY <- as.POSIXlt(data$timestamp)$yday + 1 # Plus 1 because 1st January has value 0 otherwise.
summary(data$DOY)
hist(data$DOY)


# Treat data with missing coordinates ####

# In Ornitela tags, 0 used instead of NA for latitude and longitude.
index_zero <- which(data$location.long == 0 & data$location.lat == 0)
length(index_zero) # 1787
if(length(index_zero) > 0){
  data$location.lat[index_zero] <- data$location.long[index_zero] <- NA
}

# Leave out data with NA coordinates
index_na <- which(is.na(data$location.long) == T | is.na(data$location.lat) == T)
length(index_na) # 1787
if(length(index_na) > 0){data <- data[-index_na,]}

rm(index_zero, index_na)


# Couple deployment info ####

depl_info <- read.table(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/masterfile_deployments_STE_FR_2026-02-19.txt"),
                        sep = "\t", header = T, stringsAsFactors = F, na.strings = c("", NA),
                        encoding = "latin1")

# Create unambiguous animal IDs
depl_info$individual.global.identifier <- paste(depl_info$study.name.2,
                                                depl_info$individual.local.identifier, sep = "_")
unique(depl_info$individual.global.identifier)[1:20]


# Will also remove data from tags not included in deployment info table

depl_info$tag.local.identifier
unique(data$tag.local.identifier) %in% depl_info$tag.local.identifier
unique(data$tag.local.identifier)[which(unique(data$tag.local.identifier) %in% depl_info$tag.local.identifier == F)]

library(lubridate)

# Lecture tolérante : jj/mm/aaaa hh:mm, jj/mm/aa hh:mm, ou date seule
parse_dt <- function(x){
  as.POSIXct(parse_date_time(x, orders = c("dmY HMS", "dmY HM", "dmy HM", "dmY", "dmy"),
                             tz = "GMT", quiet = TRUE), tz = "GMT")
}

depl_info$deploy.on.date    <- parse_dt(depl_info$deploy.on.date)
depl_info$deploy.on.date_2  <- as.POSIXct(strptime(substr(depl_info$deploy.on.date, 1, 10),
                                                   format = "%Y-%m-%d", tz = "GMT"))
depl_info$deploy.on.year    <- substr(depl_info$deploy.on.date, 1, 4)
depl_info$deploy.off.date   <- parse_dt(depl_info$deploy.off.date)

# Contrôle : seules les lignes sans date de fin doivent avoir NA pour deploy.off.date
summary(depl_info$deploy.on.date)       # aucun NA attendu
summary(depl_info$deploy.off.date)

depl_info <- depl_info[order(depl_info$study.name.2, depl_info$deploy.on.date),]

data$individual.local.identifier <- NA
data$individual.global.identifier <- NA
data$tag.manufacturer.name <- NA
data$tag.model <- NA

index.to.keep <- NULL

start.time <- Sys.time()
total <- length(unique(data$tag.local.identifier))
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)

for(i in 1:length(unique(data$tag.local.identifier))){
  
  index_depl_info <- which(depl_info$tag.local.identifier == unique(data$tag.local.identifier)[i])
  index_tag <- which(data$tag.local.identifier == unique(data$tag.local.identifier)[i])
  
  for(j in index_depl_info){
    # Nested indices necessary, otherwise which() with multiple arguments takes MUCH too long!
    
    if(depl_info$deploy.on.date.precise[j] == "y"){
      index_depl_1 <- which(data$timestamp[index_tag] > depl_info$deploy.on.date[j])
    } else {
      # Remove first deployment day in case release time is unknown
      index_depl_1 <- which(data$timestamp[index_tag] >
                              depl_info$deploy.on.date_2[j] + 24*60*60)
    }
    
    if(is.na(depl_info$deploy.off.date[j]) == T){
      index_depl <- index_tag[index_depl_1]
    } else {
      index_depl_2 <- which(data$timestamp[index_tag[index_depl_1]] < depl_info$deploy.off.date[j])
      index_depl <- index_tag[index_depl_1[index_depl_2]]
    }
    
    if(length(index_depl) > 0){
      # Add to index.to.keep
      index.to.keep[(length(index.to.keep)+1):(length(index.to.keep)+length(index_depl))] <- index_depl
      print(length(index_depl))
      
      # Annotate data with deployment info
      data$individual.local.identifier[index_depl] <- depl_info$individual.local.identifier[j]
      data$individual.global.identifier[index_depl] <- depl_info$individual.global.identifier[j]
      data$tag.manufacturer.name[index_depl] <- depl_info$tag.manufacturer.name[j]
      data$tag.model[index_depl] <- depl_info$tag.model[j]
    }
  }
  setWinProgressBar(pb, i, title = paste(round(i/total*100, 0), "% done"))
}
comp.time <- Sys.time() - start.time; comp.time # 5 sec
close(pb)
length(index.to.keep)
nrow(data) - length(index.to.keep) # 22350 to remove

start.time <- Sys.time()
data <- data[index.to.keep,]
comp.time <- Sys.time() - start.time; comp.time # < 1 sec

# Clear objects
rm(start.time, comp.time, i, j, index.to.keep, index_depl, index_depl_1, index_depl_2,
   index_tag, index_depl_info, pb, total)


# Couple animal info ####

bird_info <- read.table(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/masterfile_birds_STE_FR_2026-02-19.txt"),
                        sep = "\t", header = T, stringsAsFactors = F, quote = "", na.strings = c("", NA),
                        encoding = "latin1")

# Create unambiguous animal IDs
bird_info$individual.global.identifier <- paste(bird_info$study.name.2,
                                                bird_info$individual.local.identifier, sep = "_")
unique(bird_info$individual.global.identifier)[1:20]
sort(summary(factor(bird_info$individual.global.identifier), maxsum = 500), decreasing = T)[1:20]

bird_info$individual.global.identifier
unique(data$individual.global.identifier) %in% bird_info$individual.global.identifier
unique(data$individual.global.identifier)[which(unique(data$individual.global.identifier) %in% bird_info$individual.global.identifier == F)]

bird_info$animal.exact.date.of.death <- parse_dt(bird_info$animal.exact.date.of.death)
bird_info[, c("individual.local.identifier", "animal.exact.date.of.death")]   # contrôle visuel

data$individual.sex <- NA
data$individual.taxon.canonical.name <- NA
data$study.name.2 <- NA
data$individual.year.1st.depl <- NA
data$individual.age.1st.depl <- NA

index.to.keep <- NULL

start.time <- Sys.time()
total <- length(unique(data$individual.global.identifier))
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)
for(i in 1:length(unique(data$individual.global.identifier))){
  
  index_bird_info <- which(bird_info$individual.global.identifier == unique(data$individual.global.identifier)[i])
  index_depl_info <- which(depl_info$individual.global.identifier == unique(data$individual.global.identifier)[i])
  index_bird <- which(data$individual.global.identifier == unique(data$individual.global.identifier)[i])
  
  # Nested indices necessary, otherwise which() with multiple arguments takes MUCH too long!
  if(is.na(bird_info$animal.exact.date.of.death[index_bird_info]) == F){
    index_alive <- which(data$timestamp[index_bird] < bird_info$animal.exact.date.of.death[index_bird_info])
    index_bird_alive <- index_bird[index_alive]
  } else {
    index_bird_alive <- index_bird
  }
  
  if(length(index_bird_alive) > 0){
    # Add to index.to.keep
    index.to.keep[(length(index.to.keep)+1):(length(index.to.keep)+length(index_bird_alive))] <- index_bird_alive
    print(length(index_bird_alive))
    
    # Annotate data with animal info
    data$individual.sex[index_bird_alive] <- bird_info$animal.sex[index_bird_info]
    data$individual.taxon.canonical.name[index_bird_alive] <-
      bird_info$individual.taxon.canonical.name[index_bird_info]
    data$study.name.2[index_bird_alive] <- bird_info$study.name.2[index_bird_info]
    
    data$individual.year.1st.depl[index_bird_alive] <- depl_info$deploy.on.year[index_depl_info[1]]
    data$individual.age.1st.depl[index_bird_alive] <- depl_info$animal.life.stage[index_depl_info[1]]
  }
  setWinProgressBar(pb, i, title = paste(round(i/total*100, 0), "% done"))
}
comp.time <- Sys.time() - start.time; comp.time # 6 sec
close(pb)
length(index.to.keep)
nrow(data) - length(index.to.keep) # 268 to remove

start.time <- Sys.time()
data <- data[index.to.keep,]
comp.time <- Sys.time() - start.time; comp.time # < 1 sec

# Clear objects
rm(start.time, comp.time, i, index_alive, index_bird, index_bird_alive, index_bird_info,
   index_depl_info, index.to.keep, pb, total)


# Annotation check ####

sort(summary(factor(data$individual.global.identifier), maxsum = 500))
# hist(summary(factor(data$individual.global.identifier), maxsum = 500))
summary(factor(data$tag.manufacturer.name))
summary(factor(data$tag.model))

summary(factor(data$individual.sex))
summary(factor(data$individual.year.1st.depl))
summary(factor(data$individual.age.1st.depl))
summary(factor(data$study.name.2))
summary(factor(data$individual.taxon.canonical.name))


# Save/load intermediate workspace (1) ####

#save.image(paste("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_1_", Sys.Date(), ".RData", sep = ""))
# load("E:/PhD/STE_France/Student_project_2025/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_1_2025-03-31.RData")


# Age classes ####

# Calendar year
data$age_CY <- NA
data$age_CY[which(data$individual.age.1st.depl == "adult")] <- "adult"
data$age_CY[which(data$individual.age.1st.depl == "1CY")] <-
  as.numeric(substr(data$timestamp[which(data$individual.age.1st.depl == "1CY")],1,4)) -
  as.numeric(data$individual.year.1st.depl[which(data$individual.age.1st.depl == "1CY")]) + 1
data$age_CY[which(data$individual.age.1st.depl == "2CY")] <-
  as.numeric(substr(data$timestamp[which(data$individual.age.1st.depl == "2CY")],1,4)) -
  as.numeric(data$individual.year.1st.depl[which(data$individual.age.1st.depl == "2CY")]) + 2
summary(factor(data$age_CY))
summary(factor(data$age_CY[which(data$individual.local.identifier == "Henri de la Trejasse")]))
# Works fine.

# Year
data$age_year <- NA
# Use July 1st as threshold
data$age_year[which(data$DOY >= 182 & data$age_CY != "adult")] <- data$age_CY[which(data$DOY >= 182 & data$age_CY != "adult")]
data$age_year[which(data$DOY < 182 & data$age_CY != "adult")] <- as.numeric(data$age_CY[which(data$DOY < 182 & data$age_CY != "adult")]) - 1
data$age_year[which(data$age_CY == "adult")] <- data$age_CY[which(data$age_CY == "adult")]
summary(factor(data$age_year))


# Treat special phases ####
# Here: Periods in captivity and when tag was loose

special_phases_info <- read.table(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/masterfile_special_phases_STE_FR_2026-02-19.txt"), # Bien maj pour 2026 donc OK
                                  sep = "\t", header = T, stringsAsFactors = F, na.strings = c("", NA))

special_phases_info$phase.start <- as.POSIXct(strptime(special_phases_info$phase.start, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
special_phases_info$phase.end <- as.POSIXct(strptime(special_phases_info$phase.end, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))

special_phases_info$phase.type

index_rm_collect <- NULL
for(i in which(special_phases_info$phase.type %in% c("Captive", "Tag_loose"))){
  index_rm <- which(data$tag.local.identifier == special_phases_info$tag.local.identifier[i] &
                      data$timestamp > special_phases_info$phase.start[i])
  if(is.na(special_phases_info$phase.end[i]) == F){
    index_rm <- index_rm[which(data$timestamp[index_rm] < special_phases_info$phase.end[i])]
  }
  
  if(length(index_rm) > 0){index_rm_collect <- c(index_rm_collect, index_rm)}
}

length(index_rm_collect) # 3694 positions
summary(factor(data$individual.global.identifier[index_rm_collect])) # Only Abel de la Trejasse
if(length(index_rm_collect) > 0){data <- data[-index_rm_collect,]}

rm(i, index_rm, index_rm_collect)


# Distinguish high-res/low-res positions ####

data$res <- "low"
data$res[which(data$dtime_orig <= 6/60)] <- "high"

summary(factor(data$res)) / nrow(data) # 75% HF!


# Distinguish day/night ####

# Apply -6 dgr as solar angle
library(conflicted)
conflicts_prefer(terra::extract)

start.time <- Sys.time()

day_end <- day_start <- as.POSIXct(strptime(NA, "%Y-%m-%d %H:%M:%S", tz="GMT"))

day_start <- crepuscule(cbind(data$location.long, data$location.lat), crs=st_crs("EPSG:4326"),
                        dateTime=data$timestamp, solarDep=6, direction="dawn", POSIXct.out=T)[,2]
day_end <- crepuscule(cbind(data$location.long, data$location.lat), crs=st_crs("EPSG:4326"),
                      dateTime=data$timestamp, solarDep=6, direction="dusk", POSIXct.out=T)[,2]

comp.time <- Sys.time() - start.time; comp.time # 5 min

# Check
day_start[1:10]
day_end[1000:1010]
length(which(is.na(day_start))); length(which(is.na(day_end)))

data$daynight_6 <- ifelse(data$timestamp > day_start & data$timestamp < day_end,
                          "day", "night")
summary(as.factor(data$daynight_6)) / nrow(data) # 98.8% day

rm(start.time, comp.time)


# Determine relative time of the day ####

start.time <- Sys.time()
sunrise <- sunriset(cbind(data$location.long, data$location.lat), crs=st_crs("EPSG:4326"),
                    dateTime=data$timestamp, direction="sunrise", POSIXct.out=T)[,2]
sunset <- sunriset(cbind(data$location.long, data$location.lat), crs=st_crs("EPSG:4326"),
                   dateTime=data$timestamp, direction="sunset", POSIXct.out=T)[,2]
comp.time <- Sys.time() - start.time; comp.time # 6 min

# Check
cbind(as.character(sunrise[1:10]), as.character(day_start[1:10]))
cbind(as.character(sunset[1000:1010]), as.character(day_end[1000:1010]))
length(which(is.na(sunrise))); length(which(is.na(sunset)))

data$daylength <- (as.numeric(sunset) - as.numeric(sunrise)) / 3600 # In h
summary(data$daylength)
hist(data$daylength)

data$daytime_rel <- ((as.numeric(data$timestamp) - as.numeric(sunrise)) / 3600) /
  data$daylength
summary(data$daytime_rel)
hist(data$daytime_rel)
tapply(data$daytime_rel, data$daynight, hist)
# Looks good for night.
# For day: Far from uniform dsitribution across daytime! :/

hist(data$daytime_rel[which(data$daynight == "day" & data$res == "low")],
     main = "Day LF", xlab = "Relative time of day")
hist(data$daytime_rel[which(data$daynight == "day" & data$res == "high")],
     main = "Day HF", xlab = "Relative time of day")
# Relatively homogeneous for low-res data.

# Check maximum:
# View(data[which(data$daytime_rel == max(data$daytime_rel)),])
# Yes, OK.

data$sunrise <- sunrise
data$sunset <- sunset

# Second daynight variable
data$daynight_0 <- ifelse(data$daytime_rel > 0 & data$daytime_rel < 1, "day", "night")
summary(factor(data$daynight_0)) / nrow(data) # 97.3% day

rm(start.time, comp.time, day_end, day_start, sunrise, sunset)


# Distinguish stationary/flight positions ####

# Check ground speed unit
data$ground.speed[1:10]
hist(data$ground.speed)
hist(data$ground.speed, breaks = c(seq(0,40,2), max(data$ground.speed, na.rm = T)), xlim = c(0,40))
tapply(data$ground.speed, data$tag.manufacturer.name, summary)
# for(i in c("Milsar", "Ornitela", "UvA-BiTS")){
#   hist(data$ground.speed[which(data$tag.manufacturer.name == i)],
#        breaks = c(seq(0,40,2), max(data$ground.speed[which(data$tag.manufacturer.name == i)], na.rm = T)),
#        xlim = c(0,40), xaxs = "i", main = i, xlab = "Speed")
# }
# -> Ornitela is in km/h, rest in m/s

index_Ornitela <- which(data$tag.manufacturer.name == "Ornitela")
if(mean(data$ground.speed[index_Ornitela]) > 5){
  data$ground.speed[index_Ornitela] <- data$ground.speed[index_Ornitela] / 3.6
}
tapply(data$ground.speed, data$tag.manufacturer.name, summary)

rm(i, index_Ornitela)

summary(data$ground.speed) # No NA's.
hist(data$ground.speed, xlim = c(0,20), breaks = 100)

# Remove positions with NA speed
index_rm <- which(is.na(data$ground.speed)); length(index_rm) # 0
if(length(index_rm) > 0){data <- data[-index_rm,]}
rm(index_rm)

# Species x tag type x res variable
data$species_tag_type_res <- paste(data$individual.taxon.canonical.name, data$tag.manufacturer.name, data$res)
summary(factor(data$species_tag_type_res))

# 2-step process: First make histograms, then manually determine antimode, then plot histograms again to check
pdf(paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Speed_distributions_prelim_"), Sys.Date(), ".pdf", sep = ""),
    height = 5, width = 5)

start.time <- Sys.time()
total <- length(unique(data$species_tag_type_res))
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)
count <- 1

for(i in sort(unique(data$species_tag_type_res))){
  index_species_tag_type_res <- which(data$species_tag_type_res == i)
  
  hist(data$ground.speed[index_species_tag_type_res],
       xlim = c(0,20), ylim = c(0,0.15), xlab = "GPS ground speed (m/s)",
       breaks = c(seq(0,20,0.5),
                  max(data$ground.speed[index_species_tag_type_res], na.rm = T)),
       main = i, col = "gray90", border = "gray60")
  
  setWinProgressBar(pb, count, title = paste(round(count/total*100, 0), "% done"))
  count <- count + 1
}
close(pb)
comp.time <- Sys.time() - start.time; comp.time
dev.off()

# Determine antimode by hand: Export table to fill in Excel
write.table(data.frame(species_tag_type_res = sort(unique(data$species_tag_type_res)),
                       antimode_manual = NA),
            file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Speed_thresholds_manual_empty_"), Sys.Date(), ".csv", sep = ""),
            row.names = F, sep = ",")

# Import filled table
#speed_thresholds_manual <- read.table("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Speed_thresholds_manual_filled_2026-06-04.txt",
#                                      sep = "\t", header = T, stringsAsFactors = F, na.strings = c("", NA))
speed_thresholds_manual <- read.table(
  here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Speed_thresholds_manual_filled_2026-06-04.txt"),
  sep = "\t", header = T, stringsAsFactors = F, na.strings = c("", NA),
  fileEncoding = "UTF-16"
)

# Plot histograms with manual antimodes
# (Bonus: Determine proportion flying)

prop.flying_collect <- NULL

data$move <- "move"
data$move[which(is.na(data$ground.speed) == T)] <- NA

pdf(paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation//2.1_OUTPUT_Speed_distributions_manual_"), Sys.Date(), ".pdf", sep = ""),
    height = 5, width = 5)

start.time <- Sys.time()
total <- length(unique(data$species_tag_type_res))
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)
count <- 1

for(i in sort(unique(data$species_tag_type_res))){
  index_species_tag_type_res <- which(data$species_tag_type_res == i)
  
  hist(data$ground.speed[index_species_tag_type_res],
       xlim = c(0,20), ylim = c(0,0.15), xlab = "GPS ground speed (m/s)",
       breaks = c(seq(0,20,0.5),
                  max(data$ground.speed[index_species_tag_type_res], na.rm = T)),
       main = i, col = "gray90", border = "gray60")
  abline(v = speed_thresholds_manual$antimode_manual[count], col = "red", lwd = 2)
  
  # Label GPS positions
  index_sit <- which(data$ground.speed[index_species_tag_type_res] < speed_thresholds_manual$antimode_manual[count])
  data$move[index_species_tag_type_res[index_sit]] <- "sit"
  
  print(paste(round(max(data$ground.speed[index_species_tag_type_res[index_sit]]),2), speed_thresholds_manual$antimode_manual[count]))
  
  # Prop. flying
  prop.flying_collect <- c(prop.flying_collect,
                           round(length(which(data$move[index_species_tag_type_res] == "move")) /
                                   length(index_species_tag_type_res) *100,2))
  
  setWinProgressBar(pb, count, title = paste(round(count/total*100, 0), "% done"))
  count <- count + 1
}
close(pb)
comp.time <- Sys.time() - start.time; comp.time # < 1 sec
dev.off()

# Summary table
speed_thresholds_summary <- speed_thresholds_manual
speed_thresholds_summary$prop.flying <- prop.flying_collect

rm(prop.flying_collect, start.time, comp.time, total, count,
   index_sit, pb, index_species_tag_type_res)

# Add prop. flying for LF using threshold from HF
speed_thresholds_summary$prop.flying_2 <- NA

for(i in which(grepl("low", speed_thresholds_summary$species_tag_type_res))){
  index_species_tag_type_res <- which(data$species_tag_type_res == speed_thresholds_summary$species_tag_type_res[i])
  
  speed_thresholds_summary$prop.flying_2[i] <-
    round(length(which(data$ground.speed[index_species_tag_type_res] >= speed_thresholds_manual$antimode_manual[i-1])) /
            length(index_species_tag_type_res) *100,2)
}

rm(index_species_tag_type_res)

# Add prop. flying for LF using min. or max. threshold from LF
# speed_thresholds_LF <- speed_thresholds_summary$antimode_manual[which(grepl("low", speed_thresholds_summary$species_tag_type_res))]
# summary(speed_thresholds_LF)
# 
# speed_thresholds_summary$prop.flying_min_thresh <- NA
# speed_thresholds_summary$prop.flying_max_thresh <- NA
# 
# for(i in which(grepl("low", speed_thresholds_summary$species_tag_type_res))){
#   index_species_tag_type_res <- which(data$species_tag_type_res == speed_thresholds_summary$species_tag_type_res[i])
#   
#   speed_thresholds_summary$prop.flying_min_thresh[i] <-
#     round(length(which(data$ground.speed[index_species_tag_type_res] >= min(speed_thresholds_LF))) /
#             length(index_species_tag_type_res) *100,2)
#   
#   speed_thresholds_summary$prop.flying_max_thresh[i] <-
#     round(length(which(data$ground.speed[index_species_tag_type_res] >= max(speed_thresholds_LF))) /
#             length(index_species_tag_type_res) *100,2)
# }
# 
# rm(speed_thresholds_LF)

# Histogram of flight speed per individual

pdf(paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Speed_distributions_ind_"), Sys.Date(), ".pdf", sep = ""),
    height = 6, width = 9)

# Combination ind x tag
tag_ind_res <- paste(data$individual.global.identifier, data$tag.local.identifier, data$res)

par(mfrow = c(3,3), mar = c(5,4,4,2))
for(i in sort(unique(tag_ind_res))){
  # i <- "F2655213"
  index <- which(tag_ind_res == i)
  if(length(index) > 500){
    hist(data$ground.speed[index],
         xlim = c(0,20), ylim = c(0,0.15), xlab = "GPS ground speed (m/s)",
         breaks = c(seq(0,20,0.5), max(data$ground.speed[index], na.rm = T)),
         main = i, col = "gray90", border = "gray60")
  }
  # Speed threshold low-res
  abline(v = speed_thresholds_manual$antimode_manual[which(
    grepl(data$individual.taxon.canonical.name[index[1]], speed_thresholds_manual$species_tag_type_res) &
      grepl(data$tag.manufacturer.name[index[1]], speed_thresholds_manual$species_tag_type_res) &
      grepl(ifelse(grepl("low", i) , "low", "high"), speed_thresholds_manual$species_tag_type_res))],
    col = "red", lwd = 2)
}
dev.off()

rm(i, tag_ind_res, index)


# Prop. flying day vs. night ####
# ("Proof of concept")

speed_thresholds_summary$prop.flying_night <- speed_thresholds_summary$prop.flying_day <- NA
speed_thresholds_summary$n_night <- NA

for(i in 1:nrow(speed_thresholds_summary)){
  index <- which(data$species_tag_type_res == speed_thresholds_summary$species_tag_type_res[i])
  if(length(index) > 0){
    speed_thresholds_summary$prop.flying_day[i] <-
      round(length(which(data$daynight_6[index] == "day" & data$move[index] == "move")) /
              length(which(data$daynight_6[index] == "day")) * 100,2)
    speed_thresholds_summary$prop.flying_night[i] <- 
      round(length(which(data$daynight_6[index] == "night" & data$move[index] == "move")) /
              length(which(data$daynight_6[index] == "night")) * 100,2)
    speed_thresholds_summary$n_night[i] <- length(which(data$daynight_6[index] == "night"))
  }
}

# View(speed_thresholds_summary[order(speed_thresholds_summary$prop.flying_night, decreasing = T),])

# Export summary
write.table(speed_thresholds_summary,
            file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Speed_thresholds_summary_"), Sys.Date(), ".csv", sep = ""),
            row.names = F, sep = ",")

rm(i, index)


# Save/load intermediate workspace (2) ####

save.image(paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_2_"), Sys.Date(), ".RData", sep = ""))
# load("E:/PhD/STE_France/Student_project_2025/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_2_2025-04-02.RData")


# Remove outliers ####

# 1) Based on segment speed

# 2D delta distance (meters)
longlat <- cbind(data$location.long[2:nrow(data)], data$location.lat[2:nrow(data)])
coord <- SpatialPoints(longlat, proj4string=CRS("EPSG:4326"))
longlat_min1 <- cbind(data$location.long[1:(nrow(data)-1)], data$location.lat[1:(nrow(data)-1)])
coord_min1 <- SpatialPoints(longlat_min1, proj4string=CRS("EPSG:4326"))

data$ddist_2D <- c(NA, distGeo(p1 = coord, p2 = coord_min1)) # in m
index_ID.switch <- which(data$individual.global.identifier[2:nrow(data)] != data$individual.global.identifier[1:(nrow(data)-1)]) + 1
if(length(index_ID.switch) != 0){data$ddist_2D[index_ID.switch] <- NA}

# Delta time (minutes)
data$dtime_new <- NA
data$dtime_new[2:nrow(data)] <- (as.numeric(data$timestamp[2:nrow(data)]) - 
                                   as.numeric(data$timestamp[1:(nrow(data)-1)])) / 60
if(length(index_ID.switch) != 0){data$dtime_new[index_ID.switch] <- NA}

# Segment speed
data$seg.speed_2D <- data$ddist_2D / (data$dtime_new * 60)

# Check
hist(data$seg.speed_2D); data$seg.speed_2D[order(data$seg.speed_2D, decreasing = T)][1:50]
hist(data$seg.speed_2D[which(data$seg.speed_2D > 10 & data$seg.speed_2D < 80)])
hist(data$seg.speed_2D[which(data$seg.speed_2D > 10 & data$seg.speed_2D < 80)], ylim = c(0,5000))
length(which(data$seg.speed_2D > 30)) # 739
# -> Use 30 m/s as threshold
# Note that threshold of 30 m/s implies that ddist may be 108 km for dtime of 1 h, without recognition as outlier!

seg.speed.threshold <- 30

# Code separated in extra-file
run <- 0 # Counter
n_orig <- nrow(data)
n_outl_collect <- NULL # Vector to collect n_outl per run

# "START"
source(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_INPUT_seg.speeds_2D.R")) # Using 30 m/s as threshold
run; outl # outl == 1 as long as unrealistic segment speeds present. run tells the number of times that Seg.speed.R script was running.
delta_n <- n_orig - nrow(data); delta_n
# Rerun code from "START" to here until outl == 0. -> No seg.speed_2D > 30 left.
# 28 runs, 882 outliers removed.
n_outl_collect
plot(n_outl_collect ~ 1:n_outl_collect)
# Last 16 runs with only 1 outlier removed [always same index value]! -> Problematic!?

# New columns:
summary(data$ddist_2D)
hist(data$ddist_2D)
sort(data$ddist_2D, decreasing = T)[1:20]

index_view <- which(data$ddist_2D == sort(data$ddist_2D, decreasing = T)[1])
# View(data[(index_view-5):(index_view+5),c("tag.local.identifier", "timestamp", "ddist_2D")])
# 1st is Major de Rians: Data gap between 2023-04-19 14:03:53 (Mauritania) and 2023-05-05 16:06:40 (Algeria)

index_view <- which(data$ddist_2D == sort(data$ddist_2D, decreasing = T)[2])
# View(data[(index_view-5):(index_view+5),c("tag.local.identifier", "timestamp", "ddist_2D")])

index_large_ddist <- which(data$ddist_2D %in% sort(data$ddist_2D, decreasing = T)[1:20])
data$dtime_new[index_large_ddist]
summary(data$dtime_new[index_large_ddist])
# It's all relatively large time intervals! -> Looks fine!

summary(data$seg.speed_2D)
hist(data$seg.speed_2D)
hist(data$seg.speed_2D, breaks = 50)

rm(longlat, longlat_min1, coord, coord_min1, index_ID.switch,
   run, outl, delta_n, n_outl_collect, n_orig, seg.speed.threshold, index_view, index, index_large_ddist,
   index_outl)


# 2) Identified via Movebank overview map beforehand

index_outl <- c(which(data$location.lat < 9), # Atlantic ocean around equator (n = 27)
                which(data$location.lat > 36.2 & data$location.lat < 37.5 &
                        data$location.long > -1.7 & data$location.long < 0.1)) # Mediterranean between Murcia and Oran (n = 7)
length(index_outl) # None left!

if(length(index_outl) > 0){data <- data[-index_outl,]}
rm(index_outl)


# Determine Lon/lat bounding box
# For download ERA5 data

range(data$location.lat)
range(data$location.long)

c(floor(min(data$location.lat)), ceiling(max(data$location.lat))) # 9 to 49
c(floor(min(data$location.long)), ceiling(max(data$location.long))) # -15 to 13


# Check lat/long lineplots per bird-year

data$bird_year <- paste(data$individual.global.identifier,
                        substr(data$timestamp,1,4), sep = "_")
summary(factor(data$bird_year))

pdf(paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Lat-lon_p.birdyear_"), Sys.Date(), ".pdf", sep = ""),
    height = 4, width = 10)

par(mfrow = c(1,3))

for(i in sort(unique(data$bird_year))){
  index <- which(data$bird_year == i)
  plot(data$location.long[index] ~ data$timestamp[index], type = "l", xlab = "Time", ylab = "Longitude")
  plot(data$location.lat[index] ~ data$timestamp[index], type = "l", xlab = "Time", ylab = "Longitude")
  plot(data$location.lat[index] ~ data$location.long[index], type = "l", xlab = "Longitude", ylab = "Latitude")
  mtext(paste(data$individual.local.identifier[index[1]], substr(data$timestamp[index[1]],1,4)),
        outer = T, line = -2.5, xpd = NA, font = 2)
}

dev.off()


# Couple SRTM elevation data ####

data$ground.level_srtm <- NA

filenames.srtm <- list.files(
  here("Alice/SRTM 3_Large tiles"),
  pattern = "\\.tif\\.tif$",
  full.names = TRUE
)
file.exists(filenames.srtm[3])
srtm_temp <- rast(filenames.srtm[3])
plot(srtm_temp)
terra::crs(srtm_temp)
summary(srtm_temp) # There are NA values!

data_sf <- st_as_sf(data, coords = c("location.long", "location.lat"), crs = st_crs("EPSG:4326"))
data_sf <- st_transform(data_sf, crs(srtm_temp))

terra::crs(srtm_temp) == terra::crs(data_sf) # TRUE

cbind(st_coordinates(data_sf)[,1], data$location.long)[1:100,]
all.equal(st_coordinates(data_sf)[,1], data$location.long)
# -> Tranformation appears not necessary in the end.

start.time <- Sys.time()
total <- length(filenames.srtm)
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)

library(terra)
for(i in 1:length(filenames.srtm)){
  
  # i <- 49
  srtm_temp <- rast(filenames.srtm[i])
  # plot(srtm_temp)
  
  index <- which(st_coordinates(data_sf)[,1] > xmin(srtm_temp) &
                   st_coordinates(data_sf)[,1] <= xmax(srtm_temp) &
                   st_coordinates(data_sf)[,2] > ymin(srtm_temp) &
                   st_coordinates(data_sf)[,2] <= ymax(srtm_temp))
  
  if(length(index) > 0){
    print(i)
    # print(summary(values(srtm_temp))) # No NA values present!? -> Doesn't work here.
    # plot(data_sf[index,], add =  T) # Pay attention, this can take a long time!
    data$ground.level_srtm[index] <- extract(srtm_temp, data_sf[index,], ID = F)[,1]
  }
  
  setWinProgressBar(pb, i, title = paste(round(i/total*100, 0), "% done"))
}
close(pb)
comp.time <- Sys.time() - start.time; comp.time # 6 min

summary(data$ground.level_srtm) #38k NA's
hist(data$ground.level_srtm)

# Check NA's
summary(data$location.lat[is.na(data$ground.level_srtm)])
hist(data$location.lat[is.na(data$ground.level_srtm)])

summary(data$location.long[is.na(data$ground.level_srtm)])
hist(data$location.long[is.na(data$ground.level_srtm)])

plot(data$location.lat[is.na(data$ground.level_srtm)] ~
       data$location.long[is.na(data$ground.level_srtm)], xlab = "Longitude", ylab = "Latitude")
# View(data[which(is.na(data$ground.level_srtm) & data$location.long > -5 & data$location.lat < 38),])
# Single locations over sea along the coast in Spain and Tunisia

# View(data[which(is.na(data$ground.level_srtm) & data$location.long > -5 & data$location.lat > 42),])
# Single locations over sea along the coast in W France

data$over_sea <- ifelse(is.na(data$ground.level_srtm), 1, 0)
summary(factor(data$over_sea))

data$ground.level_srtm[is.na(data$ground.level_srtm)] <- 0

# Clean up workspace
rm(pb, data_sf, srtm_temp, i, start.time, comp.time, total, filenames.srtm, index)


# Geoid correction for GPS height data ####

colnames(data)[which(colnames(data) == "height.above.msl")] <- "height.above.msl_GPS"
colnames(data)[which(colnames(data) == "height.above.ellipsoid")] <- "height.above.ellipsoid_GPS"

# Store original height a.s.l. in separate column
data$height.above.msl_GPS_orig <- data$height.above.msl_GPS

# Check height above ellipsoid
summary(data$height.above.ellipsoid_GPS)

msl_ellips_diff <- data$height.above.msl_GPS_orig - data$height.above.ellipsoid_GPS
summary(msl_ellips_diff)
hist(msl_ellips_diff[msl_ellips_diff > -100])
# It should all be between -55 and -20, but some aberrant cases!
sort(msl_ellips_diff)[1:10] # 1 with -4100
rev(sort(msl_ellips_diff))[1:200] # 71 with 0 to -4

index_false <- which(msl_ellips_diff < -100 | msl_ellips_diff > -15)
length(index_false) # 72

# Set height above ellipsoid to NA for these cases
data$height.above.ellipsoid_GPS[index_false] <- NA

# Retrieve geoid height from EGM96 raster
egm96 <- rast(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/us_nga_egm96_15.tif"))
# plot(egm96)
hist(values(egm96))
crs(egm96)

data_sf <- st_as_sf(data[,c("location.long", "location.lat")], coords = c("location.long", "location.lat"), crs = st_crs("EPSG:4326"))
data_sf <- st_transform(data_sf, crs(egm96))

terra::crs(egm96) == terra::crs(data_sf)

start.time <- Sys.time()
data$geoid_height_egm96 <- extract(egm96, data_sf, ID = F)[,1]
comp.time <- Sys.time() - start.time; comp.time # 4-10 min

summary(data$geoid_height_egm96)
unique(data$geoid_height_egm96)

rm(data_sf, egm96)

# Update height above msl
data$height.above.msl_GPS <- data$height.above.ellipsoid_GPS - data$geoid_height_egm96

# Compare original and new height a.s.l.
summary(data$height.above.msl_GPS - data$height.above.msl_GPS_orig) # Median -0.6, min -15.0, max 9.0

rm(index_false, start.time, comp.time, msl_ellips_diff)


# Calculate height above ground level ####

data$height.above.ground_GPS <- data$height.above.msl_GPS - data$ground.level_srtm
summary(data$height.above.ground_GPS)

summary(data$height.above.ground_GPS[data$res == "high" & data$move == "move"]) # Median 308 m
summary(data$height.above.ground_GPS[data$res == "low" & data$move == "move"]) # Median 229 m

hist(data$height.above.ground_GPS[data$height.above.ground_GPS > -30 & data$height.above.ground_GPS < 1000 &
                                    data$move == "move"])

# Quick check
props_GPS_high <- summary(cut(data$height.above.ground_GPS[which(is.na(data$height.above.ground_GPS) == F & data$res == "high" & data$move == "move")],
                              breaks = c(min(data$height.above.ground_GPS, na.rm = T), seq(-100,1500,5), max(data$height.above.ground_GPS, na.rm = T)),
                              include.lowest = T, right = F), maxsum = 1000) /
  length(which(is.na(data$height.above.ground_GPS) == F & data$res == "high" & data$move == "move"))
names(which.max(props_GPS_high[-c(1,length(props_GPS_high))])) # 145-150 -> 160 - 175

props_GPS_low <- summary(cut(data$height.above.ground_GPS[which(is.na(data$height.above.ground_GPS) == F & data$res == "low" & data$move == "move")],
                             breaks = c(min(data$height.above.ground_GPS, na.rm = T), seq(-100,1500,5), max(data$height.above.ground_GPS, na.rm = T)),
                             include.lowest = T, right = F), maxsum = 500) /
  length(which(is.na(data$height.above.ground_GPS) == F & data$res == "low" & data$move == "move"))
names(which.max(props_GPS_low[-c(1,length(props_GPS_low))])) # 140-145

plot(c(1) ~ 1, type = "n", main = "Short-toed Eagle flight height year-round",
     xlim = c(-30,1000), ylim = c(0,1.35), yaxs = "i",
     xlab = "Height above ground (m)", ylab = "Proportion of positions (%)")
polygon(c(32,200,200,32), c(-5,-5,8,8), col = "gray90", border = NA)
abline(h = seq(0.2,1.2,0.2), col = "gray80")
abline(v = seq(0,1000,50), col = "gray80")
lines(props_GPS_high[2:(length(props_GPS_high)-1)]*100 ~ c(seq(-100,1495,5) + 2.5), lty = 1)
lines(props_GPS_low[2:(length(props_GPS_low)-1)]*100 ~ c(seq(-100,1495,5) + 2.5), lty = 2)
box()
corners <- par("usr")
legend(legend = c("Low-frequency", "High-frequency"),
       lty = c(2,1), col = "black", xjust = 1, yjust = 1,
       x = corners[2]-(corners[2]-corners[1])*0.02, y = corners[4]-(corners[4]-corners[3])*0.05,
       seg.len = 1, bg = "white")

rm(corners, props_GPS_high, props_GPS_low)


# Re-define flight positions based on height ####

summary(data$height.above.ground_GPS[data$move == "sit"])
hist(data$height.above.ground_GPS[data$move == "sit" & data$height.above.ground_GPS > -100 &
                                    data$height.above.ground_GPS < 500], breaks = 50)

length(which(data$height.above.ground_GPS[data$move == "sit"] > 50)) / length(which(data$move == "sit")) # 21% -> 23%
length(which(data$height.above.ground_GPS[data$move == "sit"] > 30)) / length(which(data$move == "sit")) # 29% -> 31%
length(which(data$height.above.ground_GPS[data$move == "sit"] > 20)) / length(which(data$move == "sit")) # 39% -> 41%

length(which(data$height.above.ground_GPS[data$move == "sit" & data$res == "low"] > 50)) /
  length(which(data$move == "sit" & data$res == "low")) # 19% -> 19%
length(which(data$height.above.ground_GPS[data$move == "sit" & data$res == "high"] > 50)) /
  length(which(data$move == "sit" & data$res == "high")) # 22% -> Small difference high-res/low-res -> 25%

length(which(data$height.above.ground_GPS[data$move == "move"] < 50)) / length(which(data$move == "move")) # 7% -> 6%

# -> Use 50 m as threshold

data$move_2 <- data$move
data$move_2[which(data$height.above.ground_GPS > 50)] <- "move"

# Check again "flight" positions during night

speed_thresholds_summary$prop.flying_day_2 <- NA
speed_thresholds_summary$prop.flying_night_2 <- NA

data$species_tag_type_res <- paste(data$individual.taxon.canonical.name, data$tag.manufacturer.name, data$res)
for(i in 1:nrow(speed_thresholds_summary)){
  index <- which(data$species_tag_type_res == speed_thresholds_summary$species_tag_type_res[i])
  if(length(index) > 0){
    speed_thresholds_summary$prop.flying_day_2[i] <-
      round(length(which(data$daynight_6[index] == "day" & data$move_2[index] == "move")) /
              length(which(data$daynight_6[index] == "day")) * 100,2)
    speed_thresholds_summary$prop.flying_night_2[i] <- 
      round(length(which(data$daynight_6[index] == "night" & data$move_2[index] == "move")) /
              length(which(data$daynight_6[index] == "night")) * 100,2)
  }
}

# View(speed_thresholds_summary)
# -> prop.flying_night increases from 0.31 to 9.40% in low-res (0.38 to 0.47 in high-res)
# -> Bad idea! Only apply to high-res!

data$move_2 <- data$move
data$move_2[which(data$height.above.ground_GPS > 50 & data$res == "high")] <- "move"

rm(i, index)


# Add country names ####

library(rnaturalearthdata)

countries <- ne_countries(scale = "medium") # Not on very fine scale. Keep in mind.

longlat <- cbind(data$location.long, data$location.lat)
coord <- SpatialPoints(longlat, proj4string = CRS("EPSG:4326"))

#countries_sp <- as(countries, "Spatial")  # or sf::as_Spatial(countries)
#coord <- spTransform(coord, countries@proj4string) # Takes long.Peut être inutile car ne_countries renvoie un sf et non un sp
#coord <- spTransform(coord, countries_sp@proj4string)

start.time <- Sys.time()
pts_sf <- st_as_sf(data, coords = c("location.long", "location.lat"), crs = 4326)
data$country <- st_join(pts_sf, countries, left = TRUE)$name # <= ############################## Modif Alice
# Ajoute a data
str(data) # Ok

delta.time <- Sys.time() - start.time; delta.time # 3.3 min -> 4 min


summary(as.factor(data$country))
# NA should be oversea locations.
summary(as.factor(data$over_sea[which(is.na(data$country))]))
# ... and coastline locations (coarse country polygons).

rm(longlat, coord, countries, start.time, delta.time)


# Save/load intermediate workspace (3) ####

# save.image(paste("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_3_", Sys.Date(), ".RData", sep = ""))
# load("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_3_2025-04-07.RData"


# Remove vertical outliers ####
# Based on segment speed

# Delta height (meters)
data$dheight <- c(NA, abs(data$height.above.msl_GPS[2:nrow(data)] - data$height.above.msl_GPS[1:(nrow(data)-1)])) # in m
index_ID.switch <- which(data$individual.global.identifier[2:nrow(data)] != data$individual.global.identifier[1:(nrow(data)-1)]) + 1
if(length(index_ID.switch) != 0){data$dheight[index_ID.switch] <- NA}
hist(data$dheight)

# Delta time (minutes)
data$dtime_new <- NA
data$dtime_new[2:nrow(data)] <- (as.numeric(data$timestamp[2:nrow(data)]) - 
                                   as.numeric(data$timestamp[1:(nrow(data)-1)])) / 60
if(length(index_ID.switch) != 0){data$dtime_new[index_ID.switch] <- NA}

# Vertical segment speed
data$seg.speed_vert <- data$dheight / (data$dtime_new * 60) # m/s

# Check
data$seg.speed_vert[order(data$seg.speed_vert, decreasing = T)][1:50]
hist(data$seg.speed_vert)
hist(data$seg.speed_vert[which(data$seg.speed_vert > 10 & data$seg.speed_vert < 80)])
length(which(data$seg.speed_vert > 30)) # 216 -> 317
length(which(data$seg.speed_vert > 50)) # 106 -> 147
summary(factor(data$res[which(data$seg.speed_vert > 30)])) # Is nearly all in high-res.-> (314 contre 3)
summary(factor(data$res[which(data$seg.speed_vert > 50)])) # Is all in high-res! -> 147
# -> Based on histogram, use 50 m/s as threshold.
# Note that threshold of 30 m/s implies that ddist may be 108 km for dtime of 1 h, without recognition as outlier!

seg.speed.threshold <- 50

# Code separated in extra-file
run <- 0 # Counter
n_orig <- nrow(data)
n_outl_collect <- NULL # Vector to collect n_outl per run

# "START"
source(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_INPUT_seg.speeds_vert.R"))
run; outl # outl == 1 as long as unrealistic segment speeds present. run tells the number of times that Seg.speed.R script was running. -> OK
delta_n <- n_orig - nrow(data); delta_n
# Rerun code from "START" to here until outl == 0. -> No seg.speed_vert > 30 left.
# 5 runs, 114 outliers removed.
n_outl_collect
plot(n_outl_collect ~ 1:n_outl_collect)

# New columns:
summary(data$dheight)
hist(data$dheight)
sort(data$dheight, decreasing = T)[1:20]

index_view <- which(data$dheight == sort(data$dheight, decreasing = T)[1])
# View(data[(index_view-5):(index_view+5),c("tag.local.identifier", "timestamp", "dheight")])

summary(factor(data$res[which(data$dheight > 2000)])) # All > low-res.
summary(data$dtime_new[which(data$dheight > 2000)]) # All > 4 min.

summary(data$seg.speed_vert)
hist(data$seg.speed_vert)
hist(data$seg.speed_vert, breaks = 50)

summary(data$seg.speed_vert[data$res == "high"])
hist(data$seg.speed_vert[data$res == "high"])

summary(data$height.above.ground_GPS[data$res == "high"])
length(which(data$height.above.ground_GPS < -100 & data$res == "high")) # 17 -> 20

rm(index_ID.switch, run, outl, delta_n, n_outl_collect, n_orig,
   seg.speed.threshold, index_view, index, index_large_ddist, index_outl)



# Annotate high res blocks ####

data_HF <- data[which(data$res == "high"),]
str(data_HF)

# New dtime variable
data_HF$dtime_new[2:nrow(data_HF)] <- (as.numeric(data_HF$timestamp[2:nrow(data_HF)]) - 
                                         as.numeric(data_HF$timestamp[1:(nrow(data_HF)-1)])) / 60
index_ID.switch <- which(data_HF$tag.local.identifier[2:nrow(data_HF)] != data_HF$tag.local.identifier[1:(nrow(data_HF)-1)]) + 1
if(length(index_ID.switch) != 0){data_HF$dtime_new[index_ID.switch] <- NA}

summary(data_HF$dtime_new)
which(data_HF$dtime_new < 0) # 0

# Variable indicating first fix of HR bouts
data_HF$first_fix <- 0
data_HF$first_fix[1] <- 1
data_HF$first_fix[index_ID.switch] <- 1

index_Milsar <- which(data_HF$tag.manufacturer.name == "Milsar")
data_HF$first_fix[index_Milsar[which(data_HF$dtime_new[index_Milsar] > 7/60)]] <- 1

index_Ornitela <- which(data_HF$tag.manufacturer.name == "Ornitela")
data_HF$first_fix[index_Ornitela[which(data_HF$dtime_new[index_Ornitela] > 6/60)]] <- 1

index_UvABiTS <- which(data_HF$tag.manufacturer.name == "UvA-BiTS")
data_HF$first_fix[index_UvABiTS[which(data_HF$dtime_new[index_UvABiTS] > 15/60)]] <- 1

summary(factor(data_HF$first_fix))

# View(data_HF[1:10000,c("individual.local.identifier", "timestamp", "first_fix")])

# Determine blocks switches (either first fix or where tag changes)
ID.switch <- c(1, which(data_HF$tag.local.identifier[2:nrow(data_HF)] != data_HF$tag.local.identifier[1:(nrow(data_HF)-1)]) + 1)
block.switch <- c(which(data_HF$first_fix == 1))
switches <- sort(unique(c(ID.switch, block.switch)))

# For high-res blocks, add block ID and dtime from start of block

total <- length(switches)
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)
start.time <- Sys.time()

index_collect <- HF.ID_collect <- HF_dtime.from.start_collect <- NULL

count <- 0
for(i in 1:length(switches)){
  count <- count + 1
  if(is.na(switches[i+1]) == F){
    index <- switches[i]:(switches[i+1]-1)
  } else {
    index <- switches[i]:nrow(data_HF)
  }
  
  index_collect[(length(index_collect)+1):(length(index_collect)+length(index))] <- index
  HF.ID_collect[(length(HF.ID_collect)+1):(length(HF.ID_collect)+length(index))] <- count
  HF_dtime.from.start_collect[(length(HF_dtime.from.start_collect)+1):(length(HF_dtime.from.start_collect)+length(index))] <- cumsum(data_HF$dtime_orig[index])
  
  setWinProgressBar(pb, count, title = paste(round(count/total*100, 0), "% done - block ID: ", count))
}
close(pb)
delta.time <- Sys.time() - start.time; delta.time
# 20 sec for entire data_HF set (10M positions).

data_HF$HF.ID <- data_HF$HF_dtime.from.start <- NA
data_HF$HF.ID[index_collect] <- HF.ID_collect
data_HF$HF_dtime.from.start[index_collect] <- HF_dtime.from.start_collect

range(data_HF$HF.ID)
HR.bout_summary <- summary(factor(data_HF$HF.ID), maxsum = 3000)
sort(HR.bout_summary, decreasing = T)[1:100]
sort(HR.bout_summary, decreasing = F)[1:100]
summary(HR.bout_summary) # Median 245
hist(HR.bout_summary)
length(which(HR.bout_summary < 100)) / length(HR.bout_summary) # 34%
length(which(HR.bout_summary > 20)) / length(HR.bout_summary) # 87% > 1 minute

summary(data_HF$HF_dtime.from.start)
hist(data_HF$HF_dtime.from.start)

# Check individual tracks
index <- which(data_HF$HF.ID == 9)
summary(factor(data_HF$move_2[index]))
data_HF$tag.local.identifier[index[1]]
data_HF$timestamp[index[1]]; data_HF$timestamp[index[length(index)]]
plot(data_HF$location.lat[index] ~ data_HF$location.long[index], type = "l", xlab = "Longitude", ylab = "Latitude")
# Works.

rm(switches, ID.switch, block.switch, index_ID.switch,
   HF_dtime.from.start_collect, HF.ID_collect,
   i, start.time, index_Milsar, index_Ornitela, index_UvABiTS,
   count, delta.time, dtime, total, index_collect, pb) # Warning inoffensif


# Remove first minute of HR blocks ####

index.remove.startofbout <- which(data_HF$HF_dtime.from.start < 1)
length(index.remove.startofbout) # 49512
data_HF <- data_HF[-index.remove.startofbout,]

rm(index.remove.startofbout)

data_HF <- data_HF[,-which(colnames(data_HF) == "first_fix")]

length(unique(data_HF$HF.ID)) # Still 2134 HR tracks

summary(data_HF$height.above.ground_GP[data_HF$move_2 == "move"])
sort(data_HF$height.above.ground_GPS[data_HF$move_2 == "move"])[1:100]
quantile(data_HF$height.above.ground_GPS[data_HF$move_2 == "move"], 0.01, na.rm = T) # 15 m
sort(data_HF$height.above.ground_GPS[data_HF$move_2 == "move"], decreasing = T)[1:100]
quantile(data_HF$height.above.ground_GPS[data_HF$move_2 == "move"], 0.99, na.rm = T) # 1694 m!
# Still some clear outliers to the bottom, but apparently none to the top!


# Add positive/negative dheight and altitude gain ####

# Calculate dheight with positive/negative values (already existing dheight variable is absolute)

data_HF$dheight_pm <- NA
data_HF$dheight_pm[2:nrow(data_HF)] <-
  data_HF$height.above.msl_GPS[2:nrow(data_HF)] -
  data_HF$height.above.msl_GPS[1:(nrow(data_HF)-1)]

index_ID.switch <- which(data_HF$HF.ID[2:nrow(data_HF)] != data_HF$HF.ID[1:(nrow(data_HF)-1)]) + 1
if(length(index_ID.switch) != 0){data_HF$dheight_pm[index_ID.switch] <- NA}

all(abs(data_HF$dheight_pm) == data_HF$dheight, na.rm = T)

hist(data_HF$dheight_pm)
summary(data_HF$dheight_pm)

data_HF$alt_gain <- ifelse(data_HF$dheight_pm > 0, data_HF$dheight_pm, NA)
hist(data_HF$alt_gain)
sort(data_HF$alt_gain, decreasing = T)[1:10]


# Subsampling ####

# Subsample to 5 min interval

timestamp_num_min <- as.numeric(data$timestamp) / 60

dtime.aim <- 5 # Both in minutes
tolerance <- 30/60
# Positions > (dtime.aim - tolerance) can be selected, but only if no other positions are closer to dtime.aim
dindex_max <- tolerance * 60 * 2 # Assuming minimum time interval between subsequent positions of 1 sec

sub.index <- NULL

start.time <- Sys.time()
total <- nrow(data)
pb <- winProgressBar(title = "Subsampling progress:", min = 0, max = total, width = 300)
count <- 1

for(j in unique(data$tag.local.identifier)){
  index_tag <- which(data$tag.local.identifier == j)
  sub.index[length(sub.index) + 1] <- index_tag[1] # Always take first position per tag
  
  if(length(index_tag) > 1){
    for(k in 2:length(index_tag)){
      if(index_tag[k] > sub.index[length(sub.index)]){
        
        if(timestamp_num_min[index_tag[k]] -
           timestamp_num_min[sub.index[length(sub.index)]] > (dtime.aim - tolerance)){
          # Only look further if position has minimum time difference to last in vector
          
          if(k == length(index_tag)){
            # Directly take position if none comes after
            sub.index[length(sub.index) + 1] <- index_tag[k]
            
          } else if((timestamp_num_min[index_tag[k] + 1] -
                     timestamp_num_min[sub.index[length(sub.index)]]) > (dtime.aim + tolerance)) {
            # Take position if NEXT position has "large" distance to last in vector 
            sub.index[length(sub.index) + 1] <- index_tag[k]
            
          } else {
            # Check next positions, determine which is closest to dtime.aim
            # index_max either based on dtime.aim + tolerance, or last position of tag
            index_max <- ifelse((k + dindex_max) < length(index_tag), (k + dindex_max), length(index_tag))
            
            diffs.to.aim <- abs((timestamp_num_min[index_tag[k]:index_tag[index_max]] -
                                   timestamp_num_min[sub.index[length(sub.index)]]) - dtime.aim)
            
            index_next <- which(diffs.to.aim == min(diffs.to.aim))[1]
            
            # Take position closest to dtime.aim (not necessarily position k)
            sub.index[length(sub.index) + 1] <- (index_tag[k]:index_tag[index_max])[index_next]
          }
        }
      }
      setWinProgressBar(pb, count, title = paste("Subsampling progress:", round(count/total*100, 0), "% done"))
      count <- count + 1
    }
  }
}
close(pb)
comp.time <- Sys.time() - start.time; comp.time # 0.5 min -> 2.5?

# Check if duplicates are present
if(anyDuplicated(sub.index) > 0){print("Issue: Subsampling produced duplicates.")}

data_sub5 <- data[sub.index,]

summary(factor(data_sub5$res)) / nrow(data_sub5) # 97.2% LR

rm(pb, j, k, sub.index, dtime.aim, tolerance, index_max, dindex_max, index_next,
   start.time, comp.time, index_tag, count, total, timestamp_num_min, diffs.to.aim)



# Sample size overview per individual ####

year <- as.numeric(substr(data$timestamp,1,4)); summary(factor(year))

# Build overview table
ov_ind <- data.frame(individual.global.identifier = unique(data$individual.global.identifier),
                     individual.local.identifier = NA, 
                     sex =  NA, age.1st.depl = NA, period = NA, n_tags = NA, tag.local.identifier = NA,
                     tag.manufacturer.name = NA, tag.model.orig = NA,
                     n_days = NA, n_fixes_day = NA, n_fixes_day_sub5 = NA, prop_move_day = NA,
                     n_fixes_night = NA, prop_move_night = NA,
                     n_fixes_HF = NA, TS_HF = NA)

for(i in 1:nrow(ov_ind)){
  index_day <- which(data$individual.global.identifier == ov_ind$individual.global.identifier[i] &
                       data$daynight_0 == "day")
  ov_ind$individual.local.identifier[i] <- data$individual.local.identifier[index_day[1]]
  ov_ind$sex[i] <- data$individual.sex[index_day[1]]
  ov_ind$age.1st.depl[i] <- data$individual.age.1st.depl[index_day[1]]
  min_year <- min(year[index_day]); max_year <- max(year[index_day])
  ov_ind$period[i] <- ifelse(min_year == max_year, min_year,
                             paste(min_year, substr(max_year,3,4), sep = "-"))  
  ov_ind$n_tags[i] <- length(unique(data$tag.local.identifier[index_day]))
  ov_ind$tag.local.identifier[i] <- paste(sort(unique(data$tag.local.identifier[index_day])), collapse = ", ")
  ov_ind$tag.manufacturer.name[i] <- paste(sort(unique(data$tag.manufacturer.name[index_day])), collapse = ", ")
  ov_ind$tag.model.orig[i] <- paste(sort(unique(data$tag.model[index_day])), collapse = ", ")
  
  ov_ind$n_days[i] <- length(unique(substr(data$timestamp[index_day],1,10)))
  ov_ind$n_fixes_day[i] <- length(index_day)
  
  index_day_sub5 <- which(data_sub5$individual.global.identifier == ov_ind$individual.global.identifier[i] &
                            data_sub5$daynight_0 == "day")
  ov_ind$n_fixes_day_sub5[i] <- length(index_day_sub5)
  ov_ind$prop_move_day[i] <- round(length(which(data_sub5$move_2[index_day_sub5] == "move")) /
                                     length(index_day_sub5) *100,2)
  
  index_night <- which(data$individual.global.identifier == ov_ind$individual.global.identifier[i] &
                         data$daynight_0 == "night")
  ov_ind$n_fixes_night[i] <- length(index_night)
  ov_ind$prop_move_night[i] <- round(length(which(data$move[index_night] == "move")) /
                                       length(index_night) *100,2)
  
  index_HF <- which(data_HF$individual.global.identifier == ov_ind$individual.global.identifier[i])
  ov_ind$n_fixes_HF[i] <- length(index_HF)
  ov_ind$TS_HF[i] <- sum(data_HF$dtime_orig[index_HF]) / 60
}

# Check
summary(factor(ov_ind$n_tags)) # 0 ind. with 2 tags
summary(ov_ind$n_fixes_day)
# View(ov_ind[order(ov_ind$n_fixes_day, decreasing = T),])
ov_ind[order(ov_ind$n_fixes_day, decreasing = T),c("individual.local.identifier", "n_fixes_day")]

summary(ov_ind$prop_move_day)
hist(ov_ind$prop_move_day) # Quite large spread!

par(mfrow = c(1,1), mar = c(5,4,4,2))
plot(ov_ind$prop_move_day ~ ov_ind$prop_move_night)

sum(ov_ind$TS_HF) # 1441 h! -> 2314.882

# Re-arrange columns
colnames(ov_ind)
ov_ind <- ov_ind[,-which(colnames(ov_ind) %in% c("individual.global.identifier", "n_tags", "tag.manufacturer.name"))]

rm(i, index_day, index_night, max_year, min_year, year)


# Save/load intermediate workspace (4) ####

# save.image(paste("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_4_", Sys.Date(), ".RData", sep = ""))
# load(Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Workspace_intermediate_4_2025-04-02.RData")


# Remove columns ####

colnames(data)
colnames_rm <- c("tag.manufacturer.name",
                 "study.name.2",
                 "individual.taxon.canonical.name",
                 "species_tag_type_res",
                 "geoid_height_egm96",
                 "individual.global.identifier",
                 "magnetic.field.raw.x", "magnetic.field.raw.y", "magnetic.field.raw.z",
                 "tag.voltage", "sensor.type", "height.above.msl_GPS_orig")
data <- data[,-which(colnames(data) %in% colnames_rm)]
data_HF <- data_HF[,-which(colnames(data_HF) %in% colnames_rm)]
data_sub5 <- data_sub5[,-which(colnames(data_sub5) %in% colnames_rm)]





# Export files ####

# Save processed tracking data
save(data, file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_all_A_"), Sys.Date(), ".RData", sep = ""))
save(data_HF, file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_HF_A_"), Sys.Date(), ".RData", sep = ""))
save(data_sub5, file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_sub5_A_"), Sys.Date(), ".RData", sep = ""))

# Additional tables
save(ov_ind, speed_thresholds_summary,
     file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_additional_tables_"), Sys.Date(), ".RData", sep = ""))

write.table(ov_ind,
            file = paste(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_Overview_individuals_"), Sys.Date(), ".csv", sep = ""),
            row.names = F, sep = ";")


