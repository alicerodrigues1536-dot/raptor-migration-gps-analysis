###################################
##            DATA
##          OUTPUT 2.1 
###################################
rm(list=ls())    # effacement de l'historique R

# Libraries

library(here)

library(sp)
#install.packages("plotKML")
#install.packages("https://cran.r-project.org/src/contrib/Archive/plotKML/plotKML_0.8-3.tar.gz", 
#                 repos = NULL, type = "source") -> Trop compliqué
library(plotKML)


# Define output directory for kml file
output.dir.kml <- here("Alice/STE_data_preparation_February_2026/2.Data_preparation/2.4_HF_tracks_kml/")

# Load data
load(here("Alice/STE_data_preparation_February_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_HF_A2026-02-24.Rdata"))

# Restrict to high-res blocks with minimum number of positions
summary_HR_blocks <- summary(factor(data_HF$HF.ID), maxsum = length(unique(data_HF$HF.ID)))
index.remove <- which(data_HF$HF.ID %in% names(summary_HR_blocks[which(summary_HR_blocks < 100)]))

length(index.remove) # 24057 positions to remove from blocks with < 100 pos.
data_HF <- data_HF[-index.remove,]

length(unique(data_HF$HF.ID)) # 1566
max(data_HF$HF.ID) # 2508

# Prepare
data_HF$timestamp_GE <- paste(substr(data_HF$timestamp, 1, 10), "T",
                              substr(data_HF$timestamp, 12, 19), ".000", sep = "")

dat <- data_HF
coordinates(dat) <- ~location.long+location.lat
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")

# Start loop

# Do in 2 chunks for computer memory reasons
# index_to.do <- 1:800
# index_to.do <- 801:length(unique(data_HF$HF.ID))
index_to.do <- 1451:length(unique(data_HF$HF.ID))

count <- 0
total <- length(index_to.do)
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)
start.time <- Sys.time()

# Inner loop (per high-res block)

for(i in unique(data_HF$HF.ID)[index_to.do]){
  
  index <- which(data_HF$HF.ID == i)
  count <- count + 1
  
  # Prepare kml
  
  longlat <- cbind(data_HF$location.long[index], data_HF$location.lat[index])
  line <- Line(longlat)
  line.sp <- SpatialLines(list(Lines(list(line),ID="a")), proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  kml_open(file.name = paste(output.dir.kml, i, ".kml", sep = ""),
           folder.name = paste("#", i, ": ", data_HF$individual.local.identifier[index[1]], " / ", data_HF$tag.local.identifier[index[1]], " ",
                               substr(data_HF$timestamp[index[1]],1,10), " ",
                               substr(data_HF$timestamp[index[1]],12,13), ":", substr(data_HF$timestamp[index[1]],15,16), "-",
                               substr(data_HF$timestamp[index[length(index)]],12,13), ":", substr(data_HF$timestamp[index[length(index)]],15,16), sep = ""))
  
  index_move <- which(data_HF$HF.ID == i & data_HF$move == "move")
  kml_layer(dat[index_move,], shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
            points_names = "", subfolder.name = "In-flight positions", colour = "red", size = 0.4,
            html.table = paste(data_HF$tag.local.identifier[index_move], " | ",
                               data_HF$timestamp[index_move], " | ",
                               round(data_HF$height.above.ground_GPS[index_move],0), " m agl (GPS alt: ",
                               round(data_HF$height.above.msl[index_move],0), " m; GL: ",
                               round(data_HF$ground.level_srtm[index_move],0), " m)", sep = ""),
            altitude = data_HF$height.above.ground_GPS[index_move], altitudeMode = "relativeToGround",
            TimeSpan.begin = data_HF$timestamp_GE[index_move],
            TimeSpan.end = data_HF$timestamp_GE[index_move])
  
  index_move_2 <- which(data_HF$HF.ID == i & data_HF$move_2 == "move" & data_HF$move == "sit")
  kml_layer(dat[index_move_2,], shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
            points_names = "", subfolder.name = "In-flight positions (< 1.75 m/s)", colour = "chartreuse3", size = 0.4,
            html.table = paste(data_HF$tag.local.identifier[index_move_2], " | ",
                               data_HF$timestamp[index_move_2], " | ",
                               round(data_HF$height.above.ground_GPS[index_move_2],0), " m agl (GPS alt: ",
                               round(data_HF$height.above.msl[index_move_2],0), " m; GL: ",
                               round(data_HF$ground.level_srtm[index_move_2],0), " m)", sep = ""),
            altitude = data_HF$height.above.ground_GPS[index_move_2], altitudeMode = "relativeToGround",
            TimeSpan.begin = data_HF$timestamp_GE[index_move_2],
            TimeSpan.end = data_HF$timestamp_GE[index_move_2])
  
  index_sit <- which(data_HF$HF.ID == i & data_HF$move_2 == "sit")
  kml_layer(dat[index_sit,], shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
            points_names = "", subfolder.name = "Perched positions", colour = "blue", size = 0.4,
            html.table = paste(data_HF$tag.local.identifier[index_sit], " | ",
                               data_HF$timestamp[index_sit], " | ",
                               round(data_HF$height.above.ground_GPS[index_sit],0), " m agl (GPS alt: ",
                               round(data_HF$height.above.msl[index_sit],0), " m; GL: ",
                               round(data_HF$ground.level_srtm[index_sit],0), " m)", sep = ""),
            altitude = data_HF$height.above.ground_GPS[index_sit], altitudeMode = "relativeToGround",
            TimeSpan.begin = data_HF$timestamp_GE[index_sit],
            TimeSpan.end = data_HF$timestamp_GE[index_sit])
  
  kml_layer(line.sp, colour = "red", subfolder.name = "Trajectory", width = 2)
  
  kml_close(file.name = paste(output.dir.kml, i, ".kml", sep = ""))
  
  setWinProgressBar(pb, count, title = paste(round(count/total*100, 0), " % done - block ID: ", i, sep = ""))
  
}

close(pb)
delta.time <- Sys.time() - start.time; delta.time # 1st chunk: 6-10 min.




##########################################################################"
library(here)
library(leaflet)
library(htmlwidgets)
library(sp)
library(dplyr)

# Define output directory
output.dir.kml <- here("Alice/STE_data_preparation_February_2026/2.Data_preparation/2.4_HF_tracks_html/")
dir.create(output.dir.kml, showWarnings = FALSE)

# Load data
load(here("Alice/STE_data_preparation_February_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_HF_A2026-02-24.Rdata"))

# Filter high-res blocks (>100 positions)
summary_HR_blocks <- table(data_HF$HF.ID)
index.remove <- which(data_HF$HF.ID %in% names(summary_HR_blocks[summary_HR_blocks < 100]))
data_HF <- data_HF[-index.remove,]
cat("Blocs HF gardés:", length(unique(data_HF$HF.ID)), "\n")

# Test sur 50 PREMIERS blocs (2 min)
index_to.do <- 1:50
total <- length(index_to.do)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for(j in 1:length(index_to.do)){
  i <- unique(data_HF$HF.ID)[index_to.do[j]]
  index <- which(data_HF$HF.ID == i)
  
  # Crée carte interactive
  m <- leaflet() %>%
    addProviderTiles("OpenStreetMap") %>%
    addPolylines(lng = data_HF$location.long[index], 
                 lat = data_HF$location.lat[index],
                 color = "red", weight = 3, opacity = 0.8) %>%
    addCircleMarkers(data = data_HF[index,],
                     lng = ~location.long, lat = ~location.lat,
                     color = ~case_when(move == "move" ~ "red",
                                        move_2 == "move" ~ "green",
                                        TRUE ~ "blue"),
                     radius = 4,
                     popup = ~paste(tag.local.identifier, timestamp,
                                    "<br>Alt:", round(height.above.ground_GPS,0), "m agl",
                                    "<br>Vitesse:", move_2)) %>%
    setView(lng = mean(data_HF$location.long[index]), 
            lat = mean(data_HF$location.lat[index]), zoom = 15)
  
  # Sauvegarde HTML
  saveWidget(m, file.path(output.dir.kml, paste0(i, ".html")), selfcontained = TRUE)
  setTxtProgressBar(pb, j)
}

close(pb)
cat("50 cartes HTML créées dans", output.dir.kml, "\n")

