###################################
##         RDATA
##      OUTPUT 2.1 
###################################
rm(list=ls())    # effacement de l'historique R

####
## Libraries
library(here)

# Preparations ####

# Load data_HF
load(here("Alice/STE_data_preparation_June_2026/2.Data_preparation/2.1_OUTPUT_STE_France_processed_HF_A_2026-11-06.RData"))

# Define output directory for Pdf files
output.dir.pdf <- here ("Alice/STE_data_preparation_June_2026/2.Data_preparation/")

# Check HR block ID
unique(data_HF$HF.ID)
length(unique(data_HF$HF.ID))
length(which(is.na(data_HF$HF.ID) == T)) # No NAs.

# Restrict to high-res blocks with minimum number of positions
summary_HR_blocks <- summary(factor(data_HF$HF.ID), maxsum = length(unique(data_HF$HF.ID)))
index.remove <- which(data_HF$HF.ID %in% names(summary_HR_blocks[which(summary_HR_blocks < 100)]))

length(index.remove) # 24057 positions to remove from blocks with < 100 pos.
data_HF <- data_HF[-index.remove,]

# n of blocks remaining
length(unique(data_HF$HF.ID)) # 1566

# Start loop

count <- 0
total <- length(unique(data_HF$HF.ID))
pb <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)
start.time <- Sys.time()

pdf(paste(output.dir.pdf, "2.3_OUTPUT_STE_France_HF_tracks", "_", Sys.Date(), ".pdf", sep = ""), height = 5, width = 15)
par(mfrow = c(1,3), oma = c(0,0,3,10))

for(i in unique(data_HF$HF.ID)){
  
  index <- which(data_HF$HF.ID == i)
  count <- count + 1
  
  # 1) Overview 2D plot
  plot(data_HF$location.lat[index] ~ data_HF$location.long[index], type = "l",
       xlab = "Longitude", ylab = "Latitude", main = "2D track")
  
  points(data_HF$location.lat[index][which(data_HF$move[index] == "move")] ~
           data_HF$location.long[index][which(data_HF$move[index] == "move")], col = "red", pch = 16, cex = 0.5)
  points(data_HF$location.lat[index][which(data_HF$move_2[index] == "move" & data_HF$move[index] == "sit")] ~
           data_HF$location.long[index][which(data_HF$move_2[index] == "move" & data_HF$move[index] == "sit")],
         col = "chartreuse3", pch = 16, cex = 0.5)
  points(data_HF$location.lat[index][which(data_HF$move_2[index] == "sit")] ~
           data_HF$location.long[index][which(data_HF$move_2[index] == "sit")], col = "blue", pch = 16, cex = 0.5)
  
  points(data_HF$location.lat[index[1]] ~ data_HF$location.long[index[1]], pch = 2, cex = 2)
  points(data_HF$location.lat[index[length(index)]] ~ data_HF$location.long[index[length(index)]], pch = 6, cex = 2)
  
  # 2) Abs. height ~ time
  ymax <- max(c(data_HF$height.above.msl_GPS[index], data_HF$ground.level_srtm[index]), na.rm = T)
  ymin <- min(c(data_HF$height.above.msl_GPS[index], data_HF$ground.level_srtm[index]), na.rm = T)
  
  plot(data_HF$height.above.msl_GPS[index] ~ data_HF$timestamp[index], type = "l", ylim = c(ymin, ymax),
       xlab = "Time", ylab = "Altitude a.s.l. (m)", main = "Height a.s.l.")
  lines(data_HF$ground.level_srtm[index] ~ data_HF$timestamp[index], col = "gray50", lwd = 2)
  
  points(data_HF$height.above.msl_GPS[index][which(data_HF$move[index] == "move")] ~
           data_HF$timestamp[index][which(data_HF$move[index] == "move")], col = "red", pch = 16, cex = 0.5)
  points(data_HF$height.above.msl_GPS[index][which(data_HF$move_2[index] == "move" & data_HF$move[index] == "sit")] ~
           data_HF$timestamp[index][which(data_HF$move_2[index] == "move" & data_HF$move[index] == "sit")],
         col = "chartreuse3", pch = 16, cex = 0.5)
  points(data_HF$height.above.msl_GPS[index][which(data_HF$move_2[index] == "sit")] ~
           data_HF$timestamp[index][which(data_HF$move_2[index] == "sit")], col = "blue", pch = 16, cex = 0.5)
  
  points(data_HF$height.above.msl_GPS[index[1]] ~ data_HF$timestamp[index[1]], pch = 2, cex = 2)
  points(data_HF$height.above.msl_GPS[index[length(index)]] ~ data_HF$timestamp[index[length(index)]], pch = 6, cex = 2)
  
  # 3) Rel. height ~ time
  ymax <- max(c(data_HF$height.above.ground_GPS[index], 0), na.rm = T)
  ymin <- min(c(data_HF$height.above.ground_GPS[index], 0), na.rm = T)
  
  plot(data_HF$height.above.ground[index] ~ data_HF$timestamp[index], type = "l",
       xlab = "Time", ylab = "Altitude a.g.l. (m)", main = "Height a.g.l.", ylim = c(ymin, ymax))
  
  abline(h = 0, col = "gray50", lwd = 2)
  
  points(data_HF$height.above.ground_GPS[index][which(data_HF$move[index] == "move")] ~
           data_HF$timestamp[index][which(data_HF$move[index] == "move")], col = "red", pch = 16, cex = 0.5)
  points(data_HF$height.above.ground_GPS[index][which(data_HF$move_2[index] == "move" & data_HF$move[index] == "sit")] ~
           data_HF$timestamp[index][which(data_HF$move_2[index] == "move" & data_HF$move[index] == "sit")],
         col = "chartreuse3", pch = 16, cex = 0.5)
  points(data_HF$height.above.ground_GPS[index][which(data_HF$move_2[index] == "sit")] ~
           data_HF$timestamp[index][which(data_HF$move_2[index] == "sit")], col = "blue", pch = 16, cex = 0.5)
  
  points(data_HF$height.above.ground_GPS[index[1]] ~ data_HF$timestamp[index[1]], pch = 2, cex = 2)
  points(data_HF$height.above.ground_GPS[index[length(index)]] ~ data_HF$timestamp[index[length(index)]], pch = 6, cex = 2)
  
  # Legend
  corners <- par("usr")
  legend(x = corners[2] + diff(corners[1:2])*0.05, y = corners[3] + diff(corners[3:4])*0.5, xjust = 0, yjust = 0.5, xpd = NA,
         pch = c(2,6,16,16,16,NA,NA), col = c("black", "black", "red", "chartreuse3", "blue", "gray50", "black"),
         pt.cex = c(1.7,1.7,1,1,1,NA,NA), bty = "n", y.intersp = 1.3,
         lty = c(NA,NA,NA,NA,NA,1,1), lwd = c(NA,NA,NA,NA,NA,2,1),
         legend = c("Start", "End", "Flying", "Flying (< 1.75 m/s)", "Stationary", "ground level", "GPS"))
  
  # legend(x = corners[2] + diff(corners[1:2])*0.05, y = corners[3] + diff(corners[3:4])*0.5, xjust = 0, yjust = 0.5, xpd = NA,
  #        pch = c(2,6,16,NA), col = c("black", "black", "red", "gray50"),
  #        pt.cex = c(1.7,1.7,1,NA), bty = "n", y.intersp = 1.3,
  #        lty = c(NA,NA,NA,1), lwd = c(NA,NA,NA,2),
  #        legend = c("Start", "End", "GPS pos.", "Ground level"))
  
  # Title
  title <- paste("#", i, ": ", data_HF$individual.local.identifier[index[1]], " / ", data_HF$tag.local.identifier[index[1]], " ",
                 substr(data_HF$timestamp[index[1]],1,10), " ",
                 substr(data_HF$timestamp[index[1]],12,13), ":", substr(data_HF$timestamp[index[1]],15,16), "-",
                 substr(data_HF$timestamp[index[length(index)]],12,13), ":", substr(data_HF$timestamp[index[length(index)]],15,16), sep = "")
  mtext(title, outer = T, side = 3, font = 3)
  
  setWinProgressBar(pb, count, title = paste(round(count/total*100, 0), " % done - block ID: ", count, sep = ""))
  
} # End of inner loop

dev.off()

close(pb)
delta.time <- Sys.time() - start.time; delta.time # 4 min.-> 6.65 min -> 6.89 min ->  10.87402 mins
