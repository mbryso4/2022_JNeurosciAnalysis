library(ggplot2)
library(dplyr)
library(tidyr)
library(gtools)
library(readxl)
#Must change this to location of saved "Collated_data.xlsx" file
Collated_data <- read_excel("E:/Data/Cord_Prep_New/Collated_data.xlsx")
#Collated_data <- read_excel("/E/My Passport/Data/Collated_data.xlsx")

coocheck <- Collated_data %>%
  group_by(File) %>%
  mutate(NumChan = length(unique(Channel))) %>%
  filter(NumChan > 1) %>%
  mutate(DurationInd = Duration*10) 
unifiles <- as.numeric(length(unique(coocheck$File)))
coocheck$File <- as.character(coocheck$File)
coofilesplit <-   split(coocheck, coocheck$File)
holddata <- data.frame(Chan1 = NA, Chan2 = NA, FileName = NA, 
                       Chan1Frequency =  NA, 
                       Chan2Frequency = NA, PerMatch = NA, 
                       Ch1Level = NA, Ch2Level = NA, 
                       Injury = NA, ID = NA, Drug = NA)
for(i in 1:unifiles){
  assign(paste0("Frame_", i), coofilesplit[[i]])
  chansplit <- split(coofilesplit[[i]], coofilesplit[[i]]$Channel)
  combs <- combinations(length(unique(chansplit)), 2, 1:length(chansplit), repeats.allowed = FALSE)
  for(q in 1:dim(combs)[1]){
    Ch1ind <- combs[q,1]
    Ch2ind <- combs[q,2]
    Ch1 <- chansplit[[Ch1ind]]
    Ch2 <- chansplit[[Ch2ind]]
    Ch1Times <- matrix( , nrow = 2000, ncol = 2000)
    Ch2Times <- matrix( , nrow= 2000, ncol = 2000)
    for(u in 1:dim(Ch1)[1]){
      Ch1length = length(1:Ch1$DurationInd[u]) + 1
      Ch1Times[u,1:Ch1length] <- Ch1$Burst_time[u]:(Ch1$Burst_time[u]+Ch1$DurationInd[u])}
    for(u in 1:dim(Ch2)[1]){
      Ch2length = length(1:Ch2$DurationInd[u]) + 1
      Ch2Times[u,1:Ch2length] <- Ch2$Burst_time[u]:(Ch2$Burst_time[u]+Ch2$DurationInd[u])}
    Ch1Times <- Ch1Times[!is.na(Ch1Times)]
    Ch2Times <- Ch2Times[!is.na(Ch2Times)]
    matching <- Ch1Times %in% Ch2Times
    percentmatch <- (length(matching[matching == TRUE]))/length(matching)
    newframe <- data.frame(Chan1 = Ch1ind, Chan2 = Ch2ind, FileName = unique(Ch1$File), 
                           Chan1Frequency =  unique(Ch1$Mean_Burst_Frequency), 
                           Chan2Frequency = unique(Ch2$Mean_Burst_Frequency), PerMatch = percentmatch, 
                           Ch1Level = unique(Ch1$Level), Ch2Level = unique(Ch2$Level), 
                           Injury = unique(Ch1$Injury), ID = unique(Ch1$ID), Drug = unique(Ch1$Drug))
    assign(paste0("Hold_", i, q), newframe)
  }
  all_coord <- do.call("rbind", lapply(ls(pattern="Hold_"),get))
}
rm(list=ls(pattern="Hold_"))
rm(list=ls(pattern="Frame_"))

library(xlsx)
write.xlsx(all_coord, file  ="E:/Data/Cord_Prep_New/burst_coord_data.xlsx")
