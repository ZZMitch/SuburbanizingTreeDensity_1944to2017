##### Biggest Drivers #####

setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga Tree Changes 1944-2017")

drivers <- read.csv("Driver Influence/N1_drivers.csv")
treechanges <- read.csv("Tree Time Series/Tree Change/N1_treechange.csv")

### Build driver influence table ###
driver_inf <- data.frame(matrix(nrow = nrow(drivers), ncol = 21))
driver_inf_cols <- c("FID","NID","X","Y","Building construction","Road construction","Minor construction",
                    "Recreational construction","Waterbody construction","Excavation","Land clearing", "Forest expansion",
                    "Tree regeneration","Tree planting","Tree removal","Plantation planting","Plantation naturalization",
                    "Plantation harvest","Infrastructure removal","Other greening","Flooding")
colnames(driver_inf) <- driver_inf_cols

driver_inf$FID <- drivers$FID
driver_inf$NID <- drivers$NID
driver_inf$X <- drivers$X
driver_inf$Y <- drivers$Y

### Build biggest drivers table ###
# Make sure driver file has NID from tree change file
biggestdrivers <- data.frame(matrix(nrow = nrow(drivers), ncol = 6))
bigdriver_cols <- c("FID","NID","X","Y","Driver","TreeChange")
colnames(biggestdrivers) <- bigdriver_cols

biggestdrivers$FID <- drivers$FID
biggestdrivers$NID <- drivers$NID
biggestdrivers$X <- drivers$X
biggestdrivers$Y <- drivers$Y

### Driver exploration ###
# ID Drivers #
unique(drivers[3])
unique(drivers[4])
unique(drivers[5])
unique(drivers[6])
unique(drivers[7])
unique(drivers[8])
unique(drivers[9])
unique(drivers[10])
unique(drivers[11])
unique(drivers[12])
unique(drivers[13])
unique(drivers[14])

# Rename Drivers (Excel) #
# Plantation harvested to Plantation harvest
# Tree growth to Tree regeneration
# Building removal to Infrastructure removal
# Road removal to Infrastructure removal
# Trees added to Tree planting
# Plantation naturalized to Plantation naturalization
# Tennis court construction to Recreational construction
# Urban greening to Other greening - will correct manually at end
# Vegetation growth to Other greening - will correct manually at end
# Baseball diamond construction to Recreational construction
# Cleared for farming to Land clearing
# Plantation grown to Plantation planting
# River movement to Flooding
# Golf course construction to Recreational construction

### Find biggest driver for each cell ###
#biggestdrivers$Driver <- "None"

#drivers[64,4]
#treechanges[64,4]

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Building construction")
      x[i] <- treechanges[h,i]
      driver_inf[h,5] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Road construction")
      x[i] <- treechanges[h,i]
    driver_inf[h,6] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Minor construction")
      x[i] <- treechanges[h,i]
    driver_inf[h,7] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Recreational construction")
      x[i] <- treechanges[h,i]
    driver_inf[h,8] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Waterbody construction")
      x[i] <- treechanges[h,i]
    driver_inf[h,9] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Excavation")
      x[i] <- treechanges[h,i]
    driver_inf[h,10] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Land clearing")
      x[i] <- treechanges[h,i]
    driver_inf[h,11] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Forest expansion")
      x[i] <- treechanges[h,i]
    driver_inf[h,12] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Tree regeneration")
      x[i] <- treechanges[h,i]
    driver_inf[h,13] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Tree planting")
      x[i] <- treechanges[h,i]
    driver_inf[h,14] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Tree removal")
      x[i] <- treechanges[h,i]
    driver_inf[h,15] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Plantation planting")
      x[i] <- treechanges[h,i]
    driver_inf[h,16] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Plantation naturalization")
      x[i] <- treechanges[h,i]
    driver_inf[h,17] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Plantation harvest")
      x[i] <- treechanges[h,i]
    driver_inf[h,18] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Infrastructure removal")
      x[i] <- treechanges[h,i]
    driver_inf[h,19] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Other greening") #Change to tree regeneration
      x[i] <- treechanges[h,i]
    driver_inf[h,20] <- sum(x,na.rm = "TRUE")
}

for(h in 1:nrow(drivers)){
  x <- numeric(0)
  for(i in 3:14)
    if(drivers[h,i] == "Flooding")
      x[i] <- treechanges[h,i]
    driver_inf[h,21] <- sum(x,na.rm = "TRUE")
}

# Read into both tables at the same matrix cell (row, column) - see treechange analysis forloops...
# But go by grid cell rather than timestep

# For every grid cell, what is the driver (if any) associated with the maximum amount of trees lost/gained
# For the study period (1944-2017)... and what is that maximum amount

# Find number of trees lost or gained for that driver in each cell #
#biggestdrivers$TreeChange <- 0

### Add Max Driver and Value to Biggest Drivers ###
biggestdrivers$Driver <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random

# Testing ties... #
# biggestdrivers$Driver1 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver2 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver3 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver4 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver5 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver6 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver7 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver8 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random
# biggestdrivers$Driver9 <- colnames(driver_inf[5:21])[max.col(abs(driver_inf[5:21]))] #Ties = random

for(i in 1:nrow(drivers)){
  biggestdrivers$TreeChange[i] <- max(abs(driver_inf[i,5:21]))
}

#for(i in 1:nrow(drivers)){
#  if(biggestdrivers$TreeChange[i] < 5){
#    biggestdrivers$Driver[i] = "Minor"
#  }
#}

for(i in 1:nrow(drivers)){
  if(biggestdrivers$TreeChange[i] < 1){
    biggestdrivers$Driver[i] = "None"
  }
}

#Testing ties... #
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver1[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver2[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver3[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver4[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver5[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver6[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver7[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#   if(biggestdrivers$TreeChange[i] < 1){
#     biggestdrivers$Driver8[i] = "None"
#   }
# }
# 
# for(i in 1:nrow(drivers)){
#  if(biggestdrivers$TreeChange[i] < 1){
#    biggestdrivers$Driver9[i] = "None"
#  }
# }

write.csv(biggestdrivers, "bigdrivers_LDR.csv")
#write.csv(biggestdrivers, "bigdrivers5_LDR.csv")
