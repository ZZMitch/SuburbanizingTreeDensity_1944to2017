### UTM Change Analysis ###

setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga Tree Changes 1944-2017")

#library(ggplot2)

drivers <- read.csv("Driver Influence/I1_drivers.csv")
treecounts <- read.csv("Tree Time Series/Tree Counts/I1_treecount.csv")
treechanges <- read.csv("Tree Time Series/Tree Change/I1_treechange.csv")

#### Landcover Change ####
# 1944 # 
unique(drivers$LC1944)

pBuilding44 <- sum(drivers$LC1944 == "Building") / nrow(drivers)
pField44 <- sum(drivers$LC1944 == "Field") / nrow(drivers)
pForest44 <-  sum(drivers$LC1944 == "Forest") / nrow(drivers)
pForestEdge44 <- sum(drivers$LC1944 == "Forest Edge") / nrow(drivers)
pRoad44 <- sum(drivers$LC1944 == "Road") / nrow(drivers)
pShrubland44 <-  sum(drivers$LC1944 == "Shrubland") / nrow(drivers)
pTreedField44 <-  sum(drivers$LC1944 == "Treed Field") / nrow(drivers)
pTreedPlant44 <-  sum(drivers$LC1944 == "Treed Plantation") / nrow(drivers)
pImpSurface44 <-  sum(drivers$LC1944 == "Impervious Surface") / nrow(drivers)
pWater44 <-  sum(drivers$LC1944 == "Waterbody") / nrow(drivers)
pWetland44 <- sum(drivers$LC1944 == "Wetland") / nrow(drivers)
pGolfCourse44 <- sum(drivers$LC1944 == "Golf Course") / nrow(drivers)

pBuilding44 + pField44 + pForest44 + pForestEdge44 + pRoad44 + pShrubland44 + pTreedField44 + pTreedPlant44 + 
  pImpSurface44 + pWater44 + pWetland44 + pGolfCourse44

# 2017 #
unique(drivers$LC2017)

pBuilding17 <- sum(drivers$LC2017 == "Building") / nrow(drivers)
pField17 <- sum(drivers$LC2017 == "Field") / nrow(drivers)
pForest17 <-  sum(drivers$LC2017 == "Forest") / nrow(drivers)
pForestEdge17 <- sum(drivers$LC2017 == "Forest Edge") / nrow(drivers)
pRoad17 <- sum(drivers$LC2017 == "Road") / nrow(drivers)
pShrubland17 <-  sum(drivers$LC2017 == "Shrubland") / nrow(drivers)
pTreedField17 <-  sum(drivers$LC2017 == "Treed Field") / nrow(drivers)
pTreedPlant17 <-  sum(drivers$LC2017 == "Treed Plantation") / nrow(drivers)
pImpSurface17 <-  sum(drivers$LC2017 == "Impervious Surface") / nrow(drivers)
pWater17 <-  sum(drivers$LC2017 == "Waterbody") / nrow(drivers)
pWetland17 <- sum(drivers$LC2017 == "Wetland") / nrow(drivers)
pGolfCourse17 <- sum(drivers$LC2017 == "Golf Course") / nrow(drivers)

pBuilding17 + pField17 + pForest17 + pForestEdge17 + pRoad17 + pShrubland17 + pTreedField17 + pTreedPlant17 + 
  pImpSurface17 + pWater17 + pWetland17 + pGolfCourse17

# Create Landcover Change Table #
lc <- c("Forest","Forest Edge","Treed Plantation","Treed Field","Shrubland","Field","Impervious Surface","Road",
        "Building","Water","Wetland","Golf Course")

lcdata4417 <- data.frame(matrix(nrow = length(lc), ncol = 3))
lccols <- c("Landcover","p1944","p2017")
colnames(lcdata4417) <- lccols

lcdata4417$Landcover <- lc
lcdata4417$p1944 <- c(pForest44, pForestEdge44, pTreedPlant44, pTreedField44, pShrubland44, pField44, 
                      pImpSurface44, pRoad44, pBuilding44, pWater44, pWetland44, pGolfCourse44)
lcdata4417$p2017 <- c(pForest17, pForestEdge17, pTreedPlant17, pTreedField17, pShrubland17, pField17, 
                      pImpSurface17, pRoad17, pBuilding17, pWater17, pWetland17, pGolfCourse17)

write.csv(lcdata4417, file = "I1_LC4417.csv")

# LC Change Plot #
#ggplot(lcdata, aes(fill = Landcover, y = Percent, x = Year)) +
#  geom_bar(stat = "identity", position = "fill", color="red", fill=rgb(0.1,0.4,0.5,0.7))
 #ggplot(lcdata, aes(fill = Landcover, y = Percent, x = Year)) +
#  geom_bar(stat="identity", position="fill")


#### Driver Influence ####
# Align Change in number of trees to driver events #

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

#### Building construction ####
building_ch <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Building construction")
      building_ch <- c(building_ch, treechanges[j,i])
      #print(treechanges[j,i])
}
building_ch

building_yr <- numeric(0)

for(i in 3:14){
  building_yr <- c(building_yr, sum(drivers[,i] == "Building construction"))
}
building_yr

dr_building <- data.frame(matrix(NA, nrow = length(building_ch), ncol = 3))
names(dr_building) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
dr_building$EndYear[1:building_yr[1]] = 1954
dr_building$EndYear[sum(building_yr[1] + 1):sum(building_yr[1:2])] = 1966
dr_building$EndYear[(sum(building_yr[1:2]) + 1):sum(building_yr[1:3])] = 1975
dr_building$EndYear[(sum(building_yr[1:3]) + 1):sum(building_yr[1:4])] = 1980
dr_building$EndYear[(sum(building_yr[1:4]) + 1):sum(building_yr[1:5])] = 1985
#dr_building$EndYear[(sum(building_yr[1:5]) + 1):sum(building_yr[1:6])] = 1989
#dr_building$EndYear[(sum(building_yr[1:6]) + 1):sum(building_yr[1:7])] = 1995
#dr_building$EndYear[(sum(building_yr[1:7]) + 1):sum(building_yr[1:8])] = 1999
#dr_building$EndYear[(sum(building_yr[1:8]) + 1):sum(building_yr[1:9])] = 2004
#dr_building$EndYear[(sum(building_yr[1:9]) + 1):sum(building_yr[1:10])] = 2007
#dr_building$EndYear[(sum(building_yr[1:10]) + 1):sum(building_yr[1:11])] = 2012
#dr_building$EndYear[(sum(building_yr[1:11]) + 1):sum(building_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_building$TreeChange <- building_ch

# Fill in Driver Column #
dr_building$Driver <- "Building construction"

# Manual check (10+) #
dr_building$TreeChange[28:29] = -1

#write.csv(dr_building, "UTM_dr_buildingcon.csv")

#### Road construction ####
road_ch <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Road construction")
      road_ch <- c(road_ch, treechanges[j,i])
    #print(treechanges[j,i])
}
road_ch

road_yr <- numeric(0)

for(i in 3:14){
  road_yr <- c(road_yr, sum(drivers[,i] == "Road construction"))
}
road_yr

dr_road <- data.frame(matrix(NA, nrow = length(road_ch), ncol = 3))
names(dr_road) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
dr_road$EndYear[1:road_yr[1]] = 1954
dr_road$EndYear[sum(road_yr[1] + 1):sum(road_yr[1:2])] = 1966
dr_road$EndYear[(sum(road_yr[1:2]) + 1):sum(road_yr[1:3])] = 1975
dr_road$EndYear[(sum(road_yr[1:3]) + 1):sum(road_yr[1:4])] = 1980
dr_road$EndYear[(sum(road_yr[1:4]) + 1):sum(road_yr[1:5])] = 1985
dr_road$EndYear[(sum(road_yr[1:5]) + 1):sum(road_yr[1:6])] = 1989
#dr_road$EndYear[(sum(road_yr[1:6]) + 1):sum(road_yr[1:7])] = 1995
dr_road$EndYear[(sum(road_yr[1:7]) + 1):sum(road_yr[1:8])] = 1999
dr_road$EndYear[(sum(road_yr[1:8]) + 1):sum(road_yr[1:9])] = 2004
dr_road$EndYear[(sum(road_yr[1:9]) + 1):sum(road_yr[1:10])] = 2007
dr_road$EndYear[(sum(road_yr[1:10]) + 1):sum(road_yr[1:11])] = 2012
#dr_road$EndYear[(sum(road_yr[1:11]) + 1):sum(road_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_road$TreeChange <- road_ch

# Fill in Driver Column #
dr_road$Driver <- "Road construction"

# Manual check (10+) #

#### Minor construction ####
minor_ch <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Minor construction")
      minor_ch <- c(minor_ch, treechanges[j,i])
    #print(treechanges[j,i])
}
minor_ch

minor_yr <- numeric(0)

for(i in 3:14){
  minor_yr <- c(minor_yr, sum(drivers[,i] == "Minor construction"))
}
minor_yr

dr_minor <- data.frame(matrix(NA, nrow = length(minor_ch), ncol = 3))
names(dr_minor) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
dr_minor$EndYear[1:minor_yr[1]] = 1954
#dr_minor$EndYear[sum(minor_yr[1] + 1):sum(minor_yr[1:2])] = 1966
#dr_minor$EndYear[(sum(minor_yr[1:2]) + 1):sum(minor_yr[1:3])] = 1975
#dr_minor$EndYear[(sum(minor_yr[1:3]) + 1):sum(minor_yr[1:4])] = 1980
#dr_minor$EndYear[(sum(minor_yr[1:4]) + 1):sum(minor_yr[1:5])] = 1985
#dr_minor$EndYear[(sum(minor_yr[1:5]) + 1):sum(minor_yr[1:6])] = 1989
#dr_minor$EndYear[(sum(minor_yr[1:6]) + 1):sum(minor_yr[1:7])] = 1995
dr_minor$EndYear[(sum(minor_yr[1:7]) + 1):sum(minor_yr[1:8])] = 1999
dr_minor$EndYear[(sum(minor_yr[1:8]) + 1):sum(minor_yr[1:9])] = 2004
#dr_minor$EndYear[(sum(minor_yr[1:9]) + 1):sum(minor_yr[1:10])] = 2007
#dr_minor$EndYear[(sum(minor_yr[1:10]) + 1):sum(minor_yr[1:11])] = 2012
#dr_minor$EndYear[(sum(minor_yr[1:11]) + 1):sum(minor_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_minor$TreeChange <- minor_ch

# Fill in Driver Column #
dr_minor$Driver <- "Minor construction"

# Manual check (10+) #

#### Excavation ####
excav <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Excavation")
      excav <- c(excav, treechanges[j,i])
    #print(treechanges[j,i])
}
excav

excav_yr <- numeric(0)

for(i in 3:14){
  excav_yr <- c(excav_yr, sum(drivers[,i] == "Excavation"))
}
excav_yr

dr_excav <- data.frame(matrix(NA, nrow = length(excav), ncol = 3))
names(dr_excav) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_excav$EndYear[1:excav_yr[1]] = 1954
#dr_excav$EndYear[sum(excav_yr[1] + 1):sum(excav_yr[1:2])] = 1966
dr_excav$EndYear[(sum(excav_yr[1:2]) + 1):sum(excav_yr[1:3])] = 1975
#dr_excav$EndYear[(sum(excav_yr[1:3]) + 1):sum(excav_yr[1:4])] = 1980
#dr_excav$EndYear[(sum(excav_yr[1:4]) + 1):sum(excav_yr[1:5])] = 1985
dr_excav$EndYear[(sum(excav_yr[1:5]) + 1):sum(excav_yr[1:6])] = 1989
#dr_excav$EndYear[(sum(excav_yr[1:6]) + 1):sum(excav_yr[1:7])] = 1995
#dr_excav$EndYear[(sum(excav_yr[1:7]) + 1):sum(excav_yr[1:8])] = 1999
#dr_excav$EndYear[(sum(excav_yr[1:8]) + 1):sum(excav_yr[1:9])] = 2004
#dr_excav$EndYear[(sum(excav_yr[1:9]) + 1):sum(excav_yr[1:10])] = 2007
#dr_excav$EndYear[(sum(excav_yr[1:10]) + 1):sum(excav_yr[1:11])] = 2012
#dr_excav$EndYear[(sum(excav_yr[1:11]) + 1):sum(excav_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_excav$TreeChange <- excav

# Fill in Driver Column #
dr_excav$Driver <- "Excavation"

# Manual check (10+) #

#### Waterbody construction ####
water <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Waterbody construction")
      water <- c(water, treechanges[j,i])
    #print(treechanges[j,i])
}
water

water_yr <- numeric(0)

for(i in 3:14){
  water_yr <- c(water_yr, sum(drivers[,i] == "Waterbody construction"))
}
water_yr

dr_water <- data.frame(matrix(NA, nrow = length(water), ncol = 3))
names(dr_water) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_water$EndYear[1:water_yr[1]] = 1954
dr_water$EndYear[sum(water_yr[1] + 1):sum(water_yr[1:2])] = 1966
#dr_water$EndYear[(sum(water_yr[1:2]) + 1):sum(water_yr[1:3])] = 1975
#dr_water$EndYear[(sum(water_yr[1:3]) + 1):sum(water_yr[1:4])] = 1980
dr_water$EndYear[(sum(water_yr[1:4]) + 1):sum(water_yr[1:5])] = 1985
#dr_water$EndYear[(sum(water_yr[1:5]) + 1):sum(water_yr[1:6])] = 1989
#dr_water$EndYear[(sum(water_yr[1:6]) + 1):sum(water_yr[1:7])] = 1995
#dr_water$EndYear[(sum(water_yr[1:7]) + 1):sum(water_yr[1:8])] = 1999
#dr_water$EndYear[(sum(water_yr[1:8]) + 1):sum(water_yr[1:9])] = 2004
#dr_water$EndYear[(sum(water_yr[1:9]) + 1):sum(water_yr[1:10])] = 2007
#dr_water$EndYear[(sum(water_yr[1:10]) + 1):sum(water_yr[1:11])] = 2012
#dr_water$EndYear[(sum(water_yr[1:11]) + 1):sum(water_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_water$TreeChange <- water

# Fill in Driver Column #
dr_water$Driver <- "Waterbody construction"

# Manual check (10+) #

#### Building removal ####
building_rm <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Building removal")
      building_rm <- c(building_rm, treechanges[j,i])
    #print(treechanges[j,i])
}
building_rm

building_rm_yr <- numeric(0)

for(i in 3:14){
  building_rm_yr <- c(building_rm_yr, sum(drivers[,i] == "Building removal"))
}
building_rm_yr

dr_building_rm <- data.frame(matrix(NA, nrow = length(building_rm), ncol = 3))
names(dr_building_rm) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_building_rm$EndYear[1:building_rm_yr[1]] = 1954
#dr_building_rm$EndYear[sum(building_rm_yr[1] + 1):sum(building_rm_yr[1:2])] = 1966
dr_building_rm$EndYear[(sum(building_rm_yr[1:2]) + 1):sum(building_rm_yr[1:3])] = 1975
#dr_building_rm$EndYear[(sum(building_rm_yr[1:3]) + 1):sum(building_rm_yr[1:4])] = 1980
dr_building_rm$EndYear[(sum(building_rm_yr[1:4]) + 1):sum(building_rm_yr[1:5])] = 1985
#dr_building_rm$EndYear[(sum(building_rm_yr[1:5]) + 1):sum(building_rm_yr[1:6])] = 1989
#dr_building_rm$EndYear[(sum(building_rm_yr[1:6]) + 1):sum(building_rm_yr[1:7])] = 1995
#dr_building_rm$EndYear[(sum(building_rm_yr[1:7]) + 1):sum(building_rm_yr[1:8])] = 1999
#dr_building_rm$EndYear[(sum(building_rm_yr[1:8]) + 1):sum(building_rm_yr[1:9])] = 2004
#dr_building_rm$EndYear[(sum(building_rm_yr[1:9]) + 1):sum(building_rm_yr[1:10])] = 2007
#dr_building_rm$EndYear[(sum(building_rm_yr[1:10]) + 1):sum(building_rm_yr[1:11])] = 2012
#dr_building_rm$EndYear[(sum(building_rm_yr[1:11]) + 1):sum(building_rm_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_building_rm$TreeChange <- building_rm

# Fill in Driver Column #
dr_building_rm$Driver <- "Infrastructure removal"

# Manual check (10+) #

#### Road removal ####
road_rm <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Road removal")
      road_rm <- c(road_rm, treechanges[j,i])
    #print(treechanges[j,i])
}
road_rm

road_rm_yr <- numeric(0)

for(i in 3:14){
  road_rm_yr <- c(road_rm_yr, sum(drivers[,i] == "Road removal"))
}
road_rm_yr

dr_road_rm <- data.frame(matrix(NA, nrow = length(road_rm), ncol = 3))
names(dr_road_rm) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_road_rm$EndYear[1:road_rm_yr[1]] = 1954
#dr_road_rm$EndYear[sum(road_rm_yr[1] + 1):sum(road_rm_yr[1:2])] = 1966
#dr_road_rm$EndYear[(sum(road_rm_yr[1:2]) + 1):sum(road_rm_yr[1:3])] = 1975
#dr_road_rm$EndYear[(sum(road_rm_yr[1:3]) + 1):sum(road_rm_yr[1:4])] = 1980
#dr_road_rm$EndYear[(sum(road_rm_yr[1:4]) + 1):sum(road_rm_yr[1:5])] = 1985
#dr_road_rm$EndYear[(sum(road_rm_yr[1:5]) + 1):sum(road_rm_yr[1:6])] = 1989
#dr_road_rm$EndYear[(sum(road_rm_yr[1:6]) + 1):sum(road_rm_yr[1:7])] = 1995
#dr_road_rm$EndYear[(sum(road_rm_yr[1:7]) + 1):sum(road_rm_yr[1:8])] = 1999
#dr_road_rm$EndYear[(sum(road_rm_yr[1:8]) + 1):sum(road_rm_yr[1:9])] = 2004
#dr_road_rm$EndYear[(sum(road_rm_yr[1:9]) + 1):sum(road_rm_yr[1:10])] = 2007
#dr_road_rm$EndYear[(sum(road_rm_yr[1:10]) + 1):sum(road_rm_yr[1:11])] = 2012
#dr_road_rm$EndYear[(sum(road_rm_yr[1:11]) + 1):sum(road_rm_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_road_rm$TreeChange <- road_rm

# Fill in Driver Column #
#dr_road_rm$Driver <- "Infrastructure removal"

# Manual check (10+) #

#### Tennis court construction ####
tennis <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Tennis court construction")
      tennis <- c(tennis, treechanges[j,i])
    #print(treechanges[j,i])
}
tennis

tennis_yr <- numeric(0)

for(i in 3:14){
  tennis_yr <- c(tennis_yr, sum(drivers[,i] == "Tennis court construction"))
}
tennis_yr

dr_tennis <- data.frame(matrix(NA, nrow = length(tennis), ncol = 3))
names(dr_tennis) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_tennis$EndYear[1:tennis_yr[1]] = 1954
#dr_tennis$EndYear[sum(tennis_yr[1] + 1):sum(tennis_yr[1:2])] = 1966 
#dr_tennis$EndYear[(sum(tennis_yr[1:2]) + 1):sum(tennis_yr[1:3])] = 1975
#dr_tennis$EndYear[(sum(tennis_yr[1:3]) + 1):sum(tennis_yr[1:4])] = 1980
#dr_tennis$EndYear[(sum(tennis_yr[1:4]) + 1):sum(tennis_yr[1:5])] = 1985
#dr_tennis$EndYear[(sum(tennis_yr[1:5]) + 1):sum(tennis_yr[1:6])] = 1989
#dr_tennis$EndYear[(sum(tennis_yr[1:6]) + 1):sum(tennis_yr[1:7])] = 1995
#dr_tennis$EndYear[(sum(tennis_yr[1:7]) + 1):sum(tennis_yr[1:8])] = 1999
#dr_tennis$EndYear[(sum(tennis_yr[1:8]) + 1):sum(tennis_yr[1:9])] = 2004
#dr_tennis$EndYear[(sum(tennis_yr[1:9]) + 1):sum(tennis_yr[1:10])] = 2007
#dr_tennis$EndYear[(sum(tennis_yr[1:10]) + 1):sum(tennis_yr[1:11])] = 2012
#dr_tennis$EndYear[(sum(tennis_yr[1:11]) + 1):sum(tennis_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_tennis$TreeChange <- tennis

# Fill in Driver Column #
#dr_tennis$Driver <- "Sport construction"

# Manual check (10+) #

#### Baseball diamond construction ####
baseball <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Baseball diamond construction")
      baseball <- c(baseball, treechanges[j,i])
    #print(treechanges[j,i])
}
baseball

baseball_yr <- numeric(0)

for(i in 3:14){
  baseball_yr <- c(baseball_yr, sum(drivers[,i] == "Baseball diamond construction"))
}
baseball_yr

dr_baseball <- data.frame(matrix(NA, nrow = length(baseball), ncol = 3))
names(dr_baseball) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_baseball$EndYear[1:baseball_yr[1]] = 1954
#dr_baseball$EndYear[sum(baseball_yr[1] + 1):sum(baseball_yr[1:2])] = 1966
#dr_baseball$EndYear[(sum(baseball_yr[1:2]) + 1):sum(baseball_yr[1:3])] = 1975
#dr_baseball$EndYear[(sum(baseball_yr[1:3]) + 1):sum(baseball_yr[1:4])] = 1980
#dr_baseball$EndYear[(sum(baseball_yr[1:4]) + 1):sum(baseball_yr[1:5])] = 1985
#dr_baseball$EndYear[(sum(baseball_yr[1:5]) + 1):sum(baseball_yr[1:6])] = 1989
#dr_baseball$EndYear[(sum(baseball_yr[1:6]) + 1):sum(baseball_yr[1:7])] = 1995
#dr_baseball$EndYear[(sum(baseball_yr[1:7]) + 1):sum(baseball_yr[1:8])] = 1999
#dr_baseball$EndYear[(sum(baseball_yr[1:8]) + 1):sum(baseball_yr[1:9])] = 2004
#dr_baseball$EndYear[(sum(baseball_yr[1:9]) + 1):sum(baseball_yr[1:10])] = 2007
#dr_baseball$EndYear[(sum(baseball_yr[1:10]) + 1):sum(baseball_yr[1:11])] = 2012
#dr_baseball$EndYear[(sum(baseball_yr[1:11]) + 1):sum(baseball_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_baseball$TreeChange <- baseball

# Fill in Driver Column #
#dr_baseball$Driver <- "Sport construction"

# Manual check (10+) #

#### Golf course construction ####
golf <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Golf course construction")
      golf <- c(golf, treechanges[j,i])
    #print(treechanges[j,i])
}
golf

golf_yr <- numeric(0)

for(i in 3:14){
  golf_yr <- c(golf_yr, sum(drivers[,i] == "Golf course construction"))
}
golf_yr

dr_golf <- data.frame(matrix(NA, nrow = length(golf), ncol = 3))
names(dr_golf) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_golf$EndYear[1:golf_yr[1]] = 1954
#dr_golf$EndYear[sum(golf_yr[1] + 1):sum(golf_yr[1:2])] = 1966
#dr_golf$EndYear[(sum(golf_yr[1:2]) + 1):sum(golf_yr[1:3])] = 1975
#dr_golf$EndYear[(sum(golf_yr[1:3]) + 1):sum(golf_yr[1:4])] = 1980
#dr_golf$EndYear[(sum(golf_yr[1:4]) + 1):sum(golf_yr[1:5])] = 1985
#dr_golf$EndYear[(sum(golf_yr[1:5]) + 1):sum(golf_yr[1:6])] = 1989
#dr_golf$EndYear[(sum(golf_yr[1:6]) + 1):sum(golf_yr[1:7])] = 1995
#dr_golf$EndYear[(sum(golf_yr[1:7]) + 1):sum(golf_yr[1:8])] = 1999
#dr_golf$EndYear[(sum(golf_yr[1:8]) + 1):sum(golf_yr[1:9])] = 2004
#dr_golf$EndYear[(sum(golf_yr[1:9]) + 1):sum(golf_yr[1:10])] = 2007
#dr_golf$EndYear[(sum(golf_yr[1:10]) + 1):sum(golf_yr[1:11])] = 2012
#dr_golf$EndYear[(sum(golf_yr[1:11]) + 1):sum(golf_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_golf$TreeChange <- golf

# Fill in Driver Column #
#dr_golf$Driver <- "Sport construction"

# Manual check (10+) #

#### Cleared for farming ####
farming <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Cleared for farming")
      farming <- c(farming, treechanges[j,i])
    #print(treechanges[j,i])
}
farming

farming_yr <- numeric(0)

for(i in 3:14){
  farming_yr <- c(farming_yr, sum(drivers[,i] == "Cleared for farming"))
}
farming_yr

dr_farming <- data.frame(matrix(NA, nrow = length(farming), ncol = 3))
names(dr_farming) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_farming$EndYear[1:farming_yr[1]] = 1954
#dr_farming$EndYear[sum(farming_yr[1] + 1):sum(farming_yr[1:2])] = 1966
#dr_farming$EndYear[(sum(farming_yr[1:2]) + 1):sum(farming_yr[1:3])] = 1975
#dr_farming$EndYear[(sum(farming_yr[1:3]) + 1):sum(farming_yr[1:4])] = 1980
#dr_farming$EndYear[(sum(farming_yr[1:4]) + 1):sum(farming_yr[1:5])] = 1985
#dr_farming$EndYear[(sum(farming_yr[1:5]) + 1):sum(farming_yr[1:6])] = 1989
#dr_farming$EndYear[(sum(farming_yr[1:6]) + 1):sum(farming_yr[1:7])] = 1995
#dr_farming$EndYear[(sum(farming_yr[1:7]) + 1):sum(farming_yr[1:8])] = 1999
#dr_farming$EndYear[(sum(farming_yr[1:8]) + 1):sum(farming_yr[1:9])] = 2004
#dr_farming$EndYear[(sum(farming_yr[1:9]) + 1):sum(farming_yr[1:10])] = 2007
#dr_farming$EndYear[(sum(farming_yr[1:10]) + 1):sum(farming_yr[1:11])] = 2012
#dr_farming$EndYear[(sum(farming_yr[1:11]) + 1):sum(farming_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_farming$TreeChange <- farming

# Fill in Driver Column #
#dr_farming$Driver <- "Cleared for farming"

# Manual check (10+) #

#### Plantation harvested ####
plant_har <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Plantation harvested")
      plant_har <- c(plant_har, treechanges[j,i])
    #print(treechanges[j,i])
}
plant_har

plant_har_yr <- numeric(0)

for(i in 3:14){
  plant_har_yr <- c(plant_har_yr, sum(drivers[,i] == "Plantation harvested"))
}
plant_har_yr

dr_plant_har <- data.frame(matrix(NA, nrow = length(plant_har), ncol = 3))
names(dr_plant_har) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_plant_har$EndYear[1:plant_har_yr[1]] = 1954
#dr_plant_har$EndYear[sum(plant_har_yr[1] + 1):sum(plant_har_yr[1:2])] = 1966
#dr_plant_har$EndYear[(sum(plant_har_yr[1:2]) + 1):sum(plant_har_yr[1:3])] = 1975
#dr_plant_har$EndYear[(sum(plant_har_yr[1:3]) + 1):sum(plant_har_yr[1:4])] = 1980
#dr_plant_har$EndYear[(sum(plant_har_yr[1:4]) + 1):sum(plant_har_yr[1:5])] = 1985
#dr_plant_har$EndYear[(sum(plant_har_yr[1:5]) + 1):sum(plant_har_yr[1:6])] = 1989
#dr_plant_har$EndYear[(sum(plant_har_yr[1:6]) + 1):sum(plant_har_yr[1:7])] = 1995
#dr_plant_har$EndYear[(sum(plant_har_yr[1:7]) + 1):sum(plant_har_yr[1:8])] = 1999
#dr_plant_har$EndYear[(sum(plant_har_yr[1:8]) + 1):sum(plant_har_yr[1:9])] = 2004
#dr_plant_har$EndYear[(sum(plant_har_yr[1:9]) + 1):sum(plant_har_yr[1:10])] = 2007
#dr_plant_har$EndYear[(sum(plant_har_yr[1:10]) + 1):sum(plant_har_yr[1:11])] = 2012
#dr_plant_har$EndYear[(sum(plant_har_yr[1:11]) + 1):sum(plant_har_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_plant_har$TreeChange <- plant_har

# Fill in Driver Column #
#dr_plant_har$Driver <- "Plantation harvested"

# Manual check (10+) #

#### Forest expansion ####
forest_exp <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Forest expansion")
      forest_exp <- c(forest_exp, treechanges[j,i])
    #print(treechanges[j,i])
}
forest_exp

forest_exp_yr <- numeric(0)

for(i in 3:14){
  forest_exp_yr <- c(forest_exp_yr, sum(drivers[,i] == "Forest expansion"))
}
forest_exp_yr

dr_forest_exp <- data.frame(matrix(NA, nrow = length(forest_exp), ncol = 3))
names(dr_forest_exp) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_forest_exp$EndYear[1:forest_exp_yr[1]] = 1954
#dr_forest_exp$EndYear[sum(forest_exp_yr[1] + 1):sum(forest_exp_yr[1:2])] = 1966
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:2]) + 1):sum(forest_exp_yr[1:3])] = 1975
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:3]) + 1):sum(forest_exp_yr[1:4])] = 1980
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:4]) + 1):sum(forest_exp_yr[1:5])] = 1985
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:5]) + 1):sum(forest_exp_yr[1:6])] = 1989
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:6]) + 1):sum(forest_exp_yr[1:7])] = 1995
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:7]) + 1):sum(forest_exp_yr[1:8])] = 1999
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:8]) + 1):sum(forest_exp_yr[1:9])] = 2004
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:9]) + 1):sum(forest_exp_yr[1:10])] = 2007
dr_forest_exp$EndYear[(sum(forest_exp_yr[1:10]) + 1):sum(forest_exp_yr[1:11])] = 2012
#dr_forest_exp$EndYear[(sum(forest_exp_yr[1:11]) + 1):sum(forest_exp_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_forest_exp$TreeChange <- forest_exp

# Fill in Driver Column #
dr_forest_exp$Driver <- "Forest expansion"

# Manual check (10+) #

#### Tree growth ####
tree_gr <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Tree growth")
      tree_gr <- c(tree_gr, treechanges[j,i])
    #print(treechanges[j,i])
}
tree_gr

tree_gr_yr <- numeric(0)

for(i in 3:14){
  tree_gr_yr <- c(tree_gr_yr, sum(drivers[,i] == "Tree growth"))
}
tree_gr_yr

dr_tree_gr <- data.frame(matrix(NA, nrow = length(tree_gr), ncol = 3))
names(dr_tree_gr) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
dr_tree_gr$EndYear[1:tree_gr_yr[1]] = 1954
#dr_tree_gr$EndYear[sum(tree_gr_yr[1] + 1):sum(tree_gr_yr[1:2])] = 1966
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:2]) + 1):sum(tree_gr_yr[1:3])] = 1975
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:3]) + 1):sum(tree_gr_yr[1:4])] = 1980
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:4]) + 1):sum(tree_gr_yr[1:5])] = 1985
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:5]) + 1):sum(tree_gr_yr[1:6])] = 1989
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:6]) + 1):sum(tree_gr_yr[1:7])] = 1995
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:7]) + 1):sum(tree_gr_yr[1:8])] = 1999
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:8]) + 1):sum(tree_gr_yr[1:9])] = 2004
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:9]) + 1):sum(tree_gr_yr[1:10])] = 2007
dr_tree_gr$EndYear[(sum(tree_gr_yr[1:10]) + 1):sum(tree_gr_yr[1:11])] = 2012
#dr_tree_gr$EndYear[(sum(tree_gr_yr[1:11]) + 1):sum(tree_gr_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_tree_gr$TreeChange <- tree_gr

# Fill in Driver Column #
dr_tree_gr$Driver <- "Tree growth"

# Manual check (10+) #

#### Trees added ####
tree_ad <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Tree planting")
      tree_ad <- c(tree_ad, treechanges[j,i])
    #print(treechanges[j,i])
}
tree_ad

tree_ad_yr <- numeric(0)

for(i in 3:14){
  tree_ad_yr <- c(tree_ad_yr, sum(drivers[,i] == "Tree planting"))
}
tree_ad_yr

dr_tree_ad <- data.frame(matrix(NA, nrow = length(tree_ad), ncol = 3))
names(dr_tree_ad) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
dr_tree_ad$EndYear[1:tree_ad_yr[1]] = 1954
dr_tree_ad$EndYear[sum(tree_ad_yr[1] + 1):sum(tree_ad_yr[1:2])] = 1966
#dr_tree_ad$EndYear[(sum(tree_ad_yr[1:2]) + 1):sum(tree_ad_yr[1:3])] = 1975
dr_tree_ad$EndYear[(sum(tree_ad_yr[1:3]) + 1):sum(tree_ad_yr[1:4])] = 1980
dr_tree_ad$EndYear[(sum(tree_ad_yr[1:4]) + 1):sum(tree_ad_yr[1:5])] = 1985
dr_tree_ad$EndYear[(sum(tree_ad_yr[1:5]) + 1):sum(tree_ad_yr[1:6])] = 1989
dr_tree_ad$EndYear[(sum(tree_ad_yr[1:6]) + 1):sum(tree_ad_yr[1:7])] = 1995
dr_tree_ad$EndYear[(sum(tree_ad_yr[1:7]) + 1):sum(tree_ad_yr[1:8])] = 1999
#dr_tree_ad$EndYear[(sum(tree_ad_yr[1:8]) + 1):sum(tree_ad_yr[1:9])] = 2004
#dr_tree_ad$EndYear[(sum(tree_ad_yr[1:9]) + 1):sum(tree_ad_yr[1:10])] = 2007
dr_tree_ad$EndYear[(sum(tree_ad_yr[1:10]) + 1):sum(tree_ad_yr[1:11])] = 2012
#dr_tree_ad$EndYear[(sum(tree_ad_yr[1:11]) + 1):sum(tree_ad_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_tree_ad$TreeChange <- tree_ad

# Fill in Driver Column #
dr_tree_ad$Driver <- "Tree planting"

# Manual check (10+) #
dr_tree_ad$TreeChange[11] = 4

#### Tree removal ####
tree_rm <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Tree removal")
      tree_rm <- c(tree_rm, treechanges[j,i])
    #print(treechanges[j,i])
}
tree_rm

tree_rm_yr <- numeric(0)

for(i in 3:14){
  tree_rm_yr <- c(tree_rm_yr, sum(drivers[,i] == "Tree removal"))
}
tree_rm_yr

dr_tree_rm <- data.frame(matrix(NA, nrow = length(tree_rm), ncol = 3))
names(dr_tree_rm) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
dr_tree_rm$EndYear[1:tree_rm_yr[1]] = 1954
dr_tree_rm$EndYear[sum(tree_rm_yr[1] + 1):sum(tree_rm_yr[1:2])] = 1966
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:2]) + 1):sum(tree_rm_yr[1:3])] = 1975
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:3]) + 1):sum(tree_rm_yr[1:4])] = 1980
#dr_tree_rm$EndYear[(sum(tree_rm_yr[1:4]) + 1):sum(tree_rm_yr[1:5])] = 1985
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:5]) + 1):sum(tree_rm_yr[1:6])] = 1989
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:6]) + 1):sum(tree_rm_yr[1:7])] = 1995
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:7]) + 1):sum(tree_rm_yr[1:8])] = 1999
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:8]) + 1):sum(tree_rm_yr[1:9])] = 2004
#dr_tree_rm$EndYear[(sum(tree_rm_yr[1:9]) + 1):sum(tree_rm_yr[1:10])] = 2007
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:10]) + 1):sum(tree_rm_yr[1:11])] = 2012
dr_tree_rm$EndYear[(sum(tree_rm_yr[1:11]) + 1):sum(tree_rm_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_tree_rm$TreeChange <- tree_rm

# Fill in Driver Column #
dr_tree_rm$Driver <- "Tree removal"

# Manual check (10+) #

#### Plantation naturalized ####
plant_nat <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Plantation naturalized")
      plant_nat <- c(plant_nat, treechanges[j,i])
    #print(treechanges[j,i])
}
plant_nat

plant_nat_yr <- numeric(0)

for(i in 3:14){
  plant_nat_yr <- c(plant_nat_yr, sum(drivers[,i] == "Plantation naturalized"))
}
plant_nat_yr

dr_plant_nat <- data.frame(matrix(NA, nrow = length(plant_nat), ncol = 3))
names(dr_plant_nat) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_plant_nat$EndYear[1:plant_nat_yr[1]] = 1954
#dr_plant_nat$EndYear[sum(plant_nat_yr[1] + 1):sum(plant_nat_yr[1:2])] = 1966
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:2]) + 1):sum(plant_nat_yr[1:3])] = 1975
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:3]) + 1):sum(plant_nat_yr[1:4])] = 1980
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:4]) + 1):sum(plant_nat_yr[1:5])] = 1985
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:5]) + 1):sum(plant_nat_yr[1:6])] = 1989
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:6]) + 1):sum(plant_nat_yr[1:7])] = 1995
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:7]) + 1):sum(plant_nat_yr[1:8])] = 1999
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:8]) + 1):sum(plant_nat_yr[1:9])] = 2004
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:9]) + 1):sum(plant_nat_yr[1:10])] = 2007
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:10]) + 1):sum(plant_nat_yr[1:11])] = 2012
#dr_plant_nat$EndYear[(sum(plant_nat_yr[1:11]) + 1):sum(plant_nat_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_plant_nat$TreeChange <- plant_nat

# Fill in Driver Column #
#dr_plant_nat$Driver <- "Plantation naturalized"

# Manual check (10+) #

#### Plantation grown ####
plant_gr <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Plantation grown")
      plant_gr <- c(plant_gr, treechanges[j,i])
    #print(treechanges[j,i])
}
plant_gr

plant_gr_yr <- numeric(0)

for(i in 3:14){
  plant_gr_yr <- c(plant_gr_yr, sum(drivers[,i] == "Plantation grown"))
}
plant_gr_yr

dr_plant_gr <- data.frame(matrix(NA, nrow = length(plant_gr), ncol = 3))
names(dr_plant_gr) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_plant_gr$EndYear[1:plant_gr_yr[1]] = 1954
#dr_plant_gr$EndYear[sum(plant_gr_yr[1] + 1):sum(plant_gr_yr[1:2])] = 1966
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:2]) + 1):sum(plant_gr_yr[1:3])] = 1975
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:3]) + 1):sum(plant_gr_yr[1:4])] = 1980
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:4]) + 1):sum(plant_gr_yr[1:5])] = 1985
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:5]) + 1):sum(plant_gr_yr[1:6])] = 1989
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:6]) + 1):sum(plant_gr_yr[1:7])] = 1995
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:7]) + 1):sum(plant_gr_yr[1:8])] = 1999
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:8]) + 1):sum(plant_gr_yr[1:9])] = 2004
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:9]) + 1):sum(plant_gr_yr[1:10])] = 2007
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:10]) + 1):sum(plant_gr_yr[1:11])] = 2012
#dr_plant_gr$EndYear[(sum(plant_gr_yr[1:11]) + 1):sum(plant_gr_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_plant_gr$TreeChange <- plant_gr

# Fill in Driver Column #
#dr_plant_gr$Driver <- "Plantation grown"

# Manual check (10+) #

#### Urban greening ####
urban_gr <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Urban greening")
      urban_gr <- c(urban_gr, treechanges[j,i])
    #print(treechanges[j,i])
}
urban_gr

urban_gr_yr <- numeric(0)

for(i in 3:14){
  urban_gr_yr <- c(urban_gr_yr, sum(drivers[,i] == "Urban greening"))
}
urban_gr_yr

dr_urban_gr <- data.frame(matrix(NA, nrow = length(urban_gr), ncol = 3))
names(dr_urban_gr) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_urban_gr$EndYear[1:urban_gr_yr[1]] = 1954
#dr_urban_gr$EndYear[sum(urban_gr_yr[1] + 1):sum(urban_gr_yr[1:2])] = 1966
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:2]) + 1):sum(urban_gr_yr[1:3])] = 1975
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:3]) + 1):sum(urban_gr_yr[1:4])] = 1980
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:4]) + 1):sum(urban_gr_yr[1:5])] = 1985
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:5]) + 1):sum(urban_gr_yr[1:6])] = 1989
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:6]) + 1):sum(urban_gr_yr[1:7])] = 1995
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:7]) + 1):sum(urban_gr_yr[1:8])] = 1999
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:8]) + 1):sum(urban_gr_yr[1:9])] = 2004
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:9]) + 1):sum(urban_gr_yr[1:10])] = 2007
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:10]) + 1):sum(urban_gr_yr[1:11])] = 2012
#dr_urban_gr$EndYear[(sum(urban_gr_yr[1:11]) + 1):sum(urban_gr_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_urban_gr$TreeChange <- urban_gr

# Fill in Driver Column #
#dr_urban_gr$Driver <- "Other greening"

# Manual check (10+) #

#### Vegetation growth ####
veg_gr <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "Vegetation growth")
      veg_gr <- c(veg_gr, treechanges[j,i])
    #print(treechanges[j,i])
}
veg_gr

veg_gr_yr <- numeric(0)

for(i in 3:14){
  veg_gr_yr <- c(veg_gr_yr, sum(drivers[,i] == "Vegetation growth"))
}
veg_gr_yr

dr_veg_gr <- data.frame(matrix(NA, nrow = length(veg_gr), ncol = 3))
names(dr_veg_gr) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_veg_gr$EndYear[1:veg_gr_yr[1]] = 1954
#dr_veg_gr$EndYear[sum(veg_gr_yr[1] + 1):sum(veg_gr_yr[1:2])] = 1966
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:2]) + 1):sum(veg_gr_yr[1:3])] = 1975
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:3]) + 1):sum(veg_gr_yr[1:4])] = 1980
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:4]) + 1):sum(veg_gr_yr[1:5])] = 1985
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:5]) + 1):sum(veg_gr_yr[1:6])] = 1989
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:6]) + 1):sum(veg_gr_yr[1:7])] = 1995
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:7]) + 1):sum(veg_gr_yr[1:8])] = 1999
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:8]) + 1):sum(veg_gr_yr[1:9])] = 2004
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:9]) + 1):sum(veg_gr_yr[1:10])] = 2007
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:10]) + 1):sum(veg_gr_yr[1:11])] = 2012
#dr_veg_gr$EndYear[(sum(veg_gr_yr[1:11]) + 1):sum(veg_gr_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_veg_gr$TreeChange <- veg_gr

# Fill in Driver Column #
#dr_veg_gr$Driver <- "Other greening"

# Manual check (10+) #

#### River movement ####
river <- numeric(0)

for(i in 3:14){ # For all timesteps...
  for(j in 1:nrow(drivers)) #For all driver squares...
    if(drivers[j,i] == "River movement")
      river <- c(river, treechanges[j,i])
    #print(treechanges[j,i])
}
river

river_yr <- numeric(0)

for(i in 3:14){
  river_yr <- c(river_yr, sum(drivers[,i] == "River movement"))
}
river_yr

dr_river <- data.frame(matrix(NA, nrow = length(river), ncol = 3))
names(dr_river) <- c("EndYear", "TreeChange", "Driver")

# Fill in EndYear Column #
#dr_river$EndYear[1:river_yr[1]] = 1954
dr_river$EndYear[sum(river_yr[1] + 1):sum(river_yr[1:2])] = 1966
dr_river$EndYear[(sum(river_yr[1:2]) + 1):sum(river_yr[1:3])] = 1975
dr_river$EndYear[(sum(river_yr[1:3]) + 1):sum(river_yr[1:4])] = 1980
#dr_river$EndYear[(sum(river_yr[1:4]) + 1):sum(river_yr[1:5])] = 1985
dr_river$EndYear[(sum(river_yr[1:5]) + 1):sum(river_yr[1:6])] = 1989
dr_river$EndYear[(sum(river_yr[1:6]) + 1):sum(river_yr[1:7])] = 1995
#dr_river$EndYear[(sum(river_yr[1:7]) + 1):sum(river_yr[1:8])] = 1999
dr_river$EndYear[(sum(river_yr[1:8]) + 1):sum(river_yr[1:9])] = 2004
dr_river$EndYear[(sum(river_yr[1:9]) + 1):sum(river_yr[1:10])] = 2007
#dr_river$EndYear[(sum(river_yr[1:10]) + 1):sum(river_yr[1:11])] = 2012
#dr_river$EndYear[(sum(river_yr[1:11]) + 1):sum(river_yr[1:12])] = 2017
### REMOVE YEARS WITH 0 ENTRIES ###

# Fill in TreeChange Column #
dr_river$TreeChange <- river

# Fill in Driver Column #
dr_river$Driver <- "River movement"

# Manual check (10+) #

#### Merge Driver Tables ####
dr_treechange_hum <- rbind(dr_building, dr_road, dr_minor, dr_excav, dr_water, dr_building_rm, dr_road_rm, 
                           dr_tennis, dr_baseball, dr_golf, dr_farming, dr_plant_har)
dr_treechange_nat <- rbind(dr_forest_exp, dr_tree_gr, dr_tree_ad, dr_tree_rm, dr_plant_nat, dr_plant_gr,
                           dr_urban_gr, dr_veg_gr, dr_river)

write.csv(dr_treechange_hum, "I1_dr_tc_hum.csv")
write.csv(dr_treechange_nat, "I1_dr_tc_nat.csv")

### Create BoxPlot (all years, all values) ###
boxplot(TreeChange ~ Driver, data = dr_treechange_hum, xaxt = "n")
axis(1, labels = c("Building\nConstruction", "Excavation", "Infrastructure\nRemoval", 
                   "Minor\nConstruction", "Road\nConstruction",
                   "Waterbody\nConstruction"), at = 1:6, tick = FALSE, cex.axis = 1)
boxplot(TreeChange ~ Driver, data = dr_treechange_hum, plot = FALSE)

boxplot(TreeChange ~ Driver, data = dr_treechange_nat, xaxt = "n")
axis(1, labels = c("Forest\nExpansion", "River inundation",
                   "Tree\nRegeneration", "Tree\nRemoval", 
                   "Trees\nPlanted"), at = 1:5, tick = FALSE, cex.axis = 1)
boxplot(TreeChange ~ Driver, data = dr_treechange_nat, plot = FALSE)

### Calculate Means ###
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Building construction","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Road construction","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Minor construction","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Excavation","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Waterbody construction","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Infrastructure removal","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Sport construction","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Cleared for farming","TreeChange"])
mean(dr_treechange_hum[dr_treechange_hum$Driver == "Plantation harvested","TreeChange"])

mean(dr_treechange_nat[dr_treechange_nat$Driver == "Forest expansion","TreeChange"])
mean(dr_treechange_nat[dr_treechange_nat$Driver == "Tree growth","TreeChange"])
mean(dr_treechange_nat[dr_treechange_nat$Driver == "Trees added","TreeChange"])
mean(dr_treechange_nat[dr_treechange_nat$Driver == "Tree removal","TreeChange"])
mean(dr_treechange_nat[dr_treechange_nat$Driver == "Plantation naturalized","TreeChange"])
mean(dr_treechange_nat[dr_treechange_nat$Driver == "Plantation grown","TreeChange"])
mean(dr_treechange_nat[dr_treechange_nat$Driver == "Other greening","TreeChange"])
mean(dr_treechange_nat[dr_treechange_nat$Driver == "River movement","TreeChange"])

### Driver Influence TimeSeries ###
# See excel file