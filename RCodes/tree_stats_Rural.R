setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga 1944-Now Project/MTB/Tree Corrections")

require(rgdal)
require(raster)

### Master Data ###
master <- read.csv("trees_all.csv")
Rur_drivers <- read.csv("Rur_drivers.csv")
#master_shp <- readOGR(dsn = ".", layer = "trees_allyears_mtb")
#proj4string(master_shp)

#plot(master_shp)

### Neighborhood 1 ###
Rur <- readOGR(dsn = ".", layer = "Rural")
#proj4string(UTM) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#proj4string(UTM)

# 1944 #
Rur_1944 = subset(master, Year == 1944 & Location == "Rural")

#UTM_1944_shp = subset(master_shp, Year == 1944 & Location == "UTM")
#proj4string(UTM_1944_shp)

plot(Rur)
#plot(UTM_1944_shp, pch = 19, cex = 0.1)
points(Rur_1944$POINT_X, Rur_1944$POINT_Y, pch = 19, cex = 0.1)

Rurr <- raster(Rur)
res(Rurr) <- 30
Rurr

Rurr <- rasterize(Rur, Rurr)
plot(Rurr)
quads <- as(Rurr, 'SpatialPolygons')
plot(quads, add = TRUE)
points(Rur_1944$POINT_X, Rur_1944$POINT_Y, pch = 19, cex = 0.1)

#quads1_rur <- as(quads, "SpatialPolygonsDataFrame")
#writeOGR(quads1_rur, dsn = '.', layer = 'quads_rur', driver = "ESRI Shapefile")

Rur_1944_tc <- rasterize(Rur_1944[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1944_tc <- mask(Rur_1944_tc, Rur)
plot(Rur_1944_tc, main = "1944 Tree Density")
plot(Rur, add = TRUE)

# 1954 #
Rur_1954 = subset(master, Year == 1954 & Location == "Rural")
Rur_1954_tc <- rasterize(Rur_1954[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1954_tc <- mask(Rur_1954_tc, Rur)
plot(Rur_1954_tc, main = "1954 Tree Density")
plot(Rur, add = TRUE)

# 1966 #
Rur_1966 = subset(master, Year == 1966 & Location == "Rural")
Rur_1966_tc <- rasterize(Rur_1966[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1966_tc <- mask(Rur_1966_tc, Rur)
plot(Rur_1966_tc, main = "1966 Tree Density")
plot(Rur, add = TRUE)

# 1975 #
Rur_1975 = subset(master, Year == 1975 & Location == "Rural")
Rur_1975_tc <- rasterize(Rur_1975[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1975_tc <- mask(Rur_1975_tc, Rur)
plot(Rur_1975_tc, main = "1975 Tree Density")
plot(Rur, add = TRUE)

# 1980 #
Rur_1980 = subset(master, Year == 1980 & Location == "Rural")
Rur_1980_tc <- rasterize(Rur_1980[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1980_tc <- mask(Rur_1980_tc, Rur)
plot(Rur_1980_tc, main = "1980 Tree Density")
plot(Rur, add = TRUE)

# 1985 #
Rur_1985 = subset(master, Year == 1985 & Location == "Rural")
Rur_1985_tc <- rasterize(Rur_1985[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1985_tc <- mask(Rur_1985_tc, Rur)
plot(Rur_1985_tc, main = "1985 Tree Density")
plot(Rur, add = TRUE)

# 1989 #
Rur_1989 = subset(master, Year == 1989 & Location == "Rural")
Rur_1989_tc <- rasterize(Rur_1989[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1989_tc <- mask(Rur_1989_tc, Rur)
plot(Rur_1989_tc, main = "1989 Tree Density")
plot(Rur, add = TRUE)

# 1995 #
Rur_1995 = subset(master, Year == 1995 & Location == "Rural")
Rur_1995_tc <- rasterize(Rur_1995[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1995_tc <- mask(Rur_1995_tc, Rur)
plot(Rur_1995_tc, main = "1995 Tree Density")
plot(Rur, add = TRUE)

# 1999 #
Rur_1999 = subset(master, Year == 1999 & Location == "Rural")
Rur_1999_tc <- rasterize(Rur_1999[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_1999_tc <- mask(Rur_1999_tc, Rur)
plot(Rur_1999_tc, main = "1999 Tree Density")
plot(Rur, add = TRUE)

# 2004 #
Rur_2004 = subset(master, Year == 2004 & Location == "Rural")
Rur_2004_tc <- rasterize(Rur_2004[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_2004_tc <- mask(Rur_2004_tc, Rur)
plot(Rur_2004_tc, main = "2004 Tree Density")
plot(Rur, add = TRUE)

# 2007 #
Rur_2007 = subset(master, Year == 2007 & Location == "Rural")
Rur_2007_tc <- rasterize(Rur_2007[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_2007_tc <- mask(Rur_2007_tc, Rur)
plot(Rur_2007_tc, main = "2007 Tree Density")
plot(Rur, add = TRUE)

# 2012 #
Rur_2012 = subset(master, Year == 2012 & Location == "Rural")
Rur_2012_tc <- rasterize(Rur_2012[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_2012_tc <- mask(Rur_2012_tc, Rur)
plot(Rur_2012_tc, main = "2012 Tree Density")
plot(Rur, add = TRUE)

# 2017 #
Rur_2017 = subset(master, Year == 2017 & Location == "Rural")
Rur_2017_tc <- rasterize(Rur_2017[,c(4,5)], Rurr, fun = 'count', background = 0)
Rur_2017_tc <- mask(Rur_2017_tc, Rur)
plot(Rur_2017_tc, main = "2017 Tree Density")
plot(Rur, add = TRUE)

### Analysis ###

# Create Raster Stack #
Rur_stack <- stack(Rur_1944_tc, Rur_1954_tc, Rur_1966_tc, Rur_1975_tc, Rur_1980_tc, Rur_1985_tc, Rur_1989_tc,
                   Rur_1995_tc, Rur_1999_tc, Rur_2004_tc, Rur_2007_tc, Rur_2012_tc, Rur_2017_tc)
Rur_stack
plot(Rur_stack)

years <- c(1944, 1954, 1966, 1975, 1980, 1985, 1989, 1995, 1999, 2004, 2007, 2012, 2017)


# Calculate Slope #
fun_slope <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[2]
  }
}

slope <- calc(Rur_stack, fun_slope)
slope_m <- mask(slope, Rur)

plot(slope_m, main = "Tree Density Slope")
plot(Rur, add = TRUE)

# Calculate Pvalue #
fun_pval <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[8]
  }
}

pval <- calc(Rur_stack, fun_pval)
pval_m <- mask(pval, Rur)

plot(pval_m, main = "Tree Density Significance")
plot(Rur, add = TRUE)

# Save Rasters #
writeRaster(slope_m, "Rur_44to17_Slope.tif", "GTiff", overwrite = TRUE)
writeRaster(pval_m, "Rur_44to17_Pvalue.tif", "GTiff", overwrite = TRUE)

### Build Tree Count Tables ###
Rur_1944_tc_tbl <- as.data.frame(Rur_1944_tc,xy = TRUE)
Rur_1944_tc_tbl <- Rur_1944_tc_tbl[complete.cases(Rur_1944_tc_tbl),]

Rur_1954_tc_tbl <- as.data.frame(Rur_1954_tc,xy = TRUE)
Rur_1954_tc_tbl <- Rur_1954_tc_tbl[complete.cases(Rur_1954_tc_tbl),]

Rur_1966_tc_tbl <- as.data.frame(Rur_1966_tc,xy = TRUE)
Rur_1966_tc_tbl <- Rur_1966_tc_tbl[complete.cases(Rur_1966_tc_tbl),]

Rur_1975_tc_tbl <- as.data.frame(Rur_1975_tc,xy = TRUE)
Rur_1975_tc_tbl <- Rur_1975_tc_tbl[complete.cases(Rur_1975_tc_tbl),]

Rur_1980_tc_tbl <- as.data.frame(Rur_1980_tc,xy = TRUE)
Rur_1980_tc_tbl <- Rur_1980_tc_tbl[complete.cases(Rur_1980_tc_tbl),]

Rur_1985_tc_tbl <- as.data.frame(Rur_1985_tc,xy = TRUE)
Rur_1985_tc_tbl <- Rur_1985_tc_tbl[complete.cases(Rur_1985_tc_tbl),]

Rur_1989_tc_tbl <- as.data.frame(Rur_1989_tc,xy = TRUE)
Rur_1989_tc_tbl <- Rur_1989_tc_tbl[complete.cases(Rur_1989_tc_tbl),]

Rur_1995_tc_tbl <- as.data.frame(Rur_1995_tc,xy = TRUE)
Rur_1995_tc_tbl <- Rur_1995_tc_tbl[complete.cases(Rur_1995_tc_tbl),]

Rur_1999_tc_tbl <- as.data.frame(Rur_1999_tc,xy = TRUE)
Rur_1999_tc_tbl <- Rur_1999_tc_tbl[complete.cases(Rur_1999_tc_tbl),]

Rur_2004_tc_tbl <- as.data.frame(Rur_2004_tc,xy = TRUE)
Rur_2004_tc_tbl <- Rur_2004_tc_tbl[complete.cases(Rur_2004_tc_tbl),]

Rur_2007_tc_tbl <- as.data.frame(Rur_2007_tc,xy = TRUE)
Rur_2007_tc_tbl <- Rur_2007_tc_tbl[complete.cases(Rur_2007_tc_tbl),]

Rur_2012_tc_tbl <- as.data.frame(Rur_2012_tc,xy = TRUE)
Rur_2012_tc_tbl <- Rur_2012_tc_tbl[complete.cases(Rur_2012_tc_tbl),]

Rur_2017_tc_tbl <- as.data.frame(Rur_2017_tc,xy = TRUE)
Rur_2017_tc_tbl <- Rur_2017_tc_tbl[complete.cases(Rur_2017_tc_tbl),]

# Merge Tables #
Rur_all_tc <- data.frame(X = numeric(nrow(Rur_drivers)), Y = numeric(nrow(Rur_drivers)), 
                         tc1944 = numeric(nrow(Rur_drivers)), tc1954 = numeric(nrow(Rur_drivers)), tc1966 = numeric(nrow(Rur_drivers)),
                         tc1975 = numeric(nrow(Rur_drivers)), tc1980 = numeric(nrow(Rur_drivers)), tc1985 = numeric(nrow(Rur_drivers)), 
                         tc1989 = numeric(nrow(Rur_drivers)), tc1995 = numeric(nrow(Rur_drivers)), tc1999 = numeric(nrow(Rur_drivers)), 
                         tc2004 = numeric(nrow(Rur_drivers)), tc2007 = numeric(nrow(Rur_drivers)), tc2012 = numeric(nrow(Rur_drivers)), 
                         tc2017 = numeric(nrow(Rur_drivers)))

Rur_all_tc$X <-  Rur_1944_tc_tbl[,c(1)]
Rur_all_tc$Y <-  Rur_1944_tc_tbl[,c(2)]
Rur_all_tc$tc1944 <- Rur_1944_tc_tbl[,c(3)]
Rur_all_tc$tc1954 <- Rur_1954_tc_tbl[,c(3)]
Rur_all_tc$tc1966 <- Rur_1966_tc_tbl[,c(3)]
Rur_all_tc$tc1975 <- Rur_1975_tc_tbl[,c(3)]
Rur_all_tc$tc1980 <- Rur_1980_tc_tbl[,c(3)]
Rur_all_tc$tc1985 <- Rur_1985_tc_tbl[,c(3)]
Rur_all_tc$tc1989 <- Rur_1989_tc_tbl[,c(3)]
Rur_all_tc$tc1995 <- Rur_1995_tc_tbl[,c(3)]
Rur_all_tc$tc1999 <- Rur_1999_tc_tbl[,c(3)]
Rur_all_tc$tc2004 <- Rur_2004_tc_tbl[,c(3)]
Rur_all_tc$tc2007 <- Rur_2007_tc_tbl[,c(3)]
Rur_all_tc$tc2012 <- Rur_2012_tc_tbl[,c(3)]
Rur_all_tc$tc2017 <- Rur_2017_tc_tbl[,c(3)]

write.csv(Rur_all_tc, file = "Rur_treecount.csv")

### Build Tree Change Tables ###
Rur_all_tch <- Rur_1944_tc_tbl[,c(1,2)]
Rur_all_tch$c44to54 <- Rur_1954_tc_tbl[,c(3)] - Rur_1944_tc_tbl[,c(3)]
Rur_all_tch$c54to66 <- Rur_1966_tc_tbl[,c(3)] - Rur_1954_tc_tbl[,c(3)]
Rur_all_tch$c66to75 <- Rur_1975_tc_tbl[,c(3)] - Rur_1966_tc_tbl[,c(3)]
Rur_all_tch$c75to80 <- Rur_1980_tc_tbl[,c(3)] - Rur_1975_tc_tbl[,c(3)]
Rur_all_tch$c80to85 <- Rur_1985_tc_tbl[,c(3)] - Rur_1980_tc_tbl[,c(3)]
Rur_all_tch$c85to89 <- Rur_1989_tc_tbl[,c(3)] - Rur_1985_tc_tbl[,c(3)]
Rur_all_tch$c89to95 <- Rur_1995_tc_tbl[,c(3)] - Rur_1989_tc_tbl[,c(3)]
Rur_all_tch$c95to99 <- Rur_1999_tc_tbl[,c(3)] - Rur_1995_tc_tbl[,c(3)]
Rur_all_tch$c99to04 <- Rur_2004_tc_tbl[,c(3)] - Rur_1999_tc_tbl[,c(3)]
Rur_all_tch$c04to07 <- Rur_2007_tc_tbl[,c(3)] - Rur_2004_tc_tbl[,c(3)]
Rur_all_tch$c07to12 <- Rur_2012_tc_tbl[,c(3)] - Rur_2007_tc_tbl[,c(3)]
Rur_all_tch$c12to17 <- Rur_2017_tc_tbl[,c(3)] - Rur_2012_tc_tbl[,c(3)]

write.csv(Rur_all_tch, file = "Rur_treechange.csv")
