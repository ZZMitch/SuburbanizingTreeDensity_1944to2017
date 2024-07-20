setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga 1944-Now Project/MTB/Tree Corrections")

require(rgdal)
require(raster)

### Master Data ###
master <- read.csv("trees_all.csv")
UTM_drivers <- read.csv("UTM_drivers.csv")
#master_shp <- readOGR(dsn = ".", layer = "trees_allyears_mtb")
#proj4string(master_shp)

#plot(master_shp)

### UTM ###

UTM <- readOGR(dsn = ".", layer = "UTM1")
#proj4string(UTM) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#proj4string(UTM)

# 1944 #
UTM_1944 = subset(master, Year == 1944 & Location == "UTM")

#UTM_1944_shp = subset(master_shp, Year == 1944 & Location == "UTM")
#proj4string(UTM_1944_shp)

plot(UTM)
#plot(UTM_1944_shp, pch = 19, cex = 0.1)
points(UTM_1944$POINT_X, UTM_1944$POINT_Y, pch = 19, cex = 0.1)

UTMr <- raster(UTM)
res(UTMr) <- 30
UTMr

UTMr <- rasterize(UTM, UTMr)
plot(UTMr)
quads <- as(UTMr, 'SpatialPolygons')
plot(quads, add = TRUE)
points(UTM_1944$POINT_X, UTM_1944$POINT_Y, pch = 19, cex = 0.1)

#quads1_utm <- as(quads, "SpatialPolygonsDataFrame")
#writeOGR(quads1_utm, dsn = '.', layer = 'quads_utm', driver = "ESRI Shapefile")

UTM_1944_tc <- rasterize(UTM_1944[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1944_tc <- mask(UTM_1944_tc, UTM)
plot(UTM_1944_tc, main = "1944 Tree Density")
plot(UTM, add = TRUE)

# 1954 #
UTM_1954 = subset(master, Year == 1954 & Location == "UTM")
UTM_1954_tc <- rasterize(UTM_1954[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1954_tc <- mask(UTM_1954_tc, UTM)
plot(UTM_1954_tc, main = "1954 Tree Density")
plot(UTM, add = TRUE)

# 1966 #
UTM_1966 = subset(master, Year == 1966 & Location == "UTM")
UTM_1966_tc <- rasterize(UTM_1966[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1966_tc <- mask(UTM_1966_tc, UTM)
plot(UTM_1966_tc, main = "1966 Tree Density")
plot(UTM, add = TRUE)

# 1975 #
UTM_1975 = subset(master, Year == 1975 & Location == "UTM")
UTM_1975_tc <- rasterize(UTM_1975[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1975_tc <- mask(UTM_1975_tc, UTM)
plot(UTM_1975_tc, main = "1975 Tree Density")
plot(UTM, add = TRUE)

# 1980 #
UTM_1980 = subset(master, Year == 1980 & Location == "UTM")
UTM_1980_tc <- rasterize(UTM_1980[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1980_tc <- mask(UTM_1980_tc, UTM)
plot(UTM_1980_tc, main = "1980 Tree Density")
plot(UTM, add = TRUE)

# 1985 #
UTM_1985 = subset(master, Year == 1985 & Location == "UTM")
UTM_1985_tc <- rasterize(UTM_1985[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1985_tc <- mask(UTM_1985_tc, UTM)
plot(UTM_1985_tc, main = "1985 Tree Density")
plot(UTM, add = TRUE)

# 1989 #
UTM_1989 = subset(master, Year == 1989 & Location == "UTM")
UTM_1989_tc <- rasterize(UTM_1989[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1989_tc <- mask(UTM_1989_tc, UTM)
plot(UTM_1989_tc, main = "1989 Tree Density")
plot(UTM, add = TRUE)

# 1995 #
UTM_1995 = subset(master, Year == 1995 & Location == "UTM")
UTM_1995_tc <- rasterize(UTM_1995[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1995_tc <- mask(UTM_1995_tc, UTM)
plot(UTM_1995_tc, main = "1995 Tree Density")
plot(UTM, add = TRUE)

# 1999 #
UTM_1999 = subset(master, Year == 1999 & Location == "UTM")
UTM_1999_tc <- rasterize(UTM_1999[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1999_tc <- mask(UTM_1999_tc, UTM)
plot(UTM_1999_tc, main = "1999 Tree Density")
plot(UTM, add = TRUE)

# 2004 #
UTM_2004 = subset(master, Year == 2004 & Location == "UTM")
UTM_2004_tc <- rasterize(UTM_2004[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_2004_tc <- mask(UTM_2004_tc, UTM)
plot(UTM_2004_tc, main = "2004 Tree Density")
plot(UTM, add = TRUE)

# 2007 #
UTM_2007 = subset(master, Year == 2007 & Location == "UTM")
UTM_2007_tc <- rasterize(UTM_2007[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_2007_tc <- mask(UTM_2007_tc, UTM)
plot(UTM_2007_tc, main = "2007 Tree Density")
plot(UTM, add = TRUE)

# 2012 #
UTM_2012 = subset(master, Year == 2012 & Location == "UTM")
UTM_2012_tc <- rasterize(UTM_2012[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_2012_tc <- mask(UTM_2012_tc, UTM)
plot(UTM_2012_tc, main = "2012 Tree Density")
plot(UTM, add = TRUE)

# 2017 #
UTM_2017 = subset(master, Year == 2017 & Location == "UTM")
UTM_2017_tc <- rasterize(UTM_2017[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_2017_tc <- mask(UTM_2017_tc, UTM)
plot(UTM_2017_tc, main = "2017 Tree Density")
plot(UTM, add = TRUE)

### Analysis ###

# Create Raster Stack #
UTM_stack <- stack(UTM_1944_tc, UTM_1954_tc, UTM_1966_tc, UTM_1975_tc, UTM_1980_tc, UTM_1985_tc, UTM_1989_tc,
                       UTM_1995_tc, UTM_1999_tc, UTM_2004_tc, UTM_2007_tc, UTM_2012_tc, UTM_2017_tc)
UTM_stack
plot(UTM_stack)

# Jaccard changes #
#require(zonator)
#UTM_jc <- cross_jaccard(UTM_stack, 0)
#UTM_jc

years <- c(1944, 1954, 1966, 1975, 1980, 1985, 1989, 1995, 1999, 2004, 2007, 2012, 2017)

# Calculate Slope #
fun_slope <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[2]
  }
}

slope <- calc(UTM_stack, fun_slope)
slope_m <- mask(slope, UTM)

plot(slope_m, main = "Tree Density Slope")
plot(UTM, add = TRUE)

# Calculate Pvalue #
fun_pval <- function(y) {
  if(sum(is.na(y)) > 13) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[8]
  }
}

pval <- calc(UTM_stack, fun_pval)
pval_m <- mask(pval, UTM)

plot(pval_m, main = "Tree Density Significance")
plot(UTM, add = TRUE)

# Save Rasters #
writeRaster(slope_m, "UTM_44to17_Slope.tif", "GTiff", overwrite = TRUE)
writeRaster(pval_m, "UTM_44to17_Pvalue.tif", "GTiff", overwrite = TRUE)

### Build Tree Count Tables ###
UTM_1944_tc_tbl <- as.data.frame(UTM_1944_tc,xy = TRUE)
UTM_1944_tc_tbl <- UTM_1944_tc_tbl[complete.cases(UTM_1944_tc_tbl),]
UTM_1954_tc_tbl <- as.data.frame(UTM_1954_tc,xy = TRUE)
UTM_1954_tc_tbl <- UTM_1954_tc_tbl[complete.cases(UTM_1954_tc_tbl),]
UTM_1966_tc_tbl <- as.data.frame(UTM_1966_tc,xy = TRUE)
UTM_1966_tc_tbl <- UTM_1966_tc_tbl[complete.cases(UTM_1966_tc_tbl),]
UTM_1975_tc_tbl <- as.data.frame(UTM_1975_tc,xy = TRUE)
UTM_1975_tc_tbl <- UTM_1975_tc_tbl[complete.cases(UTM_1975_tc_tbl),]
UTM_1980_tc_tbl <- as.data.frame(UTM_1980_tc,xy = TRUE)
UTM_1980_tc_tbl <- UTM_1980_tc_tbl[complete.cases(UTM_1980_tc_tbl),]
UTM_1985_tc_tbl <- as.data.frame(UTM_1985_tc,xy = TRUE)
UTM_1985_tc_tbl <- UTM_1985_tc_tbl[complete.cases(UTM_1985_tc_tbl),]
UTM_1989_tc_tbl <- as.data.frame(UTM_1989_tc,xy = TRUE)
UTM_1989_tc_tbl <- UTM_1989_tc_tbl[complete.cases(UTM_1989_tc_tbl),]
UTM_1995_tc_tbl <- as.data.frame(UTM_1995_tc,xy = TRUE)
UTM_1995_tc_tbl <- UTM_1995_tc_tbl[complete.cases(UTM_1995_tc_tbl),]
UTM_1999_tc_tbl <- as.data.frame(UTM_1999_tc,xy = TRUE)
UTM_1999_tc_tbl <- UTM_1999_tc_tbl[complete.cases(UTM_1999_tc_tbl),]
UTM_2004_tc_tbl <- as.data.frame(UTM_2004_tc,xy = TRUE)
UTM_2004_tc_tbl <- UTM_2004_tc_tbl[complete.cases(UTM_2004_tc_tbl),]
UTM_2007_tc_tbl <- as.data.frame(UTM_2007_tc,xy = TRUE)
UTM_2007_tc_tbl <- UTM_2007_tc_tbl[complete.cases(UTM_2007_tc_tbl),]
UTM_2012_tc_tbl <- as.data.frame(UTM_2012_tc,xy = TRUE)
UTM_2012_tc_tbl <- UTM_2012_tc_tbl[complete.cases(UTM_2012_tc_tbl),]
UTM_2017_tc_tbl <- as.data.frame(UTM_2017_tc,xy = TRUE)
UTM_2017_tc_tbl <- UTM_2017_tc_tbl[complete.cases(UTM_2017_tc_tbl),]

# Merge Tables #
UTM_all_tc <- data.frame(X = numeric(nrow(UTM_drivers)), Y = numeric(nrow(UTM_drivers)), 
  tc1944 = numeric(nrow(UTM_drivers)), tc1954 = numeric(nrow(UTM_drivers)), tc1966 = numeric(nrow(UTM_drivers)),
  tc1975 = numeric(nrow(UTM_drivers)), tc1980 = numeric(nrow(UTM_drivers)), tc1985 = numeric(nrow(UTM_drivers)), 
  tc1989 = numeric(nrow(UTM_drivers)), tc1995 = numeric(nrow(UTM_drivers)), tc1999 = numeric(nrow(UTM_drivers)), 
  tc2004 = numeric(nrow(UTM_drivers)), tc2007 = numeric(nrow(UTM_drivers)), tc2012 = numeric(nrow(UTM_drivers)), 
  tc2017 = numeric(nrow(UTM_drivers)))

UTM_all_tc$X <-  UTM_1944_tc_tbl[,c(1)]
UTM_all_tc$Y <-  UTM_1944_tc_tbl[,c(2)]
UTM_all_tc$tc1944 <- UTM_1944_tc_tbl[,c(3)]
UTM_all_tc$tc1954 <- UTM_1954_tc_tbl[,c(3)]
UTM_all_tc$tc1966 <- UTM_1966_tc_tbl[,c(3)]
UTM_all_tc$tc1975 <- UTM_1975_tc_tbl[,c(3)]
UTM_all_tc$tc1980 <- UTM_1980_tc_tbl[,c(3)]
UTM_all_tc$tc1985 <- UTM_1985_tc_tbl[,c(3)]
UTM_all_tc$tc1989 <- UTM_1989_tc_tbl[,c(3)]
UTM_all_tc$tc1995 <- UTM_1995_tc_tbl[,c(3)]
UTM_all_tc$tc1999 <- UTM_1999_tc_tbl[,c(3)]
UTM_all_tc$tc2004 <- UTM_2004_tc_tbl[,c(3)]
UTM_all_tc$tc2007 <- UTM_2007_tc_tbl[,c(3)]
UTM_all_tc$tc2012 <- UTM_2012_tc_tbl[,c(3)]
UTM_all_tc$tc2017 <- UTM_2017_tc_tbl[,c(3)]

write.csv(UTM_all_tc, file = "UTM_treecount.csv")

### Build Tree Change Tables ###
UTM_all_tch <- UTM_1944_tc_tbl[,c(1,2)]
UTM_all_tch$c44to54 <- UTM_1954_tc_tbl[,c(3)] - UTM_1944_tc_tbl[,c(3)]
UTM_all_tch$c54to66 <- UTM_1966_tc_tbl[,c(3)] - UTM_1954_tc_tbl[,c(3)]
UTM_all_tch$c66to75 <- UTM_1975_tc_tbl[,c(3)] - UTM_1966_tc_tbl[,c(3)]
UTM_all_tch$c75to80 <- UTM_1980_tc_tbl[,c(3)] - UTM_1975_tc_tbl[,c(3)]
UTM_all_tch$c80to85 <- UTM_1985_tc_tbl[,c(3)] - UTM_1980_tc_tbl[,c(3)]
UTM_all_tch$c85to89 <- UTM_1989_tc_tbl[,c(3)] - UTM_1985_tc_tbl[,c(3)]
UTM_all_tch$c89to95 <- UTM_1995_tc_tbl[,c(3)] - UTM_1989_tc_tbl[,c(3)]
UTM_all_tch$c95to99 <- UTM_1999_tc_tbl[,c(3)] - UTM_1995_tc_tbl[,c(3)]
UTM_all_tch$c99to04 <- UTM_2004_tc_tbl[,c(3)] - UTM_1999_tc_tbl[,c(3)]
UTM_all_tch$c04to07 <- UTM_2007_tc_tbl[,c(3)] - UTM_2004_tc_tbl[,c(3)]
UTM_all_tch$c07to12 <- UTM_2012_tc_tbl[,c(3)] - UTM_2007_tc_tbl[,c(3)]
UTM_all_tch$c12to17 <- UTM_2017_tc_tbl[,c(3)] - UTM_2012_tc_tbl[,c(3)]

write.csv(UTM_all_tch, file = "UTM_treechange.csv")

### Spatial Pattern Analysis ###

#require(spatstat)
#require(maptools)
#require(sp)
#require(ape)

# 1944 #
#UTM_w <- as(UTM, "owin")
#UTM_1944_pp <-as.ppp(UTM_1944[,c(4,5)], UTM_w)
#UTM_2017_pp <-as.ppp(UTM_2017[,c(4,5)], UTM_w)

#dup <- duplicated(UTM_1944_pp)
#table(dup)["TRUE"]

#ktest <- Kest(UTM_1944_pp)
#plot(ktest)
#E <- envelope(UTM_1944_pp, Kest, nsim = 39)
#plot(E)

#ktest <- Kest(UTM_2017_pp)
#plot(ktest)

#qtest <- quadrat.test(UTM_1944_pp, nx = 5, ny = 5)
#qtest

#plot(UTM_1944_pp)
#plot(qtest, add = TRUE, cex = 1)

