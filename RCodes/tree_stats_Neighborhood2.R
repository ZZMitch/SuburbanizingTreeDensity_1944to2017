setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga 1944-Now Project/MTB/Tree Corrections")

require(rgdal)
require(raster)

### Master Data ###
master <- read.csv("trees_all.csv")
N2_drivers <- read.csv("N2_drivers.csv")
#master_shp <- readOGR(dsn = ".", layer = "trees_allyears_mtb")
#proj4string(master_shp)

#plot(master_shp)

### Neighborhood 1 ###

N2 <- readOGR(dsn = ".", layer = "Neighborhood_2")
#proj4string(UTM) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#proj4string(UTM)

# 1944 #
N2_1944 = subset(master, Year == 1944 & Location == "Neighborhood 2")

#UTM_1944_shp = subset(master_shp, Year == 1944 & Location == "UTM")
#proj4string(UTM_1944_shp)

plot(N2)
#plot(UTM_1944_shp, pch = 19, cex = 0.1)
points(N2_1944$POINT_X, N2_1944$POINT_Y, pch = 19, cex = 0.1)

N2r <- raster(N2)
res(N2r) <- 30
N2r

N2r <- rasterize(N2, N2r)
plot(N2r)
quads <- as(N2r, 'SpatialPolygons')
plot(quads, add = TRUE)
points(N2_1944$POINT_X, N2_1944$POINT_Y, pch = 19, cex = 0.1)

#quads1_N2 <- as(quads, "SpatialPolygonsDataFrame")
#writeOGR(quads1_N2, dsn = '.', layer = 'quads_N21', driver = "ESRI Shapefile")

N2_1944_tc <- rasterize(N2_1944[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1944_tc <- mask(N2_1944_tc, N2)
plot(N2_1944_tc, main = "1944 Tree Density")
plot(N2, add = TRUE)

# 1954 #
N2_1954 = subset(master, Year == 1954 & Location == "Neighborhood 2")
N2_1954_tc <- rasterize(N2_1954[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1954_tc <- mask(N2_1954_tc, N2)
plot(N2_1954_tc, main = "1954 Tree Density")
plot(N2, add = TRUE)

# 1966 #
N2_1966 = subset(master, Year == 1966 & Location == "Neighborhood 2")
N2_1966_tc <- rasterize(N2_1966[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1966_tc <- mask(N2_1966_tc, N2)
plot(N2_1966_tc, main = "1966 Tree Density")
plot(N2, add = TRUE)

# 1975 #
N2_1975 = subset(master, Year == 1975 & Location == "Neighborhood 2")
N2_1975_tc <- rasterize(N2_1975[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1975_tc <- mask(N2_1975_tc, N2)
plot(N2_1975_tc, main = "1975 Tree Density")
plot(N2, add = TRUE)

# 1980 #
N2_1980 = subset(master, Year == 1980 & Location == "Neighborhood 2")
N2_1980_tc <- rasterize(N2_1980[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1980_tc <- mask(N2_1980_tc, N2)
plot(N2_1980_tc, main = "1980 Tree Density")
plot(N2, add = TRUE)

# 1985 #
N2_1985 = subset(master, Year == 1985 & Location == "Neighborhood 2")
N2_1985_tc <- rasterize(N2_1985[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1985_tc <- mask(N2_1985_tc, N2)
plot(N2_1985_tc, main = "1985 Tree Density")
plot(N2, add = TRUE)

# 1989 #
N2_1989 = subset(master, Year == 1989 & Location == "Neighborhood 2")
N2_1989_tc <- rasterize(N2_1989[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1989_tc <- mask(N2_1989_tc, N2)
plot(N2_1989_tc, main = "1989 Tree Density")
plot(N2, add = TRUE)

# 1995 #
N2_1995 = subset(master, Year == 1995 & Location == "Neighborhood 2")
N2_1995_tc <- rasterize(N2_1995[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1995_tc <- mask(N2_1995_tc, N2)
plot(N2_1995_tc, main = "1995 Tree Density")
plot(N2, add = TRUE)

# 1999 #
N2_1999 = subset(master, Year == 1999 & Location == "Neighborhood 2")
N2_1999_tc <- rasterize(N2_1999[,c(4,5)], N2r, fun = 'count', background = 0)
N2_1999_tc <- mask(N2_1999_tc, N2)
plot(N2_1999_tc, main = "1999 Tree Density")
plot(N2, add = TRUE)

# 2004 #
N2_2004 = subset(master, Year == 2004 & Location == "Neighborhood 2")
N2_2004_tc <- rasterize(N2_2004[,c(4,5)], N2r, fun = 'count', background = 0)
N2_2004_tc <- mask(N2_2004_tc, N2)
plot(N2_2004_tc, main = "2004 Tree Density")
plot(N2, add = TRUE)

# 2007 #
N2_2007 = subset(master, Year == 2007 & Location == "Neighborhood 2")
N2_2007_tc <- rasterize(N2_2007[,c(4,5)], N2r, fun = 'count', background = 0)
N2_2007_tc <- mask(N2_2007_tc, N2)
plot(N2_2007_tc, main = "2007 Tree Density")
plot(N2, add = TRUE)

# 2012 #
N2_2012 = subset(master, Year == 2012 & Location == "Neighborhood 2")
N2_2012_tc <- rasterize(N2_2012[,c(4,5)], N2r, fun = 'count', background = 0)
N2_2012_tc <- mask(N2_2012_tc, N2)
plot(N2_2012_tc, main = "2012 Tree Density")
plot(N2, add = TRUE)

# 2017 #
N2_2017 = subset(master, Year == 2017 & Location == "Neighborhood 2")
N2_2017_tc <- rasterize(N2_2017[,c(4,5)], N2r, fun = 'count', background = 0)
N2_2017_tc <- mask(N2_2017_tc, N2)
plot(N2_2017_tc, main = "2017 Tree Density")
plot(N2, add = TRUE)

### Analysis ###

# Create Raster Stack #
N2_stack <- stack(N2_1944_tc, N2_1954_tc, N2_1966_tc, N2_1975_tc, N2_1980_tc, N2_1985_tc, N2_1989_tc,
                  N2_1995_tc, N2_1999_tc, N2_2004_tc, N2_2007_tc, N2_2012_tc, N2_2017_tc)
N2_stack
plot(N2_stack)

# Jaccard changes #
#require(zonator)
#N2_jc <- cross_jaccard(N2_stack, 0)
#N2_jc

years <- c(1944, 1954, 1966, 1975, 1980, 1985, 1989, 1995, 1999, 2004, 2007, 2012, 2017)


# Calculate Slope #
fun_slope <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[2]
  }
}

slope <- calc(N2_stack, fun_slope)
slope_m <- mask(slope, N2)

plot(slope_m, main = "Tree Density Slope")
plot(N2, add = TRUE)

# Calculate Pvalue #
fun_pval <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[8]
  }
}

pval <- calc(N2_stack, fun_pval)
pval_m <- mask(pval, N2)

plot(pval_m, main = "Tree Density Significance")
plot(N2, add = TRUE)

# Save Rasters #
writeRaster(slope_m, "N2_44to17_Slope.tif", "GTiff", overwrite = TRUE)
writeRaster(pval_m, "N2_44to17_Pvalue.tif", "GTiff", overwrite = TRUE)

### Build Tree Count Tables ###
N2_1944_tc_tbl <- as.data.frame(N2_1944_tc,xy = TRUE)
N2_1944_tc_tbl <- N2_1944_tc_tbl[complete.cases(N2_1944_tc_tbl),]
N2_1954_tc_tbl <- as.data.frame(N2_1954_tc,xy = TRUE)
N2_1954_tc_tbl <- N2_1954_tc_tbl[complete.cases(N2_1954_tc_tbl),]
N2_1966_tc_tbl <- as.data.frame(N2_1966_tc,xy = TRUE)
N2_1966_tc_tbl <- N2_1966_tc_tbl[complete.cases(N2_1966_tc_tbl),]
N2_1975_tc_tbl <- as.data.frame(N2_1975_tc,xy = TRUE)
N2_1975_tc_tbl <- N2_1975_tc_tbl[complete.cases(N2_1975_tc_tbl),]
N2_1980_tc_tbl <- as.data.frame(N2_1980_tc,xy = TRUE)
N2_1980_tc_tbl <- N2_1980_tc_tbl[complete.cases(N2_1980_tc_tbl),]
N2_1985_tc_tbl <- as.data.frame(N2_1985_tc,xy = TRUE)
N2_1985_tc_tbl <- N2_1985_tc_tbl[complete.cases(N2_1985_tc_tbl),]
N2_1989_tc_tbl <- as.data.frame(N2_1989_tc,xy = TRUE)
N2_1989_tc_tbl <- N2_1989_tc_tbl[complete.cases(N2_1989_tc_tbl),]
N2_1995_tc_tbl <- as.data.frame(N2_1995_tc,xy = TRUE)
N2_1995_tc_tbl <- N2_1995_tc_tbl[complete.cases(N2_1995_tc_tbl),]
N2_1999_tc_tbl <- as.data.frame(N2_1999_tc,xy = TRUE)
N2_1999_tc_tbl <- N2_1999_tc_tbl[complete.cases(N2_1999_tc_tbl),]
N2_2004_tc_tbl <- as.data.frame(N2_2004_tc,xy = TRUE)
N2_2004_tc_tbl <- N2_2004_tc_tbl[complete.cases(N2_2004_tc_tbl),]
N2_2007_tc_tbl <- as.data.frame(N2_2007_tc,xy = TRUE)
N2_2007_tc_tbl <- N2_2007_tc_tbl[complete.cases(N2_2007_tc_tbl),]
N2_2012_tc_tbl <- as.data.frame(N2_2012_tc,xy = TRUE)
N2_2012_tc_tbl <- N2_2012_tc_tbl[complete.cases(N2_2012_tc_tbl),]
N2_2017_tc_tbl <- as.data.frame(N2_2017_tc,xy = TRUE)
N2_2017_tc_tbl <- N2_2017_tc_tbl[complete.cases(N2_2017_tc_tbl),]

# Merge Tables #
N2_all_tc <- data.frame(X = numeric(nrow(N2_drivers)), Y = numeric(nrow(N2_drivers)), 
                        tc1944 = numeric(nrow(N2_drivers)), tc1954 = numeric(nrow(N2_drivers)), tc1966 = numeric(nrow(N2_drivers)),
                        tc1975 = numeric(nrow(N2_drivers)), tc1980 = numeric(nrow(N2_drivers)), tc1985 = numeric(nrow(N2_drivers)), 
                        tc1989 = numeric(nrow(N2_drivers)), tc1995 = numeric(nrow(N2_drivers)), tc1999 = numeric(nrow(N2_drivers)), 
                        tc2004 = numeric(nrow(N2_drivers)), tc2007 = numeric(nrow(N2_drivers)), tc2012 = numeric(nrow(N2_drivers)), 
                        tc2017 = numeric(nrow(N2_drivers)))

N2_all_tc$X <-  N2_1944_tc_tbl[,c(1)]
N2_all_tc$Y <-  N2_1944_tc_tbl[,c(2)]
N2_all_tc$tc1944 <- N2_1944_tc_tbl[,c(3)]
N2_all_tc$tc1954 <- N2_1954_tc_tbl[,c(3)]
N2_all_tc$tc1966 <- N2_1966_tc_tbl[,c(3)]
N2_all_tc$tc1975 <- N2_1975_tc_tbl[,c(3)]
N2_all_tc$tc1980 <- N2_1980_tc_tbl[,c(3)]
N2_all_tc$tc1985 <- N2_1985_tc_tbl[,c(3)]
N2_all_tc$tc1989 <- N2_1989_tc_tbl[,c(3)]
N2_all_tc$tc1995 <- N2_1995_tc_tbl[,c(3)]
N2_all_tc$tc1999 <- N2_1999_tc_tbl[,c(3)]
N2_all_tc$tc2004 <- N2_2004_tc_tbl[,c(3)]
N2_all_tc$tc2007 <- N2_2007_tc_tbl[,c(3)]
N2_all_tc$tc2012 <- N2_2012_tc_tbl[,c(3)]
N2_all_tc$tc2017 <- N2_2017_tc_tbl[,c(3)]

write.csv(N2_all_tc, file = "N2_treecount.csv")

### Build Tree Change Tables ###
N2_all_tch <- N2_1944_tc_tbl[,c(1,2)]
N2_all_tch$c44to54 <- N2_1954_tc_tbl[,c(3)] - N2_1944_tc_tbl[,c(3)]
N2_all_tch$c54to66 <- N2_1966_tc_tbl[,c(3)] - N2_1954_tc_tbl[,c(3)]
N2_all_tch$c66to75 <- N2_1975_tc_tbl[,c(3)] - N2_1966_tc_tbl[,c(3)]
N2_all_tch$c75to80 <- N2_1980_tc_tbl[,c(3)] - N2_1975_tc_tbl[,c(3)]
N2_all_tch$c80to85 <- N2_1985_tc_tbl[,c(3)] - N2_1980_tc_tbl[,c(3)]
N2_all_tch$c85to89 <- N2_1989_tc_tbl[,c(3)] - N2_1985_tc_tbl[,c(3)]
N2_all_tch$c89to95 <- N2_1995_tc_tbl[,c(3)] - N2_1989_tc_tbl[,c(3)]
N2_all_tch$c95to99 <- N2_1999_tc_tbl[,c(3)] - N2_1995_tc_tbl[,c(3)]
N2_all_tch$c99to04 <- N2_2004_tc_tbl[,c(3)] - N2_1999_tc_tbl[,c(3)]
N2_all_tch$c04to07 <- N2_2007_tc_tbl[,c(3)] - N2_2004_tc_tbl[,c(3)]
N2_all_tch$c07to12 <- N2_2012_tc_tbl[,c(3)] - N2_2007_tc_tbl[,c(3)]
N2_all_tch$c12to17 <- N2_2017_tc_tbl[,c(3)] - N2_2012_tc_tbl[,c(3)]

write.csv(N2_all_tch, file = "N2_treechange.csv")

# Spatial stats #
#N2_w <- as(N2, "owin")
#N2_1995_pp <-as.ppp(N2_1995[,c(4,5)], N2_w)
#N2_1999_pp <-as.ppp(N2_1999[,c(4,5)], N2_w)

#plot(N2_1995_pp)
#plot(N2_1999_pp)

#ktest <- Kest(N2_1995_pp)
#plot(ktest)

#ktest1 <- Kest(N2_1999_pp)
#plot(ktest1)

