setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga 1944-Now Project/MTB/Tree Corrections")

require(rgdal)
require(raster)

### Master Data ###
master <- read.csv("trees_all.csv")
N1_drivers <- read.csv("N1_drivers.csv")
#master_shp <- readOGR(dsn = ".", layer = "trees_allyears_mtb")
#proj4string(master_shp)

#plot(master_shp)

### Neighborhood 1 ###

N1 <- readOGR(dsn = ".", layer = "Neighborhood_1")
#proj4string(UTM) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#proj4string(UTM)

# 1944 #
N1_1944 = subset(master, Year == 1944 & Location == "Neighborhood 1")

#UTM_1944_shp = subset(master_shp, Year == 1944 & Location == "UTM")
#proj4string(UTM_1944_shp)

plot(N1)
#plot(UTM_1944_shp, pch = 19, cex = 0.1)
points(N1_1944$POINT_X, N1_1944$POINT_Y, pch = 19, cex = 0.1)

N1r <- raster(N1)
res(N1r) <- 30
N1r

N1r <- rasterize(N1, N1r)
plot(N1r)
quads <- as(N1r, 'SpatialPolygons')
plot(quads, add = TRUE)
points(N1_1944$POINT_X, N1_1944$POINT_Y, pch = 19, cex = 0.1)

#quads1_N1 <- as(quads, "SpatialPolygonsDataFrame")
#writeOGR(quads1_N1, dsn = '.', layer = 'quads_N1', driver = "ESRI Shapefile")

N1_1944_tc <- rasterize(N1_1944[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1944_tc <- mask(N1_1944_tc, N1)
plot(N1_1944_tc, main = "1944 Tree Density")
plot(N1, add = TRUE)

# 1954 #
N1_1954 = subset(master, Year == 1954 & Location == "Neighborhood 1")
N1_1954_tc <- rasterize(N1_1954[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1954_tc <- mask(N1_1954_tc, N1)
plot(N1_1954_tc, main = "1954 Tree Density")
plot(N1, add = TRUE)

# 1966 #
N1_1966 = subset(master, Year == 1966 & Location == "Neighborhood 1")
N1_1966_tc <- rasterize(N1_1966[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1966_tc <- mask(N1_1966_tc, N1)
plot(N1_1966_tc, main = "1966 Tree Density")
plot(N1, add = TRUE)

# 1975 #
N1_1975 = subset(master, Year == 1975 & Location == "Neighborhood 1")
N1_1975_tc <- rasterize(N1_1975[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1975_tc <- mask(N1_1975_tc, N1)
plot(N1_1975_tc, main = "1975 Tree Density")
plot(N1, add = TRUE)

# 1980 #
N1_1980 = subset(master, Year == 1980 & Location == "Neighborhood 1")
N1_1980_tc <- rasterize(N1_1980[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1980_tc <- mask(N1_1980_tc, N1)
plot(N1_1980_tc, main = "1980 Tree Density")
plot(N1, add = TRUE)

# 1985 #
N1_1985 = subset(master, Year == 1985 & Location == "Neighborhood 1")
N1_1985_tc <- rasterize(N1_1985[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1985_tc <- mask(N1_1985_tc, N1)
plot(N1_1985_tc, main = "1985 Tree Density")
plot(N1, add = TRUE)

# 1989 #
N1_1989 = subset(master, Year == 1989 & Location == "Neighborhood 1")
N1_1989_tc <- rasterize(N1_1989[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1989_tc <- mask(N1_1989_tc, N1)
plot(N1_1989_tc, main = "1989 Tree Density")
plot(N1, add = TRUE)

# 1995 #
N1_1995 = subset(master, Year == 1995 & Location == "Neighborhood 1")
N1_1995_tc <- rasterize(N1_1995[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1995_tc <- mask(N1_1995_tc, N1)
plot(N1_1995_tc, main = "1995 Tree Density")
plot(N1, add = TRUE)

# 1999 #
N1_1999 = subset(master, Year == 1999 & Location == "Neighborhood 1")
N1_1999_tc <- rasterize(N1_1999[,c(4,5)], N1r, fun = 'count', background = 0)
N1_1999_tc <- mask(N1_1999_tc, N1)
plot(N1_1999_tc, main = "1999 Tree Density")
plot(N1, add = TRUE)

# 2004 #
N1_2004 = subset(master, Year == 2004 & Location == "Neighborhood 1")
N1_2004_tc <- rasterize(N1_2004[,c(4,5)], N1r, fun = 'count', background = 0)
N1_2004_tc <- mask(N1_2004_tc, N1)
plot(N1_2004_tc, main = "2004 Tree Density")
plot(N1, add = TRUE)

# 2007 #
N1_2007 = subset(master, Year == 2007 & Location == "Neighborhood 1")
N1_2007_tc <- rasterize(N1_2007[,c(4,5)], N1r, fun = 'count', background = 0)
N1_2007_tc <- mask(N1_2007_tc, N1)
plot(N1_2007_tc, main = "2007 Tree Density")
plot(N1, add = TRUE)

# 2012 #
N1_2012 = subset(master, Year == 2012 & Location == "Neighborhood 1")
N1_2012_tc <- rasterize(N1_2012[,c(4,5)], N1r, fun = 'count', background = 0)
N1_2012_tc <- mask(N1_2012_tc, N1)
plot(N1_2012_tc, main = "2012 Tree Density")
plot(N1, add = TRUE)

# 2017 #
N1_2017 = subset(master, Year == 2017 & Location == "Neighborhood 1")
N1_2017_tc <- rasterize(N1_2017[,c(4,5)], N1r, fun = 'count', background = 0)
N1_2017_tc <- mask(N1_2017_tc, N1)
plot(N1_2017_tc, main = "2017 Tree Density")
plot(N1, add = TRUE)

### Analysis ###

# Create Raster Stack #
N1_stack <- stack(N1_1944_tc, N1_1954_tc, N1_1966_tc, N1_1975_tc, N1_1980_tc, N1_1985_tc, N1_1989_tc,
                  N1_1995_tc, N1_1999_tc, N1_2004_tc, N1_2007_tc, N1_2012_tc, N1_2017_tc)
N1_stack
plot(N1_stack)

years <- c(1944, 1954, 1966, 1975, 1980, 1985, 1989, 1995, 1999, 2004, 2007, 2012, 2017)


# Calculate Slope #
fun_slope <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[2]
  }
}

slope <- calc(N1_stack, fun_slope)
slope_m <- mask(slope, N1)

plot(slope_m, main = "Tree Density Slope")
plot(N1, add = TRUE)

# Calculate Pvalue #
fun_pval <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[8]
  }
}

pval <- calc(N1_stack, fun_pval)
pval_m <- mask(pval, N1)

plot(pval_m, main = "Tree Density Significance")
plot(N1, add = TRUE)

# Save Rasters #
writeRaster(slope_m, "N1_44to17_Slope.tif", "GTiff", overwrite = TRUE)
writeRaster(pval_m, "N1_44to17_Pvalue.tif", "GTiff", overwrite = TRUE)

### Build Tree Count Tables ###
N1_1944_tc_tbl <- as.data.frame(N1_1944_tc,xy = TRUE)
N1_1944_tc_tbl <- N1_1944_tc_tbl[complete.cases(N1_1944_tc_tbl),]
N1_1954_tc_tbl <- as.data.frame(N1_1954_tc,xy = TRUE)
N1_1954_tc_tbl <- N1_1954_tc_tbl[complete.cases(N1_1954_tc_tbl),]
N1_1966_tc_tbl <- as.data.frame(N1_1966_tc,xy = TRUE)
N1_1966_tc_tbl <- N1_1966_tc_tbl[complete.cases(N1_1966_tc_tbl),]
N1_1975_tc_tbl <- as.data.frame(N1_1975_tc,xy = TRUE)
N1_1975_tc_tbl <- N1_1975_tc_tbl[complete.cases(N1_1975_tc_tbl),]
N1_1980_tc_tbl <- as.data.frame(N1_1980_tc,xy = TRUE)
N1_1980_tc_tbl <- N1_1980_tc_tbl[complete.cases(N1_1980_tc_tbl),]
N1_1985_tc_tbl <- as.data.frame(N1_1985_tc,xy = TRUE)
N1_1985_tc_tbl <- N1_1985_tc_tbl[complete.cases(N1_1985_tc_tbl),]
N1_1989_tc_tbl <- as.data.frame(N1_1989_tc,xy = TRUE)
N1_1989_tc_tbl <- N1_1989_tc_tbl[complete.cases(N1_1989_tc_tbl),]
N1_1995_tc_tbl <- as.data.frame(N1_1995_tc,xy = TRUE)
N1_1995_tc_tbl <- N1_1995_tc_tbl[complete.cases(N1_1995_tc_tbl),]
N1_1999_tc_tbl <- as.data.frame(N1_1999_tc,xy = TRUE)
N1_1999_tc_tbl <- N1_1999_tc_tbl[complete.cases(N1_1999_tc_tbl),]
N1_2004_tc_tbl <- as.data.frame(N1_2004_tc,xy = TRUE)
N1_2004_tc_tbl <- N1_2004_tc_tbl[complete.cases(N1_2004_tc_tbl),]
N1_2007_tc_tbl <- as.data.frame(N1_2007_tc,xy = TRUE)
N1_2007_tc_tbl <- N1_2007_tc_tbl[complete.cases(N1_2007_tc_tbl),]
N1_2012_tc_tbl <- as.data.frame(N1_2012_tc,xy = TRUE)
N1_2012_tc_tbl <- N1_2012_tc_tbl[complete.cases(N1_2012_tc_tbl),]
N1_2017_tc_tbl <- as.data.frame(N1_2017_tc,xy = TRUE)
N1_2017_tc_tbl <- N1_2017_tc_tbl[complete.cases(N1_2017_tc_tbl),]

# Merge Tables #
N1_all_tc <- data.frame(X = numeric(nrow(N1_drivers)), Y = numeric(nrow(N1_drivers)), 
                         tc1944 = numeric(nrow(N1_drivers)), tc1954 = numeric(nrow(N1_drivers)), tc1966 = numeric(nrow(N1_drivers)),
                         tc1975 = numeric(nrow(N1_drivers)), tc1980 = numeric(nrow(N1_drivers)), tc1985 = numeric(nrow(N1_drivers)), 
                         tc1989 = numeric(nrow(N1_drivers)), tc1995 = numeric(nrow(N1_drivers)), tc1999 = numeric(nrow(N1_drivers)), 
                         tc2004 = numeric(nrow(N1_drivers)), tc2007 = numeric(nrow(N1_drivers)), tc2012 = numeric(nrow(N1_drivers)), 
                         tc2017 = numeric(nrow(N1_drivers)))

N1_all_tc$X <-  N1_1944_tc_tbl[,c(1)]
N1_all_tc$Y <-  N1_1944_tc_tbl[,c(2)]
N1_all_tc$tc1944 <- N1_1944_tc_tbl[,c(3)]
N1_all_tc$tc1954 <- N1_1954_tc_tbl[,c(3)]
N1_all_tc$tc1966 <- N1_1966_tc_tbl[,c(3)]
N1_all_tc$tc1975 <- N1_1975_tc_tbl[,c(3)]
N1_all_tc$tc1980 <- N1_1980_tc_tbl[,c(3)]
N1_all_tc$tc1985 <- N1_1985_tc_tbl[,c(3)]
N1_all_tc$tc1989 <- N1_1989_tc_tbl[,c(3)]
N1_all_tc$tc1995 <- N1_1995_tc_tbl[,c(3)]
N1_all_tc$tc1999 <- N1_1999_tc_tbl[,c(3)]
N1_all_tc$tc2004 <- N1_2004_tc_tbl[,c(3)]
N1_all_tc$tc2007 <- N1_2007_tc_tbl[,c(3)]
N1_all_tc$tc2012 <- N1_2012_tc_tbl[,c(3)]
N1_all_tc$tc2017 <- N1_2017_tc_tbl[,c(3)]

write.csv(N1_all_tc, file = "N1_treecount.csv")

### Build Tree Change Tables ###
N1_all_tch <- N1_1944_tc_tbl[,c(1,2)]
N1_all_tch$c44to54 <- N1_1954_tc_tbl[,c(3)] - N1_1944_tc_tbl[,c(3)]
N1_all_tch$c54to66 <- N1_1966_tc_tbl[,c(3)] - N1_1954_tc_tbl[,c(3)]
N1_all_tch$c66to75 <- N1_1975_tc_tbl[,c(3)] - N1_1966_tc_tbl[,c(3)]
N1_all_tch$c75to80 <- N1_1980_tc_tbl[,c(3)] - N1_1975_tc_tbl[,c(3)]
N1_all_tch$c80to85 <- N1_1985_tc_tbl[,c(3)] - N1_1980_tc_tbl[,c(3)]
N1_all_tch$c85to89 <- N1_1989_tc_tbl[,c(3)] - N1_1985_tc_tbl[,c(3)]
N1_all_tch$c89to95 <- N1_1995_tc_tbl[,c(3)] - N1_1989_tc_tbl[,c(3)]
N1_all_tch$c95to99 <- N1_1999_tc_tbl[,c(3)] - N1_1995_tc_tbl[,c(3)]
N1_all_tch$c99to04 <- N1_2004_tc_tbl[,c(3)] - N1_1999_tc_tbl[,c(3)]
N1_all_tch$c04to07 <- N1_2007_tc_tbl[,c(3)] - N1_2004_tc_tbl[,c(3)]
N1_all_tch$c07to12 <- N1_2012_tc_tbl[,c(3)] - N1_2007_tc_tbl[,c(3)]
N1_all_tch$c12to17 <- N1_2017_tc_tbl[,c(3)] - N1_2012_tc_tbl[,c(3)]

write.csv(N1_all_tch, file = "N1_treechange.csv")

