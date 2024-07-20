setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga 1944-Now Project/MTB/Tree Corrections")

require(rgdal)
require(raster)

### Master Data ###
master <- read.csv("trees_all.csv")
I2_drivers <- read.csv("I2_drivers.csv")
#master_shp <- readOGR(dsn = ".", layer = "trees_allyears_mtb")
#proj4string(master_shp)

#plot(master_shp)

### Neighborhood 1 ###

I2 <- readOGR(dsn = ".", layer = "Industrial_2")
#proj4string(UTM) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#proj4string(UTM)

# 1944 #
I2_1944 = subset(master, Year == 1944 & Location == "Industrial 2")

#UTM_1944_shp = subset(master_shp, Year == 1944 & Location == "UTM")
#proj4string(UTM_1944_shp)

plot(I2)
#plot(UTM_1944_shp, pch = 19, cex = 0.1)
points(I2_1944$POINT_X, I2_1944$POINT_Y, pch = 19, cex = 0.1)

I2r <- raster(I2)
res(I2r) <- 30
I2r

I2r <- rasterize(I2, I2r)
plot(I2r)
quads <- as(I2r, 'SpatialPolygons')
plot(quads, add = TRUE)
points(I2_1944$POINT_X, I2_1944$POINT_Y, pch = 19, cex = 0.1)

#quads1_I2 <- as(quads, "SpatialPolygonsDataFrame")
#writeOGR(quads1_I2, dsn = '.', layer = 'quads_I2', driver = "ESRI Shapefile")

I2_1944_tc <- rasterize(I2_1944[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1944_tc <- mask(I2_1944_tc, I2)
plot(I2_1944_tc, main = "1944 Tree Density")
plot(I2, add = TRUE)

# 1954 #
I2_1954 = subset(master, Year == 1954 & Location == "Industrial 2")
I2_1954_tc <- rasterize(I2_1954[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1954_tc <- mask(I2_1954_tc, I2)
plot(I2_1954_tc, main = "1954 Tree Density")
plot(I2, add = TRUE)

# 1966 #
I2_1966 = subset(master, Year == 1966 & Location == "Industrial 2")
I2_1966_tc <- rasterize(I2_1966[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1966_tc <- mask(I2_1966_tc, I2)
plot(I2_1966_tc, main = "1966 Tree Density")
plot(I2, add = TRUE)

# 1975 #
I2_1975 = subset(master, Year == 1975 & Location == "Industrial 2")
I2_1975_tc <- rasterize(I2_1975[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1975_tc <- mask(I2_1975_tc, I2)
plot(I2_1975_tc, main = "1975 Tree Density")
plot(I2, add = TRUE)

# 1980 #
I2_1980 = subset(master, Year == 1980 & Location == "Industrial 2")
I2_1980_tc <- rasterize(I2_1980[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1980_tc <- mask(I2_1980_tc, I2)
plot(I2_1980_tc, main = "1980 Tree Density")
plot(I2, add = TRUE)

# 1985 #
I2_1985 = subset(master, Year == 1985 & Location == "Industrial 2")
I2_1985_tc <- rasterize(I2_1985[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1985_tc <- mask(I2_1985_tc, I2)
plot(I2_1985_tc, main = "1985 Tree Density")
plot(I2, add = TRUE)

# 1989 #
I2_1989 = subset(master, Year == 1989 & Location == "Industrial 2")
I2_1989_tc <- rasterize(I2_1989[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1989_tc <- mask(I2_1989_tc, I2)
plot(I2_1989_tc, main = "1989 Tree Density")
plot(I2, add = TRUE)

# 1995 #
I2_1995 = subset(master, Year == 1995 & Location == "Industrial 2")
I2_1995_tc <- rasterize(I2_1995[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1995_tc <- mask(I2_1995_tc, I2)
plot(I2_1995_tc, main = "1995 Tree Density")
plot(I2, add = TRUE)

# 1999 #
I2_1999 = subset(master, Year == 1999 & Location == "Industrial 2")
I2_1999_tc <- rasterize(I2_1999[,c(4,5)], I2r, fun = 'count', background = 0)
I2_1999_tc <- mask(I2_1999_tc, I2)
plot(I2_1999_tc, main = "1999 Tree Density")
plot(I2, add = TRUE)

# 2004 #
I2_2004 = subset(master, Year == 2004 & Location == "Industrial 2")
I2_2004_tc <- rasterize(I2_2004[,c(4,5)], I2r, fun = 'count', background = 0)
I2_2004_tc <- mask(I2_2004_tc, I2)
plot(I2_2004_tc, main = "2004 Tree Density")
plot(I2, add = TRUE)

# 2007 #
I2_2007 = subset(master, Year == 2007 & Location == "Industrial 2")
I2_2007_tc <- rasterize(I2_2007[,c(4,5)], I2r, fun = 'count', background = 0)
I2_2007_tc <- mask(I2_2007_tc, I2)
plot(I2_2007_tc, main = "2007 Tree Density")
plot(I2, add = TRUE)

# 2012 #
I2_2012 = subset(master, Year == 2012 & Location == "Industrial 2")
I2_2012_tc <- rasterize(I2_2012[,c(4,5)], I2r, fun = 'count', background = 0)
I2_2012_tc <- mask(I2_2012_tc, I2)
plot(I2_2012_tc, main = "2012 Tree Density")
plot(I2, add = TRUE)

# 2017 #
I2_2017 = subset(master, Year == 2017 & Location == "Industrial 2")
I2_2017_tc <- rasterize(I2_2017[,c(4,5)], I2r, fun = 'count', background = 0)
I2_2017_tc <- mask(I2_2017_tc, I2)
plot(I2_2017_tc, main = "2017 Tree Density")
plot(I2, add = TRUE)

### Analysis ###

# Create Raster Stack #
I2_stack <- stack(I2_1944_tc, I2_1954_tc, I2_1966_tc, I2_1975_tc, I2_1980_tc, I2_1985_tc, I2_1989_tc,
                  I2_1995_tc, I2_1999_tc, I2_2004_tc, I2_2007_tc, I2_2012_tc, I2_2017_tc)
I2_stack
plot(I2_stack)

years <- c(1944, 1954, 1966, 1975, 1980, 1985, 1989, 1995, 1999, 2004, 2007, 2012, 2017)


# Calculate Slope #
fun_slope <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[2]
  }
}

slope <- calc(I2_stack, fun_slope)
slope_m <- mask(slope, I2)

plot(slope_m, main = "Tree Density Slope")
plot(I2, add = TRUE)

# Calculate Pvalue #
fun_pval <- function(y) {
  if(sum(is.na(y)) > 6) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[8]
  }
}

pval <- calc(I2_stack, fun_pval)
pval_m <- mask(pval, I2)

plot(pval_m, main = "Tree Density Significance")
plot(I2, add = TRUE)

# Save Rasters #
writeRaster(slope_m, "I2_44to17_Slope.tif", "GTiff", overwrite = TRUE)
writeRaster(pval_m, "I2_44to17_Pvalue.tif", "GTiff", overwrite = TRUE)

### Build Tree Count Tables ###
I2_1944_tc_tbl <- as.data.frame(I2_1944_tc,xy = TRUE)
I2_1944_tc_tbl <- I2_1944_tc_tbl[complete.cases(I2_1944_tc_tbl),]
I2_1954_tc_tbl <- as.data.frame(I2_1954_tc,xy = TRUE)
I2_1954_tc_tbl <- I2_1954_tc_tbl[complete.cases(I2_1954_tc_tbl),]
I2_1966_tc_tbl <- as.data.frame(I2_1966_tc,xy = TRUE)
I2_1966_tc_tbl <- I2_1966_tc_tbl[complete.cases(I2_1966_tc_tbl),]
I2_1975_tc_tbl <- as.data.frame(I2_1975_tc,xy = TRUE)
I2_1975_tc_tbl <- I2_1975_tc_tbl[complete.cases(I2_1975_tc_tbl),]
I2_1980_tc_tbl <- as.data.frame(I2_1980_tc,xy = TRUE)
I2_1980_tc_tbl <- I2_1980_tc_tbl[complete.cases(I2_1980_tc_tbl),]
I2_1985_tc_tbl <- as.data.frame(I2_1985_tc,xy = TRUE)
I2_1985_tc_tbl <- I2_1985_tc_tbl[complete.cases(I2_1985_tc_tbl),]
I2_1989_tc_tbl <- as.data.frame(I2_1989_tc,xy = TRUE)
I2_1989_tc_tbl <- I2_1989_tc_tbl[complete.cases(I2_1989_tc_tbl),]
I2_1995_tc_tbl <- as.data.frame(I2_1995_tc,xy = TRUE)
I2_1995_tc_tbl <- I2_1995_tc_tbl[complete.cases(I2_1995_tc_tbl),]
I2_1999_tc_tbl <- as.data.frame(I2_1999_tc,xy = TRUE)
I2_1999_tc_tbl <- I2_1999_tc_tbl[complete.cases(I2_1999_tc_tbl),]
I2_2004_tc_tbl <- as.data.frame(I2_2004_tc,xy = TRUE)
I2_2004_tc_tbl <- I2_2004_tc_tbl[complete.cases(I2_2004_tc_tbl),]
I2_2007_tc_tbl <- as.data.frame(I2_2007_tc,xy = TRUE)
I2_2007_tc_tbl <- I2_2007_tc_tbl[complete.cases(I2_2007_tc_tbl),]
I2_2012_tc_tbl <- as.data.frame(I2_2012_tc,xy = TRUE)
I2_2012_tc_tbl <- I2_2012_tc_tbl[complete.cases(I2_2012_tc_tbl),]
I2_2017_tc_tbl <- as.data.frame(I2_2017_tc,xy = TRUE)
I2_2017_tc_tbl <- I2_2017_tc_tbl[complete.cases(I2_2017_tc_tbl),]

# Merge Tables #
I2_all_tc <- data.frame(X = numeric(nrow(I2_drivers)), Y = numeric(nrow(I2_drivers)), 
                        tc1944 = numeric(nrow(I2_drivers)), tc1954 = numeric(nrow(I2_drivers)), tc1966 = numeric(nrow(I2_drivers)),
                        tc1975 = numeric(nrow(I2_drivers)), tc1980 = numeric(nrow(I2_drivers)), tc1985 = numeric(nrow(I2_drivers)), 
                        tc1989 = numeric(nrow(I2_drivers)), tc1995 = numeric(nrow(I2_drivers)), tc1999 = numeric(nrow(I2_drivers)), 
                        tc2004 = numeric(nrow(I2_drivers)), tc2007 = numeric(nrow(I2_drivers)), tc2012 = numeric(nrow(I2_drivers)), 
                        tc2017 = numeric(nrow(I2_drivers)))

I2_all_tc$X <-  I2_1944_tc_tbl[,c(1)]
I2_all_tc$Y <-  I2_1944_tc_tbl[,c(2)]
I2_all_tc$tc1944 <- I2_1944_tc_tbl[,c(3)]
I2_all_tc$tc1954 <- I2_1954_tc_tbl[,c(3)]
I2_all_tc$tc1966 <- I2_1966_tc_tbl[,c(3)]
I2_all_tc$tc1975 <- I2_1975_tc_tbl[,c(3)]
I2_all_tc$tc1980 <- I2_1980_tc_tbl[,c(3)]
I2_all_tc$tc1985 <- I2_1985_tc_tbl[,c(3)]
I2_all_tc$tc1989 <- I2_1989_tc_tbl[,c(3)]
I2_all_tc$tc1995 <- I2_1995_tc_tbl[,c(3)]
I2_all_tc$tc1999 <- I2_1999_tc_tbl[,c(3)]
I2_all_tc$tc2004 <- I2_2004_tc_tbl[,c(3)]
I2_all_tc$tc2007 <- I2_2007_tc_tbl[,c(3)]
I2_all_tc$tc2012 <- I2_2012_tc_tbl[,c(3)]
I2_all_tc$tc2017 <- I2_2017_tc_tbl[,c(3)]

write.csv(I2_all_tc, file = "I2_treecount.csv")

### Build Tree Change Tables ###
I2_all_tch <- I2_1944_tc_tbl[,c(1,2)]
I2_all_tch$c44to54 <- I2_1954_tc_tbl[,c(3)] - I2_1944_tc_tbl[,c(3)]
I2_all_tch$c54to66 <- I2_1966_tc_tbl[,c(3)] - I2_1954_tc_tbl[,c(3)]
I2_all_tch$c66to75 <- I2_1975_tc_tbl[,c(3)] - I2_1966_tc_tbl[,c(3)]
I2_all_tch$c75to80 <- I2_1980_tc_tbl[,c(3)] - I2_1975_tc_tbl[,c(3)]
I2_all_tch$c80to85 <- I2_1985_tc_tbl[,c(3)] - I2_1980_tc_tbl[,c(3)]
I2_all_tch$c85to89 <- I2_1989_tc_tbl[,c(3)] - I2_1985_tc_tbl[,c(3)]
I2_all_tch$c89to95 <- I2_1995_tc_tbl[,c(3)] - I2_1989_tc_tbl[,c(3)]
I2_all_tch$c95to99 <- I2_1999_tc_tbl[,c(3)] - I2_1995_tc_tbl[,c(3)]
I2_all_tch$c99to04 <- I2_2004_tc_tbl[,c(3)] - I2_1999_tc_tbl[,c(3)]
I2_all_tch$c04to07 <- I2_2007_tc_tbl[,c(3)] - I2_2004_tc_tbl[,c(3)]
I2_all_tch$c07to12 <- I2_2012_tc_tbl[,c(3)] - I2_2007_tc_tbl[,c(3)]
I2_all_tch$c12to17 <- I2_2017_tc_tbl[,c(3)] - I2_2012_tc_tbl[,c(3)]

write.csv(I2_all_tch, file = "I2_treechange.csv")


#I2_w <- as(I2, "owin")
#I2_1999_pp <-as.ppp(I2_1999[,c(4,5)], I2_w)
#I2_2004_pp <-as.ppp(I2_2004[,c(4,5)], I2_w)

#plot(I2_1999_pp)
#plot(I2_2004_pp)

#ktest2 <- Kest(I2_1999_pp)
#plot(ktest2)

#ktest3 <- Kest(I2_2004_pp)
#plot(ktest3)

