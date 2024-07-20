# Jaccard #

setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Mississauga 1944-Now Project/MTB/FInal Tree Files")

require(rgdal)
require(raster)
require(zonator)

master <- read.csv("tree_allyears_mtb.csv")

# UTM #
UTM <- readOGR(dsn = ".", layer = "UTM1")

# 1944 #
UTM_1944 = subset(master, Year == 1944 & Location == "UTM")

UTMr <- raster(UTM)
res(UTMr) <- 10
UTMr

UTMr <- rasterize(UTM, UTMr)
plot(UTMr)
quads <- as(UTMr, 'SpatialPolygons')
plot(quads, add = TRUE)
points(UTM_1944$POINT_X, UTM_1944$POINT_Y, pch = 19, cex = 0.1)

UTM_1944_t <- rasterize(UTM_1944[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1944_tr <- reclassify(UTM_1944_t, c(1, 10000, 1))

plot(UTM_1944_tr, main = "1944 Tree Binary")
plot(UTM, add = TRUE)

# 1954 #
UTM_1954 = subset(master, Year == 1954 & Location == "UTM")
UTM_1954_t <- rasterize(UTM_1954[,c(4,5)], UTMr, fun = 'count', background = 0)
UTM_1954_tr <- reclassify(UTM_1954_t, c(1, 10000, 1))

plot(UTM_1954_tr, main = "1954 Tree Binary")
plot(UTM, add = TRUE)

test <- jaccard(UTM_1944_tr, UTM_1954_tr)
test

we <- intersect(UTM_1944_tr, UTM_1954_tr)
plot(we)
