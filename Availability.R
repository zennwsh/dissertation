setwd("~/Documents/School/Dissertation/Workspace")

library(rgdal)

sg <- readOGR("./Datasets/master-plan-2014-planning-area-boundary-no-sea-shp")
sg <- spTransform(sg, CRS("+init=epsg:4326"))

# KDE - Hawker Centres -----------------------------------------------------------
hawker <- readOGR("./Datasets/hawker-centres-geojson.geojson")
hawker <- spTransform(hawker, CRS("+init=epsg:4326")) #transform CRS
hawker.coord <- data.frame(hawker@coords[,1:2]) #extract coordinates 

hawker.coord<-SpatialPointsDataFrame(hawker.coord,
                                     hawker.coord,
                                     proj4string=CRS("+init=epsg:4326"))

library(adehabitatHR)
hawker.output <- kernelUD(hawker.coord, h="href", grid = 1000)

# 75%, 50%, 25% and 10% 
range75 <- getverticeshr(hawker.output, percent = 75)
range50 <- getverticeshr(hawker.output, percent = 50)
range25 <- getverticeshr(hawker.output, percent = 25)
range10 <- getverticeshr(hawker.output, percent = 10)

library(tmap)
tmap_mode("plot")
fig1<- tm_shape(sg) + tm_fill(col="#f0f0f0") + tm_borders(col="grey", alpha=0.9) + 
  tm_shape(hawker.coord) + tm_dots(col="blue", size=0.3, alpha=0.4) +
  tm_shape(range75) + tm_borders(alpha=0.7, col="#ffa591", lwd = 2) + tm_fill(alpha=0.1,col="#ffa591") +
  tm_shape(range50) + tm_borders(alpha=0.7, col="#fb6a4a", lwd = 2) + tm_fill(alpha=0.1,col="#fb6a4a") +
  tm_shape(range25) + tm_borders(alpha=0.7, col="#de2d26", lwd = 2) + tm_fill(alpha=0.1,col="#de2d26") +
  tm_shape(range10) + tm_borders(alpha=0.7, col="#a50f15", lwd = 2) + tm_fill(alpha=0.1,col="#a50f15") +
  tm_layout(frame=F, 
            main.title="Density of hawker centres in Singapore ",
            main.title.size=1)
fig1
tmap_save(fig1, "KDE-hawker.png", width=7, height=5)


# KDE - Supermarkets -----------------------------------------------------------
supermarket <- readOGR("./Datasets/supermarkets-geojson.geojson")
supermarket <- spTransform(supermarket, CRS("+init=epsg:4326")) #transform CRS
supermarket.coord <- data.frame(supermarket@coords[,1:2]) #extract coordinates 

supermarket.coord<-SpatialPointsDataFrame(supermarket.coord,
                                          supermarket.coord,
                                     proj4string=CRS("+init=epsg:4326"))

supermarket.output <- kernelUD(supermarket.coord, h="href", grid = 1000)

# 75%, 50%, 25% and 10% 
s.range75 <- getverticeshr(supermarket.output, percent = 75)
s.range50 <- getverticeshr(supermarket.output, percent = 50)
s.range25 <- getverticeshr(supermarket.output, percent = 25)
s.range10 <- getverticeshr(supermarket.output, percent = 10)

tmap_mode("plot")
fig2<- tm_shape(sg) + tm_fill(col="#f0f0f0") + tm_borders(col="grey", alpha=0.9) + 
  tm_shape(supermarket.coord) + tm_dots(col="#488233", size=0.3, alpha=0.4) +
  tm_shape(s.range75) + tm_borders(alpha=0.7, col="#ffa591", lwd = 2) + tm_fill(alpha=0.1,col="#ffa591") +
  tm_shape(s.range50) + tm_borders(alpha=0.7, col="#fb6a4a", lwd = 2) + tm_fill(alpha=0.1,col="#fb6a4a") +
  tm_shape(s.range25) + tm_borders(alpha=0.7, col="#de2d26", lwd = 2) + tm_fill(alpha=0.1,col="#de2d26") +
  tm_shape(s.range10) + tm_borders(alpha=0.7, col="#a50f15", lwd = 2) + tm_fill(alpha=0.1,col="#a50f15") +
  tm_layout(frame=F, 
            main.title="Density of supermarkets in Singapore ",
            main.title.size=1)
fig2
tmap_save(fig2, "KDE-supermarket.png", width=7, height=5)

# KDE - Eating establishments  -----------------------------------------------------------
eating <- readOGR("./Datasets/eating-establishments-geojson.geojson")
eating <- spTransform(eating, CRS("+init=epsg:4326")) #transform CRS
eating.coord <- data.frame(eating@coords[,1:2]) #extract coordinates 

eating.coord<-SpatialPointsDataFrame(eating.coord,
                                     eating.coord,
                                     proj4string=CRS("+init=epsg:4326"))

eating.output <- kernelUD(eating.coord, h="href", grid = 1000)

# 75%, 50%, 25% and 10% 
e.range75 <- getverticeshr(eating.output, percent = 75)
e.range50 <- getverticeshr(eating.output, percent = 50)
e.range25 <- getverticeshr(eating.output, percent = 25)
e.range10 <- getverticeshr(eating.output, percent = 10)

fig3<- tm_shape(sg) + tm_fill(col="#f0f0f0") + tm_borders(col="grey", alpha=0.9) + 
  tm_shape(eating.coord) + tm_dots(col="yellow", size=0.0007, alpha=0.4) +
  tm_shape(e.range75) + tm_borders(alpha=0.7, col="#ffa591", lwd = 2) + tm_fill(alpha=0.1,col="#ffa591") +
  tm_shape(e.range50) + tm_borders(alpha=0.7, col="#fb6a4a", lwd = 2) + tm_fill(alpha=0.1,col="#fb6a4a") +
  tm_shape(e.range25) + tm_borders(alpha=0.7, col="#de2d26", lwd = 2) + tm_fill(alpha=0.1,col="#de2d26") +
  tm_shape(e.range10) + tm_borders(alpha=0.7, col="#a50f15", lwd = 2) + tm_fill(alpha=0.1,col="#a50f15") +
  tm_layout(frame=F, 
            main.title="Density of eating establishments in Singapore ",
            main.title.size=1)
fig3
tmap_save(fig3, "KDE-eating.png", width=7, height=5)
 
