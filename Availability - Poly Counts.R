setwd("~/Documents/School/Workspace")
library(maptools)
library(spatstat)
library(sp)
library(rgeos)
library(rgdal)
library(GISTools)
library(dplyr)

# 1. Normalise eating establishments by population ---- 
eating <- readOGR("./Datasets/eating-establishments-geojson.geojson")
eating <- spTransform(eating, CRS("+init=epsg:4326")) #transform CRS
sg.PA <- readOGR("./Datasets/sg-by-planning-area-2014", "MP14_PLNG_AREA_WEB_PL_fix")
sg.PA <- spTransform(sg.PA, CRS("+init=epsg:4326"))
x<-read.csv("Datasets/respopagesextod2011to2020.csv")
pop.by.PA<-aggregate(x$Pop, by=list(x$PA), FUN=sum)
rm(x)
pop.by.PA <- as.data.frame(apply(pop.by.PA,2,toupper))
PAs <- read.csv("PAs.csv")
PAs <- toupper(PAs$Planning.Area)
pop.by.PA <- subset(pop.by.PA, Group.1 %in% PAs)

y <- poly.counts(eating, sg.PA)
y <- setNames(y, sg.PA$PLN_AREA_N)
library(data.table)
y<-setDT(as.data.frame(y), keep.rownames = TRUE)[]
ee.by.PA.pop <- merge(y, pop.by.PA, by.x="rn", by.y="Group.1")
ee.by.PA.pop$x<-as.numeric(as.character(ee.by.PA.pop$x))
ee.by.PA.pop$normalised_2 <- ee.by.PA.pop$y/ee.by.PA.pop$x*100000
ee.by.PA.pop$normalised_2[sapply(ee.by.PA.pop$normalised_2, is.infinite)] <- NA

ee.by.PA.pop <- merge(sg.PA, ee.by.PA.pop, by.x="PLN_AREA_N", by.y="rn")

library(tmaptools)
library(tmap)
# breaks=c(0, 5, 10, 20, 60, 200)
# breaks=c(1, 5, 10, 20, 50, 200)
tm_shape(ee.by.PA.pop) + tm_fill("normalised_2", palette="Oranges", 
                                 breaks=c(0, 50, 100, 150, 500),
                                 title="Eateries per 100,000",
                                 colorNA="#f0f0f0",
                                 showNA=F) + 
  tm_borders(alpha=0.4) + 
  tm_layout(main.title = "(b) Eateries in Singapore by population",
            main.title.size = 1.1, frame=F, 
            legend.title.size = 1) +  tm_compass(position="left", type="arrow") +
  tm_scale_bar(position="center")

tmap_save(filename="Eating-establishments-normalised-2020.png", width=7, height=5)

# 32, 29, 40 and 25 are outliers 
plot(as.numeric(ee.by.PA.pop$normalised_2))
plot(as.numeric(ee.by.PA.pop$normalised_2[-c(29)]))
summary(ee.by.PA.pop$normalised_2)

# 1.1 Moran's I -----
library(spdep)
nb <- poly2nb(ee.by.PA.pop)
plot(ee.by.PA.pop, border = 'lightgrey')
plot(nb, coordinates(ee.by.PA.pop), add=TRUE, col='red')

listw <- nb2listw(nb, zero.policy=T)
moran.test(ee.by.PA.pop$y, listw, zero.policy = T, na.action=na.pass)
# check zero.policy= and na.action= arguments

# 2. Normalise hawker centres by population ----------  
hawker <- readOGR("./Datasets/hawker-centres-geojson.geojson")
hawker <- spTransform(hawker, CRS("+init=epsg:4326")) #transform CRS
z <- poly.counts(hawker, sg.PA)
z <- setNames(z, sg.PA$PLN_AREA_N)
z<-setDT(as.data.frame(z), keep.rownames = TRUE)[]
hc.by.PA.pop <- merge(z, pop.by.PA, by.x="rn", by.y="Group.1")
hc.by.PA.pop$x<-as.numeric(as.character(hc.by.PA.pop$x))
hc.by.PA.pop$normalised <- hc.by.PA.pop$z/hc.by.PA.pop$x*100000
hc.by.PA.pop$normalised[sapply(hc.by.PA.pop$normalised, is.infinite)] <- NA

hc.by.PA.pop <- merge(sg.PA, hc.by.PA.pop, by.x="PLN_AREA_N", by.y="rn")

very.blue <- c("#ACD0E6", "#73B2D7", "#4393C6", "#1F6EB3", "#084A92")
tm_shape(hc.by.PA.pop) + tm_fill("normalised", palette=very.blue,
                                 breaks=c(0, 0.25, 0.5, 0.75, 1, 3),
                                 title="Hawker centres per 100,000",
                                 colorNA="#f0f0f0",
                                 showNA=F) + 
  tm_borders(alpha=0.4) + 
  tm_layout(main.title.size = 0.9, frame=F,
            main.title = "(b) Hawker centres in Singapore by population") + tm_compass(position="left", type="arrow") +
  tm_scale_bar(position="center")

tmap_save(filename="Hawker-centres-normalised-2015.png", width=7, height=5)

# 2.1 Moran's I ----------  
nb2 <- poly2nb(hc.by.PA.pop)
plot(hc.by.PA.pop, border = 'lightgrey')
plot(nb2, coordinates(hc.by.PA.pop), add=TRUE, col='red')

listw <- nb2listw(nb2, zero.policy=T)
moran.test(hc.by.PA.pop$z, listw, zero.policy = T, na.action=na.pass)

# 3. Normalise supermarkets by population ----------  
sm <- readOGR("./Datasets/supermarkets-geojson.geojson")
sm <- spTransform(sm, CRS("+init=epsg:4326")) #transform CRS
a <- poly.counts(sm, sg.PA)
a <- setNames(a, sg.PA$PLN_AREA_N)
a <-setDT(as.data.frame(a), keep.rownames = TRUE)[]
sm.by.PA.pop <- merge(a, pop.by.PA, by.x="rn", by.y="Group.1")
sm.by.PA.pop$x<-as.numeric(as.character(sm.by.PA.pop$x))
sm.by.PA.pop$normalised <- sm.by.PA.pop$a/sm.by.PA.pop$x*100000
sm.by.PA.pop$normalised[sapply(sm.by.PA.pop$normalised, is.infinite)] <- NA

sm.by.PA.pop <- merge(sg.PA, sm.by.PA.pop, by.x="PLN_AREA_N", by.y="rn")

tm_shape(sm.by.PA.pop) + tm_fill("normalised", palette="YlGn",
                                 breaks=c(0, 1, 2, 3, 4, 5, 6),
                                 colorNA="#f0f0f0",
                                 showNA=F,
                                 title="Supermarkets per 
100,000 residents") + 
  tm_borders(alpha=0.4) + 
  tm_layout(main.title.size = 1.1,frame=F,
            main.title = "(b) Supermarkets in Singapore by population",
            legend.text.size=0.8) +
  tm_compass(position="left", type="arrow") +
  tm_scale_bar(position="center")

tmap_save(filename="Supermarkets-normalised-2015.png", width=7, height=5)

plot(as.numeric(sm.by.PA.pop$normalised))
summary(sm.by.PA.pop$normalised)

# 3.1 Moran's I ----------  
nb3 <- poly2nb(sm.by.PA.pop)
plot(sm.by.PA.pop, border = 'lightgrey')
plot(nb3, coordinates(sm.by.PA.pop), add=TRUE, col='red')

listw <- nb2listw(nb3, zero.policy=T)
moran.test(sm.by.PA.pop$a, listw, zero.policy = T, na.action=na.pass)

# Availability -------
availability <- hc.by.PA.pop@data[c(1, 15)]
availability$sm <- sm.by.PA.pop@data[c(15)]
availability$ee <- ee.by.PA.pop@data[c(15)]
availability <- as.data.frame(as.matrix(availability))
colnames(availability) <- c("PA", "hc", "sm", "ee")
availability <- na.omit(availability)
availability[,2:4] <- lapply(availability[,2:4], as.numeric)
availability[c(2:4)] <- round(availability[c(2:4)],3)
write.csv(availability, file="AVAIL.csv")

rm(availability)

View(pop.by.PA)

no.PAs <- setNames(as.data.frame(matrix(nrow=11, ncol=2)), c("no.PAs", "variable"))
for (i in c(1:9)){
  no.PAs[i,1] <- n_distinct(df[[i]][1])
  no.PAs[i,2] <- colnames(df[[i]][2])
}

no.PAs[10,1] <- n_distinct(by.econ.1[1])
no.PAs[10,2] <- colnames(by.econ.1[2])

no.PAs[11,1] <- n_distinct(by.elderly[1])
no.PAs[11,2] <- colnames(by.elderly[2])
