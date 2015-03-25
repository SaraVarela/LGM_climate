library (maptools)
setwd ("C:/Users/sara/Documents/_CIENCIAS/Climate_matheus") 

biomes<- readShapePoly ("tnc_terr_ecoregions.shp")
names (biomes)
X11()
plot (biomes)

## for calculating the differences between realms 

res<- list ()
a<- 0
for (x in unique (biomes$WWF_REALM2)){
a<- a +1
res1<- unlist (extract (map_bio [[1]], 
         biomes [biomes$WWF_REALM2 == x, ]))
res [[a]] <- res1
}

X11()
boxplot (res, axes=F, xlim=c(0.5, 9), ylim=c(0, 10), 
         ylab="BIO1: standard deviation", col="white", 
         border = "black", lwd=1)
axis (1, labels=unique (biomes$WWF_REALM2), at=c(1:8), lwd=2, cex=2)
axis (2, lwd=2, cex=2)

res2<- list ()
a<- 0
for (x in unique (biomes$WWF_REALM2)){
  a<- a +1
  res1<- unlist (extract (map_bio [[12]], 
                          biomes [biomes$WWF_REALM2 == x, ]))
  res2 [[a]] <- res1
}

str (res)
X11()
boxplot (res2, axes=F, xlim=c(0.5, 9), ylim=c(0,2000), 
         ylab="BIO12: standard deviation", col="white", 
         border = "black", lwd=1)
axis (1, labels=unique (biomes$WWF_REALM2), at=c(1:8), lwd=2, cex=2)
axis (2, lwd=2, cex=2)

kk<- extract (map_bio [[1]], coord)
kkk<- extract (map_bio [[12]], coord)

X11()
plot (coord [,2], kk, xlab="latitude", ylab="Bio1: standard deviation", 
      col="#60606010", pch=16)

X11()
plot (coord [,2], kkk, xlab="latitude", ylab="Bio12: standard deviation", 
      col="#60606010", pch=16)

library (raster)
library (rgdal)

mdt<- raster ("SRTM_1km.tif")
mdt2<- aggregate(mdt, fact=10, fun=mean)
mdt2
elevation<- resample (mdt2, map_bio [[1]], method='bilinear')
kkkk<- extract (elevation, coord)

X11()
plot (kkkk, kk, xlab="altitude", ylab="Bio1: standard deviation", 
      col="#60606010", pch=16)
X11()
plot (kkkk, kkk, xlab="altitude", ylab="Bio12: standard deviation", 
      col="#60606010", pch=16)

data<- data.frame (bio1=kk,bio12=kkk, elevation= kkkk, latitude= coord [,2] )
model1<- glm (bio1 ~  latitude^2 + latitude, data=data )
str (model1)
(model1$null.deviance - model1$deviance)/model1$null.deviance
summary (model1)

boxplot
