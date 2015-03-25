## virtual species
library (raster)
library (rgdal)
library (maptools)
data(wrld_simpl)

coord<- data[, 2:3, 1]


max<- 20
min<- -10
mx<- 100
mn<- 0

bio1<- data[,"bio.1",]
bio12<- data[,"bio.12",]
bio1[bio1 < max & bio1>(min)] <- 1
bio1[bio1!=1]<- 0
colnames (bio1)<- model_names
bio12[bio12 < mx & bio12>(mn)] <- 1
bio12[bio12!=1]<- 0
colnames (bio12)<- model_names
map<- bio1*bio12
par (mar=c(0,0,0,0))
map_cold<- rasterFromXYZ (cbind (coord, map[,1]))
plot (map_cold, col=c("white", "#00000010"), 
      axes=F, box=F, legend=F)
for (i in 2:9){
map_cold2<- rasterFromXYZ (cbind (coord, map[,i])) 
plot (map_cold2, col=c("white", "#00000010"), add=T, 
      axes=F, box=F, legend=F)
}
library (maps)
plot (wrld_simpl, add=T)
plot (coast)

