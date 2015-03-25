citation(package = "abind", lib.loc = NULL)

### read the variables from the models of the LGM

library (abind)
setwd ("C:/clima_Sara/downscaling_matheus_0.5/21")
clima_21<- list.files ("C:/clima_Sara/downscaling_matheus_0.5/21")

data<- read.table (clima_21[1], head=T)
for (i in 2:length(clima_21)){
  data2<- read.table (clima_21[i], head=T)
  data<- abind (data, data2, along=3)
}
rm ("data2")

dim (data)

## plot variable
dim(data)

image<- rasterFromXYZ (cbind (coord, data [,4,1]))
str (image)
colores<- colorRampPalette (c("darkblue", "blue", "lightblue", 
                              "white", "salmon", "red"))
par (mar=c(0,0,0,0))
plot (image, axes=F, box=F, col=colores(100), legend=F)
plot (coast, add=T)

image<- rasterFromXYZ (cbind (coord, data [,15,1]))
str (image)
colores<- colorRampPalette (c("lightblue", "blue2", "blue3", 
                              "blue4", "black", "black"))
par (mar=c(0,0,0,0))
plot (image, axes=F, box=F, col=colores(100), legend=F)
plot (coast, add=T)


### sd of the variables

bio1<- data[,"bio.1",]
sd_bio<- apply(bio1, 1, sd, na.rm = TRUE)
for (i in 1:18){
  bio<- data [,4+i,]
  sd_bio2<- apply(bio, 1, sd, na.rm = TRUE)
  sd_bio<- cbind (sd_bio, sd_bio2)
}


kk<- shapiro.test (bio1 [1,])
no<- kk$p.value
for (i in 2:dim (bio1)[1]){
  kk<- shapiro.test (bio1 [i,])  
  kkk<- kk$p.value
  no<- c(no, kkk)
}

no

length (no [no< 0.01]) / length (no)
map_no<- rasterFromXYZ (cbind (coord, no))
nono<- reclassify (map_no, c(-Inf, 0.01, 0, 0.01, 1, 1))

X11()
plot (nono)
plot (coast, add=T)

bio12<- data[,"bio.12",]
kk<- shapiro.test (bio12 [1,])
no<- kk$p.value
for (i in 2:dim (bio12)[1]){
  kk<- shapiro.test (bio12 [i,])  
  kkk<- kk$p.value
  no<- c(no, kkk)
}

no
length (no [no< 0.01]) / length (no)
map_no_12<- rasterFromXYZ (cbind (coord, no))
nono_12<- reclassify (map_no_12, c(-Inf, 0.01, 0, 0.01, 1, 1))

plot (nono_12)
plot (coast, add=T)
rm ("sd_bio2")
rm ("bio")
rm ("bio1")

bio1<- data[,"bio.1",]
mean_bio<- apply(bio1, 1, quantile, na.rm = TRUE)
q3_bio<- mean_bio [4,]
q1_bio<- mean_bio [2,]
for (i in 1:18){
  bio<- data [,4+i,]
  mean_bio2<- apply(bio, 1, quantile, na.rm = TRUE)
  q3_bio2<- mean_bio2 [4,]
  q1_bio2<- mean_bio2 [2,]
  q3_bio<- cbind (q3_bio, q3_bio2)
  q1_bio<- cbind (q1_bio, q1_bio2)
}

qcd<- (q3_bio - q1_bio) /(q3_bio + q1_bio) 

head (qcd)

rm ("mean_bio2")
rm ("bio")
rm ("bio1")

head (mean_bio)

dif<- sd_bio/mean_bio

## map sd
library (raster)
library (rgdal)
library (maptools)
data(wrld_simpl)

coord<- data[, 2:3, 1]

map_bio<- rasterFromXYZ (cbind (coord, sd_bio[,1]))
for (i in 2:19){
  map_bio1<- rasterFromXYZ (cbind (coord, sd_bio[,i]))
  map_bio<- stack (map_bio, map_bio1)
}
rm ("map_bio1")

map_qcd<- rasterFromXYZ (cbind (coord, abs (qcd[,1])))
for (i in 2:19){
  map_qc<- rasterFromXYZ (cbind (coord, abs (qcd[,i])))
  map_qcd<- stack (map_qcd, map_qc)
}

rm ("map_qc")

X11()
plot (map_qcd [[2]], zlim=c(0, 2))

names (map_bio)<- c(paste ("BIO", c(1:19), sep=""))
plot (map_bio)
str (wrld_simpl)
colores<- colorRampPalette (c("white", "white",  "salmon", "red", "black"))

par (mar=c(0,0,0,0))
plot (continents, col="grey", border="NA")
plot (map_bio [[1]], axes=F, box=F, col=colores(100))
plot (wrld_simpl, add=T)
text(-130, -70, "Bio1", cex=2)
plot (map_bio [[12]], axes=F, box=F)
mtext("Bio12", side=3, line=-4)

## continents <- unionSpatialPolygons(wrld_simpl, rep(1, length(wrld_simpl)))
## plot (continents)
setwd ("C:\\Users\\sara\\Documents\\_CIENCIAS\\Climate_matheus")
coast<- readShapeSpatial ("ne_110m_coastline.shp")

par (mar=c(0,0,0,0))
plot (coast)

colores<- colorRampPalette (c("white", "white",  "salmon", "red", "black"))
colores2<- colorRampPalette (c("white",  "salmon", "red", "black"))
dev.off()
par (mfrow=c(5, 4), mar=c(0,0,0,4), las=2)
for (i in c(1:11)){
plot (map_bio [[i]], axes=F, box=F, col=colores(100))
plot (coast, add=T)
#text(-130, -50, paste ("Bio", i, sep=""), cex=1.8)
}
for (i in c(12:14)){
  plot (map_bio [[i]], axes=F, box=F, col=colores2(100))
  plot (coast, add=T)
 # text(-130, -50, paste ("Bio", i, sep=""), cex=1.8)
}
plot (map_bio [[15]], axes=F, box=F,zlim=c(0,100), col=colores2(100))
plot (coast, add=T)
# text(-130, -50, paste ("Bio", 15, sep=""), cex=1.8)
for (i in c(16:19)){
  plot (map_bio [[i]], axes=F, box=F, col=colores2(100))
  plot (coast, add=T)
 # text(-130, -50, paste ("Bio", i, sep=""), cex=1.8)
}


plot (wrld_simpl, add=T, 
      border="grey10")
mtext("standard deviation",
      side = 2, line = -49,
      outer = TRUE)
plot (map_bio12, axes=F, box=F)


### identifying areas of high heterogeneity between models using the quartile coeff


precip_stab<- reclassify (map_qcd[[12]], 
                          c(-Inf, 0.5, 1, 0.5, +Inf, 0))   
for (i in 13:19){
  precip_stab1<- reclassify (map_qcd[[i]], 
  c(-Inf, 0.5, 1, 0.5, +Inf, 0))                               
  precip_stab<- precip_stab + precip_stab1
}


precip_stab<- reclassify (map_bio[[12]], 
                          c(-Inf, 100, 1, 100, +Inf, 0))  
for (i in 13:19){
  precip_stab1<- reclassify (map_bio[[i]], 
                            c(-Inf, 100, 1, 100, +Inf, 0))  
  precip_stab<- precip_stab + precip_stab1
}

dev.off()
X11()
par (mar=c(0,0,0,2))
colores2<- colorRampPalette (c( "white", "red", "darkred"))
plot (precip_stab, axes=F, box=F, col=colores2(100))
plot (coast, add=T)


temp_stab<- reclassify (map_qcd [[1]], 
c(-Inf, 0.5, 1, 0.5, +Inf, 0)) 
for (i in 2:11){
  temp_stab1<- reclassify (map_qcd [[i]], 
  c(-Inf, 0.5, 1, 0.5, +Inf, 0)) 
  temp_stab<- temp_stab + temp_stab1
}

stab<- temp_stab + precip_stab
st<- reclassify (stab, c(-Inf, 18, 0, 18, 20, 1))
X11()
plot (st, axes=F, box=F,col=c("white", "red"), legend=F)
plot (coast, add=T)

X11()
par (mar=c(0,0,0,2))
plot (temp_stab, axes=F, box=F, col=colores2(100))
plot (coast, add=T)


## absolute change, using thresholds
temp_stab<- reclassify (map_bio[[1]], c(-Inf, 5, 1, 5, +Inf, 0))
for (i in 2:11){
  temp_stab1<- reclassify (map_bio[[i]], 
                           c(-Inf, 5, 1, 5, +Inf, 0))
  temp_stab<- temp_stab + temp_stab1
}

temp_stab1<- reclassify (map_bio[[4]], 
                         c(-Inf, 500, 1, 500, +Inf, 0))
temp_stab<- temp_stab + temp_stab1

par (mar=c(0,0,0,0))
colores3<- colorRampPalette (c("black", "red", "salmon", "white"))
plot (temp_stab, axes=F, box=F, col=colores3(100))
plot (coast, add=T)

plot (precip_stab, axes=F, box=F, col=colores3(100))
plot (coast, add=T)


stab<- temp_stab + precip_stab
plot (stab)
plot (wrld_simpl, add=T)

ts<- reclassify (temp_stab, c(-Inf, 5, 0, 5, +Inf, 1))
ps<- reclassify (precip_stab, c(-Inf, 5, 0, 5, +Inf, 1))

stab2<- ts * ps
par (mar=c(0,0,0,0))
plot (stab2, axes=F, box=F, legend=F, col=c("red", "white"))
plot (coast, add=T)

### correlation between predictions and Hierarchical cluster analysis
library (amap)
model_names<- substr(clima_21, 9, nchar (clima_21)-15) 
hc<- list ()
for (i in 1:19){
raw_layer<-  t(data[,3+i,])
rownames (raw_layer)<- model_names 
cor_bio<- hcluster (raw_layer, method="correlation")
plot(cor_bio)
hc[[i]]<- cor_bio
}

head (cor_bio
names (hc)<- c(paste ("BIO", c(1:19), sep=""))
hc
par (las=1)
for (i in 1:19){
  plot (hc[[i]]) 
  mtext (names(hc)[i], side= 1, line=2)
}

### euclidean clusters
library (stats)
model_names<- substr(clima_21, 9, nchar (clima_21)-15) 
hc2<- list ()
for (i in 1:19){
  raw_layer<-  t(data[,3+i,])
  rownames (raw_layer)<- model_names 
  cor_bio<- hcluster (raw_layer, method="euclidean")
  hc2[[i]]<- cor_bio
}

names (hc2)<- c(paste ("BIO", c(1:19), sep=""))
hc
par (las=1)
for (i in 1:19){
  plot (hc2[[i]]) 
  mtext (names(hc2)[i], side= 1, line=2)
}
dev.off()
plot (hc[[4]])

res_groups2<- NULL
for (i in 1:19){
  res2<- cutree(hc[[i]], k=4) 
  res_groups2<- rbind (res_groups2, res2)
}

t(res_groups2)


rownames (res_groups2)<- c(paste ("BIO", c(1:19), sep="")) 
res_groups2

clust_categ<- hcluster (t(res_groups2), method="euclidean")
dev.off()
plot (clust_categ)

res_groups<- NULL
for (i in 1:19){
  res<- cutree(hc[[i]], h=0.2) 
  res_groups<- rbind (res_groups, res)
}

rownames (res_groups)<- c(paste ("BIO", c(1:19), sep="")) 

res_groups
### read correlation between models to contruct clusters with the GCMs
library (tree)
library (rpart)

rpart(formula, data=, method=,control=) where
?rpart

rpart(. ~ Age + Number + Start, data = kyphosis)


groups<- res_groups [rowSums (res_groups)>9, ]
clust_categ<- hcluster (t(groups), method="euclidean")
plot (clust_categ)



## correlation: Variables
cor_bio<- cor (data[,3+1,])
res_cor<- cor_bio[lower.tri(cor_bio, diag = FALSE)]
for (i in 2:19){
  cor_bio1<- cor (data[,3+i,])
  res_cor1<- cor_bio1[lower.tri(cor_bio1, diag = FALSE)] 
  res_cor<- data.frame (res_cor, res_cor1)
}

dim (res_cor)

names (res_cor)<- c(paste ("BIO", c(1:19), sep=""))
dev.off()
par (las=2)
boxplot (res_cor, axes=T, col="grey30",
         border="grey50",
         frame=F, ylab="Correlation")

unlist (colMeans (res_cor))


## correlation models
model_names<- substr(clima_21, 9, nchar (clima_21)-15) 

cor_mod<- cor (data[,3+1,])
colnames (cor_mod)<- model_names
diag(cor_mod)<-NA
for (i in 2:11){
  cor_mod1<- cor (data[,3+i,])
  diag(cor_mod1)<-NA
  cor_mod<- rbind (cor_mod, cor_mod1)
}

head (cor_mod)

temperature<- colMeans (cor_mod, na.rm=T)

cor_mod2<- cor (data[,3+12,])
colnames (cor_mod2)<- model_names
diag(cor_mod2)<-NA
for (i in 13:19){
  cor_mod1<- cor (data[,3+i,])
  diag(cor_mod1)<-NA
  cor_mod2<- rbind (cor_mod2, cor_mod1)
}

names (data[, 21, ])

dim (cor_mod)
dim (cor_mod2)
correlation<- rbind (cor_mod, cor_mod2)

precipitation<- colMeans (cor_mod, na.rm=T)

par (las=2)
boxplot (cor_mod, axes=T, col="firebrick",
         boxwex = 0.25, at = 1:9 - 0.2,
         border="lightcoral",
         frame=F, ylab="Correlation")

boxplot (cor_mod2, axes=T, col="blue", add=T,
         border="lightblue",
         boxwex = 0.25, at = 1:9 + 0.2,
         frame=F)


rbind (temperature, precipitation)



cor_bio<- cor (data[,3+12,])
cor_bio[upper.tri(cor_bio, diag = TRUE)] <- NA
colnames (cor_bio)<- model_names
rownames (cor_bio)<- model_names


