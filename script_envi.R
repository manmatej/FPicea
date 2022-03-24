library(openxlsx)
library(dplyr)
## load points
fpicea<-read.xlsx("Lokality_F.picea_CMV_mm.xlsx")
# convert coordinates DMS to Decimal
colnames(fpicea)[3]<-"lat"
colnames(fpicea)[4]<-"lon"
substring(fpicea$lat, 3, 3) <- "d"
substring(fpicea$lat, 6, 6) <- "m"
substring(fpicea$lon, 3, 3) <- "d"
substring(fpicea$lon, 6, 6) <- "m"
fpicea<-fpicea[!is.na(fpicea$lat),]
fpicea<-fpicea[!is.na(fpicea$lon),]
fpicea$y<- sp::char2dms(fpicea$lat,chd="d",chm="m",chs="\"")%>%
  as.numeric()
fpicea$x<- sp::char2dms(fpicea$lon,chd="d",chm="m",chs="\"")%>%
  as.numeric()

# data.frame to sf
library(sf)
fpicea<-st_as_sf(fpicea,coords = c("x", "y"),crs=4326)
plot(st_geometry(fpicea))
st_write(fpicea,"fpicea.gpkg",append=FALSE )


## Clip CZG to study extent in parallel
library(terra)
library(sf)
library(dplyr)
library(doParallel) 
library(foreach)

czg.path<-"y://CR/CZECH_GRIDS_095/"
czg.nam<-list.files(czg.path,pattern = "*.tif$",full.names = T)
nam<-list.files(czg.path,pattern = "*.tif$",full.names = F)
czg.names<-substr(nam,1,nchar(nam)-4)
czg.dat<-terra::rast(czg.nam)
ex<-st_read("extent.gpkg")
ex.tr<-st_transform(ex,st_crs(czg.dat))
v<-vect(ex.tr)
# dir.create("q://s_slozky_osobni/Man/clanek_2022_Fpicea_big")
ph<-"q://s_slozky_osobni/Man/clanek_2022_Fpicea_big/"


# detects cores and use all -1
UseCores <- detectCores() -2
#Register CoreCluster
cl<- makeCluster(UseCores)
registerDoParallel(cl)

foreach(i=c(1:length(czg.nam))) %dopar% {
  library(terra)
  library(dplyr)
  library(sf)
  ex<-st_read("extent.gpkg")
  ex.tr<-st_transform(ex,32633)
  v<-vect(ex.tr)
  r<-rast(czg.nam[i])
  czg.ji<-terra::crop(r,v)%>%
    terra::mask(v,filename=paste0(ph,nam[i]),
                overwrite=T)
}

#end cluster
stopCluster(cl)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# selected rasters moved to "d:\Fpicea_rasters\"
# from 
# "q://s_slozky_osobni/Man/clanek_2022_Fpicea_big/"
# and "q://s_slozky_osobni/Man/clanek_2022_Fpicea_big/clusters"
# k-mean clustering performed in SAGA 7.9.0 pcman
# try 4 and 5 classes
# six rasters input to clustering
#
# c("PotentialTotalSolarRadiat094",
#   "elevation_10m094",
#   "TopographicPositionIndex50-500m094",
#   "Q005AirTemp_f14t19",                 
#   "Q095AirTemp_f14t19",
#   "MeanPrecipitation_f14t19")
# TopographicPositionIndex50-500m094.tif
# renamed manually to
# TopographicPositionIndex50_500m094.tif

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


## CZG extract
library(terra)
czg.path<-"d:/Fpicea_rasters/"
czg.nam<-list.files(czg.path,pattern = "*.tif$",full.names = T)
nam<-list.files(czg.path,pattern = "*.tif$",full.names = F)
czg.names<-substr(nam,1,nchar(nam)-4)
r<-rast(czg.nam)
names(r)<-czg.names

sel<-c("PotentialTotalSolarRadiat094",
       "elevation_10m094",
       "TopographicPositionIndex50_500m094",
       "Q005AirTemp_f14t19",                 
       "Q095AirTemp_f14t19",
       "MeanPrecipitation_f14t19",
       "CanopyHeightDMP_DMR094",
       "Clusters4class",                    
       "Clusters5class",
       "BiotopType094")

names(r) %in% sel

r<-r[[sel]]
plot(r[[1]])



## random background CZG extract
ext<-spatSample(r[[sel]],1e5,na.rm=T)
ext$group<-"background"
saveRDS(ext,"CZG_random10e5.rds")

## presence CZG extract
library(sf)
ex<-st_read("fpicea.gpkg")
ex.tr<-st_transform(ex,32633)
v<-vect(ex.tr)
plot(v,add=T)

ext<-terra::extract(r,v)
ext<-ext[,-1]
fpicea<-cbind(ex,ext)
ext$group<-"presence"
saveRDS(ext,"CZG_presence.rds")

st_geometry(fpicea)<-NULL

library(openxlsx)
write.xlsx(fpicea,"Lokality_F.picea_CMV_mm_cluster.xlsx")


## envicompare
df.rand<-readRDS("CZG_random10e5.rds")
df.pres<-readRDS("CZG_presence.rds")
dff<-rbind(df.pres,df.rand)

## elevation
library(ggplot2)

p<- ggplot(data = dff,alpha=0.3,aes(x=elevation_10m094,fill=group)) + 
  geom_density() +
  geom_density(data=dff,alpha=0.3,aes(x=elevation_10m094,fill=group)) +
  theme(aspect.ratio = 1) +
  labs(y="Density")+
  labs(x="Elevation")+
  ggtitle("Distribution ascross altitude")
print(p)
ggsave("elevation.png",units="cm",height = 10,width = 10,dpi=200,scale=1.5)



## temperature
p<- ggplot(data = dff,alpha=0.3,aes(x=Q095AirTemp_f14t19,fill=group)) + 
  geom_density() +
  geom_density(data=dff,alpha=0.3,aes(x=Q095AirTemp_f14t19,fill=group)) +
  theme(aspect.ratio = 1) +
  labs(y="Density") +
  labs(x="Maximum air temperature 2014 - 2019 [°C]")+
  ggtitle("Distribution ascross maximum air temperature")
print(p)
ggsave("Tmax.png",units="cm",height = 10,width = 10,dpi=200,scale=1.5)

## precipitation
p<- ggplot(data = dff,alpha=0.3,aes(x=MeanPrecipitation_f14t19,fill=group)) + 
  geom_density() +
  geom_density(data=dff,alpha=0.3,aes(x=MeanPrecipitation_f14t19,fill=group)) +
  labs(y="Density") +
  labs(x="Average precipitation 2014 - 2019 [mm]")+
  ggtitle("Distribution ascross precipitation")
print(p)
ggsave("Precip.png",units="cm",height = 10,width = 10,dpi=200,scale=1.5)

## clusters
pr<-summary(factor(dff[df$group=="presence","Clusters4class"]))
pr<-data.frame(id=names(pr),presence=as.numeric(pr))
bg<-summary(factor(df[dff$group=="background","Clusters4class"]))
bg<-data.frame(id=names(bg),background=as.numeric(bg))
df<-merge(bg,pr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

df$presence<-range01(df$presence)
df$background<-range01(df$background)

library(tidyr)
df2<-pivot_longer(df,2:3)
names(df2)<-c("cluster","species","density")
library(ggplot2)

p2<-ggplot(df2, aes(x = cluster, y= density, fill = species)) +
  geom_bar(stat="identity", width=.5, position = "dodge")+
  xlab("cluster") +
  ggtitle("Distribution ascross Clusters [4 classes]")+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1))
print(p2)
ggsave("clusters.png",units="cm",height = 10,width = 10,dpi=200,scale=1.5)
