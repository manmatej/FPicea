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


## k-mean clustering white box

czg.path<-"q://s_slozky_osobni/Man/clanek_2022_Fpicea_big/"
czg.nam<-list.files(czg.path,pattern = "*.tif$",full.names = T)
nam<-list.files(czg.path,pattern = "*.tif$",full.names = F)
czg.names<-substr(nam,1,nchar(nam)-4)
czg.dat<-terra::rast(czg.nam)
sel<-c("DiurnalAnisotropicHeating094",
       "elevation_10m094",
       "TopographicPositionIndex50-500m094",
       "Q005AirTemp_f14t19",                 
       "Q095AirTemp_f14t19",
       "MeanPrecipitation_f14t19")

inputs=czg.nam[czg.names%in%sel]
output=paste0(czg.path,"Clusters_6class.tif")
out_html=paste0(czg.path,"Clusters_6class.html")

install.packages("whitebox", repos="http://R-Forge.R-project.org")
library(whitebox)
wbt_init()

wbt_k_means_clustering(inputs=inputs[1:2], output=output, classes=6, out_html,
                   max_iterations = 10, class_change = 2, initialize = "diagonal",
                   min_class_size = 10, verbose_mode = FALSE)






# Pararelize the task
st.list <- unstack(st)
names(st.list) <- names(st)

sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)
sfLibrary(raster)
sfLibrary(sf)

e.body <- sfSapply(st.list, fun=extract, y=body_32633)
sfStop()

body_extract<-cbind(e.body,body_32633)

st_write(body_extract,"body_extract.shp") # writes shapefile with exctracted values
write.table(body_extract,"body_extract_csv.csv",sep=";",row.names = F,col.names = T) # write CSV with extracted data. 






## CZG extract
library(terra)
r<-rast("d://Fpicea_rasters/czg_complet_ji.tif")
names(r)
sel<-c("canopy_height_DMP_DMR",
       "DiurnalAnisotropicHeating094",
       "elevation_10m094",
       "TopographicPositionIndex50-500m094",
       "BiotopType094")

r<-r[[sel]]
names(r)
plot(r[[1]])
ex<-st_read("extent.gpkg")
ex.tr<-st_transform(ex,st_crs(czg.dat))
v<-vect(ex.tr)
plot(v,add=T)

## random background CZG extract
library(terra)
ext<-spatSample(r[[sel]],1e6,na.rm=T)
ext$group<-"background"
saveRDS(ext,"CZG_random10e6.rds")

## presence CZG extract
data_vec<-vect("fpicea.gpkg")
ext<-terra::extract(r,data_vec)
ext$group<-"presence"
saveRDS(ext,"CZG_presence.rds")


## envicompare

df.rand<-readRDS("CZG_random10e6.rds")
df.pres<-readRDS("CZG_presence.rds")

df<-rbind(df.pres,df.rand)

## elevation
p<-ggplot(data=df,aes(x=elevation_10m094,linetype=group,colour=group)) + 
  geom_density(alpha=0.3,aes(x=elevation_10m094),size=1.5,show.legend = F) +
  scale_linetype_manual(values = c("dashed", "solid"))+
  scale_color_manual(values = c("black","grey"))+
  theme(aspect.ratio = 1) +
  labs(y="Density") +
  labs(x="Elevation [m]")
print(p)

png("CZG_elevation.png",width = 600,height = 600)
print(p)
dev.off()
names(df)















## temperature
p<-ggplot(data =df,aes(x=MeanAirTemp_f14t19,linetype=group,colour=group)) + 
  geom_density(alpha=0.3,aes(x=MeanAirTemp_f14t19),size=1.5,show.legend = F) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
  scale_color_manual(values = c(col.mech,"grey",col.lich))+
  theme(aspect.ratio = 1) +
  labs(y="Density") +
  labs(x="Average air temperature 2014 - 2019 [°C]")
print(p)

png("CZG_airtemp.png",width = 600,height = 600)
print(p)
dev.off()

## precipitation
p<-ggplot(data =df,aes(x=MeanPrecipitation_f14t19,linetype=group,colour=group)) + 
  geom_density(alpha=0.3,aes(x=MeanPrecipitation_f14t19),size=1.5,show.legend = F) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed"))+
  scale_color_manual(values = c(col.mech,"grey",col.lich))+
  theme(aspect.ratio = 1) +
  labs(y="Density") +
  labs(x="Average precipitation 2014 - 2019 [mm]")
print(p)

png("CZG_precipitation.png",width = 600,height = 600)
print(p)
dev.off()



## biotop type 

codes<-fread("d://Git/biotope_type_AOPK_biotop_codes.csv")
colnames(codes)<-c("source","biotop","name")

## Lichens ##
lisejniky<-merge(lisejniky.red,codes,by="biotop",all.x=T)

## Common
lisejniky.comon<-lisejniky[lisejniky$RL_eval %!in% rare,]
lisejniky.comon<-lisejniky.comon%>% distinct(name_lat,name,.keep_all = F)
lisejniky.comon<-lisejniky.comon[lisejniky.comon$name!="Not classified",]
lisejniky.comon<-lisejniky.comon[lisejniky.comon$name!="",]
lisejniky.comon<-lisejniky.comon[!is.na(lisejniky.comon$name),]

l<-sort(summary(as.factor(lisejniky.comon$name),maxsum=Inf),decreasing = T)
dfl<-data.frame(code=names(l),count=l)
(dfl$count[1]/sum(dfl$count))*100 # 15 % lichen records not classified biotope
dfln<-dfl
dfln$statut<-"Common"
dfln<-dfln[1:10,]
sel<-rownames(dfln)

## Red-listed 
# lisejniky.re<-lisejniky[lisejniky$name_lat!="Usnea barbata",] # without Usnea barbata
lisejniky.rare.only<-lisejniky[lisejniky$RL_eval %in% rare,] # only rare species
lisejniky.rare.only<-lisejniky.rare.only%>% distinct(name_lat,name,.keep_all = F)
lisejniky.rare.only<-lisejniky.rare.only[lisejniky.rare.only$name!="Not classified",]
lisejniky.rare.only<-lisejniky.rare.only[lisejniky.rare.only$name!="",]
lisejniky.rare.only<-lisejniky.rare.only[!is.na(lisejniky.rare.only$name),]

l<-sort(summary(as.factor(lisejniky.rare.only[,"name"]),maxsum=Inf),decreasing = T)
dfl<-data.frame(code=names(l),count=l)
dflr<-dfl
dflr$statut<-"Red-listed"
dflr<-dflr[sel,]

df<-rbind(dfln,dflr)
df$count<-as.numeric(df$count)

p<-ggplot(data=df, aes(y=reorder(code,-count), x=count,fill=statut)) +
  geom_bar(stat="identity", width=0.5,position="dodge",orientation = "y",show.legend = T)+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
  scale_fill_manual("",values = rep(c(col.lich,col.lich1),5))+
  ylab(NULL) +  
  theme(legend.position = "bottom",
        text = element_text(size = 7))
print(p)

ggsave("AOPK_biotope_type_lichens.png",p,units="cm",width = 11,height = 11,dpi=300)


