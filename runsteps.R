rm(list=ls())
source('C:/Users/akjha/Desktop/LULC/Codes/Rasterise_dev_61.R')

# T1File='C:/Users/akjha/Desktop/LULC/lucc_shape/img/1985.img';
# T2File='C:/Users/akjha/Desktop/LULC/lucc_shape/img/1995.img';
T1File='C:/Users/akjha/Desktop/LULC/lucc_shape/1985.tif';
T2File='C:/Users/akjha/Desktop/LULC/lucc_shape/1995.tif';
drvs<-c(slope='C:/Users/akjha/Desktop/LULC/lucc_shape/img/Slope.img'
,elevation='C:/Users/akjha/Desktop/LULC/lucc_shape/img/elevation.img'
,distoueb='C:/Users/akjha/Desktop/LULC/lucc_shape/img/Dist_urban.img'
,disttostream='C:/Users/akjha/Desktop/LULC/lucc_shape/img/dist_stream.img')

restrictSpatial=c(1,0,0,0,0,0,0,0,1)
clsName<-c("BuildUp","Agri","DenseForest","Fallow","Grass","MixedForest","Plantation","ScrubLan","WaterBody")
#mydemand<-as.numeric(c(2316,35245,11306,1762,5,71,3735,1562,3594))
#getNewTM(TM,as.numeric(demand))
#na.value=128
na.value=NA
neighbourl<-list()
neighbourl[[1]]<-3
neighbourl[[2]]<-10

map<-genratePredictedMap(model.type='logistic',T1File,T2File,,drvs,drvs,na.value,,restrictSpatial,neighbour=neighbourl)
writeRaster(map,filename="demandregression10000000.tiff",format='GTiff',options="INTERLEAVE=BAND",overwrite=FALSE)

############################USERCOMMAND ENDS#############################

model.type='logistic',T1File,T2File,with.class.name=NA,T1drivers,T2drivers,with.na.value,demand=NA,restrictSpatialMigration=NA,AllowedClassMigration=NA,
				conversionOrder='TP',classAllocationOrder=NA,neighbour=NA,method='random'

