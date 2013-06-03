#Input: Vector Map At T0 and grid size of analysis.
#	Param1: Shape File Name   Rasterise_dev_6.R
#	Param2: Gridsize
#rm(list=ls())

debug_getAllocatedCell<-function(dt1)
{
   return(sum(rowSums(dt1,na.rm=TRUE)))
}
debug_getSuitLength<-function(sui)
{
	for ( i in 1:length(sui)){
		print(dim(sui[[i]])[1])
	}
}

getAllocatedDT<-function(suitablityMat,transitionMat,fromClass,demand=NA,restrictSpatialMigration=NA,AllowedClassMigration=NA
				,conversionOrder='TP',classAllocationOrder=NA,neighbour=NA,method='random'){

	#SuitablityMat<-gives the fitted values as per drivres sorted in probablity order
	#transitionMat<-gives the trasition distribution to different class
	#fromClass<-represents the data table with the coloumn for each class representing the currently allocated class from which new prediction needs  to be achived
	#demand<-Demand is matrix representing the total allocatin which needs to be done to each class. The class order needs to be maintained in order of column order in "fromClass"
	#restrictSpatialMigration<-represents the vulnaribility which states that a class in matrix will remain in the same state work as policy {1 represents no change possible 0 means change possible}
	#AllowedMigration<-represents the conversion matrix[numberofClass,numberofClass] which is a kind of policy to state that which class migration is possible
	#conversionOrder<-givers the priority of conversion from class to class . low values means less liekly change and high means more likely change.In absense it will be genrated from TP
	#classAllocationOrder<-gives the priority of allocation to different class.By default it allocates is class name order 
	#neighbour<-neighbouring effects TODO
	#method<-kind of allocation {random<-represents allocation based on the reverse of vulnaribility policy order and demand purely based on drivers probablity
	#			     default<-represents allocation based on priority in order as 1st.Select all the class with no conversion allowed  	to fulfill the demand then next			
	#											  2nd.Give prority for conversion in order of vulnarability to meet demand in order of Trasition Prabability 
	#												to fulfill the demand from top suitable palce. In case of same rank class order will be taken
	#											 
	#			     }

	classNames<-names(fromClass)
	
	outputAllocation<-fromClass   #Creating output grid to be allocated as per the current grids 
	outputAllocation[]<-NA	      #Initialising it to be non allocated
	noOfAllocationGrid<-dim(fromClass)[1]
	noOfClass<-dim(fromClass)[2]
	outputAllocation<-as.data.table(cbind(id=seq(1,noOfAllocationGrid),outputAllocation))

	if(is.na(classAllocationOrder)){
		classAllocationOrderMat<-seq(1:noOfClass)
	} else {
		classAllocationOrderMat<-classAllocationOrder
	}
	

	if(is.na(AllowedClassMigration)){
		AllowedClassMigrationMat<-matrix(1,nrow=noOfClass,ncol=noOfClass)
	} else {
		AllowedClassMigrationMat<-AllowedClassMigration
	}

	if(conversionOrder=='TP'){
		conversionOrderMat<-matrix(NA,noOfClass,noOfClass)   #Create a allocatin Order
		for ( i in 1:noOfClass){
			x<-rbind(as.numeric(rownames(table(sort(transitionMat[i,])))),table(sort(transitionMat[i,]))) 
			s<-1;
			for (k in length(x[1,]):1){
				p<-as.numeric(x[1,k])
				conversionOrderMat[i,][transitionMat[i,]==p]<-seq(s,length.out=x[2,k])
				s<-s+as.numeric(x[2,k])
			}	
		}

	}else {
		conversionOrderMat<-conversion
	}


	#Modify the conversion Order based on the allowed migration 
	 conversionOrderMat[which(AllowedClassMigrationMat!=1)]<-0 #Replace all the convesion matrix with  0 to indicate these conversion are not possible


	
	if(sum(is.na(demand))!=0){
		demandMat<-colSums(getNewTM(transitionMat))
		newTM<-getNewTM(transitionMat)
	}else {
		demandMat<-demand
		newTM<-getNewTM(transitionMat,demand)
	}


	if(sum(is.na(restrictSpatialMigration))!=0){
		restrictSpatialMigrationTM=rep(0,noOfClass)
	}else{
		restrictSpatialMigrationTM<-restrictSpatialMigration
	}

	if(method=='random'){

		for( i in 1:noOfClass) {
				if(restrictSpatialMigrationTM[i]==1) 
				{	#1 Means no spatial migration is possible
					 tmp<-fromClass[,classNames[i],with=F]
					 demandMat[i]<-demandMat[i]-colSums( tmp,na.rm=TRUE)  #Reduce total demand by the allocated grid
					 if(demandMat[i]<0) 
						{ print("ERROR:Decrease in Demand of Class"); print(demandMat[i]); print ("is not psiible to have restricted flag set to 1") }
					 outputAllocation[,classNames[i]]<-tmp   #allocate all the class grid to output
					 removeAllocateGrid(suitablityMat,which(tmp==1))
					 #suitablityMat[[i]]<-suitablityMat[[i]][!(id %in% which(tmp==1))][order(prob,decreasing = TRUE)]  #Remove all the grid which is allocated from probability list 
					 #suitablityMat[[i]]<-suitablityMat[[i]][!(id %in% outputAllocation[rowSums(outputAllocation,na.rm=TRUE)!=id]$id)]  #Remove already olloocated grids to any class
					 conversionOrderMat[i,]<-0 #Conversion is done and need not be considered for further allocation
					 #newTM[i,]<-0  #Due to restrction rowise allocation will be 0; as migration is not allowed
					 newTM[i,i]<-newTM[i,i]-colSums( tmp,na.rm=TRUE) #Decrease the demand by amount with which it is already allocated
					 demandMat[i]<-demandMat[i]-newTM[i,i]  #Has to be after the demand is full filled
					 if(sum(newTM[i,])!=0) {print("ERROR:As Migration is not possible but the TM contains the migration to other class from T1")}
				}			
		 }

		for(i in 1:noOfClass){
			classFromAllocate<-classAllocationOrderMat[i]
			for(j in 1:noOfClass){
				if(sum(conversionOrderMat[classFromAllocate,]==j)!=0)     #If there is some conversion possible
				{
					classToAllocate<-which(conversionOrderMat[classFromAllocate,]==j)  #Get the current highest order of allocation which needs to be done	
				       if(newTM[classFromAllocate,classToAllocate]>0)
					{
						#probableGridToAllocate<-suitablityMat[[classToAllocate]][suitablityMat[[classFromAllocate] ]$id %in% suitablityMat[[classToAllocate]]$id][order(prob,decreasing = TRUE)]
						 probableGridToAllocate<-suitablityMat[[classToAllocate]][suitablityMat[[classToAllocate]]$id %in% suitablityMat[[classFromAllocate]]$id][order(prob,decreasing = TRUE)]					
						#probableGridToAllocate<-suitablityMat[[classToAllocate]][order(prob,decreasing = TRUE)]
						probableGridToAllocate<-probableGridToAllocate[!(id %in% outputAllocation[rowSums(outputAllocation,na.rm=T)!=id]$id)] #Filtere Out already allocated Grids	
						if(dim(probableGridToAllocate)[1]>=newTM[classFromAllocate,classToAllocate]){
						#outputAllocation[probableGridToAllocate[1:demandMat[classToAllocate]]$id,classNames[classToAllocate]]<-1
							gridId<-probableGridToAllocate[1:newTM[classFromAllocate,classToAllocate]]$id
							suitablityMat[[classToAllocate]]<-suitablityMat[[classToAllocate]][!(id %in% gridId)][order(prob,decreasing = TRUE)] #All allocate grid is removed from
																				  # suitability matrix
							removeAllocateGrid(suitablityMat,gridId)
							demandMat[classToAllocate]<-demandMat[classToAllocate]-newTM[classFromAllocate,classToAllocate]
							newTM[classFromAllocate,classToAllocate]<-0
							outputAllocation[gridId,classNames[classToAllocate]]<-1
							conversionOrderMat[classFromAllocate,classToAllocate]<-0
						}
					}
					
				}

			}
		}
			
	}	
	outputAllocation<-outputAllocation[,classNames,with=F]
	return(outputAllocation)
}

removeAllocateGrid<-function(suitablity,grids){

	for (i in 1:length(suitablityMat)){
		suitablityMat[[i]]<-suitablityMat[[i]][!(id %in% grids)][order(prob,decreasing = TRUE)]
	}
	return (suitablityMat)
}

constructSuitablity<-function(model,driverForWhichToPredict){
	library(data.table)	
	suitablity=list()
	drvWithId<-as.data.table(cbind(id=seq(1,dim(driverForWhichToPredict)[1]),driverForWhichToPredict))
	for(i in 1:length(model)){
		model.class<-class(model.fit[[i]])
		if(model.class[1]!="lm"){
			suitablity[[i]]<-as.data.table(cbind(id=drvWithId[rowSums(driverForWhichToPredict,na.rm=TRUE)!=0]$id,prob=fitted(model[[i]],driver=drvWithId)))[order(prob,decreasing = TRUE)]
		}else {
			print("OK")
			suitablity[[i]]<-as.data.table(cbind(id=as.numeric(rownames(as.data.frame((predict(model.fit[[i]],driverForWhichToPredict))[]))),prob=as.data.frame(predict(model.fit[[i]],driverForWhichToPredict))[,1]))[order(prob,decreasing = TRUE)]	
				#Here predict is usning the argument to get the probability (response)
		}	
	}
	return(suitablity)
}


getRaster<-function(file,with.single.layer=FALSE,with.na.value=NA){
	library(raster);
	if(with.single.layer){
		rst<-raster(file)
		rst<-singleToMultiBand(rst,with.na.value)
		
	} else {
		rst<-stack(file)	
	}
	return(rst)
}

getDataTable<-function(file,with.single.layer=FALSE,with.na.value=NA,with.class.name=NA){
	library(raster);
	library(data.table)
	rst<-getRaster(file,with.single.layer,with.na.value)
	if(!with.single.layer) 
	{
		classlayers<-c(paste("class",seq(1:length(names(rst))),sep="_"))
	} else {
		classlayers=names(rst)
	}
	if(sum(is.na(with.class.name))==0) {classlayers<-with.class.name;}	
	names(rst)<-classlayers;
	return(data.table(as.data.frame(values(rst))))
}


getNumberOfClass<-function(File,with.single.layer=NA,with.na.value=NA){
	t1<-getRaster(File,with.single.layer,with.na.value)	
	return(dim(t1)[3])
}

getnoOfCell<-function(File,with.single.layer=NA,with.na.value=NA){
	t1<-getRaster(File,with.single.layer,with.na.value)	
	return(dim(t1)[1]*dim(t1)[2])
}


prepareData<-function(T1File,T2File,driverfiles,classFile.with.single.layer=FALSE,with.na.value=NA,with.class.name=NA,with.driver.name=NA){
	library(raster);library(data.table);#library(pscl);library('memisc')
	
	if(sum(is.na(with.class.name))!=0) {classlayers<-c(paste("class",seq(1:length(driverfiles)),sep="_"))}  
	if(! classFile.with.single.layer) {classlayers=with.class.name}	
	
	if(sum(is.na(with.driver.name))!=0) {driverlayers<-c(paste("driver",seq(1:length(driverfiles)),sep="_"))} else { driverlayers<-with.driver.name}
	
	numberofClass<-getNumberOfClass(T1File,classFile.with.single.layer,with.na.value)
	noOfCell<-getnoOfCell(T1File,classFile.with.single.layer,with.na.value)
	datatableT1<-getDataTable(T1File,classFile.with.single.layer,with.na.value,with.class.name)
	datatableT2<-getDataTable(T2File,classFile.with.single.layer,with.na.value,with.class.name)
	driverdatatable<-getDataTable(driverfiles,FALSE,with.na.value,driverlayers)
	#driverdatatable<-getDataTable(driverfiles,FALSE,with.na.value)
	
	data<-as.data.table(cbind(id=seq(1,noOfCell),time1=datatableT1,time2=datatableT2,driver=driverdatatable))
	return(data)
}


fitModel<-function(data,model.type='regression',numberofClass){

	#Here assumtion is the first column is grid number. then all the columnwise class values for T1 then columnwise class values of T2 then all the drivers
	model.fit<-list()
	model.predict<-list()
	dataColumnNames<-colnames(data)
	classesToFit<-dataColumnNames[seq(2,numberofClass+1)]
	driversToChoose<-dataColumnNames[seq(2+2*numberofClass,length(dataColumnNames))]

	dta<-subset(data[rowSums(subset(data,select=c(classesToFit)),na.rm=TRUE)!=0],select=c(dataColumnNames[1],classesToFit,driversToChoose))  
	#select those grid which has been allocated to any class and then get all the class allocation with the drivers.

	#as.data.table(cbind(subset(dta,select=c(dataColumnNames[1])),as.data.frame(predict.model)))

	if(model.type=="logistic"){
		dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
		for( i in 1:numberofClass){
			frml<-as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+")))
			model.fit[[i]]<-glm(frml,family = binomial(), data = dta,na.action='na.omit')
			#model.fit[[i]]<-glm(formula=as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+"))),family = binomial, data = dta,na.action='na.omit')
			
		}
		
		#model1.fit<-glm(time1.class_1 ~ driver.slope + driver.distoueb + driver.disttostream, family = binomial, data = dta,na.action='na.omit')
	} else if (model.type=="mlp") {

	} else {

		for( i in 1:numberofClass){
			frml<-as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+")))
			model.fit[[i]]<-lm(frml,family = binomial(), data = dta,na.action='na.omit')
			
		}
		
		#model1.fit<-lm(time1.class_1 ~driver.slope + driver.distoueb + driver.disttostream,  data = dta,na.action='na.omit')
  
	}
	names(model.fit)<-classesToFit
	return(model.fit)

}

# biglmupdate<-function(dataset,inmodel,frml,start,end) {   #TO be used by bigglm fit inside fitModelSeparately
 # if (start==1) {
	# model <<- bigglm(formula(frml),family = binomial(), data = dataset[start:end,],na.action='na.omit')
 # }
 # else {
    # model <<- update(inmodel, data = dataset[start:end,],na.action='na.omit')
 # }
 # return (model)
 
 # # chunks <- floor(dim(dta)[1]/10000)+1
					# # start<-0; end<-0;
					# # pmodel.fit=NA
					# # foreach (ii = 1: chunks) %dopar% { 
						# # start = end +1; end =ifelse (ii == chunks, dim(dta)[1], start+10000); 
						# # #print(paste("chunk ",ii," ",start,":",end," started at:", Sys.time()));
						# # pmodel.fit<-biglmupdate(dataset = dta, pmodel.fit,frml,start, end);
						# # #print(paste("chunk ",ii,"ended at:", Sys.time()));
						# # print(pmodel.fit)
						
					# # }
				# #model.fit[[i]]<-pmodel.fit
# }

					

fitModelSeparately<-function(dt1,drvt1,model.type='regression',withCore=1){
	#library(bigmemory)
	#library(biganalytics)
	library(data.table)
	library(biglm)
	#Here assumtion is the first column is grid number. then all the columnwise class values for T1 then columnwise class values of T2 then all the drivers
	data<-data.table(cbind(id=seq(1:dim(dt1)[1]),dt1,drvt1)) #adding the grid number along with all class and drivers.
	model.fit<-list()
	dataColumnNames<-colnames(data)
	numberofClass<-dim(dt1)[2]
	classesToFit<-dataColumnNames[seq(2,numberofClass+1)]
	driversToChoose<-dataColumnNames[seq(2+numberofClass,length(dataColumnNames))]
	numberofRows<-dim(dt1)[1]

	dta<-subset(data[rowSums(subset(data,select=c(classesToFit)),na.rm=TRUE)!=0],select=c(dataColumnNames[1],classesToFit,driversToChoose))  
	#select those grid which has been allocated to any class and then get all the class allocation with the drivers.

	#as.data.table(cbind(subset(dta,select=c(dataColumnNames[1])),as.data.frame(predict.model)))

	if(model.type=="logistic"){
		dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
		if(withCore==1){
			for( i in 1:numberofClass){
				frml<-paste(classesToFit[i],"~", paste(driversToChoose, collapse= "+"))
				print(frml)
				#frml<-as.formula(paste(classesToFit[i],"~", paste(driversToChoose, collapse= "+")))
				#model.fit[[i]]<-glm(formula(frml),family = binomial, data = dta,na.action='na.omit')
				model.fit[[i]]<-glm(formula(frml),family = "binomial", data = dta,na.action='na.omit')
			}
		} else {
			##library(parallel)#library(multicore)
			library(doParallel)
			##cl <- makeCluster(withCore)
			#registerDoParallel(core=withCore)#registerDoMC(withCore) #registerDoParallel(c1)
			#foreach ( i = 1:numberofClass) %dopar%{
			#	frml<-paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+"))
			#	frml<-as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+")))
			#	model.fit[[i]]<-bigglm(formula(frml),family = binomial, data = dta,na.action='na.omit',chunksize=10000)
			#}
			
			model.fit<-foreach ( i = 1:numberofClass) %dopar%{
				frml<-paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+"))
				print(frml)
				glm(formula(frml),family = "binomial", data = dta,na.action='na.omit')
			}
		}
		names(model.fit)<-classesToFit
		#model1.fit<-glm(time1.class_1 ~ driver.slope + driver.distoueb + driver.disttostream, family = binomial, data = dta,na.action='na.omit')
	} else if (model.type=="mlp") {

	} else {

		for( i in 1:numberofClass){
			frml<-as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+")))
			model.fit[[i]]<-lm(frml,family = binomial, data = dta,na.action='na.omit')
		}
		names(model.fit)<-classesToFit
		#model1.fit<-lm(time1.class_1 ~driver.slope + driver.distoueb + driver.disttostream,  data = dta,na.action='na.omit')
  
	}
	
	return(model.fit)

}

getTp<-function(t){  #T is transition matrix 
	return(t/sum(rowSums(t)))

}

createTM<-function(dft1,dft2){
    numberofClass<-dim(dft1)[2]
    numberofRows<-dim(dft1)[1]

    dft1[is.na(dft1)]<-0
    dft2[is.na(dft2)]<-0
  #Assuming both are of same dimension
    tp<-dft1*matrix(rep(seq(1,numberofClass),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T)*(numberofClass+1)+dft2*matrix(rep(seq		
					(1,numberofClass),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T)
    tp<-rowSums(tp)
    tp<-table(tp)
    dummytp<-matrix(rep(seq(1,numberofClass),numberofClass),nrow=numberofClass,ncol=numberofClass,byrow=F)*(numberofClass+1)+matrix(rep(seq
					(1,numberofClass),numberofClass),nrow=numberofClass,ncol=numberofClass,byrow=T)
    actualtp<-matrix(0,nrow=numberofClass,ncol=numberofClass)
    tpindex<-as.numeric(rownames(as.matrix(tp))) 
    for (k in 1:length(tp)){
  	for( i in 1:numberofClass){
      		for (j in 1:numberofClass) {
              		if(dummytp[i,j]==tpindex[k])
                 		actualtp[i,j]=tp[k]                
                }
	}

   }

   return(actualtp)   
}

getNewTM<-function(x,demand=NA){
 	if(sum(is.na(demand))!=0){
 		tmp<-(round(colSums(x)*x/rowSums(x),digits=0))
	} else{
		tmp<-(round(demand*x/rowSums(x),digits=0))
	}
 return(tmp)
}

singleToMultiBand<-function(rasterVar,with.na='NA'){
	library(raster)
	library(data.table);
	tmpraster<-raster(rasterVar);
	tmpraster[]<- 1:ncell(tmpraster)
	tmp<-as.data.table(as.data.frame(values(rasterVar)))
	tmpClassCode<-rownames(table(tmp))
	if(!is.na(with.na)){
		tmpClassCode[tmpClassCode==with.na]<-NA
	}
	
	tmpClassCode<-tmpClassCode[!is.na(tmpClassCode)]
	setnames(tmp,'cls')
	tmp<-as.data.table(cbind(cell=1:ncell(tmpraster),tmp,value=1))

	st<-stack(tmpraster)
	st<-subs(st,subset(tmp[cls==tmpClassCode[1]],select =  c('cell','value')))
	#layerNames(st)[1]<-classCode[1]

	len<-length(tmpClassCode)

	for (x in c(2:len)) {
			r<-raster(tmpraster)
			r[]<- 1:ncell(tmpraster)
			r<-subs(r,subset(tmp[cls==tmpClassCode[x]],select = c('cell','value')))
			st<-stack(st,r)				 #?? Can we do memory optimisation here
		}
		#layerNames(st)[x]<-classCode[x]

	names(st)<-c(paste("class",tmpClassCode,sep="_")) 
	return(st)
}



createMutliRaster<-function(classCode,inputRaster,inputDataTable)
{
	values(inputRaster)<- 1:ncell(inputRaster)
	st<-stack(inputRaster)
	st<-subs(st,subset(inputDataTable[luClass==classCode[1]],select = names(inputDataTable)[-2]))
	#layerNames(st)[1]<-classCode[1]

	len<-length(classCode)

	for (x in c(2:len)) {
		r<-raster(inputRaster)
		r[]<- 1:ncell(inputRaster)
		r<-subs(r,subset(inputDataTable[luClass==classCode[x]],select = names(inputDataTable)[-2]))
		st<-stack(st,r)				 #?? Can we do memory optimisation here
		#layerNames(st)[x]<-classCode[x]

	}
	names(st)<-classCode
	return(st)
}


rasterise<- function(shpFile,layerName,Gridsize=1000,option="fraction")
{	library(maptools);library(rgeos);library(rgdal);library(raster);library(data.table)
	#library(rgdal) # Loads SP package by default
#Debug
inputShape<-readOGR(shpFile, layerName) # Creates a SpatialPolygonsDataFrame class (sp)
	#inputShape<-readShapeSpatial(system.file(shpFile))  #read shape file as spatial object
	inputClass<-as.character(inputShape@data$IGBP_CODE)  #TODO Can it be done without hardcode
#DEBUG
debug<-as.character(inputShape@data$IGBP_POLY)
	inputClassCode<-as.character(levels(inputShape@data$IGBP_CODE) )
	#create raster grid
	outputRaster<-raster()
	projection(outputRaster)<-projection(inputShape)
	extent(outputRaster)<-extent(inputShape)
	res(outputRaster)<-Gridsize
	names(outputRaster)="grid"
	values(outputRaster)<- 1:ncell(outputRaster)
	if(option=="fraction") {
		system.time(ex<-(extract(outputRaster,inputShape,weight=TRUE))) #TODO can we do speedup here
	} else {
		system.time(ex<-(extract(outputRaster,inputShape,weight=FALSE))) 
	}
		
	
	#function to be used to append the LULC class at the last of the column after converting it into dataframe
	
	makeDtbl<- function(x,y,z) {
		if(!is.null(x))
			if(option=="fraction"){
				cbind(x,y,z)
			} else {
				cbind(value=x,weight=1,y,z)
			}
	}

	system.time(kc<-mapply(makeDtbl,ex[1:length(ex)],inputClass[1:length(ex)],debug[1:length(ex)]))
#DEBUG
#lapply(ex, write, "ex.txt", append=T)
	
	system.time(dtbl <- data.table(do.call("rbind", kc)))
	
	d=transform(dtbl, value = as.numeric(levels(value)[value]), weight= as.numeric(levels(weight)[weight]),y=as.character(y),z=as.character(z)); dtbl=d;rm(d) #TODO get rid of this code
	tableCol<-c(c1="cell",c3="weight",c2="luClass",c4="IGBP_POLY") ;setnames(dtbl,tableCol);setkey(dtbl,cell,luClass,IGBP_POLY)
	#Datatable will be of header with (cell,weight,luClass) corresponding to the (gridnumber,fraction,LULC Class)
	#Aggregate for the cases where two similar class boundary may intersect to a cell
#DEBUG
#write.table(dtbl,file="dtbl1.csv",sep=",")
	
	dtblagg<-dtbl[,sum(weight),by="cell,luClass"] #TODO Can it be done without hardcode
	#rm(dtbl) 
	tableCol<-c(c1="cell",c2="luClass",c3="weight") ;setnames(dtblagg,tableCol)
	setkey(dtblagg,cell,luClass)  #TODO Can it be done without hardcode 
#DEBUG
#write.table(dtblagg,file="dtblagg2.csv",sep=",")

errorpoly=unique(dtbl[subset(dtblagg[weight>1],select='cell')]$IGBP_POLY)
write.table(errorpoly,file=paste(layerName,"errorpoly.csv",sep="-"),sep=",")
	mr<-createMutliRaster(inputClassCode,outputRaster,dtblagg)
	writeRaster(mr, filename=paste(layerName,".tif",sep=""), options="INTERLEAVE=BAND", overwrite=TRUE)	
	return(mr)


}

kappa <- function(CM) {
	#convert both data frames and vectors to matrices
	cmx<-as.matrix(CM)
	#try to convert a vector to a square matrix
	if (ncol(cmx) == 1)
		cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
	nr<-nrow(cmx); nc<-ncol(cmx)
	if (nr != nc)
		{ print("Error: matrix is not square"); break }
	n<-sum(cmx)
	d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
	th1v<-((th1*(1-th1))/n)
	csum<-apply(cmx,2,sum); rsum<-apply(cmx,1,sum)
	ua<-d/rsum; pa<-d/csum
	th2 <- sum(rsum*csum) / n^2; kh <- (th1-th2)/(1-th2)
	th3 <- sum( (csum + rsum) * d ) / n^2;
	th4 <- 0; for (i in 1:nr) for (j in 1:nc)
	th4 <- th4 + (cmx[i,j] * ((csum[i] + rsum[j])^2));
	th4 <- th4 / n^3;
	th1c <- 1 - th1; th2c <- 1 - th2;
	khv <- 1/n *
	(
		( ( th1 * th1c ) / th2c^2 )
		+ ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
		+ ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
	)
	#per-class kappa, user’s accuracy...
	p <- cmx/n; uap <- apply(p,1,sum); pap <- apply(p,2,sum); dp<-diag(p);
	kpu <- (dp/uap - pap)/(1 - pap);
	#...and its variance
	t1 <- uap-dp; t2 <- (pap*uap)-dp; t3 <- dp*(1 - uap - pap + dp);
	kpuv <- ( (t1/(uap^3 * (1-pap)^3)) * ((t1*t2) + t3) )/n;
	#per-class kappa, producer’s reliability...
	kpp <- (dp/pap - uap)/(1 - uap);
	#...and its variance
	t1 <- (pap-dp);
	kppv <- ( (t1/(pap^3 * (1-uap)^3)) * ((t1*t2) + t3) )/n;
	#return all statistics as a list
	list(sum.n=n, sum.naive=th1, sum.var=th1v, sum.kappa=kh, sum.kvar=khv,
		user.naive=ua, prod.naive=pa,
		user.kappa=kpu, user.kvar=kpuv, prod.kappa=kpp, prod.kvar=kppv)
}


summary.kappa <- function(kappa, alpha=0.05) {
	ciw<-function(var, n) {
		qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
	}
	print(paste("Number of observations:", kappa$sum.n), quote=F)
	print("Summary of naive statistics", quote=F)
	print(paste(
	"Overall accuracy, stdev, CV%:",
	round(kappa$sum.naive, 4), ",",
	round(sqrt(kappa$sum.var), 4), ",",
	round((sqrt(kappa$sum.var)/kappa$sum.naive)*1000,0)/10),
	quote=F)
	w<-ciw(kappa$sum.var, kappa$sum.n)
	print(paste(
	round((1-alpha)*100,0),"% confidence limits for accuracy:",
	round((kappa$sum.naive-w),4),"...",
	round((kappa$sum.naive+w),4)), quote=F, sep="")
	print("User’s accuracy", quote=F); print(round(kappa$user.naive,4));
	print("Producer’s reliability:", quote=F); print(round(kappa$prod.naive,4));
	print("Summary of kappa statistics", quote=F)
	print(paste("Overall kappa, stdev, & CV%:",
	round(kappa$sum.kappa,4), ",",
	round(sqrt(kappa$sum.kvar),4), ",",
	round((sqrt(kappa$sum.kvar)/kappa$sum.kappa)*1000,0)/10), quote=F)
	w<-ciw(kappa$sum.kvar, kappa$sum.n)
	print(paste(
		round((1-alpha)*100,0),"% confidence limits for kappa:",
		round((kappa$sum.kappa-w),4),"...",
		round((kappa$sum.kappa+w),4)), quote=F, sep="")
		print("Per-class kappa, stdev, & CV%, for user’s accuracy:", quote=F)
		print(round(kappa$user.kappa,4), quote=F);
		print(round(sqrt(kappa$user.kvar),4), quote=F);
		print(round((sqrt(kappa$user.kvar)/kappa$user.kappa)*1000,0)/10, quote=F);
		print("Per-class kappa, stdev, & CV%, for producer’s reliability:", quote=F)
		print(round(kappa$prod.kappa,4), quote=F);
		print(round(sqrt(kappa$prod.kvar),4), quote=F);
		print(round((sqrt(kappa$prod.kvar)/kappa$prod.kappa)*1000,0)/10, quote=F);
}



