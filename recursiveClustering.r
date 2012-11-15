library(cluster)
require(graphics)                   
library(clusterSim)                   
library(grDevices)

dummy=read.table("numbers.csv", header=TRUE, sep=';')
bnnumbers=read.table("numbers.csv",header=TRUE,sep=';')

#normalizing duration of calls
numbers=bnnumbers
numbers$numCalls=bnnumbers$numCalls/mean(bnnumbers$numCalls)
numbers$avgduration=bnnumbers$avgduration/mean(bnnumbers$avgduration)
numbers$avgweekend1weekday0=bnnumbers$avgweekend1weekday0/mean(bnnumbers$avgweekend1weekday0)
numbers$avgday1night0 <- bnnumbers$avgday1night0/mean(bnnumbers$avgday1night0)
numbers$avgout1in0 <- bnnumbers$avgout1in0/mean(bnnumbers$avgout1in0)
numbers$avgSMS1 <- bnnumbers$avgSMS1/mean(bnnumbers$avgSMS1)
numbers$avgVoiceCall1 <- bnnumbers$avgVoiceCall1/mean(bnnumbers$avgVoiceCall1)
numbers$avglongDuration1 <- bnnumbers$avglongDuration1/mean(bnnumbers$avglongDuration1)
numtoCluster <- subset(numbers,select=c(2,3,4,5,6,8,9,10))



#Clustering with several cluster centers to determine optimum k
## maxCluster=50
## dbIndex=array(0,dim=c(1,maxCluster))                   
## totwithinss=array(0,dim=c(1,maxCluster))
## betweenss=array(0,dim=c(1,maxCluster))
## for(k in 2:maxCluster){
##   km = kmeans(numtoCluster,k,1000)          
##   curr_dbIndex=index.DB(numtoCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
##   dbIndex[1,k]=curr_dbIndex$DB
##   totwithinss[1,k]=km$tot.withinss
##   betweenss[1,k]=km$betweenss
  
##   str=sprintf("numkmCenterSize%d.csv",k)
##   write.table(cbind(k,km$centers,km$size),str)
##   str=sprintf("numkmWithinssBetweenssDBIndex%d.csv",k)
##   write.table(cbind(k,km$tot.withinss,km$betweenss,curr_dbIndex$DB),str)
  
## }

## # We can judge the clustering quality through the below graphs:
## plot(t(dbIndex))
## plot(t(totwithinss))  
## plot(t(betweenss))

#optimum clusters = 4
km1 = kmeans(numtoCluster,5,1000)
  curr_dbIndex=index.DB(RECnumtoCluster,km1$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex=curr_dbIndex$DB
  totwithinss=km1$tot.withinss
  betweenss=km1$betweenss
  size=km1$size
  centers=km1$centers
jpeg("clusplot0.jpeg")
clusplot(numtoCluster,clus=km1$cluster,color=TRUE,plotchar=TRUE,shade=FALSE,labels=4,lines=0)
dev.off()


##importing the calls table. 
calls92=read.table("calls92.csv",header=TRUE,sep=';')
## clusterCalls=read.table("clusterCalls.csv", header=TRUE, sep=';')
## clusterCalls92=read.table("clusterCalls92.csv", header=TRUE, sep=';')
## clusterCallsNew=read.table("clusterCallsNew.csv", header=TRUE, sep=';')
FinalCalls=read.table("FinalCalls.csv", header=TRUE, sep=';')
calls92 <-  FinalCalls


## ##To be written as a function
## maxiter=10
## for(iter in 1:maxiter){
## dbIndex=array(0,dim=c(1,maxiter))                   
## totwithinss=array(0,dim=c(1,maxiter))
## betweenss=array(0,dim=c(1,maxiter))
## numberslength=dim(numbers)[1]
## clusterInfo=array(0,dim=c(numberslength,4))
## for(i in 1:numberslength){
##   presentmyHashNum=numbers$myHashNum[i]
##   presenttable=calls92[calls92$myHashNum==presentmyHashNum,]
##   length=dim(presenttable)[1]
##   tempClusInfo=array(0,dim=c(1,4))
##   for(j in 1:length){
##     hashNum=presenttable$hashNum[j]
##     tempClusInfo[1,km1$cluster[hashNum]] <- tempClusInfo[1,km1$cluster[hashNum]]+1
##   }
##   clusterInfo[i,] <- tempClusInfo/length
## }
##   str=sprintf("AugmentedTable.csv")
##   write.table(cbind(numbers, clusterInfo),str,sep=';',row.names=FALSE,na="0")
## }
## ##~To be written as a function


##To be written as a function
numberslength=dim(numbers)[1]
clusterInfo=array(0,dim=c(numberslength,5))
colnames(clusterInfo) <- c("dn1","dn2","dn3","dn4","dn5")
for(i in 1:numberslength){
  presentmyHashNum=numbers$myHashNum[i]
  presenttable=calls92[calls92$myHashNum==presentmyHashNum,]
  length=dim(presenttable)[1]
  tempClusInfo=array(0,dim=c(1,5))
  for(j in 1:length){
    hashNum=presenttable$hashNum[j]
    tempClusInfo[1,km1$cluster[hashNum]] <- tempClusInfo[1,km1$cluster[hashNum]]+1
  }
  clusterInfo[i,] <- tempClusInfo/length

}
  center_dim1 <- t(centers[1,])
  center_dim2 <- t(centers[2,])
  center_dim3 <- t(centers[3,])
  center_dim4 <- t(centers[4,])
  center_dim5 <- t(centers[5,])
  write.table(cbind(center_dim1,0,0,0,0,0),"center1.csv",sep=';',append=FALSE,row.names=FALSE,col.names=TRUE)
  write.table(cbind(center_dim2,0,0,0,0,0),"center2.csv",sep=';',append=FALSE,row.names=FALSE,col.names=TRUE)
  write.table(cbind(center_dim3,0,0,0,0,0),"center3.csv",sep=';',append=FALSE,row.names=FALSE,col.names=TRUE)
  write.table(cbind(center_dim4,0,0,0,0,0),"center4.csv",sep=';',append=FALSE,row.names=FALSE,col.names=TRUE)
  write.table(cbind(center_dim5,0,0,0,0,0),"center5.csv",sep=';',append=FALSE,row.names=FALSE,col.names=TRUE)
  str=sprintf("AugmentedTable.csv")
  clusterInfo <- data.frame(clusterInfo)
  write.table(cbind(numbers, clusterInfo),str,sep=';',row.names=FALSE,na="0")
  write.table(cbind(dbIndex, totwithinss, betweenss,t(size)), "clusterQuality.csv", sep=';',append=FALSE, row.names=FALSE, col.names=TRUE)
##~To be written as a function
str=sprintf("CurrCluster%d.jpg",1)
jpeg(str)
plot(clusterInfo$dn1,type="h", ylim=c(0,1.25),lty=1)
lines(clusterInfo$dn2,type="h",col="red",lty=1)
lines(clusterInfo$dn3,type="h",col="blue",lty=1)
lines(clusterInfo$dn4,type="h",col="green",lty=1)
lines(clusterInfo$dn5,type="h",col="green",lty=1)
legend(10,1.28,c("cluster1","cluster2","cluster3","cluster4","cluster5"), col=c("black","red","blue","green","yellow",text.col="green4"),lty=c(1,1,1,1,1))
dev.off()

recurse(1)
ClusterQuality=read.table("clusterQuality.csv",header=TRUE,sep=';')
Center1=read.table("center1.csv",header=TRUE,sep=';')
Center2=read.table("center2.csv",header=TRUE,sep=';')
Center3=read.table("center3.csv",header=TRUE,sep=';')
Center4=read.table("center4.csv",header=TRUE,sep=';')
Center5=read.table("center5.csv",header=TRUE,sep=';')


dummy <- (t(cbind((Center1), (Center2), (Center3), (Center4))))
lastcluster <- (rbind((Center1[20,]),(Center2[20,]),(Center3[20,]),(Center4[20,]),(Center5[20,])))




recurse <- function(count){
  count <- count+1
  if(count>20){break}
RECbnnumbers=read.table("AugmentedTable.csv",header=TRUE,sep=';')
#normalizing duration of calls
RECnumbers=RECbnnumbers
RECnumbers$numCalls=RECbnnumbers$numCalls/mean(RECbnnumbers$numCalls)
RECnumbers$avgduration=RECbnnumbers$avgduration/mean(RECbnnumbers$avgduration)
#choosing columns to cluster
    RECnumtoCluster <- subset(RECnumbers,select=c(2,3,4,5,6,8,9,10,11,12,13,14,15))
#clustering
    km = kmeans(RECnumtoCluster,5,1000)
str=sprintf("clusplot%d.jpg",count)
jpeg(str)
clusplot(RECnumtoCluster,clus=km$cluster,color=TRUE,plotchar=TRUE,shade=FALSE,labels=4,lines=0)
dev.off()
#clusplot(RECnumtoCluster,clus=km$cluster,color=TRUE,plotchar=TRUE,shade=FALSE,labels=4,lines=0)

#some clustering details
  curr_dbIndex=index.DB(RECnumtoCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex=curr_dbIndex$DB
  totwithinss=km$tot.withinss
  betweenss=km$betweenss
  size=km$size
  centers=km$centers
#~some clustering details

#preparing to include cluster details into the present table for the next iteration

#shift these from being arrays to single elements. Also write them out to a file and apend the subsequent values. 
#    dbIndex=array(0,dim=c(1,maxiter))
#    totwithinss=array(0,dim=c(1,maxiter))
#    betweenss=array(0,dim=c(1,maxiter))
#~shift these from being arrays to single elements. Also write them out to a file and apend the subsequent values. 

    RECnumberslength=dim(RECnumbers)[1]
    clusterInfo=array(0,dim=c(RECnumberslength,5))
    colnames(clusterInfo) <- c("dn1","dn2","dn3","dn4","dn5")
#loop to include the present cluster information into the present table for the next iteration
    for(i in 1:RECnumberslength){
        presentmyHashNum=RECnumbers$myHashNum[i]
        presenttable=calls92[calls92$myHashNum==presentmyHashNum,]
        length=dim(presenttable)[1]
        tempClusInfo=array(0,dim=c(1,5))
        for(j in 1:length){
          hashNum=presenttable$hashNum[j]
          tempClusInfo[1,km$cluster[hashNum]] <- tempClusInfo[1,km$cluster[hashNum]]+1
        }
        clusterInfo[i,] <- tempClusInfo/length
    }

#writing current iteration information into files for further reference. 
    str=sprintf("AugmentedTable.csv")
    clusterInfo <- data.frame(clusterInfo)
  center_dim1 <- t(centers[1,])
  center_dim2 <- t(centers[2,])
  center_dim3 <- t(centers[3,])
  center_dim4 <- t(centers[4,])
  center_dim5 <- t(centers[5,])
  write.table(center_dim1,"center1.csv",sep=';',append=TRUE,row.names=FALSE,col.names=FALSE)
  write.table(center_dim2,"center2.csv",sep=';',append=TRUE,row.names=FALSE,col.names=FALSE)
  write.table(center_dim3,"center3.csv",sep=';',append=TRUE,row.names=FALSE,col.names=FALSE)
  write.table(center_dim4,"center4.csv",sep=';',append=TRUE,row.names=FALSE,col.names=FALSE)
  write.table(center_dim5,"center5.csv",sep=';',append=TRUE,row.names=FALSE,col.names=FALSE)
    write.table(cbind(numbers, clusterInfo),str,sep=';',na="0",col.names=TRUE,row.names=FALSE)
    write.table(cbind(dbIndex, totwithinss, betweenss, t(size)), "clusterQuality.csv", sep=';',append=TRUE, row.names=FALSE, col.names=FALSE)
#~writing current iteration information into files for further reference.
# if(count%%100 == 0){
str=sprintf("CurrCluster%d.jpg",count)
jpeg(str)
plot(clusterInfo$dn1,type="h",ylim=c(0,1.25),lty=1)
lines(clusterInfo$dn2,type="h",col="red",lty=1)
lines(clusterInfo$dn3,type="h",col="blue",lty=1)
lines(clusterInfo$dn4,type="h",col="green",lty=1)
lines(clusterInfo$dn5,type="h",col="green",lty=1)
legend(10,1.28,c("cluster1","cluster2","cluster3","cluster4","cluster5"), col=c("black","red","blue","green","yellow",text.col="green4"),lty=c(1,1,1,1,1))
dev.off()
#}
recurse(count)
}

## par(ask=TRUE)

## recurse(1,2)

## recurse <- function(a,b){
## a+b
## write.table(a+b,"yo.csv",append=TRUE, row.names=FALSE, col.names=FALSE)
## if(a+b>23){
##   break}
## recurse(a,a+b)
## }

