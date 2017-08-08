


install.packages("data.table")
library("data.table")
library("tidyr")
library("readr")

pollutantmean <- function(directory, pollutant, id = 1:332){
  
  datall <- read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/001.csv",sep=""))
  datall<- datall[0,]
  for (i in id){
    if (i < 10) {
      datall <- rbind(datall,read.csv(paste(directory,"/00", i,".csv", sep="")))
    }
    if (i < 100 & i > 9){
      datall <- rbind(datall,read.csv(paste(directory,"/0", i,".csv", sep="")))  
    }
    if (i >99){
      datall <- rbind(datall,read.csv(paste(directory,"/", i,".csv", sep="")))  
    }  
  }
  
  colMeans(datall[pollutant],na.rm= TRUE)
}


pollutantmean(directory="C:/Users/Yigong Zhou/Desktop/specdata", pollutant= "nitrate")

pollutantmean(directory="C:/Users/Yigong Zhou/Desktop/specdata", pollutant="sulfate",id = 34)

complete <- function(directory, id= 1:332){
  datall <- read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/001.csv",sep=""))
  datall<- datall[0,]
  for (i in id){
    if (i < 10) {
      datall <- rbind(datall,read.csv(paste(directory,"/00", i,".csv", sep="")))
    }
    if (i < 100 & i > 9){
      datall <- rbind(datall,read.csv(paste(directory,"/0", i,".csv", sep="")))  
    }
    if (i >99){
      datall <- rbind(datall,read.csv(paste(directory,"/", i,".csv", sep="")))  
    }  
  }
  datall<-datall[complete.cases(datall),]
  com <- data.frame(comid=vector(),nobs=numeric())
  
  for (i in id){
    new<-c(i,nrow(datall[which(datall$ID==i),]))
    com<-rbind(com,new)
  }
  com
}
cc <- complete("C:/Users/Yigong Zhou/Desktop/specdata",54)

set.seed(42)
cc <- complete("C:/Users/Yigong Zhou/Desktop/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, 2])

corr<- function(directory, threshold = 0){

  
  datall <- read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/001.csv",sep=""))
  datall<- datall[0,]
  for (i in 1:332){
    if (i < 10) {
      datall <- rbind(datall,read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/00", i,".csv", sep="")))
    }
    if (i < 100 & i > 9){
      datall <- rbind(datall,read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/0", i,".csv", sep="")))  
    }
    if (i >99){
      datall <- rbind(datall,read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/", i,".csv", sep="")))  
    }  
  }
  datall<-datall[complete.cases(datall),]

  
  datall$count<-as.numeric(ave(datall$ID,datall$ID,FUN = length))
  
  corvec<-numeric()
  uniqid<-numeric()
  

  for (i in as.numeric(unique(datall[datall$count>threshold,]$ID))){
    corvec[i]<-as.numeric(cor(datall[datall$ID==i,][,2:3])[2,1])
  }
  
  corvec
}

cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/", 2000)                
n <- length(cr)                
cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
