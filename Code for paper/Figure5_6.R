
rm(list=ls())

library(geometry)
library(forecast)
library(tictoc)
library(abind)
library(magic)
library(car)
library(carData)
library(Matrix) #for use of rsparsematrix()
library(plm) #for panel data analysis
library(MASS)
library(psych)# for tr() function which calculates the trace of a matrix
library(lmtest)
library(sandwich)
library(survival)
library(AER)# for IV estimation
library(gsynth) ##For iterFE
library(foreign) #dbf file
library(gsynth) ##For iterFE
library(foreign) #dbf file
library(pracma) #haversine
library(highcharter)
library(reshape2)
library(gplots)
library(RColorBrewer)

###################################Set Initial Values###################################
Qt = c(50,62.5,75,87.5,100) #Quantile
###################################Read Functions#####################################
CodeSource = "~/Desktop/Chapter 1_Forecast/code/New Code/Code For Publication/" ##Set Code Directory##
FuncName = paste(CodeSource,"Functions.R",sep="")
source(FuncName) 
###################################Read Data#####################################
### Set Original Data and Outputs Directory
##Data
FileSource = "~/Desktop/Chapter 1_Forecast/data/" 
##Output
#This code could only save plots manually
########################################################################################


















#########################################Program Start###############################################

getW<-function(Dist_Limit,DbfFile)
{
  #Create Wdist
  R = 6371 #Radius of the earth in km
  #R = 3958.8 #Radius of the earth in miles
  Long = DbfFile[,6]
  Lat = DbfFile[,7]
  N = nrow(DbfFile)
  W = matrix(0,N,N)
  Dist = matrix(0,N,N)
  Dist_q = matrix(0,N,N)
  for (i in 1:N){
    for (j in 1:N){
      Lat_i = deg2rad(Lat[i])
      Lat_j = deg2rad(Lat[j])
      Long_i = deg2rad(Long[i])
      Long_j = deg2rad(Long[j])
      Dist[i,j] = 2*R*asin(sqrt( (sin( (Lat_j-Lat_i)*0.5 ))^2 + 
                                   cos(Lat_i)*cos(Lat_j)*(sin((Long_j-Long_i)*0.5))^2 )) #Haversine formula
      if (Dist[i,j]<=Dist_Limit){
        W[i,j] = 1/(Dist[i,j])^2 #each entry equals the inverse of squared distance
        Dist_q[i,j] = Dist[i,j]
      } else { 
        W[i,j] = 0 
        Dist_q[i,j] = 0
        }
    }
    W[i,i] = 0 #Each region has zero distance from itself (avoid infinite value due to 1/0)
  }
  
  #Row normalised
  for (i in 1:N){
    if (sum(W[i,])==0){
      W[i,] = 0
    } else { W[i,] = W[i,]/sum(W[i,]) }
  }
  W = as.matrix(W)
  
  return(list(DistMatrix = Dist_q, W = W))
}

shpName = paste(FileSource,"Local_Authority_Districts_May_2023_Boundaries_UK_BGC/LAD_MAY_2023_UK_BGC_V2.dbf",sep="")
DbfFile = read.dbf(shpName)

CodeName = paste(FileSource,"Lad_Region_Code.csv",sep="")
LADcode = read.csv(file = CodeName)

#Find out the location of non-existed regions , and delete them
del = 0
for (q in 1:nrow(DbfFile)){
  s = which(DbfFile[q,1] == LADcode[,1])
  if (identical(s, integer(0))){
    del = c(del, q)
  }
}
del = del[-1]
DbfFile = DbfFile[-del,]

Dist = as.matrix(as.vector(getW(100000,DbfFile)$DistMatrix))
Del = which(Dist==0)
Dist = Dist[-Del]
Dist = as.matrix(sort(Dist))

for (Sparsity in 1:length(Qt)){
  Spt = Qt[Sparsity]*0.01
  Quan = quantile(Dist,Spt)
  W = getW(Quan,DbfFile)$DistMatrix
  rownames(W) = LADcode[,1]
  colnames(W) = LADcode[,1]
  
  hmcol <- function(n){
    colfun1 <- colorRampPalette(rev(brewer.pal(9, "Reds")))
    colfun2 <- colorRampPalette(brewer.pal(9, "Reds"))
    p <- (0 - min(W)) / (max(W) - min(W))
    gap <- n * p
    c(colfun1(floor(gap)), colfun2(n - gap))
  }
  
  dev.new()
  hmap = heatmap.2(W, trace = "none", symkey = F, symbreaks = F, col = hmcol(345),density.info="none")
}




