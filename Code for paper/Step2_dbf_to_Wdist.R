rm(list=ls())

##Read Functions
CodeSource = "~/Desktop/Chapter 1_Forecast/Code For Publication/" ##Set Code Directory##
FuncName = paste(CodeSource,"Functions.R",sep="")
source(FuncName) 

##Read data
##Set Original Data and Outputs (Filtered Data) Directory##
FileSource = "~/Desktop/Chapter 1_Forecast/dataP/"
OutputSource = "~/Desktop/Chapter 1_Forecast/dataP/"

shpName = paste(FileSource,"LAD_MAY_2023_UK_BGC_V2.dbf",sep="")
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

#Create Wdist
R = 6371 #Radius of the earth in km
Long = DbfFile[,6]
Lat = DbfFile[,7]
N = nrow(DbfFile)
W = matrix(0,N,N)
Dist = matrix(0,N,N)
for (i in 1:N){
  for (j in 1:N){
    Lat_i = deg2rad(Lat[i])
    Lat_j = deg2rad(Lat[j])
    Long_i = deg2rad(Long[i])
    Long_j = deg2rad(Long[j])
    dist_ij = 2*R*asin(sqrt( (sin( (Lat_j-Lat_i)*0.5 ))^2 + 
                            cos(Lat_i)*cos(Lat_j)*(sin((Long_j-Long_i)*0.5))^2 )) #Haversine formula
    W[i,j] = 1/(dist_ij)^2 #each entry equals the inverse of squared distance
    Dist[i,j] = (dist_ij)
  }
  W[i,i] = 0 #Each region has zero distance from itself (avoid infinite value due to 1/0)
}

Wdist = as.matrix(W)
W = as.matrix(W)
W = W/rowSums(W) #Row normalised

W_Name = paste(OutputSource,"Wdist_Normalised.csv",sep="")
Wdist_Name = paste(OutputSource,"Wdist_Original.csv",sep="")
Dist_Name = paste(OutputSource,"W_RealDistance.csv",sep="")

write.csv(W,file = W_Name)
write.csv(Wdist,file = Wdist_Name)
write.csv(Dist,file = Dist_Name)



