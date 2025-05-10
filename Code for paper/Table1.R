rm(list=ls())

##Read Functions
CodeSource = "~/Desktop/Chapter 1_Forecast/code/New Code/Code For Publication/" ##Set Code Directory##
FuncName = paste(CodeSource,"Functions.R",sep="")
source(FuncName) 

##Read data
##Set Data and Outputs Directory##
FileSource = "~/Desktop/Chapter 1_Forecast/data/"
OutputSource = "~/Desktop/Chapter 1_Forecast/results/"
dataName = paste(FileSource,"dataNew.csv",sep = "")
data = read.csv(dataName)
Yold = as.matrix(data$AveragePrice)
Xold = as.matrix(data$SalesVolume)
dim(Xold)

T<-max(as.numeric(data$trend))
N = max(data$group)

Ymat = t(matrix(Yold,nrow(Yold)/N,N,byrow = TRUE))
Xmat = t(matrix(Xold,nrow(Xold)/N,N,byrow = TRUE))

LADcodeName = paste(FileSource,"Lad_Region_Code.csv",sep = "")
regionalcode<-read.csv(file = LADcodeName,header = T)
code<-as.matrix(regionalcode)
region<-code[,2]
Ladcode = code[,1]
region<-as.numeric(region)
regioncode<-c (1,2,3,4, 5, 6, 7,8, 9,10,11)

#Y statistics
Y_results = matrix(0,15,6)
panel = Ymat
for (r in 1:15){
  
  if (r < 12) {
    rss = which(region==r)
  } else {
    
    if (r == 12) {
      c_north = which(region==1 | region==2 | region==3 | region==11)
      rss = c_north
    }
    
    if (r == 13) {
      c_middle = which(region==4 | region==5 | region==10)
      rss = c_middle
    }
    
    if (r == 14) {
      c_south = which(region==6 | region==8 | region==9)
      rss = c_south
    }
    
    if (r == 15) {
      rss = which(region==region)
    }
    
  }
  N = length(rss)
  Mean = round(mean(panel[rss,]),6)
  SD = round(sd(panel[rss,]),6)
  Max = round(max(as.vector(panel[rss,])),4)
  Min = round(min(as.vector(panel[rss,])),4)
  
  panel_rss = panel[rss,]
  test = matrix(0,nrow(panel_rss),1)
  for (i in 1:nrow(panel_rss)){
  p = panel_rss[i,]
  if (pp.test(p)$p.value<0.05){
    test[i] = 1
  }
  }
  test = sum(test)
  
  Y_results[r,] = c(N,Mean,SD,Max,Min,test)
  
}
  colnames(Y_results) = c("N","Mean","SD","Max","Min","pptest")
  
  
  #X statistics
  X_results = matrix(0,15,6)
  panel = Xmat
  for (r in 1:15){
    
    if (r < 12) {
      rss = which(region==r)
    } else {
      
      if (r == 12) {
        c_north = which(region==1 | region==2 | region==3 | region==11)
        rss = c_north
      }
      
      if (r == 13) {
        c_middle = which(region==4 | region==5 | region==10)
        rss = c_middle
      }
      
      if (r == 14) {
        c_south = which(region==6 | region==8 | region==9)
        rss = c_south
      }
      
      if (r == 15) {
        rss = which(region==region)
      }
      
    }
    N = length(rss)
    Mean = round(mean(panel[rss,]),6)
    SD = round(sd(panel[rss,]),6)
    Max = round(max(as.vector(panel[rss,])),4)
    Min = round(min(as.vector(panel[rss,])),4)
    
    panel_rss = panel[rss,]
    test = matrix(0,nrow(panel_rss),1)
    for (i in 1:nrow(panel_rss)){
      p = panel_rss[i,]
      if (pp.test(p)$p.value<0.05){
        test[i] = 1
      }
    }
    test = sum(test)
    
    X_results[r,] = c(N,Mean,SD,Max,Min,test)
    
  }
  colnames(X_results) = c("N","Mean","SD","Max","Min","pptest")
  
  
################################Outputs################################
  Y_stats_name = paste(OutputSource,"Y_DataStats.csv",sep="")
  X_stats_name = paste(OutputSource,"X_DataStats.csv",sep="")
  write.csv(Y_results,file = Y_stats_name)
  write.csv(X_results,file = X_stats_name)
  
    
    
    