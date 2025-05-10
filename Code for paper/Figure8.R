
rm(list=ls())

library(ggplot2)
library(lpirfs)

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












#################Impulse responses###################

dataName2 = paste(FileSource,"dataNew.csv",sep = "")
original_data = read.csv(dataName2)
data2 = original_data
data2 = data2[,-(3:5)]
data2 = cbind(data2,original_data$SalesVolume)
colnames(data2) <- c("group","trend","endog_data","shock","exog_data")

#######Set Shock source from price
data2_pr = data2
data2_pr$shock = original_data$AveragePrice
colnames(data2_pr) <- c("group","trend","Price","HomogeneousEffect","Sales")

IR_data = data.frame(data2_pr)
IRresult = lp_lin_panel(data_set = IR_data,endog_data = "Price", shock = "HomogeneousEffect"
                        , panel_model = "pooling"
                        , l_exog_data = "Sales"
                        ,lags_exog_data = 1, confint = 1.67, hor = 12)
dev.new()
plot(IRresult)


#######Set Shock Data as lagged price
N = max(original_data$group)
Matrix_Y = t(matrix(original_data$AveragePrice,nrow(original_data)/N,N,byrow = TRUE))
W_Name = paste(FileSource,"Wdist_Original.csv",sep = "")
Wold<-read.csv(W_Name,header = T)
W2<-as.matrix(Wold)
W2 = W2[,-1]
W = W2
W = W/rowSums(W)
W = as.matrix(W)
WY = as.vector(W%*%Matrix_Y)

data2_Wpr = data2
data2_Wpr$shock = WY
colnames(data2_Wpr) <- c("group","trend","Price","SpatialEffect","Sales")
IR_data = data.frame(data2_Wpr)
IRresult = lp_lin_panel(data_set = IR_data,endog_data = "Price", shock = "SpatialEffect"
                        , panel_model = "pooling"
                        , l_exog_data = "Sales"
                        ,lags_exog_data = 1, confint = 1.67, hor = 12)
dev.new()
plot(IRresult)


