rm(list=ls())


###################################Set Initial Values###################################
Roll = 60 #Size of rolling windows
MaxLags = 6 #Size of max lag (We choose maxlag=6 here, then it will run loops from lag 1 to lag 6)
PreTime = 1:6 #Set up 1-month to T-month ahead forecast (We set T=6 here)
MaxPreTime = max(PreTime)
###################################Read Functions#####################################
CodeSource = "~/Desktop/Chapter 1_Forecast/Code For Publication/" ##Set Code Directory##
FuncName = paste(CodeSource,"Functions.R",sep="")
source(FuncName) 
###################################Read Data#####################################
### Set Original Data and Outputs Directory
##Data
FileSource = "~/Desktop/Chapter 1_Forecast/dataP/" 
##Output
OutputSource = "~/Desktop/Chapter 1_Forecast/results/"
########################################################################################










#########################################Program Start###############################################
dataName = paste(FileSource,"dataNew.csv",sep = "")
data = read.csv(dataName)
Yold = as.matrix(data$AveragePrice)
Xold = as.matrix(data$SalesVolume)
dim(Xold)


#Time periods
T<-max(as.numeric(data$trend))
N = max(data$group)

W_Name = paste(FileSource,"Wdist_Original.csv",sep = "")
Wold<-read.csv(W_Name,header = T)
W2<-as.matrix(Wold)
W2 = W2[,-1]

Yold = as.matrix(Yold)
Ymat = t(matrix(Yold,nrow(Yold)/N,N,byrow = TRUE))
W = W2
W = W/rowSums(W)
W = as.matrix(W)
#############
Xold = as.matrix(Xold)
nvar = ncol(Xold)
Xmat = vector("list",nvar)
for (i in 1:nvar){
  xx = t(matrix(Xold[,i],nrow(Xold)/N,N,byrow = TRUE))
  Xmat[[i]] = as.matrix(as.vector(xx))
}
Xold = as.matrix(Xmat[[1]])
if (nvar>1){
  for (i in 2:nvar){
    Xold = cbind(Xold, Xmat[[i]])
  }
}

tic("time")

ModelResiduals_RMSE = vector("list",MaxLags)
for (ll in 1:MaxLags){
  ModelResiduals_RMSE[[ll]] = vector("list",length(PreTime))
}

ModelResiduals_ReMSPE = vector("list",MaxLags)
for (ll in 1:MaxLags){
  ModelResiduals_ReMSPE[[ll]] = vector("list",length(PreTime))
}

ModelResiduals_ReMSPE_Individual = vector("list",MaxLags)
for (ll in 1:MaxLags){
  ModelResiduals_ReMSPE_Individual[[ll]] = vector("list",length(PreTime))
}

###############################Start the loop#######################################
for (Lags in 1:MaxLags){

#Set 'tt' as the total number of rolling windows/loops
tt = (ncol(Ymat)-Roll+1-MaxPreTime)

#Initialise coefficients
NARcoef = vector("list",tt)
for (ct in 1:tt){ NARcoef[[ct]] = as.matrix(matrix(0,N,1+Lags*2)) }
NARXcoef = vector("list",tt)
for (ct in 1:tt){ NARXcoef[[ct]] = as.matrix(matrix(0,N,1+(nvar+2)*Lags)) }
ARpcoef = vector("list",tt)
for (ct in 1:tt){ ARpcoef[[ct]] = as.matrix(matrix(0,N,1+Lags)) }
ARXpcoef = vector("list",tt)
for (ct in 1:tt){ ARXpcoef[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }
ARcoef = vector("list",tt)
for (ct in 1:tt){ ARcoef[[ct]] = as.matrix(matrix(0,N,1+Lags))}
PDcoef = vector("list",tt)
for (ct in 1:tt){ PDcoef[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }
ARMGcoef = vector("list",tt)
for (ct in 1:tt){ ARMGcoef[[ct]] = as.matrix(matrix(0,N,1+Lags))}
PDMGcoef = vector("list",tt)
for (ct in 1:tt){ PDMGcoef[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }

#Initialise predicted results Yhat
Ynar_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )
Ynarx_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )
Yarp_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )
Yarxp_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )
Yar_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )
Ypd_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )
Yarmg_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )
Ypdmg_hat = matrix(0,N,(ncol(Ymat)-Roll+1-MaxPreTime) )

#Initialise Std.Errors
NAR_SE = vector("list",tt)
for (ct in 1:tt){ NAR_SE[[ct]] = as.matrix(matrix(0,N,1+Lags*2)) }
NARX_SE = vector("list",tt)
for (ct in 1:tt){ NARX_SE[[ct]] = as.matrix(matrix(0,N,1+(nvar+2)*Lags)) }
ARp_SE = vector("list",tt)
for (ct in 1:tt){ ARp_SE[[ct]] = as.matrix(matrix(0,N,1+Lags)) }
ARXp_SE = vector("list",tt)
for (ct in 1:tt){ ARXp_SE[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }
AR_SE = vector("list",tt)
for (ct in 1:tt){ AR_SE[[ct]] = as.matrix(matrix(0,N,1+Lags))}
ARMG_SE = vector("list",tt)
for (ct in 1:tt){ ARMG_SE[[ct]] = as.matrix(matrix(0,N,1+Lags))}
PD_SE = vector("list",tt)
for (ct in 1:tt){ PD_SE[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }
PDMG_SE = vector("list",tt)
for (ct in 1:tt){ PDMG_SE[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }

#Initialise p-values
NARpvalue = vector("list",tt)
for (ct in 1:tt){ NARpvalue[[ct]] = as.matrix(matrix(0,N,1+Lags*2)) }
NARXpvalue = vector("list",tt)
for (ct in 1:tt){ NARXpvalue[[ct]] = as.matrix(matrix(0,N,1+(nvar+2)*Lags)) }
ARppvalue = vector("list",tt)
for (ct in 1:tt){ ARppvalue[[ct]] = as.matrix(matrix(0,N,1+Lags)) }
ARXppvalue = vector("list",tt)
for (ct in 1:tt){ ARXppvalue[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }
ARpvalue = vector("list",tt)
for (ct in 1:tt){ ARpvalue[[ct]] = as.matrix(matrix(0,N,1+Lags))}
ARMGpvalue = vector("list",tt)
for (ct in 1:tt){ ARMGpvalue[[ct]] = as.matrix(matrix(0,N,1+Lags))}
PDpvalue = vector("list",tt)
for (ct in 1:tt){ PDpvalue[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }
PDMGpvalue = vector("list",tt)
for (ct in 1:tt){ PDMGpvalue[[ct]] = as.matrix(matrix(0,N,1+(nvar+1)*Lags)) }

#################################################################################
Yreal = vector("list",length(PreTime)) #Initialise Out-Sample real value for different PreTime
for (p in 1:length(PreTime)){
Yreal[[p]] = matrix(0,N,1) 
}
Miu = matrix(0,N,1) #Initialise Mean Avarage of Rolling In-Sample for calculating ReMSPE
for (nn in 1:tt)
{
  ########Print the loop code progress
  Str = as.character(nn)
  NN = ncol(Ymat)-Roll+1-MaxPreTime
  StrT = as.character(NN)
  StrL = as.character(Lags)
  TS = c("MaxLags",MaxLags,"Lags",Lags,"TotalTime",StrT,"LoopTime",Str)
  print(TS)
  
  ########Initialise some values
  Y1 = as.matrix(as.vector( Ymat[,nn:(nn+Roll-1)] ) ) # (NT*1 matrix) dependent variable Y
  Y = Ymat[,nn:(nn+Roll-1)] # Y: (N*T matrix) Dependent Variable Y
  
  #Store 'PreTime'-month ahead real value of dependent variable Y
  for (p in 1:length(PreTime)){
    Yr = as.matrix(Ymat[,nn+Roll-1+PreTime[p]]) 
    Yreal[[p]] = cbind(Yreal[[p]],Yr)
  }
  
  #Store Mean Avarage of Rolling In-Sample for calculating ReMSPE
  u = rowSums(Y)
  u = u/ncol(Y)
  Miu = cbind(Miu,u) 
  
  # (NT*nvar matrix) The Combination of Explanatory Variables X
  Xold = as.matrix(Xold)
  nvar = ncol(Xold)
  Xmat1 = vector("list",nvar)
  for (i in 1:nvar){
    xx = t(matrix(Xold[,i],nrow(Xold)/N,N,byrow = TRUE))
    xx = xx[,nn:(nn+Roll-1)]
    Xmat1[[i]] = as.matrix(as.vector(xx))
  }
  X = as.matrix(Xmat1[[1]]) # Initialise an NT*1 matrix of explanatory variable of X1
  if (nvar>1){
    for (i in 2:nvar){
      X = cbind(X, Xmat1[[i]]) # X: Combine X1,X2...Xn into an NT*nvar matrix of explanatory variable of Xn
    }
  }
  
  ########Get Regression Results for NAR and NAR-X
  #NAR model
  train = getX_noX(Y, W, N, Lags) #train = NAR_noX(Y,W,Lags)
  reg = lm(Y~., data = train) # NAR regression
  summary(reg)
  Int = summary(reg)$coefficients[1] # Get NAR regression intercept
  nr = nrow(as.matrix(NARcoef[[nn]][,1]))
  NARcoef[[nn]][,1] = as.matrix( rep(Int, nr) ) # Replicate N same intercept 'Int' for N LADs, and store them
  pInt = summary(reg)$coefficients[1+ncol(NARpvalue[[nn]])*3] # Get p-value of NAR regression intercept
  nr = nrow(as.matrix(NARpvalue[[nn]][,1]))
  NARpvalue[[nn]][,1] = as.matrix( rep(pInt, nr) ) # Replicate N same p-value of 'pInt' for N LADs, and store them
  Int_SE = summary(reg)$coefficients[1+ncol(NAR_SE[[nn]])*1] # Get SE of NAR regression intercept
  nr = nrow(as.matrix(NAR_SE[[nn]][,1]))
  NAR_SE[[nn]][,1] = as.matrix( rep(Int_SE, nr) ) # Replicate N same SE of 'Int_SE' for N LADs, and store them
  for (c in 2:ncol(train)){
    Ncoef = summary(reg)$coefficients[c] # Get NAR regression coefficient for 'c'th regressor
    nr = nrow(as.matrix(NARcoef[[nn]][,c]))
    NARcoef[[nn]][,c] = as.matrix( rep(Ncoef, nr) ) # Replicate N same coefficient 'Ncoef' for N LADs, and store them
    Np = summary(reg)$coefficients[c+ncol(NARpvalue[[nn]])*3] # Get p-value of NAR regression coefficient for 'c'th regressor
    nr = nrow(as.matrix(NARpvalue[[nn]][,c]))
    NARpvalue[[nn]][,c] = as.matrix( rep(Np, nr) ) # Replicate N same p-value of 'Np' for N LADs, and store them
    N_SE = summary(reg)$coefficients[c+ncol(NAR_SE[[nn]])*1] # Get SE of NAR regression coefficient for 'c'th regressor
    nr = nrow(as.matrix(NAR_SE[[nn]][,c]))
    NAR_SE[[nn]][,c] = as.matrix( rep(N_SE, nr) ) # Replicate N same SE of 'N_SE' for N LADs, and store them
  }
  
  #NAR-X model
  trainX = getX(X, Y, W, N, Lags)
  regX = lm(Y~., data = trainX) # NAR-X regression
  summary(regX)
  Intx = summary(regX)$coefficients[1] # Get NAR-X regression intercept
  nr_x = nrow(as.matrix(NARXcoef[[nn]][,1]))
  NARXcoef[[nn]][,1] = as.matrix( rep(Intx, nr_x) ) # Replicate N same intercept 'Intx' for N LADs, and store them
  pxInt = summary(regX)$coefficients[1+ncol(NARXpvalue[[nn]])*3] # Get p-value of NAR-X regression intercept
  nr_x = nrow(as.matrix(NARXpvalue[[nn]][,1]))
  NARXpvalue[[nn]][,1] = as.matrix( rep(pxInt, nr_x) ) # Replicate N same p-value of 'pxInt' for N LADs, and store them
  Intx_SE = summary(regX)$coefficients[1+ncol(NARX_SE[[nn]])*1] # Get SE of NAR-X regression intercept
  nr_x = nrow(as.matrix(NARX_SE[[nn]][,1]))
  NARX_SE[[nn]][,1] = as.matrix( rep(Intx_SE, nr_x) ) # Replicate N same SE of 'Intx_SE' for N LADs, and store them
  for (c in 2:ncol(trainX)){
    NXcoef = summary(regX)$coefficients[c] # Get NAR-X regression coefficient for 'c'th regressor
    nr_x = nrow(as.matrix(NARXcoef[[nn]][,c]))
    NARXcoef[[nn]][,c] = as.matrix( rep(NXcoef, nr_x) ) # Replicate N same coefficient 'NXcoef' for N LADs, and store them
    Nxp = summary(regX)$coefficients[c+ncol(NARXpvalue[[nn]])*3] # Get p-value of NAR-X regression coefficient for 'c'th regressor
    nr_x = nrow(as.matrix(NARXpvalue[[nn]][,c]))
    NARXpvalue[[nn]][,c] = as.matrix( rep(Nxp, nr_x) ) # Replicate N same p-value of 'NXp' for N LADs, and store them
    Nx_SE = summary(regX)$coefficients[c+ncol(NARX_SE[[nn]])*1] # Get SE of NAR-X regression coefficient for 'c'th regressor
    nr_x = nrow(as.matrix(NARX_SE[[nn]][,c]))
    NARX_SE[[nn]][,c] = as.matrix( rep(Nx_SE, nr_x) ) # Replicate N same SE of 'NX_SE' for N LADs, and store them
  }
  
  ########Get Predicted Results for NAR and NAR-X (Under specific lag and PreTime; the 'nn'th rolling/iteration)
  #Xpred and Ypred
  Xpred = X #Xpred: (NT*nvar matrix) A combination of vactorised In-Sample Data of explanatory variables (For the use of generating Predicted Data)
  Ypred = Y #Ypred: (N*T matrix) A single matrix of In-Sample Data of dependent variable (For the use of generating Predicted Data)
  
  #Step 1. Combine 'lag Y', 'lag WY' and 'lag X' together into In-sample Regression Data
  wyy = W%*%Ypred
  yy = as.matrix(as.vector(Ypred))
  wyy = as.matrix(as.vector(wyy))
  n_pred = cbind(yy,wyy) #In-sample Regression Data without explanatory variables (For NAR)
  nx_pred = cbind(yy,wyy,Xpred) #In-sample Regression Data with explanatory variables (For NAR-X)
    
  #Step 2. Generating lags and create In-Sample Predicted Data
  pred_cbind = n_pred[(nrow(n_pred) - N*Lags+1):(nrow(n_pred)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (without X)
  x_pred_cbind = nx_pred[(nrow(nx_pred) - N*Lags+1):(nrow(nx_pred)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (with X)
  if (Lags>1) {
  for (l in 2:Lags){
      sub_n_pred = n_pred[(nrow(n_pred) - N*(Lags-l+1)+1):(nrow(n_pred)-N*(Lags-l)),] #Lag l sample (without X)
      sub_nx_pred = nx_pred[(nrow(nx_pred) - N*(Lags-l+1)+1):(nrow(nx_pred)-N*(Lags-l)),] #Lag l sample (with X)
      pred_cbind = cbind(pred_cbind,sub_n_pred)
      x_pred_cbind = cbind(x_pred_cbind,sub_nx_pred)
  }
  }
    
  Intercept = matrix(1,N,1)
  n_pred = cbind(Intercept,pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
  nx_pred = cbind(Intercept,x_pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
    
  Ynar_hat[,nn] = dot(t(NARcoef[[nn]]),t(n_pred)) #Store Predicted Results for all LADs by using NAR regression coefficients
  Ynarx_hat[,nn] = dot(t(NARXcoef[[nn]]),t(nx_pred)) #Store Predicted Results for all LADs by using NAR-X regression coefficients
    
  ########Get Regression Results for ARpool and AR-Xpool
  #ARpool model
  train_arp = getX_noX(Y, W, N, Lags, Wexsit = FALSE)
  reg_arp = lm(Y~., data = train_arp) #ARpool regression
  summary(reg_arp)
  Int_arp = summary(reg_arp)$coefficients[1] # Get ARpool regression intercept
  nr_arp = nrow(as.matrix(ARpcoef[[nn]][,1]))
  ARpcoef[[nn]][,1] = as.matrix( rep(Int_arp, nr_arp) ) # Replicate N same intercept 'Int_arp' for N LADs, and store them
  pInt_arp = summary(reg_arp)$coefficients[1+ncol(ARppvalue[[nn]])*3] # Get p-value of ARpool regression intercept
  nr_arp = nrow(as.matrix(ARppvalue[[nn]][,1]))
  ARppvalue[[nn]][,1] = as.matrix( rep(pInt_arp, nr_arp) ) # Replicate N same p-value of 'pInt_arp' for N LADs, and store them
  Int_arp_SE = summary(reg_arp)$coefficients[1+ncol(ARp_SE[[nn]])*1] # Get SE of ARpool regression intercept
  nr_arp = nrow(as.matrix(ARp_SE[[nn]][,1]))
  ARp_SE[[nn]][,1] = as.matrix( rep(Int_arp_SE, nr_arp) ) # Replicate N same SE of 'Int_arp_SE' for N LADs, and store them
  for (c in 2:ncol(train_arp)){
    coef_arp = summary(reg_arp)$coefficients[c] # Get ARpool regression coefficient for 'c'th regressor
    nr_arp = nrow(as.matrix(ARpcoef[[nn]][,c]))
    ARpcoef[[nn]][,c] = as.matrix( rep(coef_arp, nr_arp) ) # Replicate N same coefficient 'coef_arp' for N LADs, and store them
    p_arp = summary(reg_arp)$coefficients[c+ncol(ARppvalue[[nn]])*3] # Get p-value of ARpool regression coefficient for 'c'th regressor
    nr_arp = nrow(as.matrix(ARppvalue[[nn]][,c]))
    ARppvalue[[nn]][,c] = as.matrix( rep(p_arp, nr_arp) ) # Replicate N same p-value of 'p_arp' for N LADs, and store them
    SE_arp = summary(reg_arp)$coefficients[c+ncol(ARp_SE[[nn]])*1] # Get SE of ARpool regression coefficient for 'c'th regressor
    nr_arp = nrow(as.matrix(ARp_SE[[nn]][,c]))
    ARp_SE[[nn]][,c] = as.matrix( rep(SE_arp, nr_arp) ) # Replicate N same SE of 'SE_arp' for N LADs, and store them
  }
  
  #AR-Xpool model
  trainX_arp = getX(X, Y, W, N, Lags, Wexsit = FALSE) 
  regX_arp = lm(Y~., data = trainX_arp) #AR-Xpool regression
  summary(regX_arp)
  Intx_arp = summary(regX_arp)$coefficients[1] # Get AR-Xpool regression intercept
  nr_x_arp = nrow(as.matrix(ARXpcoef[[nn]][,1]))
  ARXpcoef[[nn]][,1] = as.matrix( rep(Intx_arp, nr_x_arp) ) # Replicate N same intercept 'Intx_arp' for N LADs, and store them
  pxInt_arp = summary(regX_arp)$coefficients[1+ncol(ARXppvalue[[nn]])*3] # Get p-value of AR-Xpool regression intercept
  nr_x_arp = nrow(as.matrix(ARXppvalue[[nn]][,1]))
  ARXppvalue[[nn]][,1] = as.matrix( rep(pxInt_arp, nr_x_arp) ) # Replicate N same p-value of 'pxIntx_arp' for N LADs, and store them
  xInt_arp_SE = summary(regX_arp)$coefficients[1+ncol(ARXp_SE[[nn]])*1] # Get SE of AR-Xpool regression intercept
  nr_x_arp = nrow(as.matrix(ARXp_SE[[nn]][,1]))
  ARXp_SE[[nn]][,1] = as.matrix( rep(xInt_arp_SE, nr_x_arp) ) # Replicate N same SE of 'Intx_arp' for N LADs, and store them
  for (c in 2:ncol(trainX_arp)){
    Xcoef_arp = summary(regX_arp)$coefficients[c] # Get AR-Xpool regression coefficient for 'c'th regressor
    nr_x_arp = nrow(as.matrix(ARXpcoef[[nn]][,c]))
    ARXpcoef[[nn]][,c] = as.matrix( rep(Xcoef_arp, nr_x_arp) ) # Replicate N same coefficient 'Xcoef_arp' for N LADs, and store them
    xp_arp = summary(regX_arp)$coefficients[c+ncol(ARXppvalue[[nn]])*3] # Get p-value of AR-Xpool regression coefficient for 'c'th regressor
    nr_x_arp = nrow(as.matrix(ARXppvalue[[nn]][,c]))
    ARXppvalue[[nn]][,c] = as.matrix( rep(xp_arp, nr_x_arp) ) # Replicate N same p-value of 'xp_arp' for N LADs, and store them
    xSE_arp = summary(regX_arp)$coefficients[c+ncol(ARXp_SE[[nn]])*1] # Get SE of AR-Xpool regression coefficient for 'c'th regressor
    nr_x_arp = nrow(as.matrix(ARXp_SE[[nn]][,c]))
    ARXp_SE[[nn]][,c] = as.matrix( rep(xSE_arp, nr_x_arp) ) # Replicate N same SE of 'xSE_arp' for N LADs, and store them
  }
  
  ########Get Predicted Results for ARpool and AR-Xpool (Under specific lag and PreTime; the 'nn'th rolling/iteration)
  Xpred = X #Xpred: (NT*nvar matrix) A combination of vactorised In-Sample Data of explanatory variables (For the use of generating Predicted Data)
  Ypred = Y #Ypred: (N*T matrix) A single matrix of In-Sample Data of dependent variable (For the use of generating Predicted Data)
  
  #Step 1. Combine 'lag Y' and 'lag X' together into In-sample Regression Data
  yy = as.matrix(as.vector(Ypred))
  n_pred_arp = yy
  nx_pred_arp = cbind(yy,Xpred)
    
  #Step 2. Generating lags and create In-Sample Predicted Data
  pred_cbind = n_pred_arp[(nrow(n_pred_arp) - N*Lags+1):(nrow(n_pred_arp)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (without X)
  x_pred_cbind = nx_pred_arp[(nrow(nx_pred_arp) - N*Lags+1):(nrow(nx_pred_arp)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (with X)
    if (Lags>1) {
    for (l in 2:Lags){
      sub_n_pred_arp = n_pred_arp[(nrow(n_pred_arp) - N*(Lags-l+1)+1):(nrow(n_pred_arp)-N*(Lags-l)),] #Initialise a lag l In-Sample Predicted Data (without X)
      sub_nx_pred_arp = nx_pred_arp[(nrow(nx_pred_arp) - N*(Lags-l+1)+1):(nrow(nx_pred_arp)-N*(Lags-l)),] #Initialise a lag l In-Sample Predicted Data (with X)
      pred_cbind = cbind(pred_cbind,sub_n_pred_arp)
      x_pred_cbind = cbind(x_pred_cbind,sub_nx_pred_arp)
    }
    }
  
    Intercept = matrix(1,N,1)
    n_pred_arp = cbind(Intercept,pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
    nx_pred_arp = cbind(Intercept,x_pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
    
    Yarp_hat[,nn] = dot(t(ARpcoef[[nn]]),t(n_pred_arp)) #Store Predicted Results for all LADs by using ARpool regression coefficients
    Yarxp_hat[,nn] = dot(t(ARXpcoef[[nn]]),t(nx_pred_arp)) #Store Predicted Results for all LADs by using AR-Xpool regression coefficients

  
  ########Get Regression Results for AR and AR-X
  
  ####Start a loop over N LADs for Heterogeneous Model
  for (n in 1:nrow(Y)){
    Yar = Y[n,] #Select 'n'th LAD's Y data
    
  #Heterogeneous AR model
    rega = arima(Yar,order = c(Lags,0,0),method = "CSS") #AR regression
    #Get coeffcients for AR model
    Acoef = t(as.matrix(as.numeric(rega$coef)))
    Acoef = cbind(Acoef[length(Acoef)],Acoef)
    Acoef = Acoef[-length(Acoef)]
    #Get p-values for AR model
    Ap = t(as.matrix((1-pnorm(abs(rega$coef)/sqrt(diag(rega$var.coef)/N)))*2))
    Ap = cbind(Ap[length(Ap)],Ap)
    Ap = Ap[-length(Ap)]
    #Get SE for AR model
    A_SE = t(sqrt(diag(rega$var.coef)))
    A_SE = cbind(A_SE[length(A_SE)],A_SE)
    A_SE = A_SE[-length(A_SE)]
    #Store coeffcients, p-values and SE for AR model
    ARcoef[[nn]][n,] = Acoef
    ARpvalue[[nn]][n,] = Ap
    AR_SE[[nn]][n,] = A_SE
    
  #Heterogeneous Dynamic Panel Data (AR-X) model
    trainpd = getX_AR(Xold,Yar,N,Roll,nn,Lags) 
    regpd = lm(Y~., data = trainpd)
    summary(regpd)
    for (d in 1:(ncol(trainpd))){
      Pcoef = summary(regpd)$coefficients[d] 
      PDcoef[[nn]][n,d] = Pcoef #Store coeffcients for AR-X model (Including Intercept)
      Pp = summary(regpd)$coefficients[d+ncol(PDpvalue[[nn]])*3]
      PDpvalue[[nn]][n,d] = Pp #Store p-values for AR-X model (Including p-value for Intercept)
      P_SE = summary(regpd)$coefficients[d+ncol(PD_SE[[nn]])*1]
      PD_SE[[nn]][n,d] = P_SE #Store SE for AR-X model (Including SE for Intercept)
    }

  ########Get Predicted Results for AR and AR-X (Under specific lag and PreTime; the 'nn'th rolling/iteration)
    Xpred = X #Xpred: (NT*nvar matrix) A combination of vactorised In-Sample Data of explanatory variables (For the use of generating Predicted Data)
    Ypred = Y #Ypred: (N*T matrix) A single matrix of In-Sample Data of dependent variable (For the use of generating Predicted Data)
    
    #Step 1. Combine 'lag Y' and 'lag X' together into In-sample Regression Data
    yy = as.matrix(as.vector(Ypred))
    n_pred = yy
    nx_pred = cbind(yy,Xpred)
      
    #Step 2. Generating lags and create In-Sample Predicted Data
    pred_cbind = n_pred[(nrow(n_pred) - N*Lags+1):(nrow(n_pred)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (without X)
    x_pred_cbind = nx_pred[(nrow(nx_pred) - N*Lags+1):(nrow(nx_pred)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (with X)
      if (Lags>1) {
      for (l in 2:Lags){
        sub_n_pred = n_pred[(nrow(n_pred) - N*(Lags-l+1)+1):(nrow(n_pred)-N*(Lags-l)),] #Initialise a lag l In-Sample Predicted Data (without X)
        sub_nx_pred = nx_pred[(nrow(nx_pred) - N*(Lags-l+1)+1):(nrow(nx_pred)-N*(Lags-l)),] #Initialise a lag l In-Sample Predicted Data (with X)
        pred_cbind = cbind(pred_cbind,sub_n_pred)
        x_pred_cbind = cbind(x_pred_cbind,sub_nx_pred)
      }
      }
      
      Intercept = matrix(1,N,1)
      n_pred = cbind(Intercept,pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
      nx_pred = cbind(Intercept,x_pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
      
      ar_pred = as.matrix(n_pred[n,]) #Select 'n'th LAD's predicted data for AR model
      pdpred = as.matrix(nx_pred[n,]) #Select 'n'th LAD's predicted data for AR-X model
      
      arcf = as.matrix(ARcoef[[nn]][n,])
      pdcf = as.matrix(PDcoef[[nn]][n,])
      Yar_hat[n,nn] = dot(arcf,ar_pred) #Store Predicted Results for 'n'th LAD by using AR regression coefficients
      Ypd_hat[n,nn] = dot(pdcf,pdpred) #Store Predicted Results for 'n'th LAD by using AR-X regression coefficients
 
  } #End Heterogeneous Model loop
  
  ########Get Regression Results for ARMG and AR-XMG
  #ARMG model
  for (var in 1:ncol(ARcoef[[nn]])){
    coef_armg = mean(ARcoef[[nn]][,var]) #Calculate Mean Group coefficient for ARMG model
    nr_armg = nrow(as.matrix(ARMGcoef[[nn]][,var]))
    ARMGcoef[[nn]][,var] = as.matrix( rep(coef_armg, nr_armg) ) # Replicate N same MG coefficient for N LADs, and store them
    ErrorVariance_armg = ( sum( (ARcoef[[nn]][,var] - ARMGcoef[[nn]][,var])^2 ) )/(N-1) #Error Variance for Mean Group (Pesaran, 2006)
    SE_armg = sqrt(ErrorVariance_armg/N) #Calculate SE for ARMG model
    nr_armg = nrow(as.matrix(ARMG_SE[[nn]][,var]))
    ARMG_SE[[nn]][,var] = as.matrix( rep(SE_armg, nr_armg) ) # Replicate N same SE for N LADs, and store them
    p_armg = 2*pnorm(abs(coef_armg)/SE_armg, lower.tail = FALSE) #Calculate p-value for ARMG model
    nr_armg = nrow(as.matrix(ARMGpvalue[[nn]][,var]))
    ARMGpvalue[[nn]][,var] = as.matrix( rep(p_armg, nr_armg) ) # Replicate N same p-value for N LADs, and store them
  }
  
  #PDMG (AR-XMG) model
  for (var in 1:ncol(PDcoef[[nn]])){
    coef_pdmg = mean(PDcoef[[nn]][,var])
    nr_pdmg = nrow(as.matrix(PDMGcoef[[nn]][,var]))
    PDMGcoef[[nn]][,var] = as.matrix( rep(coef_pdmg, nr_pdmg) )
    ErrorVariance_pdmg = ( sum( (PDcoef[[nn]][,var] - PDMGcoef[[nn]][,var])^2 ) )/(N-1) #Error Variance for Mean Group (Pesaran, 2006)
    SE_pdmg = sqrt(ErrorVariance_pdmg/N) #Calculate SE for AR-XMG model
    nr_pdmg = nrow(as.matrix(PDMG_SE[[nn]][,var]))
    PDMG_SE[[nn]][,var] = as.matrix( rep(SE_pdmg, nr_pdmg) ) # Replicate N same SE for N LADs, and store them
    p_pdmg = 2*pnorm(abs(coef_pdmg)/SE_pdmg, lower.tail = FALSE) #Calculate p-value for AR-XMG model
    nr_pdmg = nrow(as.matrix(PDMGpvalue[[nn]][,var]))
    PDMGpvalue[[nn]][,var] = as.matrix( rep(p_pdmg, nr_pdmg) ) # Replicate N same p-value for N LADs, and store them
  }
  
  ########Get Predicted Results for ARMG and AR-XMG (Under specific lag and PreTime; the 'nn'th rolling/iteration)
  Xpred = X #Xpred: (NT*nvar matrix) A combination of vactorised In-Sample Data of explanatory variables (For the use of generating Predicted Data)
  Ypred = Y #Ypred: (N*T matrix) A single matrix of In-Sample Data of dependent variable (For the use of generating Predicted Data)
  
    #Step 1. Combine 'lag Y' and 'lag X' together into In-sample Regression Data
    yy = as.matrix(as.vector(Ypred))
    n_pred_armg = yy
    nx_pred_pdmg = cbind(yy,Xpred)
    
    #Step 2. Generating lags and create In-Sample Predicted Data
    pred_cbind = n_pred_armg[(nrow(n_pred_armg) - N*Lags+1):(nrow(n_pred_armg)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (without X)
    x_pred_cbind = nx_pred_pdmg[(nrow(nx_pred_pdmg) - N*Lags+1):(nrow(nx_pred_pdmg)-N*(Lags-1)),] #Initialise a lag 1 In-Sample Predicted Data (with X)
    if (Lags>1) {
    for (l in 2:Lags){
      sub_n_pred_armg = n_pred_armg[(nrow(n_pred_armg) - N*(Lags-l+1)+1):(nrow(n_pred_armg)-N*(Lags-l)),] #Initialise a lag l In-Sample Predicted Data (without X)
      sub_nx_pred_pdmg = nx_pred_pdmg[(nrow(nx_pred_pdmg) - N*(Lags-l+1)+1):(nrow(nx_pred_pdmg)-N*(Lags-l)),] #Initialise a lag l In-Sample Predicted Data (with X)
      pred_cbind = cbind(pred_cbind,sub_n_pred_armg)
      x_pred_cbind = cbind(x_pred_cbind,sub_nx_pred_pdmg)
    }
    }
    
    Intercept = matrix(1,N,1)
    n_pred_armg = cbind(Intercept,pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
    nx_pred_pdmg = cbind(Intercept,x_pred_cbind) #Add Intercept Predicted Value (Identity matrix 'In')
    
    Yarmg_hat[,nn] = dot(t(ARMGcoef[[nn]]),t(n_pred_armg)) #Store Predicted Results for 'n'th LAD by using ARMG regression coefficients
    Ypdmg_hat[,nn] = dot(t(PDMGcoef[[nn]]),t(nx_pred_pdmg)) #Store Predicted Results for 'n'th LAD by using AR-XMG regression coefficients

  
}

Miu = Miu[,-1]

for (p in 1:length(PreTime)){
  
  Yreal[[p]] = Yreal[[p]][,-1]
  
  Results_RMSE = matrix(0,15,8)
  Col_str_RMSE = c("RMSE NAR","RMSE ARpool","RMSE ARMG","RMSE AR","RMSE NAR-X","RMSE AR-Xpool","RMSE AR-XMG","RMSE AR-X")
  colnames(Results_RMSE) = Col_str_RMSE
  
  Results_ReMSPE = matrix(0,15,8)
  Col_str_ReMSPE = c("ReMSPE NAR","ReMSPE ARpool","ReMSPE ARMG","ReMSPE AR","ReMSPE NAR-X","ReMSPE AR-Xpool","ReMSPE AR-XMG",
              "ReMSPE AR-X")
  colnames(Results_ReMSPE) = Col_str_ReMSPE
  
  Results_ReMSPE_Individual = matrix(0,15,8)
  Col_str_ReMSPE_Individual = c("ReMSPE NAR","ReMSPE ARpool","ReMSPE ARMG","ReMSPE AR","ReMSPE NAR-X","ReMSPE AR-Xpool","ReMSPE AR-XMG",
                                "ReMSPE AR-X")
  colnames(Results_ReMSPE_Individual) = Col_str_ReMSPE_Individual
  
  LADcodeName = paste(FileSource,"Lad_Region_Code.csv",sep = "")
  regionalcode<-read.csv(file = LADcodeName,header = T)
  code<-as.matrix(regionalcode)
  region<-code[,2]
  Ladcode = code[,1]
  region<-as.numeric(region)
  regioncode<-c (1,2,3,4, 5, 6, 7,8, 9,10,11)
  
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
    
  Yreal_s = Yreal[[p]][rss,]
  Miu_s = Miu[rss,]
  
  Ynar_hat_s = Ynar_hat[rss,]
  Ynarx_hat_s = Ynarx_hat[rss,]
  Yarp_hat_s = Yarp_hat[rss,]
  Yarxp_hat_s = Yarxp_hat[rss,]
  Yar_hat_s = Yar_hat[rss,]
  Ypd_hat_s = Ypd_hat[rss,]
  Yarmg_hat_s = Yarmg_hat[rss,]
  Ypdmg_hat_s = Ypdmg_hat[rss,]
  
  Results_RMSE[r,1] = as.matrix( sqrt(mean( (Yreal_s - Ynar_hat_s)^2 )) ) #RMSE NAR
  Results_RMSE[r,2] = as.matrix( sqrt(mean( (Yreal_s - Yarp_hat_s)^2 )) ) #RMSE ARpool
  Results_RMSE[r,3] = as.matrix( sqrt(mean( (Yreal_s - Yarmg_hat_s)^2 )) ) #RMSE ARMG
  Results_RMSE[r,4] = as.matrix( sqrt(mean( (Yreal_s - Yar_hat_s)^2 )) ) #RMSE AR
  Results_RMSE[r,5] = as.matrix( sqrt(mean( (Yreal_s - Ynarx_hat_s)^2 )) ) #RMSE NAR-X
  Results_RMSE[r,6] = as.matrix( sqrt(mean( (Yreal_s - Yarxp_hat_s)^2 )) ) #RMSE AR-Xpool
  Results_RMSE[r,7] = as.matrix( sqrt(mean( (Yreal_s - Ypdmg_hat_s)^2 )) ) #RMSE AR-XMG
  Results_RMSE[r,8] = as.matrix( sqrt(mean( (Yreal_s - Ypd_hat_s)^2 )) ) #RMSE AR-X
  
  Results_ReMSPE[r,1] = mean( (Yreal_s - Ynar_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 ) #ReMSPE NAR
  Results_ReMSPE[r,2] = mean( (Yreal_s - Yarp_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 ) #ReMSPE ARpool
  Results_ReMSPE[r,3] = mean( (Yreal_s - Yarmg_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 ) #ReMSPE ARMG
  Results_ReMSPE[r,4] = mean( (Yreal_s - Yar_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 ) #ReMSPE AR
  Results_ReMSPE[r,5] = mean( (Yreal_s - Ynarx_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 ) #ReMSPE NAR-X
  Results_ReMSPE[r,6] = mean( (Yreal_s - Yarxp_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 ) #ReMSPE AR-Xpool
  Results_ReMSPE[r,7] = mean( (Yreal_s - Ypdmg_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 ) #ReMSPE AR-XMG
  Results_ReMSPE[r,8] = mean( (Yreal_s - Ypd_hat_s)^2 ) / mean( (Miu_s - Yreal_s)^2 )  #ReMSPE AR-X
  
  ReMSPENAR = matrix(0,1,N)
  ReMSPEARp = matrix(0,1,N)
  ReMSPEARMG = matrix(0,1,N)
  ReMSPEAR = matrix(0,1,N)
  ReMSPENARX = matrix(0,1,N)
  ReMSPEARXp = matrix(0,1,N)
  ReMSPEPDMG = matrix(0,1,N)
  ReMSPEPD = matrix(0,1,N)
  for (lad in 1:N){

    ReMSPENAR[lad] = mean( (Yreal[[p]][lad,] - Ynar_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 )
    ReMSPEARp[lad] = mean( (Yreal[[p]][lad,] - Yarp_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 )
    ReMSPEARMG[lad] = mean( (Yreal[[p]][lad,] - Yarmg_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 )
    ReMSPEAR[lad] = mean( (Yreal[[p]][lad,] - Yar_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 )
    ReMSPENARX[lad] = mean( (Yreal[[p]][lad,] - Ynarx_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 )
    ReMSPEARXp[lad] = mean( (Yreal[[p]][lad,] - Yarxp_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 )
    ReMSPEPDMG[lad] = mean( (Yreal[[p]][lad,] - Ypdmg_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 )
    ReMSPEPD[lad] = mean( (Yreal[[p]][lad,] - Ypd_hat[lad,])^2 ) / mean( (Miu[lad,] - Yreal[[p]][lad,])^2 ) 
    
  }
  
  Results_ReMSPE_Individual[r,1] = length(which(ReMSPENAR[rss]<1)) #ReMSPE Individual NAR
  Results_ReMSPE_Individual[r,2] = length(which(ReMSPEARp[rss]<1)) #ReMSPE Individual ARpool
  Results_ReMSPE_Individual[r,3] = length(which(ReMSPEARMG[rss]<1)) #ReMSPE Individual ARMG
  Results_ReMSPE_Individual[r,4] = length(which(ReMSPEAR[rss]<1)) #ReMSPE Individual AR
  Results_ReMSPE_Individual[r,5] = length(which(ReMSPENARX[rss]<1)) #ReMSPE Individual NAR-X
  Results_ReMSPE_Individual[r,6] = length(which(ReMSPEARXp[rss]<1)) #ReMSPE Individual AR-Xpool
  Results_ReMSPE_Individual[r,7] = length(which(ReMSPEPDMG[rss]<1)) #ReMSPE Individual AR-XMG
  Results_ReMSPE_Individual[r,8] = length(which(ReMSPEPD[rss]<1)) #ReMSPE Individual AR-X
  
  }
  
  Row_str_RMSE = c("North East","North West","Yorkshire","East Midlands","West Midlands","East of England",
                   "London","South East","South West","Wales","Scotland","*North","*Middle","*South","UK Nation")
  rownames(Results_RMSE) = Row_str_RMSE
  
  Row_str_ReMSPE = c("North East","North West","Yorkshire","East Midlands","West Midlands","East of England",
                     "London","South East","South West","Wales","Scotland","*North","*Middle","*South","UK Nation")
  rownames(Results_ReMSPE) = Row_str_ReMSPE
  
  Row_str_ReMSPE_Individual = c("North East","North West","Yorkshire","East Midlands","West Midlands","East of England",
                                "London","South East","South West","Wales","Scotland","*North","*Middle","*South","UK Nation")
  rownames(Results_ReMSPE_Individual) = Row_str_ReMSPE_Individual
  
  Results_RMSE = round(Results_RMSE,6)*100
  Results_ReMSPE = round(Results_ReMSPE,4)
  ModelResiduals_RMSE[[Lags]][[p]] = Results_RMSE
  ModelResiduals_ReMSPE[[Lags]][[p]] = Results_ReMSPE
  ModelResiduals_ReMSPE_Individual[[Lags]][[p]] = Results_ReMSPE_Individual
  
  LagName = as.character(Lags)
  PreTimeName = as.character(PreTime[p])
  ModelResiduals_RMSE_Name = paste(OutputSource,"ModelResiduals_RMSE","_Lags",LagName,"_PreTime",PreTimeName,"_Roll",Roll,".xlsx",sep = "")
  ModelResiduals_ReMSPE_Name = paste(OutputSource,"ModelResiduals_ReMSPE","_Lags",LagName,"_PreTime",PreTimeName,"_Roll",Roll,".xlsx",sep = "")
  ModelResiduals_ReMSPE_Individual_Name = paste(OutputSource,"ModelResiduals_ReMSPE_Individual","_Lags",LagName,"_PreTime",PreTimeName,"_Roll",Roll,".xlsx",sep = "")
  
  write.xlsx(ModelResiduals_RMSE[[Lags]][[p]],file = ModelResiduals_RMSE_Name)
  write.xlsx(ModelResiduals_ReMSPE[[Lags]][[p]],file = ModelResiduals_ReMSPE_Name)
  write.xlsx(ModelResiduals_ReMSPE_Individual[[Lags]][[p]],file = ModelResiduals_ReMSPE_Individual_Name)
  
  
  if (Lags == 1){
    if (p == 1){
      ReMSPE_Individual_Map = cbind(t(ReMSPENAR),t(ReMSPEARp),t(ReMSPEARMG),t(ReMSPEAR),t(ReMSPENARX),
                                    t(ReMSPEARXp),t(ReMSPEPDMG),t(ReMSPEPD))
      colnames(ReMSPE_Individual_Map) = Col_str_ReMSPE
      MapName = paste(OutputSource,"ModelResiduals_ReMSPE_Individual_Lags1_PreTime1.rda",sep = "")
      save(ReMSPE_Individual_Map,file = MapName)
    }
  }
  
  #############################################DM test#####################################################
  ModelResiduals = vector("list",8)
  ModelResiduals[[1]] = Yreal[[p]] - Ynar_hat
  ModelResiduals[[2]] = Yreal[[p]] - Ynarx_hat
  ModelResiduals[[3]] = Yreal[[p]] - Yarp_hat
  ModelResiduals[[4]] = Yreal[[p]] - Yarxp_hat
  ModelResiduals[[5]] = Yreal[[p]] - Yar_hat
  ModelResiduals[[6]] = Yreal[[p]] - Ypd_hat
  ModelResiduals[[7]] = Yreal[[p]] - Yarmg_hat
  ModelResiduals[[8]] = Yreal[[p]] - Ypdmg_hat
  
  #Output an empty Excel file
  EmptyFile = "Please See Results in Following Sheets"
  LagName = as.character(Lags)
  PreTimeName = as.character(PreTime[p])
  ExcelFileName = paste(OutputSource,"DM_test_New","_Lags",LagName,"_PreTime",PreTimeName,"_Roll",Roll,".xlsx",sep = "")
  write.xlsx(EmptyFile,file = ExcelFileName,col.names = FALSE,row.names = FALSE)
  
  ResidualName = c("NAR","NARX","ARp","ARXp","AR","ARX","ARMG","ARXMG")
  for (res1 in 1:7){
    for (res2 in (res1+1):8){
      
      LADcodeName = paste(FileSource,"Lad_Region_Code.csv",sep = "")
      regionalcode<-read.csv(file = LADcodeName,header = T)
      code<-as.matrix(regionalcode)
      region<-code[,2]
      Ladcode = code[,1]
      region<-as.numeric(region)
      regioncode<-c (1,2,3,4, 5, 6, 7,8, 9,10,11)
      
      res1_res2 = matrix(0,15,4)
      colnames(res1_res2)<-cbind("*","**","***","total")
      res2_res1 = matrix(0,15,4)
      colnames(res2_res1)<-cbind("*","**","***","total")
      
      dm_res1_res2 = as.matrix(rep(" ",N))
      dm_res2_res1 = as.matrix(rep(" ",N))
      
      #DM test (res1 vs res2), Lags = 1, PreSelect = 1
      e1 = ModelResiduals[[res2]]
      e2 = ModelResiduals[[res1]]
      result = rep(0,nrow(e1))
      for (i in 1:nrow(e1)){
        ###For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1.
        test = dm.test(e1[i,],e2[i,],alternative = "greater", h = 1)
        result[i] = test$p.value
      }
      dm_res1_res2[which(result<0.10)] = "*"
      dm_res1_res2[which(result<0.05)] = "**"
      dm_res1_res2[which(result<0.01)] = "***"
      
      #DM test (res2 vs res1), Lags = 1, PreSelect = 1
      e1 = ModelResiduals[[res1]]
      e2 = ModelResiduals[[res2]]
      result = rep(0,nrow(e1))
      for (i in 1:nrow(e1)){
        ###For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1.
        test = dm.test(e1[i,],e2[i,],alternative = "greater", h = 1)
        result[i] = test$p.value
      }
      dm_res2_res1[which(result<0.10)] = "*"
      dm_res2_res1[which(result<0.05)] = "**"
      dm_res2_res1[which(result<0.01)] = "***"
      
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
        
        #res1 vs res2
        res1_res2[r,1] = length(which(dm_res1_res2[rss]=="*"))
        res1_res2[r,2] = length(which(dm_res1_res2[rss]=="**"))
        res1_res2[r,3] = length(which(dm_res1_res2[rss]=="***"))
        res1_res2[r,4] = length(which(dm_res1_res2[rss]=="*"|dm_res1_res2[rss]=="**"|dm_res1_res2[rss]=="***"))
        #res2 vs res1
        res2_res1[r,1] = length(which(dm_res2_res1[rss]=="*"))
        res2_res1[r,2] = length(which(dm_res2_res1[rss]=="**"))
        res2_res1[r,3] = length(which(dm_res2_res1[rss]=="***"))
        res2_res1[r,4] = length(which(dm_res2_res1[rss]=="*"|dm_res2_res1[rss]=="**"|dm_res2_res1[rss]=="***"))
        
      }
      ResVS = cbind(res1_res2,res2_res1)
      ResidualName1 = ResidualName[res1]
      ResidualName2 = ResidualName[res2]
      total1 = sprintf("%s vs %s_Total", ResidualName1,ResidualName2)
      total2 = sprintf("%s vs %s_Total", ResidualName2,ResidualName1)
      colnames(ResVS) = c("0.1p","0.05p","0.01p",total1,"0.1","0.05","0.01",total2)
      rownames(ResVS) = c("North East","North West","Yorkshire","East Midlands","West Midlands",
                          "East of England","London","South East","South West","Wales","Scotland"
                          ,"*North","*Middle","*South","UK Nation")
      
      path = sprintf("%s %s", ResidualName1,ResidualName2)
      fileN = paste(path,sep="")
      write.xlsx(ResVS,file = ExcelFileName,sheetName = fileN,append = TRUE)
    }
  }
  
}

}


########################################Create Regional Charts########################################

ModelResiduals_Regional_RMSE = vector("list",15)
ModelResiduals_Regional_ReMSPE = vector("list",15)
ModelResiduals_Regional_ReMSPE_Individual = vector("list",15)

for (r in 1:15){
  
  RegionName = Row_str_RMSE[r] #Give Region Name
  RegionNameRep = rep (RegionName,length(PreTime)*MaxLags) #Replications of RegionName
  PreTime_Num = 0 #Initialise PreTime
  Lags_Num = 0 #Initialise Lags
  Regional_RMSE_Residuals = matrix(0,length(PreTime)*MaxLags,length(Col_str_RMSE)) #Initialise Regional RMSE
  Regional_ReMSPE_Residuals = matrix(0,length(PreTime)*MaxLags,length(Col_str_RMSE)) #Initialise Regional ReMSPE
  Regional_ReMSPE_Individual_Residuals = matrix(0,length(PreTime)*MaxLags,length(Col_str_RMSE)) #Initialise Regional Individual ReMSPE
  
  for (p in 1:length(PreTime)){
    for (l in 1:MaxLags){
      
      PreTime_Num = c(PreTime_Num,p) #Give PreTime Number
      Lags_Num = c(Lags_Num,l) #Give Lags Number
      
      Regional_RMSE_Residuals[MaxLags*(p-1)+l,] = ModelResiduals_RMSE[[l]][[p]][r,] #Store RMSE residuals of Region "r" under PreTime "p" and Lags "l"
      Regional_ReMSPE_Residuals[MaxLags*(p-1)+l,] = ModelResiduals_ReMSPE[[l]][[p]][r,] #Store ReMSPE residuals of Region "r" under PreTime "p" and Lags "l"
      Regional_ReMSPE_Individual_Residuals[MaxLags*(p-1)+l,] = ModelResiduals_ReMSPE_Individual[[l]][[p]][r,] #Store Individual ReMSPE residuals of Region "r" under PreTime "p" and Lags "l"
      
    }
  }
  PreTime_Num = as.matrix(PreTime_Num[-1])
  Lags_Num = as.matrix(Lags_Num[-1])
  
  Model_Names = c("PreTime","Lags",colnames(ModelResiduals_RMSE[[1]][[1]]))
  
  Regional_RMSE_Residuals = cbind(PreTime_Num,Lags_Num,Regional_RMSE_Residuals)
  rownames(Regional_RMSE_Residuals) = RegionNameRep
  colnames(Regional_RMSE_Residuals) = Model_Names
  
  Regional_ReMSPE_Residuals = cbind(PreTime_Num,Lags_Num,Regional_ReMSPE_Residuals)
  rownames(Regional_ReMSPE_Residuals) = RegionNameRep
  colnames(Regional_ReMSPE_Residuals) = Model_Names
  
  Regional_ReMSPE_Individual_Residuals = cbind(PreTime_Num,Lags_Num,Regional_ReMSPE_Individual_Residuals)
  rownames(Regional_ReMSPE_Individual_Residuals) = RegionNameRep
  colnames(Regional_ReMSPE_Individual_Residuals) = Model_Names
  
  ModelResiduals_Regional_RMSE[[r]] = Regional_RMSE_Residuals
  ModelResiduals_Regional_ReMSPE[[r]] = Regional_ReMSPE_Residuals
  ModelResiduals_Regional_ReMSPE_Individual[[r]] = Regional_ReMSPE_Individual_Residuals
  
  
  RegionExcelName_RMSE = paste(OutputSource,"RMSE_",RegionName,"_MaxLags",as.character(MaxLags),"_MaxPreTime",as.character(length(PreTime)),"_Roll",Roll,".xlsx",sep = "")
  write.xlsx(Regional_RMSE_Residuals,file = RegionExcelName_RMSE)
  
  RegionExcelName_ReMSPE = paste(OutputSource,"ReMSPE_",RegionName,"_MaxLags",as.character(MaxLags),"_MaxPreTime",as.character(length(PreTime)),"_Roll",Roll,".xlsx",sep = "")
  write.xlsx(Regional_ReMSPE_Residuals,file = RegionExcelName_ReMSPE)
  
  RegionExcelName_ReMSPE_Ind = paste(OutputSource,"ReMSPE_Ind_",RegionName,"_MaxLags",as.character(MaxLags),"_MaxPreTime",as.character(length(PreTime)),"_Roll",Roll,".xlsx",sep = "")
  write.xlsx(Regional_ReMSPE_Individual_Residuals,file = RegionExcelName_ReMSPE_Ind)
}


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

