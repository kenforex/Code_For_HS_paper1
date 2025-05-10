
rm(list=ls())

library(ggplot2)

###################################Set Initial Values###################################
MaxLags = 1
###################################Read Functions#####################################
CodeSource = "~/Desktop/Chapter 1_Forecast/code/New Code/Code For Publication/" ##Set Code Directory##
FuncName = paste(CodeSource,"Functions.R",sep="")
source(FuncName) 
###################################Read Data#####################################
### Set Original Data and Outputs Directory
##Data
FileSource = "~/Desktop/Chapter 1_Forecast/data/" 
##Output
OutputSource = "~/Desktop/Chapter 1_Forecast/results/"
########################################################################################



















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

###############################Start the loop#######################################
for (Lags in 1:MaxLags){
  
  #Initialise coefficients
  NARcoef = matrix(0,N,1+Lags*2)
  NARXcoef = matrix(0,N,1+(nvar+2)*Lags)
  ARpcoef = matrix(0,N,1+Lags)
  ARXpcoef = matrix(0,N,1+(nvar+1)*Lags)
  ARcoef = matrix(0,N,1+Lags)
  ARMGcoef = matrix(0,N,1+Lags)
  PDcoef = matrix(0,N,1+(nvar+1)*Lags)
  PDMGcoef = matrix(0,N,1+(nvar+1)*Lags)
  
  Group_ARMGcoef = matrix(0,15,1+Lags)
  Group_PDMGcoef = matrix(0,15,1+(nvar+1)*Lags)
  
  
  #Initialise Std.Errors
  NAR_SE = matrix(0,N,1+Lags*2)
  NARX_SE = matrix(0,N,1+(nvar+2)*Lags)
  ARp_SE = matrix(0,N,1+Lags)
  ARXp_SE = matrix(0,N,1+(nvar+1)*Lags)
  AR_SE = matrix(0,N,1+Lags)
  ARMG_SE = matrix(0,N,1+Lags)
  PD_SE = matrix(0,N,1+(nvar+1)*Lags)
  PDMG_SE = matrix(0,N,1+(nvar+1)*Lags)
  
  Group_ARMG_SE = matrix(0,15,1+Lags)
  Group_PDMG_SE = matrix(0,15,1+(nvar+1)*Lags)
  
  #Initialise p-values
  NARpvalue = matrix(0,N,1+Lags*2)
  NARXpvalue = matrix(0,N,1+(nvar+2)*Lags)
  ARppvalue = matrix(0,N,1+Lags)
  ARXppvalue = matrix(0,N,1+(nvar+1)*Lags)
  ARpvalue = matrix(0,N,1+Lags)
  ARMGpvalue = matrix(0,N,1+Lags)
  PDpvalue = matrix(0,N,1+(nvar+1)*Lags)
  PDMGpvalue = matrix(0,N,1+(nvar+1)*Lags)
  
  Group_ARMGpvalue = matrix(0,15,1+Lags)
  Group_PDMGpvalue = matrix(0,15,1+(nvar+1)*Lags)
  
  #################################################################################
 
    ########Initialise some values
    Y1 = as.matrix(as.vector( Ymat ) ) # (NT*1 matrix) dependent variable Y
    Y = Ymat # Y: (N*T matrix) Dependent Variable Y
    
    # (NT*nvar matrix) The Combination of Explanatory Variables X
    Xold = as.matrix(Xold)
    nvar = ncol(Xold)
    Xmat1 = vector("list",nvar)
    for (i in 1:nvar){
      xx = t(matrix(Xold[,i],nrow(Xold)/N,N,byrow = TRUE))
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
    nr = nrow(as.matrix(NARcoef[,1]))
    NARcoef[,1] = as.matrix( rep(Int, nr) ) # Replicate N same intercept 'Int' for N LADs, and store them
    pInt = summary(reg)$coefficients[1+ncol(NARpvalue)*3] # Get p-value of NAR regression intercept
    nr = nrow(as.matrix(NARpvalue[,1]))
    NARpvalue[,1] = as.matrix( rep(pInt, nr) ) # Replicate N same p-value of 'pInt' for N LADs, and store them
    Int_SE = summary(reg)$coefficients[1+ncol(NAR_SE)*1] # Get SE of NAR regression intercept
    nr = nrow(as.matrix(NAR_SE[,1]))
    NAR_SE[,1] = as.matrix( rep(Int_SE, nr) ) # Replicate N same SE of 'Int_SE' for N LADs, and store them
    for (c in 2:ncol(train)){
      Ncoef = summary(reg)$coefficients[c] # Get NAR regression coefficient for 'c'th regressor
      nr = nrow(as.matrix(NARcoef[,c]))
      NARcoef[,c] = as.matrix( rep(Ncoef, nr) ) # Replicate N same coefficient 'Ncoef' for N LADs, and store them
      Np = summary(reg)$coefficients[c+ncol(NARpvalue)*3] # Get p-value of NAR regression coefficient for 'c'th regressor
      nr = nrow(as.matrix(NARpvalue[,c]))
      NARpvalue[,c] = as.matrix( rep(Np, nr) ) # Replicate N same p-value of 'Np' for N LADs, and store them
      N_SE = summary(reg)$coefficients[c+ncol(NAR_SE)*1] # Get SE of NAR regression coefficient for 'c'th regressor
      nr = nrow(as.matrix(NAR_SE[,c]))
      NAR_SE[,c] = as.matrix( rep(N_SE, nr) ) # Replicate N same SE of 'N_SE' for N LADs, and store them
    }
    
    #NAR-X model
    trainX = getX(X, Y, W, N, Lags)
    regX = lm(Y~., data = trainX) # NAR-X regression
    summary(regX)
    Intx = summary(regX)$coefficients[1] # Get NAR-X regression intercept
    nr_x = nrow(as.matrix(NARXcoef[,1]))
    NARXcoef[,1] = as.matrix( rep(Intx, nr_x) ) # Replicate N same intercept 'Intx' for N LADs, and store them
    pxInt = summary(regX)$coefficients[1+ncol(NARXpvalue)*3] # Get p-value of NAR-X regression intercept
    nr_x = nrow(as.matrix(NARXpvalue[,1]))
    NARXpvalue[,1] = as.matrix( rep(pxInt, nr_x) ) # Replicate N same p-value of 'pxInt' for N LADs, and store them
    Intx_SE = summary(regX)$coefficients[1+ncol(NARX_SE)*1] # Get SE of NAR-X regression intercept
    nr_x = nrow(as.matrix(NARX_SE[,1]))
    NARX_SE[,1] = as.matrix( rep(Intx_SE, nr_x) ) # Replicate N same SE of 'Intx_SE' for N LADs, and store them
    for (c in 2:ncol(trainX)){
      NXcoef = summary(regX)$coefficients[c] # Get NAR-X regression coefficient for 'c'th regressor
      nr_x = nrow(as.matrix(NARXcoef[,c]))
      NARXcoef[,c] = as.matrix( rep(NXcoef, nr_x) ) # Replicate N same coefficient 'NXcoef' for N LADs, and store them
      Nxp = summary(regX)$coefficients[c+ncol(NARXpvalue)*3] # Get p-value of NAR-X regression coefficient for 'c'th regressor
      nr_x = nrow(as.matrix(NARXpvalue[,c]))
      NARXpvalue[,c] = as.matrix( rep(Nxp, nr_x) ) # Replicate N same p-value of 'NXp' for N LADs, and store them
      Nx_SE = summary(regX)$coefficients[c+ncol(NARX_SE)*1] # Get SE of NAR-X regression coefficient for 'c'th regressor
      nr_x = nrow(as.matrix(NARX_SE[,c]))
      NARX_SE[,c] = as.matrix( rep(Nx_SE, nr_x) ) # Replicate N same SE of 'NX_SE' for N LADs, and store them
    }
    
    ########Get Regression Results for ARpool and AR-Xpool
    #ARpool model
    train_arp = getX_noX(Y, W, N, Lags, Wexsit = FALSE)
    reg_arp = lm(Y~., data = train_arp) #ARpool regression
    summary(reg_arp)
    Int_arp = summary(reg_arp)$coefficients[1] # Get ARpool regression intercept
    nr_arp = nrow(as.matrix(ARpcoef[,1]))
    ARpcoef[,1] = as.matrix( rep(Int_arp, nr_arp) ) # Replicate N same intercept 'Int_arp' for N LADs, and store them
    pInt_arp = summary(reg_arp)$coefficients[1+ncol(ARppvalue)*3] # Get p-value of ARpool regression intercept
    nr_arp = nrow(as.matrix(ARppvalue[,1]))
    ARppvalue[,1] = as.matrix( rep(pInt_arp, nr_arp) ) # Replicate N same p-value of 'pInt_arp' for N LADs, and store them
    Int_arp_SE = summary(reg_arp)$coefficients[1+ncol(ARp_SE)*1] # Get SE of ARpool regression intercept
    nr_arp = nrow(as.matrix(ARp_SE[,1]))
    ARp_SE[,1] = as.matrix( rep(Int_arp_SE, nr_arp) ) # Replicate N same SE of 'Int_arp_SE' for N LADs, and store them
    for (c in 2:ncol(train_arp)){
      coef_arp = summary(reg_arp)$coefficients[c] # Get ARpool regression coefficient for 'c'th regressor
      nr_arp = nrow(as.matrix(ARpcoef[,c]))
      ARpcoef[,c] = as.matrix( rep(coef_arp, nr_arp) ) # Replicate N same coefficient 'coef_arp' for N LADs, and store them
      p_arp = summary(reg_arp)$coefficients[c+ncol(ARppvalue)*3] # Get p-value of ARpool regression coefficient for 'c'th regressor
      nr_arp = nrow(as.matrix(ARppvalue[,c]))
      ARppvalue[,c] = as.matrix( rep(p_arp, nr_arp) ) # Replicate N same p-value of 'p_arp' for N LADs, and store them
      SE_arp = summary(reg_arp)$coefficients[c+ncol(ARp_SE)*1] # Get SE of ARpool regression coefficient for 'c'th regressor
      nr_arp = nrow(as.matrix(ARp_SE[,c]))
      ARp_SE[,c] = as.matrix( rep(SE_arp, nr_arp) ) # Replicate N same SE of 'SE_arp' for N LADs, and store them
    }
    
    #AR-Xpool model
    trainX_arp = getX(X, Y, W, N, Lags, Wexsit = FALSE) 
    regX_arp = lm(Y~., data = trainX_arp) #AR-Xpool regression
    summary(regX_arp)
    Intx_arp = summary(regX_arp)$coefficients[1] # Get AR-Xpool regression intercept
    nr_x_arp = nrow(as.matrix(ARXpcoef[,1]))
    ARXpcoef[,1] = as.matrix( rep(Intx_arp, nr_x_arp) ) # Replicate N same intercept 'Intx_arp' for N LADs, and store them
    pxInt_arp = summary(regX_arp)$coefficients[1+ncol(ARXppvalue)*3] # Get p-value of AR-Xpool regression intercept
    nr_x_arp = nrow(as.matrix(ARXppvalue[,1]))
    ARXppvalue[,1] = as.matrix( rep(pxInt_arp, nr_x_arp) ) # Replicate N same p-value of 'pxIntx_arp' for N LADs, and store them
    xInt_arp_SE = summary(regX_arp)$coefficients[1+ncol(ARXp_SE)*1] # Get SE of AR-Xpool regression intercept
    nr_x_arp = nrow(as.matrix(ARXp_SE[,1]))
    ARXp_SE[,1] = as.matrix( rep(xInt_arp_SE, nr_x_arp) ) # Replicate N same SE of 'Intx_arp' for N LADs, and store them
    for (c in 2:ncol(trainX_arp)){
      Xcoef_arp = summary(regX_arp)$coefficients[c] # Get AR-Xpool regression coefficient for 'c'th regressor
      nr_x_arp = nrow(as.matrix(ARXpcoef[,c]))
      ARXpcoef[,c] = as.matrix( rep(Xcoef_arp, nr_x_arp) ) # Replicate N same coefficient 'Xcoef_arp' for N LADs, and store them
      xp_arp = summary(regX_arp)$coefficients[c+ncol(ARXppvalue)*3] # Get p-value of AR-Xpool regression coefficient for 'c'th regressor
      nr_x_arp = nrow(as.matrix(ARXppvalue[,c]))
      ARXppvalue[,c] = as.matrix( rep(xp_arp, nr_x_arp) ) # Replicate N same p-value of 'xp_arp' for N LADs, and store them
      xSE_arp = summary(regX_arp)$coefficients[c+ncol(ARXp_SE)*1] # Get SE of AR-Xpool regression coefficient for 'c'th regressor
      nr_x_arp = nrow(as.matrix(ARXp_SE[,c]))
      ARXp_SE[,c] = as.matrix( rep(xSE_arp, nr_x_arp) ) # Replicate N same SE of 'xSE_arp' for N LADs, and store them
    }
    
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
      ARcoef[n,] = Acoef
      ARpvalue[n,] = Ap
      AR_SE[n,] = A_SE
      
      #Heterogeneous Dynamic Panel Data (AR-X) model
      trainpd = getX_AR(Xold,Yar,N,T,1,Lags) 
      regpd = lm(Y~., data = trainpd)
      summary(regpd)
      for (d in 1:(ncol(trainpd))){
        Pcoef = summary(regpd)$coefficients[d] 
        PDcoef[n,d] = Pcoef #Store coeffcients for AR-X model (Including Intercept)
        Pp = summary(regpd)$coefficients[d+ncol(PDpvalue)*3]
        PDpvalue[n,d] = Pp #Store p-values for AR-X model (Including p-value for Intercept)
        P_SE = summary(regpd)$coefficients[d+ncol(PD_SE)*1]
        PD_SE[n,d] = P_SE #Store SE for AR-X model (Including SE for Intercept)
      }
      
    }
    
      
    ########Get Global Homogeneous Regression Results for ARMG and AR-XMG
    #ARMG model
    for (var in 1:ncol(ARcoef)){
      coef_armg = mean(ARcoef[,var]) #Calculate Mean Group coefficient for ARMG model
      nr_armg = nrow(as.matrix(ARMGcoef[,var]))
      ARMGcoef[,var] = as.matrix( rep(coef_armg, nr_armg) ) # Replicate N same MG coefficient for N LADs, and store them
      ErrorVariance_armg = ( sum( (ARcoef[,var] - ARMGcoef[,var])^2 ) )/(N-1) #Error Variance for Mean Group (Pesaran, 2006)
      SE_armg = sqrt(ErrorVariance_armg/N) #Calculate SE for ARMG model
      nr_armg = nrow(as.matrix(ARMG_SE[,var]))
      ARMG_SE[,var] = as.matrix( rep(SE_armg, nr_armg) ) # Replicate N same SE for N LADs, and store them
      p_armg = 2*pnorm(abs(coef_armg)/SE_armg, lower.tail = FALSE) #Calculate p-value for ARMG model
      nr_armg = nrow(as.matrix(ARMGpvalue[,var]))
      ARMGpvalue[,var] = as.matrix( rep(p_armg, nr_armg) ) # Replicate N same p-value for N LADs, and store them
    }
    
    #PDMG (AR-XMG) model
    for (var in 1:ncol(PDcoef)){
      coef_pdmg = mean(PDcoef[,var])
      nr_pdmg = nrow(as.matrix(PDMGcoef[,var]))
      PDMGcoef[,var] = as.matrix( rep(coef_pdmg, nr_pdmg) )
      ErrorVariance_pdmg = ( sum( (PDcoef[,var] - PDMGcoef[,var])^2 ) )/(N-1) #Error Variance for Mean Group (Pesaran, 2006)
      SE_pdmg = sqrt(ErrorVariance_pdmg/N) #Calculate SE for AR-XMG model
      nr_pdmg = nrow(as.matrix(PDMG_SE[,var]))
      PDMG_SE[,var] = as.matrix( rep(SE_pdmg, nr_pdmg) ) # Replicate N same SE for N LADs, and store them
      p_pdmg = 2*pnorm(abs(coef_pdmg)/SE_pdmg, lower.tail = FALSE) #Calculate p-value for AR-XMG model
      nr_pdmg = nrow(as.matrix(PDMGpvalue[,var]))
      PDMGpvalue[,var] = as.matrix( rep(p_pdmg, nr_pdmg) ) # Replicate N same p-value for N LADs, and store them
    }
    
    
    ########Get Grouping Homogeneous Regression Results for ARMG and AR-XMG
    for (r in 1:15){
      CodeName = paste(FileSource,"Lad_Region_Code.csv",sep="")
      regionalcode<-read.csv(CodeName)
      code<-as.matrix(regionalcode)
      region<-code[,2]
      region<-as.numeric(region)
      
      N<-max(as.numeric(data$group))
      
      if (r < 12) {
        ind_w = which(region==r)
      } else {
        
        if (r == 12) {
          c_north = which(region==1 | region==2 | region==3 | region==11)
          ind_w = c_north
        }
        
        if (r == 13) {
          c_middle = which(region==4 | region==5 | region==10)
          ind_w = c_middle
        }
        
        if (r == 14) {
          c_south = which(region==6 | region==8 | region==9)
          ind_w = c_south
        }
        
        if (r == 15) {
          ind_w = 1:N
        }
        
      }
      
      Group_ARcoef = ARcoef[ind_w,]
      Group_PDcoef = PDcoef[ind_w,]
      
      
      #Grouping ARMG coefs
      for (var in 1:ncol(ARcoef)){
        Group_coef_armg = mean(Group_ARcoef[,var]) #Calculate ARMG Mean Group coefficient for specific group
        RepGroup_ARMGcoef = as.matrix( rep(Group_coef_armg, length(ind_w)) ) # Replicate same ARMG coefficient for n LADs in specific group, and store them
        Group_ARMGcoef[r,var] = Group_coef_armg
        Group_ErrorVariance_armg = ( sum( (Group_ARcoef[,var] - RepGroup_ARMGcoef)^2 ) )/(length(ind_w)-1) #Error Variance for Mean Group (Pesaran, 2006)
        Group_SE_armg = sqrt(Group_ErrorVariance_armg/length(ind_w)) #Calculate SE for ARMG model for specific group
        Group_ARMG_SE[r,var] = Group_SE_armg
        Group_p_armg = 2*pnorm(abs(Group_coef_armg)/Group_SE_armg, lower.tail = FALSE) #Calculate p-value for ARMG model
        Group_ARMGpvalue[r,var] = Group_p_armg
      }
      #Grouping PDMG coefs
      for (var in 1:ncol(PDcoef)){
        Group_coef_pdmg = mean(Group_PDcoef[,var]) #Calculate PDMG Mean Group coefficient for specific group
        RepGroup_PDMGcoef = as.matrix( rep(Group_coef_pdmg, length(ind_w)) ) # Replicate same PDMG coefficient for n LADs in specific group, and store them
        Group_PDMGcoef[r,var] = Group_coef_pdmg
        Group_ErrorVariance_pdmg = ( sum( (Group_PDcoef[,var] - RepGroup_PDMGcoef)^2 ) )/(length(ind_w)-1) #Error Variance for Mean Group (Pesaran, 2006)
        Group_SE_pdmg = sqrt(Group_ErrorVariance_pdmg/length(ind_w)) #Calculate SE for PDMG model for specific group
        Group_PDMG_SE[r,var] = Group_SE_pdmg
        Group_p_pdmg = 2*pnorm(abs(Group_coef_pdmg)/Group_SE_pdmg, lower.tail = FALSE) #Calculate p-value for PDMG model
        Group_PDMGpvalue[r,var] = Group_p_pdmg
      }
     
    
    }
    
}


  ###############################################End Regression###############################################

  LadNames = as.matrix(data$RegionName[1:N])
  #########################################Store Heterogeneous Model Coefficients#########################################
  #AR and AR-X model
  AR_Regression_Chart = matrix(0,N*2,1+MaxLags)
  PD_Regression_Chart = matrix(0,N*2,1+(1+nvar)*MaxLags)
  ARcoef = round(ARcoef,5)
  PDcoef = round(PDcoef,5)
  AR_SE = round(AR_SE,5)
  PD_SE = round(PD_SE,5)
  
  LadNames_Chart = ""
  for (lad in 1:N){
    ###################################AR Model###################################
    #Get single LAD's coefficients
    AR_Regression_Coef = as.character(ARcoef[lad,])
    AR_Regression_SE = as.character(AR_SE[lad,])
    
    AR_Regression_pvalue = ARpvalue[lad,]
    for (p in 1:length(AR_Regression_Coef)){
      
      #Assign p-value significance for this LAD's coefficients
      if (AR_Regression_pvalue[p]<=0.01){
        AR_Regression_Coef[p] = paste(AR_Regression_Coef[p],"***",sep = "")
      } else {
        if (AR_Regression_pvalue[p]<=0.05){
          AR_Regression_Coef[p] = paste(AR_Regression_Coef[p],"**",sep = "")
        } else {
          if (AR_Regression_pvalue[p]<=0.1){
            AR_Regression_Coef[p] = paste(AR_Regression_Coef[p],"*",sep = "")
          }
        }
      }
      
      #Assign "()" for this LAD's SE
      AR_Regression_SE[p] = paste("(",AR_Regression_SE[p],")",sep = "")
      
    }
    
    #Store coefficients with significance
    AR_Regression_Chart[(lad-1)*2+1,] = AR_Regression_Coef
    
    #Store SE
    AR_Regression_Chart[lad*2,] = AR_Regression_SE
    
    
    ###################################AR-X Model###################################
    #Get single LAD's coefficients
    PD_Regression_Coef = as.character(PDcoef[lad,])
    PD_Regression_SE = as.character(PD_SE[lad,])
    
    PD_Regression_pvalue = PDpvalue[lad,]
    for (p in 1:length(PD_Regression_Coef)){
      
      #Assign p-value significance for this LAD's coefficients
      if (PD_Regression_pvalue[p]<=0.01){
        PD_Regression_Coef[p] = paste(PD_Regression_Coef[p],"***",sep = "")
      } else {
        if (PD_Regression_pvalue[p]<=0.05){
          PD_Regression_Coef[p] = paste(PD_Regression_Coef[p],"**",sep = "")
        } else {
          if (PD_Regression_pvalue[p]<=0.1){
            PD_Regression_Coef[p] = paste(PD_Regression_Coef[p],"*",sep = "")
          }
        }
      }
      
      #Assign "()" for this LAD's SE
      PD_Regression_SE[p] = paste("(",PD_Regression_SE[p],")",sep = "")
      
    }
    
    #Store coefficients with significance
    PD_Regression_Chart[(lad-1)*2+1,] = PD_Regression_Coef
    
    #Store SE
    PD_Regression_Chart[lad*2,] = PD_Regression_SE
    
    #Store LAD's Name
    NameSE = paste("SE-",as.character(lad),sep = "")
    LadNames_Chart = c(LadNames_Chart,LadNames[lad],NameSE)
    
  }
  LadNames_Chart = LadNames_Chart[-1]
  
  #Add row names
  rownames(AR_Regression_Chart) = LadNames_Chart
  rownames(PD_Regression_Chart) = LadNames_Chart
  
  #########################################Store Homogeneous Model Coefficients#########################################
  NAR_Regression_Chart = matrix(0,2,1+2*MaxLags)
  NARX_Regression_Chart = matrix(0,2,1+(2+nvar)*MaxLags)
  ARp_Regression_Chart = matrix(0,2,1+MaxLags)
  ARXp_Regression_Chart = matrix(0,2,1+(1+nvar)*MaxLags)
  ARMG_Regression_Chart = matrix(0,2,1+MaxLags)
  PDMG_Regression_Chart = matrix(0,2,1+(1+nvar)*MaxLags)
  Group_ARMG_Regression_Chart = matrix(0,2*15,1+MaxLags)
  Group_PDMG_Regression_Chart = matrix(0,2*15,1+(1+nvar)*MaxLags)
  
  NARcoef_single = as.character(round(NARcoef[1,],4))
  NARXcoef_single = as.character(round(NARXcoef[1,],4))
  ARpcoef_single = as.character(round(ARpcoef[1,],4))
  ARXpcoef_single = as.character(round(ARXpcoef[1,],4))
  ARMGcoef_single = as.character(round(ARMGcoef[1,],4))
  PDMGcoef_single = as.character(round(PDMGcoef[1,],4))
  Group_ARMGcoef_single = as.matrix(round(Group_ARMGcoef,4))
  Group_PDMGcoef_single = as.matrix(round(Group_PDMGcoef,4))
  
  NAR_SE_single = as.character(round(NAR_SE[1,],4))
  NARX_SE_single = as.character(round(NARX_SE[1,],4))
  ARp_SE_single = as.character(round(ARp_SE[1,],4))
  ARXp_SE_single = as.character(round(ARXp_SE[1,],4))
  ARMG_SE_single = as.character(round(ARMG_SE[1,],4))
  PDMG_SE_single = as.character(round(PDMG_SE[1,],4))
  Group_ARMG_SE_single = as.matrix(round(Group_ARMG_SE,4))
  Group_PDMG_SE_single = as.matrix(round(Group_PDMG_SE,4))
  
  NARpvalue_single = round(NARpvalue[1,],4)
  NARXpvalue_single = round(NARXpvalue[1,],4)
  ARppvalue_single = round(ARppvalue[1,],4)
  ARXppvalue_single = round(ARXppvalue[1,],4)
  ARMGpvalue_single = round(ARMGpvalue[1,],4)
  PDMGpvalue_single = round(PDMGpvalue[1,],4)
  Group_ARMGpvalue_single = round(Group_ARMGpvalue,4)
  Group_PDMGpvalue_single = round(Group_PDMGpvalue,4)
  
  ####NAR Model
  NAR_Regression_Coef = NARcoef_single
  NAR_Regression_SE = NAR_SE_single
  NAR_Regression_pvalue = NARpvalue_single
  for (p in 1:length(NAR_Regression_Coef)){
    
    #Assign p-value significance for this LAD's coefficients
    if (NAR_Regression_pvalue[p]<=0.01){
      NAR_Regression_Coef[p] = paste(NAR_Regression_Coef[p],"***",sep = "")
    } else {
      if (NAR_Regression_pvalue[p]<=0.05){
        NAR_Regression_Coef[p] = paste(NAR_Regression_Coef[p],"**",sep = "")
      } else {
        if (NAR_Regression_pvalue[p]<=0.1){
          NAR_Regression_Coef[p] = paste(NAR_Regression_Coef[p],"*",sep = "")
        }
      }
    }
    
    #Assign "()" for this LAD's SE
    NAR_Regression_SE[p] = paste("(",NAR_Regression_SE[p],")",sep = "")
    
    #Store coefficients with significance
    NAR_Regression_Chart[1,] = NAR_Regression_Coef
    
    #Store SE
    NAR_Regression_Chart[2,] = NAR_Regression_SE
    
  }
  
  
  ####NAR-X Model
  NARX_Regression_Coef = NARXcoef_single
  NARX_Regression_SE = NARX_SE_single
  NARX_Regression_pvalue = NARXpvalue_single
  for (p in 1:length(NARX_Regression_Coef)){
    
    #Assign p-value significance for this LAD's coefficients
    if (NARX_Regression_pvalue[p]<=0.01){
      NARX_Regression_Coef[p] = paste(NARX_Regression_Coef[p],"***",sep = "")
    } else {
      if (NARX_Regression_pvalue[p]<=0.05){
        NARX_Regression_Coef[p] = paste(NARX_Regression_Coef[p],"**",sep = "")
      } else {
        if (NARX_Regression_pvalue[p]<=0.1){
          NARX_Regression_Coef[p] = paste(NARX_Regression_Coef[p],"*",sep = "")
        }
      }
    }
    
    #Assign "()" for this LAD's SE
    NARX_Regression_SE[p] = paste("(",NARX_Regression_SE[p],")",sep = "")
    
    #Store coefficients with significance
    NARX_Regression_Chart[1,] = NARX_Regression_Coef
    
    #Store SE
    NARX_Regression_Chart[2,] = NARX_Regression_SE
    
  }
  
  ####ARp Model
  ARp_Regression_Coef = ARpcoef_single
  ARp_Regression_SE = ARp_SE_single
  ARp_Regression_pvalue = ARppvalue_single
  for (p in 1:length(ARp_Regression_Coef)){
    
    #Assign p-value significance for this LAD's coefficients
    if (ARp_Regression_pvalue[p]<=0.01){
      ARp_Regression_Coef[p] = paste(ARp_Regression_Coef[p],"***",sep = "")
    } else {
      if (ARp_Regression_pvalue[p]<=0.05){
        ARp_Regression_Coef[p] = paste(ARp_Regression_Coef[p],"**",sep = "")
      } else {
        if (ARp_Regression_pvalue[p]<=0.1){
          ARp_Regression_Coef[p] = paste(ARp_Regression_Coef[p],"*",sep = "")
        }
      }
    }
    
    #Assign "()" for this LAD's SE
    ARp_Regression_SE[p] = paste("(",ARp_Regression_SE[p],")",sep = "")
    
    #Store coefficients with significance
    ARp_Regression_Chart[1,] = ARp_Regression_Coef
    
    #Store SE
    ARp_Regression_Chart[2,] = ARp_Regression_SE
    
  }
  
  ####ARXp Model
  ARXp_Regression_Coef = ARXpcoef_single
  ARXp_Regression_SE = ARXp_SE_single
  ARXp_Regression_pvalue = ARXppvalue_single
  for (p in 1:length(ARXp_Regression_Coef)){
    
    #Assign p-value significance for this LAD's coefficients
    if (ARXp_Regression_pvalue[p]<=0.01){
      ARXp_Regression_Coef[p] = paste(ARXp_Regression_Coef[p],"***",sep = "")
    } else {
      if (ARXp_Regression_pvalue[p]<=0.05){
        ARXp_Regression_Coef[p] = paste(ARXp_Regression_Coef[p],"**",sep = "")
      } else {
        if (ARXp_Regression_pvalue[p]<=0.1){
          ARXp_Regression_Coef[p] = paste(ARXp_Regression_Coef[p],"*",sep = "")
        }
      }
    }
    
    #Assign "()" for this LAD's SE
    ARXp_Regression_SE[p] = paste("(",ARXp_Regression_SE[p],")",sep = "")
    
    #Store coefficients with significance
    ARXp_Regression_Chart[1,] = ARXp_Regression_Coef
    
    #Store SE
    ARXp_Regression_Chart[2,] = ARXp_Regression_SE
    
  }
  
  
  ####ARMG Model
  ARMG_Regression_Coef = ARMGcoef_single
  ARMG_Regression_SE = ARMG_SE_single
  ARMG_Regression_pvalue = ARMGpvalue_single
  for (p in 1:length(ARMG_Regression_Coef)){
    
    #Assign p-value significance for this LAD's coefficients
    if (ARMG_Regression_pvalue[p]<=0.01){
      ARMG_Regression_Coef[p] = paste(ARMG_Regression_Coef[p],"***",sep = "")
    } else {
      if (ARMG_Regression_pvalue[p]<=0.05){
        ARMG_Regression_Coef[p] = paste(ARMG_Regression_Coef[p],"**",sep = "")
      } else {
        if (ARMG_Regression_pvalue[p]<=0.1){
          ARMG_Regression_Coef[p] = paste(ARMG_Regression_Coef[p],"*",sep = "")
        }
      }
    }
    
    #Assign "()" for this LAD's SE
    ARMG_Regression_SE[p] = paste("(",ARMG_Regression_SE[p],")",sep = "")
    
    #Store coefficients with significance
    ARMG_Regression_Chart[1,] = ARMG_Regression_Coef
    
    #Store SE
    ARMG_Regression_Chart[2,] = ARMG_Regression_SE
    
  }
  
  
  ####ARXMG Model
  PDMG_Regression_Coef = PDMGcoef_single
  PDMG_Regression_SE = PDMG_SE_single
  PDMG_Regression_pvalue = PDMGpvalue_single
  for (p in 1:length(PDMG_Regression_Coef)){
    
    #Assign p-value significance for this LAD's coefficients
    if (PDMG_Regression_pvalue[p]<=0.01){
      PDMG_Regression_Coef[p] = paste(PDMG_Regression_Coef[p],"***",sep = "")
    } else {
      if (PDMG_Regression_pvalue[p]<=0.05){
        PDMG_Regression_Coef[p] = paste(PDMG_Regression_Coef[p],"**",sep = "")
      } else {
        if (PDMG_Regression_pvalue[p]<=0.1){
          PDMG_Regression_Coef[p] = paste(PDMG_Regression_Coef[p],"*",sep = "")
        }
      }
    }
    
    #Assign "()" for this LAD's SE
    PDMG_Regression_SE[p] = paste("(",PDMG_Regression_SE[p],")",sep = "")
    
    #Store coefficients with significance
    PDMG_Regression_Chart[1,] = PDMG_Regression_Coef
    
    #Store SE
    PDMG_Regression_Chart[2,] = PDMG_Regression_SE
    
  }
  
  GroupNames = c("North East","North West","Yorkshire","East Midlands","West Midlands",
                 "East of England","London","South East","South West","Wales","Scotland"
                 ,"*North","*Middle","*South","UK Nation")
  
  ####Group ARMG Model
  Group_ARMG_names = matrix(0,2*15,1)
  for (r in 1:15){
    
    g_ARMG_Regression_Coef = as.character(Group_ARMGcoef_single[r,])
    g_ARMG_Regression_SE = as.character(Group_ARMG_SE_single[r,])
    g_ARMG_Regression_pvalue = Group_ARMGpvalue_single[r,]
    for (p in 1:length(g_ARMG_Regression_Coef)){
      
      #Assign p-value significance for this LAD's coefficients
      if (g_ARMG_Regression_pvalue[p]<=0.01){
        g_ARMG_Regression_Coef[p] = paste(g_ARMG_Regression_Coef[p],"***",sep = "")
      } else {
        if (g_ARMG_Regression_pvalue[p]<=0.05){
          g_ARMG_Regression_Coef[p] = paste(g_ARMG_Regression_Coef[p],"**",sep = "")
        } else {
          if (g_ARMG_Regression_pvalue[p]<=0.1){
            g_ARMG_Regression_Coef[p] = paste(g_ARMG_Regression_Coef[p],"*",sep = "")
          }
        }
      }
      
      #Assign "()" for this LAD's SE
      g_ARMG_Regression_SE[p] = paste("(",g_ARMG_Regression_SE[p],")",sep = "")
      
      #Store coefficients with significance
      Group_ARMG_Regression_Chart[(r-1)*2+1,] = g_ARMG_Regression_Coef
      
      #Store SE
      Group_ARMG_Regression_Chart[(r-1)*2+2,] = g_ARMG_Regression_SE
      
      #Store Group Names
      Group_ARMG_names[(r-1)*2+1,] = GroupNames[r]
      Group_ARMG_names[(r-1)*2+2,] = "SE"
      
    }
    
  }
  row.names(Group_ARMG_Regression_Chart)<-Group_ARMG_names
  colnames(Group_ARMG_Regression_Chart)<-c("Intercept","Lagged Price")
  Name_Group_ARMG_Regression_Chart = paste(OutputSource,"Group_ARMG_Regression_Chart.csv",sep = "")
  write.csv(Group_ARMG_Regression_Chart,file = Name_Group_ARMG_Regression_Chart)
  
  
  ####Group PDMG Model
  Group_PDMG_names = matrix(0,2*15,1)
  for (r in 1:15){
    
    g_PDMG_Regression_Coef = as.character(Group_PDMGcoef_single[r,])
    g_PDMG_Regression_SE = as.character(Group_PDMG_SE_single[r,])
    g_PDMG_Regression_pvalue = Group_PDMGpvalue_single[r,]
    for (p in 1:length(g_PDMG_Regression_Coef)){
      
      #Assign p-value significance for this LAD's coefficients
      if (g_PDMG_Regression_pvalue[p]<=0.01){
        g_PDMG_Regression_Coef[p] = paste(g_PDMG_Regression_Coef[p],"***",sep = "")
      } else {
        if (g_PDMG_Regression_pvalue[p]<=0.05){
          g_PDMG_Regression_Coef[p] = paste(g_PDMG_Regression_Coef[p],"**",sep = "")
        } else {
          if (g_PDMG_Regression_pvalue[p]<=0.1){
            g_PDMG_Regression_Coef[p] = paste(g_PDMG_Regression_Coef[p],"*",sep = "")
          }
        }
      }
      
      #Assign "()" for this LAD's SE
      g_PDMG_Regression_SE[p] = paste("(",g_PDMG_Regression_SE[p],")",sep = "")
      
      #Store coefficients with significance
      Group_PDMG_Regression_Chart[(r-1)*2+1,] = g_PDMG_Regression_Coef
      
      #Store SE
      Group_PDMG_Regression_Chart[(r-1)*2+2,] = g_PDMG_Regression_SE
      
      #Store Group Names
      Group_PDMG_names[(r-1)*2+1,] = GroupNames[r]
      Group_PDMG_names[(r-1)*2+2,] = "SE"
      
    }
    
  }
  row.names(Group_PDMG_Regression_Chart)<-Group_PDMG_names
  colnames(Group_PDMG_Regression_Chart)<-c("Intercept","Lagged Price","Lagged Sales")
  Name_Group_PDMG_Regression_Chart = paste(OutputSource,"Group_ARXMG_Regression_Chart.csv",sep = "")
  write.csv(Group_PDMG_Regression_Chart,file = Name_Group_PDMG_Regression_Chart)
  
  
  
  ######################Plot Heterogeneous Nodes######################
  ##Classify nodes by its region
  ARcoef_Classified = ARcoef
  PDcoef_Classified = PDcoef
  ARpvalue_Classified = ARpvalue
  PDpvalue_Classified = PDpvalue
  max_rid = max(regionalcode$Region.ID)
  l_progress = 0
  plot_region.name = regionalcode$Region.Name
  Plot_GroupName = c("NE","NW","YS","EM","WM",
                     "EE","LD","SE","SW","WS","SC")
  for (rid in 1:max_rid){
    rid_index = which(regionalcode$Region.ID==rid)
    l_start = max(l_progress)
    l_progress = l_progress + length(rid_index)
    sort_index = (l_start+1):(l_progress)
    ARcoef_Classified[sort_index,] = ARcoef[rid_index,]
    PDcoef_Classified[sort_index,] = PDcoef[rid_index,]
    ARpvalue_Classified[sort_index,] = ARpvalue[rid_index,]
    PDpvalue_Classified[sort_index,] = PDpvalue[rid_index,]
    plot_region.name[sort_index] = Plot_GroupName[rid]
  }
  
  ARpvalue_color = ARpvalue_Classified
  PDpvalue_color = PDpvalue_Classified
  ###p<0.05
  pid = which(ARpvalue_color<0.1)
  pidx = which(ARpvalue_color>=0.1)
  ARpvalue_color[pid] = "P < 0.1"
  ARpvalue_color[pidx] = "P ≥ 0.1"
  pid = which(PDpvalue_color<0.1)
  pidx = which(PDpvalue_color>=0.1)
  PDpvalue_color[pid] = "P < 0.1"
  PDpvalue_color[pidx] = "P ≥ 0.1"
  
  #plot(PDcoef_Classified[,3],col = PDpvalue_color[,3])
  
  
  for (c in 1:3){
  
  Name_C = as.character(c)
  if (c<3) {
    ###Heter-AR Plot
    TitleName <- "Heterogeneous AR Coefficients"
    
    if (c==1){ subTitleName = "Intercept" }
    if (c==2){ subTitleName = "Lagged House Price" }
    
    Coef = ARcoef_Classified[,c]
    P_value = ARpvalue_color[,c]
    ARcoef_data = data.frame(plot_region.name,Coef,P_value)
    
    Coef_Max = round(max(Coef)*1.3,3)
    Coef_Min = round(min(Coef)*1.1,3)
    
    figure_AR <- ggplot ()+
      geom_point (data = ARcoef_data,
                  aes (x = factor(ARcoef_data$plot_region.name,level=c("NE","NW","YS","EM","WM",
                                                                       "EE","LD","SE","SW","WS","SC")), 
                       y = ARcoef_data$Coef, colour = ARcoef_data$P_value),
                  size = 3, alpha = 0.8, shape = 16)+
      scale_color_manual(values = c("P < 0.1" = "green", "P ≥ 0.1" = "red")) +
      guides(color=guide_legend(title="P-Value")) +
      scale_y_continuous (limits = c (Coef_Min, Coef_Max), breaks = seq (Coef_Min, Coef_Max, round((Coef_Max-Coef_Min)/5,3)))+
      theme (panel.background = element_rect(fill = 'white', color = 'black'),
             panel.grid.major = element_line(color = 'grey', linetype = 'dotted'),
             panel.grid.minor = element_line(color = 'grey', linetype = 'dotted'),
             axis.text = element_text (size = 12, colour = "black"), 
             axis.title = element_text (size =15, colour = "black"), 
             legend.position = c (0.9, 0.9),
             legend.background = element_blank (),
             legend.text = element_text (size = 12), 
             text = element_text (family ="arial"),
             plot.title = element_text (family ="arial", size = 16))+
      labs (title = TitleName,
            subtitle = subTitleName)+
      labs ( x ="Regions", y = "Coefficients")
    
    filename <- paste(OutputSource,"HeterAR_Coef_",Name_C,sep = "")
    ggsave(filename = sprintf("%s.png", filename), plot = figure_AR,  device   = "png",dpi = 300)
    
    dev.new()
    plot(figure_AR)
  } 
    
  ###Heter-ARX Plot
  TitleName <- "Heterogeneous AR-X Coefficients"
  
  if (c==1){ subTitleName = "Intercept" }
  if (c==2){ subTitleName = "Lagged House Price" }
  if (c==3){ subTitleName = "Lagged Sales Volume" }
  
  Coef = PDcoef_Classified[,c]
  P_value = PDpvalue_color[,c]
  PDcoef_data = data.frame(plot_region.name,Coef,P_value)
  
  Coef_Max = round(max(Coef)*1.3,3)
  Coef_Min = round(min(Coef)*1.1,3)
  
  figure_PD <- ggplot ()+
    geom_point (data = PDcoef_data,
                aes (x = factor(PDcoef_data$plot_region.name,level=c("NE","NW","YS","EM","WM",
                                                                     "EE","LD","SE","SW","WS","SC")), 
                     y = PDcoef_data$Coef, colour = PDcoef_data$P_value),
                size = 3, alpha = 0.8, shape = 16)+
    scale_color_manual(values = c("P < 0.1" = "green", "P ≥ 0.1" = "red")) +
    guides(color=guide_legend(title="P-Value")) +
    scale_y_continuous (limits = c (Coef_Min, Coef_Max), breaks = seq (Coef_Min, Coef_Max, round((Coef_Max-Coef_Min)/5,3)))+
    theme (panel.background = element_rect(fill = 'white', color = 'black'),
           panel.grid.major = element_line(color = 'grey', linetype = 'dotted'),
           panel.grid.minor = element_line(color = 'grey', linetype = 'dotted'),
           axis.text = element_text (size = 12, colour = "black"), 
           axis.title = element_text (size =15, colour = "black"), 
           legend.position = c (0.9, 0.9),
           legend.background = element_blank (),
           legend.text = element_text (size = 12), 
           text = element_text (family ="arial"),
           plot.title = element_text (family ="arial", size = 16))+
    labs (title = TitleName,
          subtitle = subTitleName)+
    labs ( x ="Regions", y = "Coefficients")
  
    filename <- paste(OutputSource,"HeterARX_Coef_",Name_C,sep = "")
    ggsave(filename = sprintf("%s.png", filename), plot = figure_PD,  device   = "png",dpi = 300)
  
  dev.new()
  plot(figure_PD)

  }
  
  
 