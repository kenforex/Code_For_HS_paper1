library(abind)
library(magic)
library(psych)
library(survival)
library(AER)
library(gsynth)
library(pracma)
library(plyr)
library(Matrix)
library(carData)
library(car)
library(KRLS)
library(foreign)
library(Metrics)
library(MASS)
library(strucchange)
library(zoo)
library(sandwich)
library(urca)
library(lmtest)
library(vars)
library(geometry)
library(forecast)
library(tictoc)
library(plm)
library(xlsx)
library(moments)
library(tseries)

#Function 1
getRegDf<-function(logYmatNchar,  W) 
{
  
  Time = ncol(logYmatNchar); tt = 1:Time
  
  logYmatNchar1 = W%*%logYmatNchar
  
  #logYmatNchar = logYmatNchar[-1,]
  
  #Zmat = do.call(rbind, rep(list(Nodal), Time-1))
  
  
  
  X = cbind(network_effect = as.vector(logYmatNchar1[,tt[-length(tt)]]),                                        ### obtain all variables in X
            
            self_effect = as.vector(logYmatNchar[,tt[-length(tt)]]) )
  
  Yvec = as.vector(logYmatNchar[,tt[-1]])                                                                ### the response Y vector
  
  regDf = data.frame(Y = Yvec, X)
  
  return(regDf)
  
}

#Function 2
NAR_noX<-function(logYmatNchar,  W, Lags) 
{
  
  Time = ncol(logYmatNchar); tt = 1:Time
  
  logYmatNchar1 = W%*%logYmatNchar
  
  #logYmatNchar = logYmatNchar[-1,]
  
  #Zmat = do.call(rbind, rep(list(Nodal), Time-1))
  
  X = cbind(self_effect_Lag1 = as.vector(logYmatNchar[,tt[Lags:(length(tt)-1)]]),
            
            network_effect_Lag1 = as.vector(logYmatNchar1[,tt[Lags:(length(tt)-1)]])                                        ### obtain all variables in X
            
             )
  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    X = cbind(X,
              self_effect_fromLag2 = as.vector(logYmatNchar[,tt[L:(length(tt)-l)]]),
              network_effect_fromLag2 = as.vector(logYmatNchar1[,tt[L:(length(tt)-l)]])
              
              )
  }
  
  Yvec = as.vector(logYmatNchar[,(Lags+1):length(tt)])                                                                ### the response Y vector
  
  regDf = data.frame(Y = Yvec, X)
  
  return(regDf)
  
}

#Function 3
getRegDf_withlags_NoW<-function(logYmatNchar, W, Lags) 
{
  
  Time = ncol(logYmatNchar); tt = 1:Time
  
  #logYmatNchar1 = W%*%logYmatNchar
  
  #logYmatNchar = logYmatNchar[-1,]
  
  #Zmat = do.call(rbind, rep(list(Nodal), Time-1))
  
  X = cbind( self_effect_Lag1 = as.vector(logYmatNchar[,tt[Lags:(length(tt)-1)]]) )                                        ### obtain all variables in X
            

  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    X = cbind(X,
              self_effect_fromLag2 = as.vector(logYmatNchar[,tt[L:(length(tt)-l)]])
              
    )
  }
  
  Yvec = as.vector(logYmatNchar[,(Lags+1):length(tt)])                                                                ### the response Y vector
  
  regDf = data.frame(Y = Yvec, X)
  
  return(regDf)
  
}

getRegDf_AR_withlags<-function(logYmatNchar,Lags) 
{
  
  Time = length(logYmatNchar); tt = 1:Time

  #logYmatNchar = logYmatNchar[-1,]
  
  #Zmat = do.call(rbind, rep(list(Nodal), Time-1))
  
  X = cbind(self_effect_Lag1 = as.vector(logYmatNchar[tt[Lags:(length(tt)-1)]])                                        ### obtain all variables in X
            
  )
  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    X = cbind(X,
              self_effect_fromLag2 = as.vector(logYmatNchar[tt[L:(length(tt)-l)]])
              )
  }
  
  Yvec = as.vector(logYmatNchar[(Lags+1):length(tt)])                                                                ### the response Y vector
  
  regDf = data.frame(Y = Yvec, X)
  
  return(regDf)
  
}

#Get regression In-Sample Data with X (explanatory variables)
getX<-function(X,Y,W,N,Lags,Wexsit = TRUE) 
{ 
  Yvec = Y #Yvec: Dependent Variable (lies on the LHS of the regression formula)
  Time = ncol(Yvec); tt = 1:Time
  X = as.matrix(X)
  nvar = ncol(X)
  XX = vector("list",nvar)
  for (i in 1:nvar){
    
    tX = t(matrix(X[,i],nrow(X)/N,N,byrow = TRUE))
    XX[[i]] = tX
  }
  
  X1 = W%*%Y
  
  #Xvec: Independent Variable (lies on the RHS of the regression formula)
  if (Wexsit == TRUE){
    Xvec = cbind(self = as.vector(Y[,tt[Lags:(length(tt)-1)]]),
                 network = as.vector(X1[,tt[Lags:(length(tt)-1)]]))
  } else { Xvec = as.vector(Y[,tt[Lags:(length(tt)-1)]]) }
  
  for (i in 1:nvar){
    
    Xvec = cbind(Xvec,
                 as.vector(XX[[i]][,tt[Lags:(length(tt)-1)]]) )
  }
  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    
    if (Wexsit == TRUE){
      Xvec = cbind(Xvec,
                   self = as.vector(Y[,tt[L:(length(tt)-l)]]),
                   network = as.vector(X1[,tt[L:(length(tt)-l)]]) )
    } else { Xvec = cbind(Xvec,
                          self = as.vector(Y[,tt[L:(length(tt)-l)]]) )}
    
    
    for (i in 1:nvar){
      
      Xvec = cbind(Xvec,
                   as.vector(XX[[i]][,tt[L:(length(tt)-l)]]) )
    }
    
  }
  
  Yvec = as.vector(Yvec[,tt[(Lags+1):length(tt)]])
  
  regDf = data.frame(Y = Yvec, Xvec)
  
  return(regDf)
  
}



#Get regression In-Sample Data for the use of pool estimates with Individual Heterogeneous FEs (with X (explanatory variables))
getX_HeterFE<-function(X,Y,W,N,Lags,Wexsit = TRUE) 
{ 
  Yvec = Y #Yvec: Dependent Variable (lies on the LHS of the regression formula)
  Time = ncol(Yvec); tt = 1:Time
  X = as.matrix(X)
  nvar = ncol(X)
  XX = vector("list",nvar)
  for (i in 1:nvar){
    
    tX = t(matrix(X[,i],nrow(X)/N,N,byrow = TRUE))
    XX[[i]] = tX
  }
  
  X1 = W%*%Y
  
  #Xvec: Independent Variable (lies on the RHS of the regression formula)
  if (Wexsit == TRUE){
    Xvec = cbind(as.vector(Y[,tt[Lags:(length(tt)-1)]]),
                 as.vector(X1[,tt[Lags:(length(tt)-1)]]))
  } else { Xvec = as.vector(Y[,tt[Lags:(length(tt)-1)]]) }
  
  for (i in 1:nvar){
    
    Xvec = cbind(Xvec,
                 as.vector(XX[[i]][,tt[Lags:(length(tt)-1)]]) )
  }
  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    
    if (Wexsit == TRUE){
      Xvec = cbind(Xvec,
                   as.vector(Y[,tt[L:(length(tt)-l)]]),
                   as.vector(X1[,tt[L:(length(tt)-l)]]) )
    } else { Xvec = cbind(Xvec,
                          as.vector(Y[,tt[L:(length(tt)-l)]]) )}
    
    
    for (i in 1:nvar){
      
      Xvec = cbind(Xvec,
                   as.vector(XX[[i]][,tt[L:(length(tt)-l)]]) )
    }
    
  }
  
  Yvec = as.vector(Yvec[,tt[(Lags+1):length(tt)]])
  
  FixedEffects = as.character(1:N)
  
  regDf = data.frame(Y = Yvec, FE = FixedEffects, X = Xvec)
  
  return(regDf)
  
}



#Get regression In-Sample Data with X (explanatory variables) and WX (explanatory variables with spatial matrix)
getX_WX<-function(X,Y,W,N,Lags,Wexsit = TRUE) 
{ 
  Yvec = Y #Yvec: Dependent Variable (lies on the LHS of the regression formula)
  Time = ncol(Yvec); tt = 1:Time
  X = as.matrix(X)
  nvar = ncol(X)
  XX = vector("list",nvar)
  for (i in 1:nvar){
    
    tX = t(matrix(X[,i],nrow(X)/N,N,byrow = TRUE))
    XX[[i]] = tX
  }
  
  X1 = W%*%Y1
  
  #Xvec: Independent Variable (lies on the RHS of the regression formula)
  if (Wexsit == TRUE){
    Xvec = cbind(self = as.vector(Y[,tt[Lags:(length(tt)-1)]]),
                 network = as.vector(X1[,tt[Lags:(length(tt)-1)]]))
  } else { Xvec = as.vector(Y[,tt[Lags:(length(tt)-1)]]) }
  
  for (i in 1:nvar){
    
    if (Wexsit == TRUE){
      ZW = W%*%XX[[i]]
      Xvec = cbind(Xvec,
                   as.vector(XX[[i]][,tt[Lags:(length(tt)-1)]]),
                   as.vector(ZW[,tt[Lags:(length(tt)-1)]]))
    } else { Xvec = cbind(Xvec,
                          as.vector(XX[[i]][,tt[Lags:(length(tt)-1)]]) ) }
    
  }
  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    
    if (Wexsit == TRUE){
      Xvec = cbind(Xvec,
                   self = as.vector(Y[,tt[L:(length(tt)-l)]]),
                   network = as.vector(X1[,tt[L:(length(tt)-l)]]) )
    } else { Xvec = cbind(Xvec,
                          self = as.vector(Y[,tt[L:(length(tt)-l)]]) )}
    
    
    for (i in 1:nvar){
      
      Xvec = cbind(Xvec,
                   as.vector(XX[[i]][,tt[L:(length(tt)-l)]]) )
    }
    
  }
  
  Yvec = as.vector(Yvec[,tt[(Lags+1):length(tt)]])
  
  regDf = data.frame(Y = Yvec, Xvec)
  
  return(regDf)
  
}

#Get regression In-Sample Data but without X (explanatory variables)
getX_noX<-function(Y,W,N,Lags,Wexsit = TRUE) 
{ 
  Yvec = Y #Yvec: Dependent Variable (lies on the LHS of the regression formula)
  Time = ncol(Yvec); tt = 1:Time
  
  X1 = W%*%Y
  
  #Xvec: Independent Variable (lies on the RHS of the regression formula)
  if (Wexsit == TRUE){
    Xvec = cbind(self = as.vector(Y[,tt[Lags:(length(tt)-1)]]),
                 network = as.vector(X1[,tt[Lags:(length(tt)-1)]]))
  } else { Xvec = as.vector(Y[,tt[Lags:(length(tt)-1)]]) }
  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    
    if (Wexsit == TRUE){
      Xvec = cbind(Xvec,
                   self = as.vector(Y[,tt[L:(length(tt)-l)]]),
                   network = as.vector(X1[,tt[L:(length(tt)-l)]]) )
    } else { Xvec = cbind(Xvec,
                          self = as.vector(Y[,tt[L:(length(tt)-l)]]) )}
    
  }
  
  Yvec = as.vector(Yvec[,tt[(Lags+1):length(tt)]])
  
  regDf = data.frame(Y = Yvec, Xvec)
  
  return(regDf)
  
}


#Get regression In-Sample Data for the use of pool estimates with Individual Heterogeneous FEs (without X (explanatory variables))
getX_noX_HeterFE<-function(Y,W,N,Lags,Wexsit = TRUE) 
{ 
  Yvec = Y #Yvec: Dependent Variable (lies on the LHS of the regression formula)
  Time = ncol(Yvec); tt = 1:Time
  
  X1 = W%*%Y
  
  #Xvec: Independent Variable (lies on the RHS of the regression formula)
  if (Wexsit == TRUE){
    Xvec = cbind(as.vector(Y[,tt[Lags:(length(tt)-1)]]),
                 as.vector(X1[,tt[Lags:(length(tt)-1)]]))
  } else { Xvec = as.vector(Y[,tt[Lags:(length(tt)-1)]]) }
  
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    
    if (Wexsit == TRUE){
      Xvec = cbind(Xvec,
                   as.vector(Y[,tt[L:(length(tt)-l)]]),
                   as.vector(X1[,tt[L:(length(tt)-l)]]) )
    } else { Xvec = cbind(Xvec,
                          as.vector(Y[,tt[L:(length(tt)-l)]]) )}
    
  }
  
  Yvec = as.vector(Yvec[,tt[(Lags+1):length(tt)]])
  
  FixedEffects = as.character(1:N)
  
  regDf = data.frame(Y = Yvec, FE = FixedEffects, X = Xvec)
  
  return(regDf)
  
}

#AR model:Get regression In-Sample Data but without X (explanatory variables)
getX_AR<-function(Xold,Yar,N,Roll,nn,Lags) 
{ 
  Time = length(Yar); tt = 1:Time
  nvar = ncol(Xold)
  
  #Xpd: Independent Variable (lies on the RHS of the regression formula)
  Xpd = as.matrix(Yar[tt[Lags:(length(tt)-1)]]) 
  xxpd = as.matrix(rep(0,Roll))
  for (i in 1:nvar){
    xx = t(matrix(Xold[,i],nrow(Xold)/N,N,byrow = TRUE))
    xx = xx[n,]
    xx = as.matrix(xx[nn:(nn+Roll-1)])
    xxpd = cbind(xxpd, xx)
  }
  xxpd = xxpd[,-1]
  if (nvar>1){
    Xpd = cbind(Xpd,
                as.matrix(xxpd[tt[Lags:(length(tt)-1)],]) )
  } else {
    Xpd = cbind(Xpd,
                as.matrix(xxpd[tt[Lags:(length(tt)-1)]]) )
  }
  l = 1
  L = Lags
  while (L>1){
    l = l + 1
    L = L - 1
    if (nvar>1){
      Xpd = cbind(Xpd,
                  as.matrix(Yar[tt[L:(length(tt)-l)]]),
                  as.matrix(xxpd[tt[L:(length(tt)-l)],])
      )
    } else {
      Xpd = cbind(Xpd,
                  as.matrix(Yar[tt[L:(length(tt)-l)]]),
                  as.matrix(xxpd[tt[L:(length(tt)-l)]])
      )
    }
    
  }
  Ypd = as.matrix(Yar[tt[(Lags+1):length(tt)]]) #Ypd: Dependent Variable (lies on the LHS of the regression formula)
  
  regDf = data.frame(Y = Ypd, Xpd)
  
  return(regDf)
  
}


