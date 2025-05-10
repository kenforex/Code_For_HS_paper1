rm(list=ls())

########################################Read Functions########################################
CodeSource = "~/Desktop/Chapter 1_Forecast/code/New Code/Code For Publication/" ##Set Code Directory##
FuncName = paste(CodeSource,"Functions.R",sep="")
source(FuncName) 
library(foreign)

########################################Read Data########################################
##Set Original Data and Outputs (Filtered Data) Directory##
FileSource = "~/Desktop/Chapter 1_Forecast/dataP/"
OutputSource = "~/Desktop/Chapter 1_Forecast/dataP/" 

FileName = paste(FileSource,"UK-HPI-full-file-2024-01.csv",sep="")
raw_data = read.csv(file = FileName)

shpName = paste(FileSource,"LAD_MAY_2023_UK_BGC_V2.dbf",sep="")
shp = read.dbf(shpName)

CPI_Name = paste(FileSource,"cpi.csv",sep="")
CPI = read.csv(CPI_Name)

Region_Code_EN_Name = paste(FileSource,"Local_Authority_District_to_Region_(December_2023)_Lookup_in_England.csv",sep="")
Region_Code_EN = read.csv(Region_Code_EN_Name)

########################################Filtering Data########################################
###Step 1 Select columns for needed variables
Col = c(1:4,10)
data = raw_data[,Col]

###Step 2 Select Time from 2004m1 to 2023m12
Time_Col = data$Date
Year = as.matrix( as.numeric( substr(Time_Col,7,10) ) )
Year_rowFilter = which((Year>=2004)&(Year<=2022))
data = data[Year_rowFilter,]

###Step 3 Delete Northern Ireland data due to lack of data
Code_Col = data$AreaCode
Region_Code_NI = as.matrix( substr(Code_Col,1,1) )
Region_NI_rowFilter = which(Region_Code_NI=="N")
data = data[-Region_NI_rowFilter,]

###Step 4 Delete LADs that does not exist in shapefile
LADcode = as.matrix(shp$LAD23CD)
TableName = rownames( as.matrix(table(data$AreaCode)) )
#Start Search and Delete Process
del = 0
for (n in 1:length(TableName)){
  Match = which(LADcode == TableName[n])
  if (identical(Match, integer(0))){
    q = which(data$AreaCode == TableName[n])
    del = c(del, q)
  }
}
del = del[-1]
data = data[-del,]
TableName = as.matrix( rownames( as.matrix(table(data$AreaCode)) ) )
#Delete shp's LADs that are in shapefile but not in our filtered dataset
shp_del = setdiff(LADcode,TableName)
for (n in 1:length(shp_del)){
  shp_Match = which(shp$LAD24CD==shp_del[n])
  shp = shp[-shp_Match,]
}

###Step 5 Assign regional code
Region_Code = as.matrix(rep(0,length(TableName))) #Set up RegionCode column
RegionName = TableName #Set up RegionName column
Lad_Region_Code = as.matrix(cbind(TableName,Region_Code,RegionName))
colnames(Lad_Region_Code) = c("LAD Code","Region ID","Region Name")
#England 9 regions
for (n in 1:nrow(Region_Code_EN)){
  Lad_Match = which(Lad_Region_Code[,1]==Region_Code_EN[n,1])
  Lad_Region_Code[Lad_Match,3] = Region_Code_EN[n,4]
  Lad_Region_Code[Lad_Match,2] = substr(Region_Code_EN[n,3],9,9)
}
#Scotland
Region_Code_Scot = as.matrix( substr(Lad_Region_Code[,1],1,1) )
Region_Scot_rowFilter = which(Region_Code_Scot=="S")
Lad_Region_Code[Region_Scot_rowFilter,3] = "Scotland"
Lad_Region_Code[Region_Scot_rowFilter,2] = "11"
#Wales
Region_Code_Wales = as.matrix( substr(Lad_Region_Code[,1],1,1) )
Region_Wales_rowFilter = which(Region_Code_Wales=="W")
Lad_Region_Code[Region_Wales_rowFilter,3] = "Wales"
Lad_Region_Code[Region_Wales_rowFilter,2] = "10"


########################################Dealing with Data########################################
N = length(table(data$AreaCode))
T = nrow(data)/N
###House price using logarithm and divided by CPI
CPI = as.matrix(rep(CPI$CPI,N))
data$AveragePrice = log( data$AveragePrice/CPI )
###Sales using logarithm
data$SalesVolume = log( data$SalesVolume )
###Set Trend Number
trend = as.matrix( rep(0:(T-1),N) )
data = cbind(trend,data)
###Sort data by areacode and then by trend
data_sort1 = data[order(data$AreaCode),]
data_sort2 = data_sort1[order(data_sort1$trend),]
data = data_sort2
###Set Group Number
group = as.matrix( rep(1:N,T) )
data = cbind(group,data)
###First Difference
Price_Matrix = t(matrix(data$AveragePrice,T,N,byrow = TRUE))
Sales_Matrix = t(matrix(data$SalesVolume,T,N,byrow = TRUE))
Diff_Price_Matrix = Price_Matrix[,2:T] - Price_Matrix[,1:(T-1)] #First Difference
Diff_Sales_Matrix = Sales_Matrix[,2:T] - Sales_Matrix[,1:(T-1)] #First Difference
Diff_data = data[-(which(data$trend==0)),] #Delete Trend 0
Diff_data$AveragePrice = as.vector(Diff_Price_Matrix)
Diff_data$SalesVolume = as.vector(Diff_Sales_Matrix)


########################################Output Data########################################
DataOutputName = paste(OutputSource,"dataNew_NoReturns.csv",sep = "")
write.csv(data,file = DataOutputName,row.names = FALSE)
DataOutputName = paste(OutputSource,"dataNew.csv",sep = "")
write.csv(Diff_data,file = DataOutputName,row.names = FALSE)

Lad_Region_Code_OutputName = paste(OutputSource,"Lad_Region_Code.csv",sep = "")
write.csv(Lad_Region_Code,file = Lad_Region_Code_OutputName,row.names = FALSE)



