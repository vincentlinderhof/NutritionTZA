library(polycor)
library(Hmisc)

# read in HQI files
dataPath <- "C:/Users/afver/OneDrive/Documenten/Removable Disk/verbe038/TZA"
HQI2008<- readRDS(file.path(dataPath, "HQI2008.rds")) 
HQI2010<- readRDS(file.path(dataPath, "HQI2010.rds")) 
HQI2012<- readRDS(file.path(dataPath, "HQI2012.rds")) 


#Function for format
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#create matrices
#2008
res2008<-rcorr(as.matrix(HQI2008[,10:16]))
flattenCorrMatrix(res2008$r, res2008$P)
#2010
res2010<-rcorr(as.matrix(HQI2010[,10:16]))
flattenCorrMatrix(res2008$r, res2008$P)
#2012
res2012<-rcorr(as.matrix(HQI2012[,10:16]))
flattenCorrMatrix(res2008$r, res2008$P)


