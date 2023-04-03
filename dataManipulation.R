#RSript to process raw data directly from publication. Should be run first

#Import two datasets

IDData = read.csv("metatable_with_viral_status.csv")
GeneCountData = read.csv("swab_gene_counts.csv")


#Transpose GeneCountData

TransGeneCountData = data.frame(t(GeneCountData))

#Adjust colnames

colnames(TransGeneCountData) <- TransGeneCountData[1,]
TransGeneCountData <- TransGeneCountData[-1,]

#Add the viral status and patient ID columns to the GeneCountData
library(dplyr)
library(tibble)

TransGeneCountData <- TransGeneCountData %>% rownames_to_column(var = "G_ID")

#Merge Datasets

CombinedCovidData <- cbind(IDData[,c(1,8)], TransGeneCountData)

#Check to make sure Patient IDs (CBZ_ID) match up in both samples

Check = sum(CombinedCovidData$CZB_ID == CombinedCovidData$G_ID)
Check == 234 #should be True if there are 234 total patients

#Delete one of patient ID rows

CombinedCovidData<- CombinedCovidData[, -which(names(CombinedCovidData) == "G_ID")]


#Binarize the viral status column into covid (SC2) and non_covid (other_virus & no_virus).

covid_status <- ifelse(CombinedCovidData$viral_status %in% c("other_virus", "no_virus"), "no_covid", ifelse(CombinedCovidData$viral_status == "SC2", "covid", NA))
BinCombinedCovidData<-cbind(covid_status,CombinedCovidData)
BinCombinedCovidData<- BinCombinedCovidData[, -which(names(BinCombinedCovidData) == "viral_status")]

#Transform all gene count values to numerical

BinCombinedCovidData[, c(3:15981)] <- apply(BinCombinedCovidData[, c(3:15981)], 2, as.numeric)

#Scale Features

Features<-BinCombinedCovidData %>% 
  dplyr::select(-c(1:2))

Scaled<-Features %>% mutate_all(scale)


#Combine covid status and patient ID columns with scaled features

ScaledCovidData = cbind(BinCombinedCovidData[,(1:2)], Scaled)




