
    

#Re-plot confidence matrices
CovidConfMatRF = data.frame(Obs = as.factor(testing_set$covid_status), Pred = predict(CovidRF, testing_set, type="class")) 
CovidConfMatRF = table(CovidConfMatRF) 
CovidConfMatRF


FULLRFDATA = BinCombinedCovidData[,-c(2)]
trainIndex <- createDataPartition(FULLRFDATA$covid_status, p = 0.6, list = FALSE)

training_set <- FULLRFDATA[trainIndex, ]
testing_set <- FULLRFDATA[-trainIndex, ]

FullRF = randomForest(as.factor(covid_status) ~., data = training_set) 

#Check feature importance 

FeatImp = FullRF$importance

imp = varImpPlot(CovidRF,cex=1,main="Importance of Selected Features")



# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(FeatImp)
imp$Features <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$Importance = imp$MeanDecreaseGini
imp$MeanDecreaseGini = NULL
rownames(imp) = order(imp$Importance)
imp


# this is the plot part, be sure to use reorder with the correct measure name
library(ggplot2) 
ggplot(imp, aes(x=Features, y=Importance, fill=Features)) + geom_col()+
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name")

  