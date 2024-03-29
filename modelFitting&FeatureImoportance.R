#RScript to generate models that can predict whether a patient has COVID_19 

#Objective1 - Assess metrics of several models to see if they are comparative to current diagnostic tool

#Generate Training and Test data

set.seed(123)
MFData = FinalFeatData[,-c(2)] #Delete patient ID column as it won't be used in model training

library(ggplot2)
library(caret)

trainIndex <- createDataPartition(MFData$covid_status, p = 0.6, list = FALSE)

training_set <- MFData[trainIndex, ]
testing_set <- MFData[-trainIndex, ]


#Create Function to assess sensitivity, specificity, and accuracy

calculate_metrics <- function(cm) {
  accuracy <- sum(diag(cm)) / sum(cm)
  specificity <- cm[2, 2] / sum(cm[2, ])
  sensitivity <- cm[1, 1] / sum(cm[1, ])
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}

#Train a RF model

set.seed(973)
library(randomForest)

CovidRF = randomForest(as.factor(covid_status) ~., data = training_set)

CovidConfMatRF = data.frame(Observed = as.factor(testing_set$covid_status), Predicted = predict(CovidRF, testing_set, type="class"))
CovidConfMatRF = table(CovidConfMatRF)
CovidConfMatRF 

calculate_metrics(CovidConfMatRF)

#Train SVM

library(e1071)

svmMod<-svm(as.factor(covid_status)~., data=training_set, kernel="linear")

SVMCovidConfMat<-data.frame(Observed=testing_set$covid_status,Predicted=predict(svmMod, testing_set, type = "class"))
SVMCONFmat = table(SVMCovidConfMat) 
calculate_metrics(SVMCONFmat)

#Train Naive Bayes Model

library(naivebayes)
NBmodel <- naive_bayes(covid_status ~ ., data = training_set, usekernel = T)
NBCovidConfMat<-data.frame(Observed=testing_set$covid_status,Predicted=predict(NBmodel, testing_set, type = "class"))
NBCONFMat=table(NBCovidConfMat)
NBCONFMat
calculate_metrics(NBCONFMat)

#Run KNN

library(class)
classifier_knn <- knn(training_set[,-c(1)], testing_set[,-c(1)],
                      cl =training_set$covid_status,
                      k = 9)
KNNCovidConfMat <- table(Observed=(testing_set$covid_status), Predicted = (classifier_knn))
KNNCovidConfMat
calculate_metrics(KNNCovidConfMat)

#Train a trilayered neural network

library(neuralnet)

set.seed(79)
CovNN = neuralnet(as.formula("covid_status ~ ."), data = training_set, hidden = c(10,20,10))


pred <- predict(CovNN, testing_set)
labels <- c("covid", "no_covid")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()

NNCONFMat = table(Observed = (testing_set$covid_status), Predicted=(prediction_label))
calculate_metrics(NNCONFMat)


#Objective2 - Discover feature importance in the RF model to understand what genes are most involved in dictating covid vs no covid 

#Re-plot confidence matrices

CovidConfMatRF = data.frame(Obs = as.factor(testing_set$covid_status), Pred = predict(CovidRF, testing_set, type="class"))
CovidConfMatRF = table(CovidConfMatRF)
CovidConfMatRF

#Create feature importance data

FeatImp = CovidRF$importance
imp <- as.data.frame(FeatImp)
imp$Features <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$Importance = imp$MeanDecreaseGini
imp$MeanDecreaseGini = NULL
imp

#change feature names

imp$Features[which(imp$Features=="ENSG00000167996")]="FTH1"
imp$Features[which(imp$Features=="ENSG00000163359")]="COL6A3"
imp$Features[which(imp$Features=="ENSG00000115590")]="IL1R2"
imp$Features[which(imp$Features=="ENSG00000176845")]="METRNL"
imp$Features[which(imp$Features=="ENSG00000133067")]="LGR6"
imp$Features[which(imp$Features=="ENSG00000133328")]="PLAAT2"
imp$Features[which(imp$Features=="ENSG00000188215")]="DCUN1D3"
imp$Features[which(imp$Features=="ENSG00000165949")]="IFI27"






