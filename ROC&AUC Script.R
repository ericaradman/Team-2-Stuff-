
# Load libraries 
library(pROC)
library(MASS)
library(tidyverse)
library(dplyr)
library(tidyr)
```
# Load data
```{r}
CDat = read.csv("GoodCovidDataWLabels.csv")
head(CDat)
str(CDat)
```

# Binarize Data to combine other_virus and no_virus categories (same as feature selection)
```{r}
CDat$covid_status <- ifelse(CDat$viral_status %in% c("other_virus", "no_virus"), "no_covid", ifelse(CDat$viral_status == "SC2", "covid", NA))
covid_status<-CDat$covid_status
CDat$viral_status<-NULL
CDat<-cbind(covid_status,CDat)
head(CDat)
```

# Create new dataframes with just labels and features
```{r}
RespDat<-CDat %>% 
  dplyr::select(1:3)

Features<-CDat %>% 
  dplyr::select(-c(1:3))
```

# Run feature selection using lm method and assign p-values to each feature to assign predictive power
```{r}

FeatureSel<-Features %>% 
  mutate(covid_status=CDat$covid_status) %>% 
  pivot_longer(cols=-covid_status,
               names_to="Gene",
               values_to="Expression")

Pvals <- FeatureSel %>% 
  group_by(Gene) %>% 
  summarize(P = anova(lm(Expression ~ covid_status))[1,"Pr(>F)"]) %>% 
  dplyr::select(Gene,P)

```

#Assign p-value cutoff to only keep aroudn 8-10 features
```{r}
str(FeatureSel)

Keep<-Pvals %>% 
  filter(Pvals$P < 0.000015) #p-value of 0.000015 leaves 8 genes
Keep<-paste(Keep$Gene)

ScaledSub<- Features %>% 
  dplyr::select(all_of(Keep))
names(ScaledSub)
```
# Subset original data to only use these columns
```{r}


subset_df <- CDat[, c("covid_status", "Patient_ID", "ENSG00000115590", "ENSG00000133067", "ENSG00000133328", "ENSG00000163359", "ENSG00000165949", "ENSG00000167996", "ENSG00000176845", "ENSG00000188215")]
print(subset_df)
Data<-subset_df
```

# Run random forest 
```{r}
library(randomForest)
library(caret)
Data<- Data[,-c(2)]
dim(Data)
head(Data)

TrainingSet = (1:nrow(Data)) %%2 == 1
TestSet = (1:nrow(Data)) %%2 == 0

TrainingCancerData = Data[TrainingSet,]
dim(TrainingCancerData)

TestingCancerData = Data[TestSet,]
dim(TestingCancerData)


TrainData = TrainingCancerData
TestData = TestingCancerData
head(TrainData)

CovidRF = randomForest(as.factor(covid_status) ~., data = TrainData)
```

# SVM
```{r}
library(e1071)

svmMod<-svm(as.factor(covid_status)~., data=TrainData, kernel="linear")
summary(svmMod)

ConfMat<-data.frame(Obs=TestData$covid_status,pred=predict(svmMod))
table(ConfMat) #SVM shows very poor accuracy (55%). Sens is also much lower
```

# ROCs for rf and svm 
```{r}
rf_prediction <- predict(CovidRF, TestData, type = "prob")
ROC_rf <- roc(TestData$covid_status, rf_prediction[,2])


svm_prediction <- predict(svmMod, TestData)
roc_svm_test <- roc(TestData$covid_status, as.numeric(svm_prediction))


plot(ROC_rf, col = "red", main = "ROC For Random Forest (red) and svm (green)")
lines(roc_svm_test, col = "green")
```


# Run Naive Bayes
```{r}
library(naivebayes)
NBmodel <- naive_bayes(covid_status ~ ., data = TrainData, usekernel = T)
p2 <- predict(NBmodel, TestData)
tab2 <- table(p2, TestData$covid_status)
tab2 #NB model also has low accuracy
```

# Run KNN
```{r}

library(class)
classifier_knn <- knn(TrainData[,-c(1)], TestData[,-c(1)],
                      cl =TrainData$covid_status,
                      k = 11)
classifier_knn
cm <- table(TestData$covid_status, classifier_knn)
cm #Shows low sensitivity, decent specificity
```


# ROCs for KNN and naive bayes
```{r}
rf_prediction <- predict(CovidRF, TestData, type = "prob")
ROC_rf <- roc(TestData$covid_status, rf_prediction[,2])
auc_rf<- auc(ROC_rf)
auc_rf

svm_prediction <- predict(svmMod, TestData)
roc_svm_test <- roc(TestData$covid_status, as.numeric(svm_prediction))
auc_svm<- auc(roc_svm_test)
auc_svm

prob <- attr(cm, "prob")
roc_KNN<- roc(TestData$covid_status, as.numeric(prob))

nb_prediction<- predict(NBmodel, TestData)
roc_nb<- roc(TestData$covid_status, as.numeric(nb_prediction))
auc_nb<- auc(roc_nb)
auc_nb

plot(ROC_rf, col = "red", main = "ROC For Random Forest (red), SVM (green), Naive Bayes (blue)")
lines(roc_svm_test, col = "green")
lines(roc_nb, col = "blue")
legend("bottomright", legend=c("NB AUC=",auc_nb, "SVM AUC = ",auc_svm, "RF AUC=", auc_rf), title="AUC Values", text.font=4)

