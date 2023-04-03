#RScript to generate all fiures from the poster. This should be ran after all others


##Scaled vs Non-scaled boxplot(Figure 1)

#Before Scaling
library(reshape2)

Keep<-Pvals %>% 
  filter(Pvals$P < 0.000015) #p-value of  0.000015 leaves 8 genes
Keep<-paste(Keep$Gene)

SelectedFeatsNOSCALE<- Features %>% 
  dplyr::select(all_of(Keep))

BPNOscaleData = melt(SelectedFeatsNOSCALE)
colnames(BPNOscaleData) = c("Gene", "Expression")


library(ggplot2)

DataBoxplotBefScale = ggplot(BPNOscaleData, aes(x=Gene, y=Expression, color = Gene)) + geom_boxplot() + labs(x = "Gene", y = "Expression", title = "Non-scaled Data") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#After Scaling

SelectedFeatsAFTERSCALE<- Scaled %>% 
  dplyr::select(all_of(Keep))

BPscaleData = melt(SelectedFeatsAFTERSCALE)
colnames(BPscaleData) = c("Gene", "Expression")


DataBoxplotAfScale = ggplot(BPscaleData, aes(x=Gene, y=Expression, color = Gene)) + geom_boxplot() + labs(x = "Gene", y = "Expression", title = "Scaled Data") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



library(gridExtra)

grid.arrange(DataBoxplotAfScale, DataBoxplotBefScale)

##Feature importance p-value bar chart (Figure 2)

featureplot<-ggplot(aes(x=P),data=Pvals) +
  
  geom_histogram(bins=30, fill='blue', color='white', ) + xlab("p-value") + ylab("Feature Count")
print(featureplot)


##ROCPlots (Figure 3)

#Obtain ROCs

library(pROC)

rf_prediction <- predict(CovidRF, testing_set, type = "prob")
ROC_rf <- roc(testing_set$covid_status, rf_prediction[,2])
auc(ROC_rf)


svm_prediction <- predict(svmMod, testing_set)
roc_svm_test <- roc(testing_set$covid_status, as.numeric(svm_prediction))
auc(roc_svm_test)

NN_prediction <- predict(CovNN, testing_set)
roc_NN_test <- roc(testing_set$covid_status, NN_prediction[,2])
auc(roc_NN_test)

NB_prediction <- predict(NBmodel, testing_set)
roc_NB_test <- roc(testing_set$covid_status, as.numeric(NB_prediction))
auc(roc_NB_test)

roc_KNN_test <- roc(testing_set$covid_status, as.numeric(classifier_knn))
auc(roc_KNN_test)

#Make roc plots
p1r = ggroc(roc_svm_test, color = "blue")
p2r = ggroc(ROC_rf, color = "red")
p3r = ggroc(roc_NN_test, color = "green")
p4r = ggroc(roc_KNN_test, color = "yellow")
p5r = ggroc(roc_NB_test, color = "grey", )

#Arrange plots
library(grid)

grid.arrange(p1r, p2r, p3r, p4r, p5r, nrow = 2)

##Confusion Matrices (Figure 4)
library(dplyr)
library(yardstick)


#Neural Network
NNcm <- conf_mat(NNCONFMat, Observed, Predicted)
NNcm
p1 = autoplot(NNcm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("NN")+ labs(y="") #Empty y title so that predict can be a global title


#KNN
KNNcm <- conf_mat(KNNCovidConfMat, Observed, Predicted)
KNNcm
p2=autoplot(KNNcm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("KNN")+ labs(y="")

#NB
NBcm <- conf_mat(NBCONFMat, Observed, Predicted)
NBcm
p3=autoplot(NBcm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("NB")
#SVM
SVMcm <- conf_mat(SVMCONFmat, Observed, Predicted)
SVMcm
p4=autoplot(SVMcm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("SVM")+ labs(y="")

#RF
RFcm <- conf_mat(CovidConfMatRF, Observed, Predicted)
RFcm
p5=autoplot(RFcm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("RF") + labs(y="")

#arrange them together
grid.arrange(p1, p2, p3, p4, p5, nrow = 3)


##Model Metrics plot (Figure 5)

accuracydata<- read.csv("accuracy.csv")


ModelMetricPlot<- accuracydata %>% ggplot(aes(Metric, scaled, fill = type)) +
  
  geom_histogram(stat = "identity", position = "dodge2") +
  
  labs(y="Percentage",x="Model",
       
       title = "Accuracy of Models and Antigen/Molecular Test") +
  
  theme(axis.text = element_text(size = 6))

print(ModelMetricPlot)

#Feature importance Plot (Figure 6)

FeatImpPlot = ggplot(imp, aes(x=reorder(Features,Importance), y=Importance, fill=Features)) + geom_col() + geom_text(aes(label=round(Importance,2),y=Importance-0.3),size=4) +
  ylab("Feature Importance") +
  xlab("Gene Name")+
  ggtitle("Importance of Selected Features") +
  theme(plot.title=element_text(hjust=0.5,size=20),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15),axis.text.x=element_text(angle=90,vjust=0.5,hjust=1,size=13),legend.position="none") 

print(FeatImpPlot)

