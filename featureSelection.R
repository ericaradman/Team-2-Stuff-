#Rscript to select 8-10 most predictive features

#Import Data that was generate using data manipulation script
FeatSelData = ScaledCovidData

#Create Data frame with just features

ScaledFeatures<-FeatSelData %>% 
  dplyr::select(-c(1:2))


#Run feature selection using lm method and assign p-values to each feature to assign predictive power

library(tidyr)
FeatureSel<-ScaledFeatures %>% 
  mutate(covid_status=FeatSelData$covid_status) %>% 
  pivot_longer(cols=-covid_status,
               names_to="Gene",
               values_to="Expression")


Pvals <- FeatureSel %>% 
  group_by(Gene) %>% 
  summarize(P = anova(lm(Expression ~ covid_status))[1,"Pr(>F)"]) %>% 
  dplyr::select(Gene,P)

#Assign p-value cutoff to only keep aroudn 8-10 features

Keep<-Pvals %>% 
  filter(Pvals$P < 0.000015) #p-value of  0.000015 leaves 8 genes
Keep<-paste(Keep$Gene)

SelectedFeats<- ScaledFeatures %>% 
  dplyr::select(all_of(Keep))
names(SelectedFeats)

#Add covid status and patient ID columns to selected features data to create final dataset useed to train classifiers

FinalFeatData <- cbind(FeatSelData[,c(1:2)], SelectedFeats)





