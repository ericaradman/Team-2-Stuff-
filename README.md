# Team-2-Stuff-
Final group project (Team Covid or Novid) 

Note: All scripts must be ran in order

Note: All scripts must be ran in order

Begin by running the dataManipulation.R script. The dataset used in this project was a combination of two datasets from the [original publication](https://www.nature.com/articles/s41467-020-19587-y). The [first dataset](https://github.com/czbiohub/covid19-transcriptomics-pathogenesis-diagnostics-results/blob/master/data/metatable_with_viral_status.csv) contains the viral status of each patient in the study as well as other identifier information (gender,age,etc). The [second dataset](https://github.com/czbiohub/covid19-transcriptomics-pathogenesis-diagnostics-results/blob/master/data/swab_gene_counts.csv) contains the gene count data for the patients. 

The dataManipulation.R script combines the viral status and patient ID columns from the first dataset with the gene counts of the second dataset (CombCovidData). It then binarizes the viral status column to contain just non-covid and covid categories which is more representative of an actual covid test. It also scales all of the features to obtain a consistent ditribution for each feature.This outputs a dataset containing binary covid status, patient ID, and scaled gene expression data. 

Next, the featureSelection.R script can be ran. This script takes the processed data generated from dataManipulation.R and selects the 8 most predictive features using a defined p-value cutoff. This number of genes was chosen as Rao et al. (2022) reports 8-10 gene host expression tests are suitable for clinical translation. This outputs a dataset containing covid status, patient ID, and scaled gene expression data for the 8 features.

Now that the most predictive features are selected and the data is scaled, the modelFitting&FeatureImoportance.R script can be run. This script creates a 60:40 training and test set and fits 5 different classification models (Random Forest, Support Vector Machine, Naive Bayes, K-Nearest Neighbor, and Neural Network). It also creates a confusion matrix and calculates accuracy, specificity, and sensitivity for each model via the function "calculate_metrics" that is created at the beginning of the script. After the models are trained, it utilizes the Random Forest model to assign an importance score to each feature. Finally, it creates a dataset consisting of the features and their importance scores. Features in this dataset are converted to their actual gene names rather than their ENSGXXXXXXXXX codes.

Finally, the Figure.R script can be run to generate all of the figures that will be in the final poster. Figure1(first two figures printed combined): a boxplot comparison of non-scaled and scaled features. Figure2: a bar chart showing the distribution of feature importance across p-values. Figure3: the ROC plots for each model (AUC calculations and labels added manually on poster)(Blue = SVM, Red = RF, Green = NN, Yellow = KNN, Grey = NB). Figure4: the confusion matrices for each model. Figure5: is a bar chart showing the accuracy, specificity, and sensitivity of each model as well as that of current gold standard COVID tests (requires the accuracy.csv file). Figure6: a bar chart showing the importance of each feature in increasing order. 

To run this in BASH, use use BashCovid.sh 

Reference:

Rao, A. M. et al. (2022) “A robust host-response-based signature distinguishes bacterial and viral infections across diverse global populations,” Cell reports. Medicine, 3(12), p. 100842. doi: 10.1016/j.xcrm.2022.100842.
