
##### SOUTHEASTERN PERENNIAL ###

rm(list = ls())

source("2_Functions.R")

packages <- list("tidyverse","naniar","caret","ggplot2","randomForest","readxl",
                 "plotly","Boruta","here")


lapply(packages, require,character.only=T)

####### 


train_imputed <- read_csv(here("Datasets and Tables","train_imputed.csv"))

test_imputed <- read_csv(here("Datasets and Tables","test_imputed.csv"))



###### FILTER OUT BY SOUTHEASTERN PERENNUIALS SPECIES ##### 

Southeastern_perennials <- c("H_carnosus","H_atrorubens","H_radula",
                           "H_silphioides","H_floridanus","H_heterophyllus",
                            "H_longifolius","H_angustifolius")



##### keeping only the Southeastern perennials  ### 

train_imputed <- train_imputed %>% filter(Species %in% Southeastern_perennials)

test_imputed <- test_imputed %>% filter(Species %in% Southeastern_perennials)

### converting the species column into a factor ### 

train_imputed$Species <- factor(train_imputed$Species)

test_imputed$Species <- factor(test_imputed$Species)

### 

######## Computing Variable Importance by GINI ### 

Gini_southeastern <- Gini(train_imputed)

importance_by_gini <- Gini_southeastern[[2]]

### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Gini_Importance_southeastern.csv",
          row.names = FALSE)

Gini_southeastern_plot <- Gini_southeastern[[1]]

## checking if it plots 

Gini_southeastern_plot

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S15.svg",
       dpi = 300)


######
### Now identifying the optimal subset of features ### 
#####

subsets <- c(1:71)

params_rfe <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                         verbose = F)

### Applying this function on the train data ##

RFE_southeastern <- RFE(train_imputed)

### Table containing the importance of each variable in the best subset
Rfe_Imp_southeastern_best_subset <- RFE_southeastern[[4]]


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_southeastern_best_subset,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Rfe_southeastern_best_subset.csv",
          row.names = FALSE)

##### Plotting the importance values ###

RFE_southeastern_plot <- RFE_southeastern[[2]]

RFE_southeastern_plot


ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S16.svg",
       dpi = 300)

##### Plotting the variation of accuracy with the removal of variables ##

Variation_plot <- RFE_southeastern[[1]] 

Variation_plot

##### Table containing the optimal susbet of traits ### 

optimal_subset_rfe_southeastern <- RFE_southeastern[[3]]

##### 
## Now removing the features outside of the optimal subset ### 
## from both train and test 

###### train

train_optimal <- train_imputed %>% 
  dplyr::select(Species,optimal_subset_rfe_southeastern$Features)


### test

test_optimal <- test_imputed %>% 
  dplyr::select(Species,optimal_subset_rfe_southeastern$Features)



###########################
#### BORUTA ######
##### FINDING THE STRONGEST, WEAKLY RELEVANT AND REDUNDANT VARIABLES ###

### Applying the function to the train optimal dataset ##

Boruta_results <- Boruta_analysis(train_optimal)

### Table with strongly divergent traits along with thier importances

Boruta_feature_analysis <- Boruta_results[[2]] 


####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_southeastern_perennials.csv",
          row.names = FALSE)


####### Plotting the importance values ###

Boruta_importance_plots <- Boruta_results[[1]]

Boruta_importance_plots

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S17.svg",dpi = 300)


### Subsetting the training and test data based on the most important features

train_boruta <- train_optimal %>% 
                dplyr::select(Species,Boruta_feature_analysis$Feature)


test_boruta <- test_optimal %>% 
               dplyr::select(Species,Boruta_feature_analysis$Feature)

#####################
## MODELING ####
################

##### Random forest ##

params <- trainControl(method = "cv",
                       number = 10)


### Applying the function to the training data ##

RF_results <- Random_forest_analysis(train_boruta)

### a dataframe containing per class metrics 

per_class_metrics_RF <- RF_results[[3]]


### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_RF_southeastern.csv")


## a dataframe containing the macro averaged metrics ###

Macro_averaged_metrics_Rf <- RF_results[[4]]


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/RF_Macro_averaged_metrics_southeastern.csv")


#### GBM ### 

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)

params <- trainControl(method = "cv",
                       number = 10)


##### Applying the function to the training data ##

GBM_results <- Gbm_analysis(train_boruta)


### a dataframe containing per class metrics 

per_class_metrics_GBM_boruta <- GBM_results[[3]]


### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_GBM_southeastern.csv")


######### a dataframe containing macro averaged metrics

Macro_averaged_metrics_GBM <- GBM_results[[4]]


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/GBM_Macro_averaged_metrics_southeastern.csv")



#### Putting the models in a list so that we can plot their performance 

Rf <- RF_results[[1]]

gbm <- GBM_results[[1]]


### Model comparison plots 

Model_list <- resamples(list(RF=Rf,GBM=gbm))


bwplot(Model_list)


