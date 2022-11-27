
##### ANNUAL ###

rm(list = ls())

source("2_Functions.R")

packages <- list("tidyverse","naniar","caret","ggplot2","randomForest","readxl",
                 "plotly","Boruta","here")


lapply(packages, require,character.only=T)

### reading in data #### 

train_imputed <- read_csv(here("Datasets and Tables","train_imputed.csv"))

test_imputed <- read_csv(here("Datasets and Tables","test_imputed.csv"))


###### FILTER OUT BY ANNUAL SPECIES ##### 

Annuals <- c("H_praeco_ssp_runyonii","H_debili_ssp_tardiflorus",
             "H_neglectus","H_petiolari_ssp_petiolaris",
             "H_niveu_ssp_tephrodes","H_annuus","H_argophyllus")



##### keeping only the annuals  ### 
train_imputed <- train_imputed %>% filter(Species %in% Annuals)

test_imputed <- test_imputed %>% filter(Species %in% Annuals)

### converting the species column into a factor ### 

train_imputed$Species <- factor(train_imputed$Species)

test_imputed$Species <- factor(test_imputed$Species)

 
#### Applying this function on the train data ##


Gini_annual <- Gini(train_imputed)

importance_by_gini <- Gini_annual[[2]]


### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Gini_Importance_Annual.csv",
          row.names = FALSE) 


Gini_annual_plot <- Gini_annual[[1]]

## checking if it plots 

Gini_annual_plot

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S9.svg",dpi = 300)


######
### Now identifying the optimal subset of features ### 
#####

subsets <- c(1:71)

params_rfe <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                         verbose = F)


### Applying this function on the train data ##

RFE_annual <- RFE(train_imputed)

### Table containing the importance of each variable in the best subset
Rfe_Imp_Annual_best_subset <- RFE_annual[[4]]


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Annual_best_subset,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Rfe_Annual_best_subset.csv",
          row.names = FALSE)


##### Plotting the importance values ###

RFE_annual_plot <- RFE_annual[[2]]

RFE_annual_plot


ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S10.svg",
       dpi = 300)


##### Plotting the variation of accuracy with the removal of variables ##

Variation_plot <- RFE_annual[[1]] 

Variation_plot

##### Table containing the optimal susbet of traits ### 

optimal_subset_rfe_annual <- RFE_annual[[3]]




##### 
## Now removing the features outside of the optimal subset ### 
## from both train and test 

###### train

train_optimal <- train_imputed %>% 
                 dplyr::select(Species,optimal_subset_rfe_annual$Features)



### test

test_optimal <- test_imputed %>% 
                dplyr::select(Species,optimal_subset_rfe_annual$Features)



###########################
#### BORUTA ######
##### FINDING THE STRONGEST, WEAKLY RELEVANT AND REDUNDANT VARIABLES ###



### Applying the function to the train optimal dataset ##

Boruta_results <- Boruta_analysis(train_optimal)

### Table with strongly divergent traits along with thier importances

Boruta_feature_analysis <- Boruta_results[[2]] 


####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Annual.csv",
          row.names = FALSE)


####### Plotting the importance values ###

Boruta_importance_plots <- Boruta_results[[1]]

Boruta_importance_plots


ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S11.svg",
       dpi = 300)



### Subsetting the training and test data based on the most important features

train_boruta <- train_optimal %>% 
                dplyr::select(Species,Boruta_feature_analysis$Feature)


test_boruta <- test_optimal %>% 
               dplyr::select(Species,Boruta_feature_analysis$Feature)

#####################
## MODELING ####
################

##### Random forest ##

### Initializing the parameters
params <- trainControl(method = "cv",
                       number = 10)


### Applying the function to the training data ##

RF_results <- Random_forest_analysis(train_boruta)

### a dataframe containing per class metrics 

per_class_metrics_RF <- RF_results[[3]]


### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_RF_annual.csv")


## a dataframe containing the macro averaged metrics ###

Macro_averaged_metrics_Rf <- RF_results[[4]]


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/RF_Macro_averaged_metrics_annual.csv")


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
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_GBM_annual.csv")


######### a dataframe containing macro averaged metrics

Macro_averaged_metrics_GBM <- GBM_results[[4]]

### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/GBM_Macro_averaged_metrics_annual.csv")



#### Putting the models in a list so that we can plot their performance 

Rf <- RF_results[[1]]

gbm <- GBM_results[[1]]


### Model comparison plots 

Model_list <- resamples(list(RF=Rf,GBM=gbm))


bwplot(Model_list)




