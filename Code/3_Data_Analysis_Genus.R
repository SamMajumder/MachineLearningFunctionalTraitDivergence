
#### GENUS ###

rm(list = ls())

### sourcing the functions we need ## 

source("2_Functions.R")     ### run this line to load the custom functions

packages <- list("tidyverse","here","caret","ggplot2","randomForest",
                 "plotly","Boruta")


### Loading all packages ### 
lapply(packages, require,character.only=T)   

### reading in data #### 

train_imputed <- read_csv(here("Datasets and Tables","train_imputed.csv")) 

test_imputed <- read_csv(here("Datasets and Tables","test_imputed.csv")) 


#### change the character into a factor (Species column)

train_imputed$Species <- factor(train_imputed$Species)

test_imputed$Species <- factor(test_imputed$Species)

#### Applying this function on the train data ##


Gini_genus <- Gini(train_imputed)

importance_by_gini <- Gini_genus[[2]]

### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Gini_Importance_Genus.csv",
          row.names = FALSE) 


Gini_genus_plot <- Gini_genus[[1]]

## checking if it plots 

Gini_genus_plot

## saving the plot 

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S6.svg",dpi = 300)

### Now we have to initialize a few things ###

subsets <- c(1:71)  ## number of trait subsets we are going to include

params_rfe <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                         verbose = F)     #### initializing the parameters

### Applying this function on the train data ##

RFE_genus <- RFE(train_imputed)

### Table containing the importance of each variable in the best subset
Rfe_Imp_Genus_best_subset <- RFE_genus[[4]]


### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Genus_best_subset,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Rfe_Genus_best_subset.csv",
          row.names = FALSE) 



##### Plotting the importance values ###

RFE_genus_plot <- RFE_genus[[2]]

RFE_genus_plot

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S7.svg",dpi = 300)

##### Plotting the variation of accuracy with the removal of variables ##

Variation_plot <- RFE_genus[[1]] 

Variation_plot

##### Table containing the optimal susbet of traits ### 

optimal_subset_rfe_genus <- RFE_genus[[3]]


##### 
## Now removing the features outside of the optimal subset ### 
## from both train and test 

###### train

train_optimal <- train_imputed %>% 
  dplyr::select(Species,optimal_subset_rfe_genus$Features)


### test

test_optimal <- test_imputed %>% 
  dplyr::select(Species,optimal_subset_rfe_genus$Features)


### Applying the function to the train optimal dataset ##

Boruta_results <- Boruta_analysis(train_optimal)

### Table with strongly divergent traits along with thier importances

Boruta_feature_analysis <- Boruta_results[[2]] 

####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Genus.csv",
          row.names = FALSE)

####### Plotting the importance values ###

Boruta_importance_plots <- Boruta_results[[1]]

Boruta_importance_plots

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S8.svg",
       dpi = 300) 


### Subsetting the training and test data based on the most important features

train_boruta <- train_optimal %>% 
  dplyr::select(Species,Boruta_feature_analysis$Feature)


test_boruta <- test_optimal %>% 
  dplyr::select(Species,Boruta_feature_analysis$Feature)  


### Initializing the parameters 
params <- trainControl(method = "cv",
                       number = 10)


### Applying the function to the training data ##

RF_results <- Random_forest_analysis(train_boruta)

### a dataframe containing per class metrics 

per_class_metrics_RF <- RF_results[[3]]


### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_RF_genus.csv")


## a dataframe containing the macro averaged metrics ###

Macro_averaged_metrics_Rf <- RF_results[[4]]

### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/RF_Macro_averaged_metrics_genus.csv")



#### Initializing some parameters and hyperparameters ### 

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
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_GBM_genus.csv")  


######### a dataframe containing macro averaged metrics

Macro_averaged_metrics_GBM <- GBM_results[[4]]


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/GBM_Macro_averaged_metrics_genus.csv")



#### Putting the models in a list so that we can plot their performance 

Rf <- RF_results[[1]]

gbm <- GBM_results[[1]]


### Model comparison plots 

Model_list <- resamples(list(RF=Rf,GBM=gbm))


bwplot(Model_list)




































