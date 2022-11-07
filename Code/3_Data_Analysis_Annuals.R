
##### ANNUAL ###

rm(list = ls())


packages <- list("tidyverse","naniar","caret","ggplot2","randomForest","readxl",
                 "plotly","Boruta","htmlwidgets","here")


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


##### ### Computing Variable Importance by GINI ### 
set.seed(1234)

Rf_annuals <- randomForest(train_imputed$Species~.,data = train_imputed)

importance_by_gini <- varImp(Rf_annuals)

importance_by_gini <- data.frame(Features = row.names(importance_by_gini),
                                 Overall = importance_by_gini$Overall)


## making a ggplot ### 


ggplot(data = importance_by_gini,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Importance of each Variable as per GINI for Annuals") +
  theme(text = element_text(size = 10)) 


ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S9.svg",dpi = 300)


### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Gini_Importance_Annual.csv",
           row.names = FALSE)


######
### Now identifying the optimal subset of features ### 
#####

subsets <- c(1:71)

params_rfe <- rfeControl(functions = rfFuncs,method = "cv",number = 10,
                         verbose = F)

set.seed(1234)

features_rfe_gini <- rfe(Species~.,data = train_imputed,
                         sizes=subsets,rfeControl=params_rfe)


## variation of accuracy with the removal of variables ### 
features_rfe_gini

## these are the predictors in the optimal subset 

optimal_subset_rfe_annual <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_annual

#### Importance of each variable 

Rfe_Imp_Annual <- data.frame(varImp(features_rfe_gini))

Rfe_Imp_Annual <- data.frame(Features = rownames(Rfe_Imp_Annual),
                            Overall = Rfe_Imp_Annual$Overall)

Rfe_Imp_Annual_best_subset <- Rfe_Imp_Annual %>%
                              dplyr::filter(Features %in% optimal_subset_rfe_annual$Features)


### plotting the variation of accuracy with the removal of variables
ggplot(features_rfe_gini)

ggplot(data = Rfe_Imp_Annual_best_subset,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Optimal subset of features for Annuals") +
  theme(text = element_text(size = 10)) 


ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S10.svg",
       dpi = 300)




### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Annual_best_subset,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Rfe_Annual_best_subset.csv",
          row.names = FALSE)


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

Boruta_annual <- Boruta(Species ~., train_optimal)

## Putting the importance decisions in a nice table ### 

Boruta_feature_analysis <- data.frame(attStats(Boruta_annual)) 

### Making the colnames into row names ###

Boruta_feature_analysis$Feature <- rownames(Boruta_feature_analysis)

### Subsetting the dataset based on the strongly relevant traits ## 
## i.e., Confirmed ###

Boruta_feature_analysis <- Boruta_feature_analysis %>% 
                           dplyr::filter(decision == "Confirmed")


colnames(Boruta_feature_analysis)

ggplot(data = Boruta_feature_analysis,
       aes(x=reorder(Feature,meanImp), y = meanImp, fill = Feature)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Strongly important variables for Annuals") +
  theme(text = element_text(size = 10)) 


ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S11.svg",
       dpi = 300)




####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Annual.csv",
          row.names = FALSE)


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


set.seed(1234)

Rf_Annual <- train(Species~.,data=train_boruta,
            method="rf",trControl=params,
            verbose=F)

Rf_Annual

p_rf_boruta <- predict(Rf_Annual,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_RF_annual.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_RF_boruta[-1],2,mean))


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

set.seed(1234)
gbm_Annual <- train(Species~., data=train_boruta,
             method="gbm",trControl=params,
             verbose=F,tuneGrid=grid)

gbm_Annual

p_gbm <- predict(gbm_Annual,test_boruta)

c_gbm <- confusionMatrix(p_gbm,test_boruta$Species)


##############
### Extracting data from the confusion matrix 

###############    #### Gradient Boosting ######## 

per_class_metrics_GBM_boruta <- data.frame(c_gbm$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_GBM_annual.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                           apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/GBM_Macro_averaged_metrics_annual.csv")



### Model comparison plots 

Model_list <- resamples(list(RF=Rf_Annual,GBM=gbm_Annual))


bwplot(Model_list) 

#### We need to standardize the data before plotting 

scaled_boruta <- (train_boruta[-1])

### Dataset containing scaled trait values and Species 

df <- data.frame(Species = train_boruta$Species,
                 scaled_boruta)


### 3D PLOTLY PLOTS ###

plot <- plot_ly(df,x= ~LTD,y= ~LA,z= ~FTFM, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Leaf Trichome Density'),
                 yaxis = list(title = 'Leaf Area'),
                 zaxis = list(title = 'Flower Total Fresh Mass'))
  )


saveWidget(plot,"Annual3d.html")




