
##### PERENNIAL ###

rm(list = ls())


packages <- list("tidyverse","naniar","caret","ggplot2","randomForest","readxl",
                 "plotly","Boruta","htmlwidgets")


lapply(packages, require,character.only=T)

####### 

train_imputed <- read.csv("train_imputed.csv") 

test_imputed <- read.csv("test_imputed.csv")


###### FILTER OUT BY ANNUAL SPECIES ##### 

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
set.seed(1234)

Rf_southeastern <- randomForest(train_imputed$Species~.,data = train_imputed)

importance_by_gini <- varImp(Rf_southeastern)

importance_by_gini <- data.frame(Features = row.names(importance_by_gini),
                                 Overall = importance_by_gini$Overall)


## making a ggplot ### 


ggplot(data = importance_by_gini,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Importance of each Variable as per GINI for Southeastern Perennials") +
  theme(text = element_text(size = 10)) 


ggsave("Figure 3d.svg",dpi = 300)





### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,"Gini_Importance_southeastern.csv",row.names = FALSE)


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

optimal_subset_rfe_southeastern <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_southeastern

#### Importance of each variable 

Rfe_Imp_southeastern <- data.frame(varImp(features_rfe_gini))

Rfe_Imp_southeastern <- data.frame(Features = rownames(Rfe_Imp_southeastern),
                                Overall = Rfe_Imp_southeastern$Overall)

Rfe_Imp_southeastern_best_subset <- Rfe_Imp_southeastern %>%
  dplyr::filter(Features %in% optimal_subset_rfe_southeastern$Features)


### plotting the variation of accuracy with the removal of variables
ggplot(features_rfe_gini)

ggplot(data = Rfe_Imp_southeastern_best_subset,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Optimal subset of features for southeastern perennial") +
  theme(text = element_text(size = 10)) 


ggsave("Figure 4d.svg",dpi = 300)




### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_southeastern_best_subset,"Rfe_southeastern_best_subset.csv",row.names = FALSE)


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

Boruta_southeastern <- Boruta(Species ~., train_optimal)

## Putting the importance decisions in a nice table ### 

Boruta_feature_analysis <- data.frame(attStats(Boruta_southeastern)) 

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
  ggtitle("Strongly important variables for southeastern perennials") +
  theme(text = element_text(size = 10)) 


ggsave("Figure 5d.svg",dpi = 300)




####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,"Boruta_southeastern_perennials.csv",row.names = FALSE)


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

Rf_Southeastern <- train(Species~.,data=train_boruta,
                      method="rf",trControl=params,
                      verbose=F)

Rf_Southeastern

p_rf_boruta <- predict(Rf_Southeastern,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,"per_class_metrics_RF_southeastern.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_RF_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,"RF_Macro_averaged_metrics_southeastern.csv")


#### GBM ### 

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)

params <- trainControl(method = "cv",
                       number = 10)

set.seed(1234)
gbm_Southeastern <- train(Species~., data=train_boruta,
                       method="gbm",trControl=params,
                       verbose=F,tuneGrid=grid)

gbm_Southeastern

p_gbm <- predict(gbm_Southeastern,test_boruta)

c_gbm <- confusionMatrix(p_gbm,test_boruta$Species)


##############
### Extracting data from the confusion matrix 

###############    #### Gradient Boosting ######## 

per_class_metrics_GBM_boruta <- data.frame(c_gbm$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM_boruta,"per_class_metrics_GBM_southeastern.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                           apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,"GBM_Macro_averaged_metrics_southeastern.csv")



### Model comparison plots 

Model_list <- resamples(list(RF=Rf_Southeastern,GBM=gbm_Southeastern))


bwplot(Model_list) 

########## 

#### We need to standardize the data before plotting 

scaled_boruta <- (train_boruta[-1])

### Dataset containing scaled trait values and Species 

df <- data.frame(Species = train_boruta$Species,
                 scaled_boruta)


### 3D PLOTLY PLOTS ###

plot <- plot_ly(df,x= ~L_Circ,y= ~LTD,z= ~LA, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Leaf Circularity'),
                 yaxis = list(title = 'Leaf  Trichome Density'),
                 zaxis = list(title = 'Leaf Area'))
  )

plot



saveWidget(plot,"SoutheasternPerennials3d.html")




