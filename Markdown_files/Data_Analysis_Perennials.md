

```r
##### PERENNIAL ###

rm(list = ls())

packages <- list("tidyverse","naniar","caret","ggplot2","randomForest","readxl",
                 "plotly","Boruta","htmlwidgets")


lapply(packages, require,character.only=T)
```

```
## [[1]]
## [1] TRUE
## 
## [[2]]
## [1] TRUE
## 
## [[3]]
## [1] TRUE
## 
## [[4]]
## [1] TRUE
## 
## [[5]]
## [1] TRUE
## 
## [[6]]
## [1] TRUE
## 
## [[7]]
## [1] TRUE
## 
## [[8]]
## [1] TRUE
## 
## [[9]]
## [1] TRUE
```

```r
### reading in data #### 

train_imputed <- read.csv("train_imputed.csv")

test_imputed <- read.csv("test_imputed.csv")

####### 

###### Only keep the perennial species ##### 

Perennials <- c("H_salicifolius","H_maximiliani","H_giganteus",
                "H_verticillatus","H_grosseserratus","H_divaricatus",
                "H_microcephalus","H_cusickii")



##### keeping only the annuals  ### 
train_imputed <- train_imputed %>% filter(Species %in% Perennials)

test_imputed <- test_imputed %>% filter(Species %in% Perennials)

### converting the species column into a factor ### 

train_imputed$Species <- factor(train_imputed$Species)

test_imputed$Species <- factor(test_imputed$Species)

### 

##### Computing Variable Importance by GINI ### 
set.seed(1234)

Rf_perennials <- randomForest(train_imputed$Species~.,data = train_imputed)

importance_by_gini <- varImp(Rf_perennials)

importance_by_gini <- data.frame(Features = row.names(importance_by_gini),
                                 Overall = importance_by_gini$Overall)


## making a ggplot ### 


ggplot(data = importance_by_gini,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Importance of each Variable as per GINI for Perennials") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
ggsave("Figure 3b.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,"Gini_Importance_Perennials.csv",row.names = FALSE)


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
```

```
## 
## Recursive feature selection
## 
## Outer resampling method: Cross-Validated (10 fold) 
## 
## Resampling performance over subset size:
## 
##  Variables Accuracy  Kappa AccuracySD KappaSD Selected
##          1   0.2846 0.1737    0.10443 0.12096         
##          2   0.4448 0.3610    0.08740 0.10182         
##          3   0.6662 0.6142    0.11287 0.13014         
##          4   0.7969 0.7651    0.11058 0.12782         
##          5   0.8359 0.8096    0.12272 0.14270         
##          6   0.8585 0.8362    0.07264 0.08407         
##          7   0.8567 0.8332    0.10229 0.11936         
##          8   0.8490 0.8246    0.10600 0.12297         
##          9   0.8484 0.8235    0.11460 0.13376         
##         10   0.8192 0.7897    0.11257 0.13173         
##         11   0.8567 0.8328    0.10229 0.11996         
##         12   0.8657 0.8434    0.06805 0.07992         
##         13   0.8561 0.8325    0.09615 0.11288         
##         14   0.8348 0.8080    0.06068 0.07114         
##         15   0.8330 0.8058    0.09488 0.11091         
##         16   0.8425 0.8166    0.06684 0.07835         
##         17   0.8496 0.8251    0.06385 0.07468         
##         18   0.8265 0.7984    0.07712 0.08970         
##         19   0.8502 0.8256    0.09759 0.11471         
##         20   0.8514 0.8270    0.08574 0.10035         
##         21   0.8662 0.8442    0.08485 0.09922         
##         22   0.8437 0.8182    0.08196 0.09592         
##         23   0.8508 0.8266    0.07942 0.09292         
##         24   0.8585 0.8353    0.09838 0.11582         
##         25   0.8662 0.8441    0.10029 0.11810         
##         26   0.8502 0.8254    0.08933 0.10552         
##         27   0.8739 0.8531    0.10152 0.11966         
##         28   0.8662 0.8443    0.08350 0.09795         
##         29   0.8829 0.8636    0.07532 0.08898         
##         30   0.8906 0.8725    0.08338 0.09822         
##         31   0.8752 0.8548    0.07469 0.08808         
##         32   0.8835 0.8646    0.06288 0.07382         
##         33   0.8918 0.8744    0.06099 0.07173         
##         34   0.8675 0.8458    0.06353 0.07529         
##         35   0.8918 0.8744    0.06099 0.07170         
##         36   0.8835 0.8646    0.06288 0.07382         
##         37   0.9067 0.8917    0.05957 0.06964         
##         38   0.8990 0.8828    0.05045 0.05915         
##         39   0.9072 0.8921    0.06726 0.07910        *
##         40   0.8990 0.8828    0.05045 0.05915         
##         41   0.8995 0.8833    0.06961 0.08159         
##         42   0.9067 0.8917    0.05957 0.06964         
##         43   0.8995 0.8833    0.06961 0.08159         
##         44   0.8984 0.8819    0.06370 0.07439         
##         45   0.8912 0.8737    0.07227 0.08448         
##         46   0.9067 0.8917    0.05957 0.06964         
##         47   0.8907 0.8732    0.05396 0.06298         
##         48   0.9067 0.8917    0.05957 0.06964         
##         49   0.8984 0.8821    0.06370 0.07421         
##         50   0.8835 0.8648    0.06288 0.07373         
##         51   0.8835 0.8648    0.06288 0.07369         
##         52   0.8984 0.8819    0.06370 0.07439         
##         53   0.8995 0.8833    0.06961 0.08159         
##         54   0.9067 0.8917    0.05957 0.06964         
##         55   0.9067 0.8917    0.05957 0.06964         
##         56   0.8995 0.8833    0.06961 0.08159         
##         57   0.9067 0.8917    0.05957 0.06964         
##         58   0.8990 0.8827    0.06213 0.07250         
##         59   0.9067 0.8917    0.05957 0.06964         
##         60   0.8984 0.8819    0.06370 0.07439         
##         61   0.8829 0.8639    0.07379 0.08620         
##         62   0.9067 0.8917    0.05957 0.06964         
##         63   0.8995 0.8833    0.06961 0.08159         
##         64   0.8984 0.8819    0.06370 0.07439         
##         65   0.8984 0.8819    0.06370 0.07439         
##         66   0.9067 0.8917    0.05957 0.06964         
##         67   0.9067 0.8917    0.05957 0.06964         
##         68   0.9067 0.8917    0.05957 0.06964         
##         69   0.8984 0.8819    0.06370 0.07439         
##         70   0.9067 0.8917    0.05957 0.06964         
##         71   0.8918 0.8744    0.06099 0.07170         
## 
## The top 5 variables (out of 39):
##    LD13C, L_Circ, WPFF, LA, FRFM
```

```r
## these are the predictors in the optimal subset 

optimal_subset_rfe_perennials <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_perennials
```

```
##           Features
## 1            LD13C
## 2           L_Circ
## 3             WPFF
## 4               LA
## 5             FRFM
## 6        L_DryMass
## 7             WPFB
## 8          L_Aarea
## 9              FRN
## 10     L_FreshMass
## 11          L_Peri
## 12         L_VeinD
## 13            FTDM
## 14            FTFM
## 15            FDFM
## 16       L_LaminaT
## 17            FRDM
## 18            FPFM
## 19         L_Tough
## 20             FDC
## 21           WPSMF
## 22           L_Con
## 23            FDDM
## 24             FDD
## 25 L_NightRespmass
## 26            LDMC
## 27           WPBMF
## 28        L_LipidC
## 29             LWC
## 30             FDA
## 31             FTA
## 32            FPDM
## 33             FPA
## 34             FTD
## 35       L_MidribT
## 36             FTC
## 37             FPD
## 38    L_T_activity
## 39             FRL
```

```r
#### Importance of each variable 

Rfe_Imp_Perennial <- data.frame(varImp(features_rfe_gini))

Rfe_Imp_Perennial <- data.frame(Features = rownames(Rfe_Imp_Perennial),
                             Overall = Rfe_Imp_Perennial$Overall)

Rfe_Imp_Perennial_best_subset <- Rfe_Imp_Perennial %>%
  dplyr::filter(Features %in% optimal_subset_rfe_perennials$Features)


### plotting the variation of accuracy with the removal of variables
ggplot(features_rfe_gini)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
ggplot(data = Rfe_Imp_Perennial_best_subset,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Optimal subset of features for Perennial") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
ggsave("Figure 4b.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Perennial_best_subset,"Rfe_Perennial_best_subset.csv",row.names = FALSE)


##### 
## Now removing the features outside of the optimal subset ### 
## from both train and test 

###### train

train_optimal <- train_imputed %>% 
  dplyr::select(Species,optimal_subset_rfe_perennials$Features)


### test

test_optimal <- test_imputed %>% 
  dplyr::select(Species,optimal_subset_rfe_perennials$Features)



###########################
#### BORUTA ######
##### FINDING THE STRONGEST, WEAKLY RELEVANT AND REDUNDANT VARIABLES ###

Boruta_perennial <- Boruta(Species ~., train_optimal)

## Putting the importance decisions in a nice table ### 

Boruta_feature_analysis <- data.frame(attStats(Boruta_perennial)) 

### Making the colnames into row names ###

Boruta_feature_analysis$Feature <- rownames(Boruta_feature_analysis)

### Subsetting the dataset based on the strongly relevant traits ## 
## i.e., Confirmed ###

Boruta_feature_analysis <- Boruta_feature_analysis %>% 
  dplyr::filter(decision == "Confirmed")


colnames(Boruta_feature_analysis)
```

```
## [1] "meanImp"   "medianImp" "minImp"    "maxImp"    "normHits"  "decision"  "Feature"
```

```r
ggplot(data = Boruta_feature_analysis,
       aes(x=reorder(Feature,meanImp), y = meanImp, fill = Feature)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Strongly important variables for Perennials") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
ggsave("Figure 5b.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,"Boruta_Perennials.csv",row.names = FALSE)


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

Rf_Perennial <- train(Species~.,data=train_boruta,
                   method="rf",trControl=params,
                   verbose=F)

Rf_Perennial
```

```
## Random Forest 
## 
## 128 samples
##  39 predictor
##   8 classes: 'H_cusickii', 'H_divaricatus', 'H_giganteus', 'H_grosseserratus', 'H_maximiliani', 'H_microcephalus', 'H_salicifolius', 'H_verticillatus' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 115, 117, 116, 114, 115, 114, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.8834915  0.8650617
##   20    0.8755911  0.8565926
##   39    0.8833750  0.8653143
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
```

```r
p_rf_boruta <- predict(Rf_Perennial,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,"per_class_metrics_RF_perennial.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_RF_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,"RF_Macro_averaged_metrics_perennial.csv")


#### GBM ### 

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)

params <- trainControl(method = "cv",
                       number = 10)

set.seed(1234)
gbm_Perennial <- train(Species~., data=train_boruta,
                    method="gbm",trControl=params,
                    verbose=F,tuneGrid=grid)

gbm_Perennial
```

```
## Stochastic Gradient Boosting 
## 
## 128 samples
##  39 predictor
##   8 classes: 'H_cusickii', 'H_divaricatus', 'H_giganteus', 'H_grosseserratus', 'H_maximiliani', 'H_microcephalus', 'H_salicifolius', 'H_verticillatus' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 115, 117, 116, 114, 115, 114, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.9074176  0.8923125
##   4                  1000     0.9074176  0.8923125
##   6                   600     0.9151099  0.9015315
##   6                  1000     0.9151099  0.9015315
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at
##  a value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 600, interaction.depth = 6, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
p_gbm <- predict(gbm_Perennial,test_boruta)

c_gbm <- confusionMatrix(p_gbm,test_boruta$Species)


##############
### Extracting data from the confusion matrix 

###############    #### Gradient Boosting ######## 

per_class_metrics_GBM_boruta <- data.frame(c_gbm$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM_boruta,"per_class_metrics_GBM_perennial.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                           apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,"GBM_Macro_averaged_metrics_perennial.csv")



### Model comparison plots 

Model_list <- resamples(list(RF=Rf_Perennial,GBM=gbm_Perennial))


bwplot(Model_list) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

```r
#### We need to standardize the data before plotting 

scaled_boruta <- (train_boruta[-1])

### Dataset containing scaled trait values and Species 

df <- data.frame(Species = train_boruta$Species,
                 scaled_boruta)

### 3D PLOTLY PLOTS ###

plot <- plot_ly(df,x= ~LD13C,y= ~L_Circ,z= ~LA, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'LD13C'),
                 yaxis = list(title = 'Leaf Circularity'),
                 zaxis = list(title = 'Leaf Area'))
  )

plot
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
saveWidget(plot,"Perennials3d.html")
```

