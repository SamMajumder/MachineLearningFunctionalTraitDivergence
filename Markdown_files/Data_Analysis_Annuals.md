

```r
##### ANNUAL ###

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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
ggsave("Figure 3c.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,"Gini_Importance_Annual.csv",row.names = FALSE)


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
##          1   0.4177 0.3072    0.12353 0.14272         
##          2   0.6843 0.6223    0.08714 0.10260         
##          3   0.7376 0.6861    0.09530 0.11113         
##          4   0.7881 0.7461    0.09072 0.10630         
##          5   0.8019 0.7625    0.11103 0.13109         
##          6   0.8307 0.7970    0.10654 0.12780         
##          7   0.8147 0.7781    0.09655 0.11537         
##          8   0.8070 0.7684    0.10957 0.13033         
##          9   0.8230 0.7883    0.11298 0.13360         
##         10   0.8153 0.7792    0.10791 0.12778         
##         11   0.8456 0.8157    0.11935 0.14120         
##         12   0.8465 0.8170    0.12036 0.14210         
##         13   0.8542 0.8260    0.09219 0.10946         
##         14   0.8676 0.8428    0.07680 0.09033         
##         15   0.8527 0.8248    0.08608 0.10154         
##         16   0.8604 0.8338    0.08108 0.09555         
##         17   0.8527 0.8247    0.09341 0.10947         
##         18   0.8681 0.8427    0.07488 0.08884         
##         19   0.8598 0.8325    0.07350 0.08753         
##         20   0.8521 0.8234    0.07891 0.09329         
##         21   0.8598 0.8331    0.08196 0.09689         
##         22   0.8675 0.8414    0.07590 0.09039         
##         23   0.8598 0.8322    0.07350 0.08758         
##         24   0.8675 0.8417    0.07590 0.09020         
##         25   0.8675 0.8414    0.07590 0.09039         
##         26   0.8886 0.8669    0.08493 0.10145         
##         27   0.8823 0.8593    0.09306 0.11111         
##         28   0.8746 0.8505    0.09935 0.11794         
##         29   0.8746 0.8506    0.09935 0.11779         
##         30   0.8809 0.8581    0.09236 0.10958         
##         31   0.8746 0.8506    0.09935 0.11779         
##         32   0.8732 0.8489    0.07596 0.09048         
##         33   0.8732 0.8491    0.08417 0.09965         
##         34   0.8886 0.8670    0.07680 0.09173         
##         35   0.8948 0.8746    0.08867 0.10535         
##         36   0.8655 0.8399    0.08261 0.09787         
##         37   0.8780 0.8547    0.08315 0.09867         
##         38   0.8717 0.8477    0.09544 0.11190         
##         39   0.8794 0.8563    0.08144 0.09683         
##         40   0.8871 0.8656    0.08928 0.10596         
##         41   0.8794 0.8567    0.10285 0.12097         
##         42   0.8655 0.8403    0.09723 0.11407         
##         43   0.8794 0.8563    0.08144 0.09683         
##         44   0.8655 0.8403    0.09723 0.11407         
##         45   0.8723 0.8478    0.07232 0.08561         
##         46   0.8871 0.8653    0.07308 0.08750         
##         47   0.8768 0.8527    0.11185 0.13347         
##         48   0.8845 0.8613    0.09975 0.12069         
##         49   0.8948 0.8743    0.07233 0.08687         
##         50   0.8851 0.8629    0.09071 0.10778         
##         51   0.8866 0.8649    0.08901 0.10564         
##         52   0.8859 0.8633    0.09822 0.11859         
##         53   0.8943 0.8736    0.08068 0.09644         
##         54   0.8943 0.8741    0.08845 0.10488         
##         55   0.8936 0.8722    0.09079 0.11067         
##         56   0.9020 0.8830    0.07923 0.09471        *
##         57   0.8943 0.8741    0.08845 0.10488         
##         58   0.8783 0.8541    0.09800 0.11834         
##         59   0.9008 0.8807    0.08391 0.10296         
##         60   0.8859 0.8633    0.09822 0.11859         
##         61   0.8943 0.8741    0.08845 0.10488         
##         62   0.8859 0.8639    0.09822 0.11772         
##         63   0.8943 0.8741    0.08845 0.10488         
##         64   0.8943 0.8745    0.08845 0.10459         
##         65   0.8859 0.8633    0.09822 0.11859         
##         66   0.8871 0.8655    0.08158 0.09659         
##         67   0.8943 0.8741    0.08845 0.10488         
##         68   0.8788 0.8553    0.09136 0.10986         
##         69   0.8943 0.8741    0.08845 0.10488         
##         70   0.8859 0.8632    0.09822 0.11883         
##         71   0.8943 0.8741    0.08845 0.10488         
## 
## The top 5 variables (out of 56):
##    LTD, LA, FDFM, FTFM, WPTB
```

```r
## these are the predictors in the optimal subset 

optimal_subset_rfe_annual <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_annual
```

```
##           Features
## 1              LTD
## 2               LA
## 3             FDFM
## 4             FTFM
## 5             WPTB
## 6        L_DryMass
## 7             FTDM
## 8             FRFM
## 9             FDDM
## 10        L_LipidC
## 11            FDAF
## 12            FPAF
## 13 L_NightRespArea
## 14       L_LaminaT
## 15          L_Peri
## 16           WPRMF
## 17            FRDM
## 18             FTA
## 19             FTC
## 20             FTD
## 21     L_FreshMass
## 22            FPFM
## 23             FDA
## 24             FPA
## 25         L_Aarea
## 26             FDD
## 27             FDC
## 28         L_Tough
## 29            FAIR
## 30            FPDM
## 31          L_Circ
## 32             FRW
## 33             P_T
## 34            SPAD
## 35            L_Ci
## 36             FRL
## 37           WPBMF
## 38           WPLMF
## 39             P_D
## 40            WPFB
## 41    L_T_activity
## 42          L_life
## 43 L_NightRespmass
## 44           LD13C
## 45            WPFF
## 46              LC
## 47             LMA
## 48              LS
## 49             FRN
## 50           L_Con
## 51          L_iWUE
## 52       L_MidribT
## 53            FTWC
## 54            FPWC
## 55            FDWC
## 56          L_AshC
```

```r
#### Importance of each variable 

Rfe_Imp_Annual <- data.frame(varImp(features_rfe_gini))

Rfe_Imp_Annual <- data.frame(Features = rownames(Rfe_Imp_Annual),
                            Overall = Rfe_Imp_Annual$Overall)

Rfe_Imp_Annual_best_subset <- Rfe_Imp_Annual %>%
                              dplyr::filter(Features %in% optimal_subset_rfe_annual$Features)


### plotting the variation of accuracy with the removal of variables
ggplot(features_rfe_gini)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
ggplot(data = Rfe_Imp_Annual_best_subset,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Optimal subset of features for Annuals") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
ggsave("Figure 4c.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Annual_best_subset,"Rfe_Annual_best_subset.csv",row.names = FALSE)


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
  ggtitle("Strongly important variables for Annuals") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
ggsave("Figure 5c.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,"Boruta_Annual.csv",row.names = FALSE)


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
```

```
## Random Forest 
## 
## 136 samples
##  55 predictor
##   7 classes: 'H_annuus', 'H_argophyllus', 'H_debili_ssp_tardiflorus', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_petiolari_ssp_petiolaris', 'H_praeco_ssp_runyonii' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 122, 121, 121, 123, 123, 122, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.8990659  0.8793571
##   28    0.8808242  0.8566007
##   55    0.8818498  0.8582074
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
```

```r
p_rf_boruta <- predict(Rf_Annual,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,"per_class_metrics_RF_annual.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_RF_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,"RF_Macro_averaged_metrics_annual.csv")


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
```

```
## Stochastic Gradient Boosting 
## 
## 136 samples
##  55 predictor
##   7 classes: 'H_annuus', 'H_argophyllus', 'H_debili_ssp_tardiflorus', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_petiolari_ssp_petiolaris', 'H_praeco_ssp_runyonii' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 122, 121, 121, 123, 123, 122, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.9134982  0.8972961
##   4                  1000     0.8981136  0.8786952
##   6                   600     0.9206410  0.9055802
##   6                  1000     0.9200916  0.9046975
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at
##  a value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 600, interaction.depth = 6, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
p_gbm <- predict(gbm_Annual,test_boruta)

c_gbm <- confusionMatrix(p_gbm,test_boruta$Species)


##############
### Extracting data from the confusion matrix 

###############    #### Gradient Boosting ######## 

per_class_metrics_GBM_boruta <- data.frame(c_gbm$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM_boruta,"per_class_metrics_GBM_annual.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                           apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,"GBM_Macro_averaged_metrics_annual.csv")



### Model comparison plots 

Model_list <- resamples(list(RF=Rf_Annual,GBM=gbm_Annual))


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

plot <- plot_ly(df,x= ~LTD,y= ~LA,z= ~FTFM, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Leaf Trichome Density'),
                 yaxis = list(title = 'Leaf Area'),
                 zaxis = list(title = 'Flower Total Fresh Mass'))
  )

plot
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
saveWidget(plot,"Annual3d.html")
```

