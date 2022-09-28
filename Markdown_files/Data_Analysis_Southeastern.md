

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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
ggsave("Figure 3d.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
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
##          1   0.4908 0.4081    0.14334 0.16229         
##          2   0.7306 0.6854    0.09543 0.11159         
##          3   0.7799 0.7424    0.09152 0.10604         
##          4   0.8165 0.7859    0.10094 0.11682         
##          5   0.8713 0.8495    0.08763 0.10185         
##          6   0.8775 0.8566    0.11654 0.13595         
##          7   0.9023 0.8854    0.08496 0.09983         
##          8   0.9023 0.8854    0.08496 0.09983         
##          9   0.8968 0.8793    0.10802 0.12641         
##         10   0.8968 0.8793    0.09447 0.11063         
##         11   0.8959 0.8781    0.07905 0.09306         
##         12   0.8959 0.8781    0.07905 0.09306         
##         13   0.8959 0.8784    0.07905 0.09259         
##         14   0.8956 0.8778    0.08791 0.10332         
##         15   0.8845 0.8650    0.10247 0.12029         
##         16   0.9025 0.8859    0.08195 0.09630         
##         17   0.9019 0.8855    0.07479 0.08747         
##         18   0.8956 0.8780    0.07973 0.09334         
##         19   0.9071 0.8915    0.07607 0.08914         
##         20   0.9300 0.9181    0.06923 0.08093         
##         21   0.9077 0.8922    0.07011 0.08181         
##         22   0.9192 0.9058    0.05967 0.06933         
##         23   0.9068 0.8911    0.06498 0.07574         
##         24   0.9244 0.9115    0.03641 0.04313         
##         25   0.9179 0.9038    0.04767 0.05618         
##         26   0.9241 0.9113    0.04547 0.05336         
##         27   0.9241 0.9113    0.04547 0.05336         
##         28   0.9186 0.9046    0.05914 0.06965         
##         29   0.9179 0.9039    0.03747 0.04399         
##         30   0.9189 0.9050    0.04553 0.05351         
##         31   0.9234 0.9103    0.02587 0.03071         
##         32   0.9179 0.9039    0.03747 0.04399         
##         33   0.9123 0.8972    0.05250 0.06190         
##         34   0.9234 0.9102    0.03921 0.04645         
##         35   0.9123 0.8974    0.05250 0.06137         
##         36   0.9179 0.9039    0.04767 0.05584         
##         37   0.9241 0.9111    0.04547 0.05348         
##         38   0.9186 0.9047    0.05302 0.06204         
##         39   0.9123 0.8971    0.06020 0.07078         
##         40   0.9234 0.9102    0.03921 0.04645         
##         41   0.9015 0.8843    0.07346 0.08653         
##         42   0.9126 0.8975    0.04695 0.05533         
##         43   0.9297 0.9176    0.03543 0.04179         
##         44   0.9186 0.9046    0.05914 0.06938         
##         45   0.9123 0.8972    0.06020 0.07054         
##         46   0.9179 0.9037    0.04767 0.05611         
##         47   0.9234 0.9101    0.04715 0.05587         
##         48   0.9234 0.9102    0.03921 0.04641         
##         49   0.9179 0.9037    0.04767 0.05611         
##         50   0.9234 0.9102    0.03921 0.04641         
##         51   0.9234 0.9102    0.03921 0.04641         
##         52   0.9070 0.8908    0.05909 0.06974         
##         53   0.9234 0.9101    0.03921 0.04649         
##         54   0.9234 0.9101    0.03921 0.04649         
##         55   0.9126 0.8973    0.04695 0.05562         
##         56   0.9070 0.8909    0.05297 0.06242         
##         57   0.9287 0.9164    0.04577 0.05413         
##         58   0.9290 0.9167    0.03768 0.04478         
##         59   0.9182 0.9039    0.03917 0.04652         
##         60   0.9234 0.9101    0.04715 0.05587         
##         61   0.9290 0.9167    0.03768 0.04478         
##         62   0.9179 0.9037    0.04767 0.05611         
##         63   0.9234 0.9101    0.03921 0.04649         
##         64   0.9342 0.9229    0.04372 0.05181         
##         65   0.9234 0.9101    0.03921 0.04649         
##         66   0.9234 0.9101    0.03921 0.04649         
##         67   0.9179 0.9037    0.04767 0.05611         
##         68   0.9342 0.9229    0.04372 0.05181         
##         69   0.9342 0.9229    0.04372 0.05181         
##         70   0.9123 0.8971    0.06020 0.07101         
##         71   0.9398 0.9294    0.04844 0.05729        *
## 
## The top 5 variables (out of 71):
##    L_Circ, LTD, LA, LS, LWC
```

```r
## these are the predictors in the optimal subset 

optimal_subset_rfe_southeastern <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_southeastern
```

```
##           Features
## 1           L_Circ
## 2              LTD
## 3               LA
## 4               LS
## 5              LWC
## 6             LDMC
## 7             WPFB
## 8        L_DryMass
## 9      L_FreshMass
## 10            WPFF
## 11              LC
## 12            FRWC
## 13            WPTB
## 14             FPD
## 15             FRN
## 16            FDAF
## 17            FPAF
## 18             FRW
## 19            FPFM
## 20            FPDM
## 21           WPLMF
## 22             P_T
## 23             FRL
## 24             FTC
## 25             FTD
## 26            FRFM
## 27             FTA
## 28             FPA
## 29             P_D
## 30           LD13C
## 31            FAIR
## 32            FTWC
## 33            FTFM
## 34          L_Peri
## 35              LN
## 36            FDWC
## 37        L_LipidC
## 38            FRDM
## 39            FDFM
## 40       L_LaminaT
## 41           WPRMF
## 42            FTDM
## 43            FDDM
## 44       L_MidribT
## 45           WPBMF
## 46             R_T
## 47           WPSMF
## 48             FDC
## 49 L_NightRespArea
## 50             FDA
## 51             FDD
## 52          L_AshC
## 53          L_iWUE
## 54             D_T
## 55            L_Ci
## 56             LCN
## 57    L_T_activity
## 58            SPAD
## 59              LP
## 60           LD15C
## 61             LMA
## 62         L_VeinD
## 63 L_NightRespmass
## 64           L_Con
## 65            FPWC
## 66          L_life
## 67           LPNUE
## 68         L_Aarea
## 69         L_Tough
## 70        Leaf_N_P
## 71         L_Amass
```

```r
#### Importance of each variable 

Rfe_Imp_southeastern <- data.frame(varImp(features_rfe_gini))

Rfe_Imp_southeastern <- data.frame(Features = rownames(Rfe_Imp_southeastern),
                                Overall = Rfe_Imp_southeastern$Overall)

Rfe_Imp_southeastern_best_subset <- Rfe_Imp_southeastern %>%
  dplyr::filter(Features %in% optimal_subset_rfe_southeastern$Features)


### plotting the variation of accuracy with the removal of variables
ggplot(features_rfe_gini)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
ggplot(data = Rfe_Imp_southeastern_best_subset,
       aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Optimal subset of features for southeastern perennial") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
ggsave("Figure 4d.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
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
  ggtitle("Strongly important variables for southeastern perennials") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
ggsave("Figure 5d.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
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
```

```
## Random Forest 
## 
## 169 samples
##  67 predictor
##   8 classes: 'H_angustifolius', 'H_atrorubens', 'H_carnosus', 'H_floridanus', 'H_heterophyllus', 'H_longifolius', 'H_radula', 'H_silphioides' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 149, 153, 152, 152, 153, 154, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9237908  0.9108343
##   34    0.9368873  0.9261034
##   67    0.9202206  0.9068612
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 34.
```

```r
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
```

```
## Stochastic Gradient Boosting 
## 
## 169 samples
##  67 predictor
##   8 classes: 'H_angustifolius', 'H_atrorubens', 'H_carnosus', 'H_floridanus', 'H_heterophyllus', 'H_longifolius', 'H_radula', 'H_silphioides' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 149, 153, 152, 152, 153, 154, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.9517075  0.9435738
##   4                  1000     0.9517075  0.9435738
##   6                   600     0.9638399  0.9576080
##   6                  1000     0.9638399  0.9576080
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at
##  a value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 600, interaction.depth = 6, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

```r
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
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
saveWidget(plot,"SoutheasternPerennials3d.html")
```

