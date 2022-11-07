

```r
##### SOUTHEASTERN PERENNIAL ###

rm(list = ls())


packages <- list("tidyverse","naniar","caret","ggplot2","randomForest","readxl",
                 "plotly","Boruta","htmlwidgets","here")


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
## 
## [[10]]
## [1] TRUE
```

```r
####### 


train_imputed <- read_csv(here("Datasets and Tables","train_imputed.csv"))
```

```
## Rows: 513 Columns: 72
## ── Column specification ──────────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): Species
## dbl (71): SPAD, L_Aarea, L_Amass, L_Con, L_Ci, L_iWUE, L_NightRespArea, L_NightRespmass, LA, L_Peri, L_Circ, LS, LMA, L_FreshM...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
test_imputed <- read_csv(here("Datasets and Tables","test_imputed.csv"))
```

```
## Rows: 205 Columns: 72
## ── Column specification ──────────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): Species
## dbl (71): SPAD, L_Aarea, L_Amass, L_Con, L_Ci, L_iWUE, L_NightRespArea, L_NightRespmass, LA, L_Peri, L_Circ, LS, LMA, L_FreshM...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S15.svg",
       dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Gini_Importance_southeastern.csv",
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
##          1   0.4869 0.4041    0.10275 0.11660         
##          2   0.7576 0.7172    0.10677 0.12295         
##          3   0.8222 0.7927    0.12977 0.14992         
##          4   0.8405 0.8142    0.07770 0.08930         
##          5   0.8886 0.8698    0.05633 0.06578         
##          6   0.9105 0.8957    0.06596 0.07685         
##          7   0.8876 0.8687    0.07721 0.08975         
##          8   0.8948 0.8776    0.08867 0.10274         
##          9   0.9144 0.9007    0.07582 0.08773         
##         10   0.9085 0.8939    0.07054 0.08139         
##         11   0.9252 0.9134    0.07196 0.08319         
##         12   0.9181 0.9050    0.06710 0.07753         
##         13   0.9311 0.9202    0.06541 0.07557         
##         14   0.9308 0.9197    0.06102 0.07070         
##         15   0.9305 0.9194    0.06696 0.07757         
##         16   0.9305 0.9194    0.06696 0.07757         
##         17   0.9239 0.9116    0.07103 0.08187         
##         18   0.9298 0.9183    0.05488 0.06363         
##         19   0.9298 0.9182    0.05488 0.06361         
##         20   0.9357 0.9251    0.05922 0.06870         
##         21   0.9409 0.9311    0.05749 0.06665         
##         22   0.9357 0.9249    0.05922 0.06868         
##         23   0.9357 0.9249    0.05922 0.06868         
##         24   0.9409 0.9310    0.05749 0.06674         
##         25   0.9304 0.9190    0.06529 0.07546         
##         26   0.9295 0.9176    0.05617 0.06540         
##         27   0.9295 0.9178    0.04885 0.05662         
##         28   0.9295 0.9176    0.05617 0.06540         
##         29   0.9301 0.9185    0.05497 0.06369         
##         30   0.9354 0.9245    0.05371 0.06230         
##         31   0.9413 0.9315    0.05750 0.06676         
##         32   0.9354 0.9245    0.05371 0.06230         
##         33   0.9301 0.9185    0.05497 0.06369         
##         34   0.9295 0.9176    0.05617 0.06540         
##         35   0.9295 0.9176    0.05617 0.06540         
##         36   0.9357 0.9249    0.06047 0.07045         
##         37   0.9416 0.9319    0.05749 0.06674         
##         38   0.9354 0.9245    0.05371 0.06230         
##         39   0.9236 0.9108    0.05079 0.05912         
##         40   0.9295 0.9176    0.05617 0.06540         
##         41   0.9357 0.9251    0.05374 0.06235         
##         42   0.9416 0.9319    0.05749 0.06674         
##         43   0.9299 0.9181    0.05624 0.06551         
##         44   0.9354 0.9246    0.06043 0.07040         
##         45   0.9295 0.9176    0.05617 0.06540         
##         46   0.9416 0.9319    0.05749 0.06674         
##         47   0.9236 0.9108    0.05079 0.05912         
##         48   0.9357 0.9249    0.06047 0.07045         
##         49   0.9292 0.9173    0.04959 0.05771         
##         50   0.9472 0.9383    0.06040 0.07016         
##         51   0.9409 0.9310    0.05749 0.06674         
##         52   0.9465 0.9376    0.05451 0.06321         
##         53   0.9469 0.9381    0.05446 0.06313         
##         54   0.9527 0.9449    0.05690 0.06600        *
##         55   0.9469 0.9381    0.05446 0.06313         
##         56   0.9416 0.9319    0.05749 0.06674         
##         57   0.9413 0.9316    0.05748 0.06672         
##         58   0.9527 0.9449    0.05690 0.06600         
##         59   0.9469 0.9381    0.05446 0.06313         
##         60   0.9413 0.9316    0.05117 0.05928         
##         61   0.9469 0.9381    0.05446 0.06313         
##         62   0.9469 0.9381    0.05446 0.06313         
##         63   0.9469 0.9381    0.05446 0.06313         
##         64   0.9469 0.9381    0.05446 0.06313         
##         65   0.9469 0.9381    0.05446 0.06313         
##         66   0.9469 0.9381    0.05446 0.06313         
##         67   0.9527 0.9449    0.05690 0.06600         
##         68   0.9469 0.9381    0.05446 0.06313         
##         69   0.9469 0.9381    0.05446 0.06313         
##         70   0.9469 0.9381    0.05446 0.06313         
##         71   0.9469 0.9381    0.05446 0.06313         
## 
## The top 5 variables (out of 54):
##    L_Circ, LTD, LWC, LDMC, LA
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
## 3              LWC
## 4             LDMC
## 5               LA
## 6             WPFB
## 7               LS
## 8        L_DryMass
## 9      L_FreshMass
## 10            FPAF
## 11             FRN
## 12             FPD
## 13            WPFF
## 14             P_T
## 15            FDAF
## 16            WPTB
## 17            FPFM
## 18              LC
## 19             FRL
## 20            FRWC
## 21             P_D
## 22           WPLMF
## 23           LD13C
## 24            FDFM
## 25            FRFM
## 26             FRW
## 27             FTC
## 28              LN
## 29             FTD
## 30            FPDM
## 31             FPA
## 32            FAIR
## 33            FDDM
## 34             FTA
## 35            FTFM
## 36          L_Peri
## 37            FTDM
## 38             FDC
## 39             FDD
## 40             FDA
## 41            FTWC
## 42 L_NightRespArea
## 43             LCN
## 44            FRDM
## 45             R_T
## 46       L_LaminaT
## 47       L_MidribT
## 48             D_T
## 49        L_LipidC
## 50           WPBMF
## 51           WPRMF
## 52    L_T_activity
## 53           LPNUE
## 54          L_iWUE
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S16.svg",
       dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_southeastern_best_subset,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Rfe_southeastern_best_subset.csv",
          row.names = FALSE)


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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S17.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_southeastern_perennials.csv",
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

Rf_Southeastern <- train(Species~.,data=train_boruta,
                      method="rf",trControl=params,
                      verbose=F)

Rf_Southeastern
```

```
## Random Forest 
## 
## 169 samples
##  54 predictor
##   8 classes: 'H_angustifolius', 'H_atrorubens', 'H_carnosus', 'H_floridanus', 'H_heterophyllus', 'H_longifolius', 'H_radula', 'H_silphioides' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 149, 153, 152, 152, 153, 154, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa    
##    2    0.943652  0.9345087
##   28    0.954902  0.9477464
##   54    0.941201  0.9313030
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 28.
```

```r
p_rf_boruta <- predict(Rf_Southeastern,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_RF_southeastern.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_RF_boruta[-1],2,mean))


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
##  54 predictor
##   8 classes: 'H_angustifolius', 'H_atrorubens', 'H_carnosus', 'H_floridanus', 'H_heterophyllus', 'H_longifolius', 'H_radula', 'H_silphioides' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 149, 153, 152, 152, 153, 154, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.9466667  0.9379565
##   4                  1000     0.9516667  0.9437369
##   6                   600     0.9516667  0.9438942
##   6                  1000     0.9511111  0.9431644
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at a
##  value of 10
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

write.csv(per_class_metrics_GBM_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_GBM_southeastern.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                           apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/GBM_Macro_averaged_metrics_southeastern.csv")



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


saveWidget(plot,"SoutheasternPerennials3d.html")
```

