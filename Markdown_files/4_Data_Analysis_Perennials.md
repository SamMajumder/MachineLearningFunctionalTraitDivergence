

```r
##### PERENNIAL ###

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
### reading in data #### 


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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S12.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Gini_Importance_Perennials.csv",
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
##          1   0.4200 0.3320    0.10902 0.12712         
##          2   0.5880 0.5251    0.09895 0.11403         
##          3   0.6652 0.6122    0.15264 0.17813         
##          4   0.7127 0.6663    0.12534 0.14543         
##          5   0.7810 0.7454    0.10250 0.12074         
##          6   0.8284 0.8014    0.07051 0.08138         
##          7   0.8426 0.8178    0.05538 0.06416         
##          8   0.8355 0.8097    0.06800 0.07861         
##          9   0.8527 0.8295    0.07138 0.08257         
##         10   0.8195 0.7914    0.10543 0.12123         
##         11   0.8586 0.8366    0.07235 0.08302         
##         12   0.8266 0.7999    0.06434 0.07365         
##         13   0.8271 0.8006    0.08329 0.09535         
##         14   0.8182 0.7903    0.08957 0.10202         
##         15   0.8271 0.8006    0.09454 0.10777         
##         16   0.8431 0.8189    0.08490 0.09731         
##         17   0.8271 0.8006    0.10755 0.12296         
##         18   0.8271 0.8004    0.11915 0.13630         
##         19   0.8438 0.8194    0.06326 0.07250         
##         20   0.8443 0.8200    0.06115 0.07040         
##         21   0.8431 0.8189    0.07676 0.08768         
##         22   0.8515 0.8282    0.05761 0.06604         
##         23   0.8515 0.8283    0.05761 0.06612         
##         24   0.8515 0.8283    0.05761 0.06612         
##         25   0.8515 0.8282    0.05761 0.06604         
##         26   0.8503 0.8268    0.05949 0.06842         
##         27   0.8455 0.8212    0.07466 0.08674         
##         28   0.8603 0.8386    0.05628 0.06522         
##         29   0.8455 0.8216    0.06527 0.07579         
##         30   0.8461 0.8220    0.08705 0.10097         
##         31   0.8467 0.8227    0.08634 0.10023         
##         32   0.8467 0.8229    0.09485 0.11001         
##         33   0.8538 0.8311    0.07774 0.08960         
##         34   0.8307 0.8044    0.08551 0.09908         
##         35   0.8532 0.8305    0.08656 0.09975         
##         36   0.8615 0.8399    0.07203 0.08317         
##         37   0.8621 0.8403    0.09901 0.11486         
##         38   0.8621 0.8403    0.09213 0.10703         
##         39   0.8615 0.8400    0.08842 0.10199         
##         40   0.8532 0.8304    0.08444 0.09776         
##         41   0.8692 0.8488    0.07428 0.08589         
##         42   0.8758 0.8563    0.05055 0.05873         
##         43   0.8663 0.8454    0.05249 0.06089         
##         44   0.8758 0.8564    0.05055 0.05869         
##         45   0.8836 0.8656    0.05479 0.06307         
##         46   0.8699 0.8494    0.08309 0.09640         
##         47   0.8615 0.8397    0.07203 0.08361         
##         48   0.8918 0.8748    0.07254 0.08411        *
##         49   0.8681 0.8475    0.04836 0.05604         
##         50   0.8835 0.8652    0.06288 0.07288         
##         51   0.8758 0.8563    0.05055 0.05873         
##         52   0.8681 0.8475    0.04836 0.05604         
##         53   0.8598 0.8381    0.04620 0.05336         
##         54   0.8746 0.8553    0.04129 0.04724         
##         55   0.8692 0.8486    0.08266 0.09588         
##         56   0.8610 0.8391    0.05499 0.06410         
##         57   0.8818 0.8636    0.04402 0.05031         
##         58   0.8758 0.8564    0.05055 0.05869         
##         59   0.8746 0.8553    0.04129 0.04724         
##         60   0.8687 0.8478    0.06705 0.07867         
##         61   0.8599 0.8383    0.05837 0.06700         
##         62   0.8770 0.8576    0.07032 0.08182         
##         63   0.8527 0.8297    0.05184 0.06033         
##         64   0.8687 0.8479    0.05798 0.06759         
##         65   0.8687 0.8479    0.05798 0.06759         
##         66   0.8603 0.8385    0.05628 0.06544         
##         67   0.8669 0.8464    0.03833 0.04379         
##         68   0.8758 0.8563    0.05055 0.05873         
##         69   0.8687 0.8479    0.05798 0.06759         
##         70   0.8687 0.8481    0.05798 0.06735         
##         71   0.8758 0.8563    0.05055 0.05873         
## 
## The top 5 variables (out of 48):
##    WPFF, LD13C, L_Circ, WPFB, L_Aarea
```

```r
## these are the predictors in the optimal subset 

optimal_subset_rfe_perennials <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_perennials
```

```
##       Features
## 1         WPFF
## 2        LD13C
## 3       L_Circ
## 4         WPFB
## 5      L_Aarea
## 6           LA
## 7          FRN
## 8    L_DryMass
## 9      L_Tough
## 10        FRFM
## 11        FPFM
## 12        FDFM
## 13        FTDM
## 14   L_LaminaT
## 15        FTFM
## 16        FRDM
## 17 L_FreshMass
## 18      L_Peri
## 19         LMA
## 20   L_MidribT
## 21        FDDM
## 22       L_Con
## 23         FPD
## 24    L_LipidC
## 25        LDMC
## 26     L_VeinD
## 27         FDC
## 28        FPDM
## 29        FPWC
## 30       WPSMF
## 31         LWC
## 32         FDD
## 33         FDA
## 34          LS
## 35        FDWC
## 36        SPAD
## 37       WPBMF
## 38      L_AshC
## 39        FAIR
## 40       LD15C
## 41         LTD
## 42    Leaf_N_P
## 43         FPA
## 44         D_T
## 45       WPLMF
## 46         FTD
## 47         FTA
## 48          LC
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S13.svg",
       dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Perennial_best_subset,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Rfe_Perennial_best_subset.csv",
          row.names = FALSE)


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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S14.svg",
       dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Perennials.csv",
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

Rf_Perennial <- train(Species~.,data=train_boruta,
                   method="rf",trControl=params,
                   verbose=F)

Rf_Perennial
```

```
## Random Forest 
## 
## 128 samples
##  48 predictor
##   8 classes: 'H_cusickii', 'H_divaricatus', 'H_giganteus', 'H_grosseserratus', 'H_maximiliani', 'H_microcephalus', 'H_salicifolius', 'H_verticillatus' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 115, 117, 116, 114, 115, 114, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.8603230  0.8386679
##   25    0.8699634  0.8496897
##   48    0.8616300  0.8403558
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 25.
```

```r
p_rf_boruta <- predict(Rf_Perennial,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_RF_perennial.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_RF_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/RF_Macro_averaged_metrics_perennial.csv")


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
##  48 predictor
##   8 classes: 'H_cusickii', 'H_divaricatus', 'H_giganteus', 'H_grosseserratus', 'H_maximiliani', 'H_microcephalus', 'H_salicifolius', 'H_verticillatus' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 115, 117, 116, 114, 115, 114, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.9086081  0.8941748
##   4                  1000     0.9002747  0.8845748
##   6                   600     0.9008242  0.8855078
##   6                  1000     0.8931319  0.8765423
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at a
##  value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 600, interaction.depth = 4, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
p_gbm <- predict(gbm_Perennial,test_boruta)

c_gbm <- confusionMatrix(p_gbm,test_boruta$Species)


##############
### Extracting data from the confusion matrix 

###############    #### Gradient Boosting ######## 

per_class_metrics_GBM_boruta <- data.frame(c_gbm$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_GBM_perennial.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                           apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/GBM_Macro_averaged_metrics_perennial.csv")



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


saveWidget(plot,"Perennials3d.html")
```

