

```r
##### ANNUAL ###

rm(list = ls())


packages <- list("tidyverse","naniar","caret","ggplot2","randomForest","readxl",
                 "plotly","Boruta","htmlwidgets","here")


lapply(packages, require,character.only=T)
```

```
## Loading required package: tidyverse
```

```
## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
## ✔ readr   2.1.2     ✔ forcats 0.5.2
```

```
## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```
## Loading required package: naniar
```

```
## Loading required package: caret
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```
## Loading required package: randomForest
```

```
## randomForest 4.7-1.1
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## Loading required package: readxl
```

```
## Loading required package: plotly
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```
## Loading required package: Boruta
```

```
## Loading required package: htmlwidgets
```

```
## Loading required package: here
```

```
## Warning: package 'here' was built under R version 4.2.2
```

```
## here() starts at C:/Users/samba/Documents/Chapter_1_Analysis
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
```

```
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S9.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
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
##          1   0.4476 0.3520    0.13551 0.14800         
##          2   0.6697 0.6068    0.09761 0.11462         
##          3   0.7244 0.6709    0.09952 0.11968         
##          4   0.7846 0.7425    0.07061 0.08631         
##          5   0.7751 0.7318    0.11842 0.14215         
##          6   0.8019 0.7640    0.09843 0.11836         
##          7   0.8350 0.8035    0.08585 0.10275         
##          8   0.8426 0.8130    0.10077 0.12034         
##          9   0.8641 0.8377    0.10567 0.12681         
##         10   0.8722 0.8478    0.10616 0.12727         
##         11   0.8660 0.8390    0.11176 0.13655         
##         12   0.8937 0.8715    0.11411 0.14029         
##         13   0.8654 0.8381    0.11224 0.13714         
##         14   0.8875 0.8644    0.12059 0.14723         
##         15   0.8996 0.8791    0.11123 0.13545        *
##         16   0.8991 0.8781    0.10654 0.13139         
##         17   0.8779 0.8527    0.10466 0.12884         
##         18   0.8842 0.8599    0.10632 0.13120         
##         19   0.8779 0.8528    0.10466 0.12880         
##         20   0.8769 0.8519    0.10567 0.12876         
##         21   0.8923 0.8703    0.10567 0.12896         
##         22   0.8779 0.8532    0.10466 0.12726         
##         23   0.8631 0.8354    0.09604 0.11677         
##         24   0.8789 0.8542    0.10830 0.13187         
##         25   0.8774 0.8517    0.11050 0.13584         
##         26   0.8788 0.8529    0.11313 0.13963         
##         27   0.8717 0.8450    0.10670 0.13145         
##         28   0.8568 0.8271    0.09721 0.12010         
##         29   0.8579 0.8277    0.10135 0.12558         
##         30   0.8706 0.8439    0.10762 0.13166         
##         31   0.8717 0.8444    0.10670 0.13210         
##         32   0.8568 0.8267    0.09721 0.12075         
##         33   0.8717 0.8444    0.10670 0.13210         
##         34   0.8573 0.8270    0.10725 0.13240         
##         35   0.8719 0.8452    0.09931 0.12257         
##         36   0.8640 0.8351    0.10535 0.13032         
##         37   0.8645 0.8358    0.10484 0.12971         
##         38   0.8636 0.8353    0.10617 0.13067         
##         39   0.8656 0.8374    0.10104 0.12448         
##         40   0.8656 0.8374    0.10104 0.12448         
##         41   0.8723 0.8456    0.09840 0.12153         
##         42   0.8656 0.8376    0.10104 0.12446         
##         43   0.8585 0.8290    0.09860 0.12141         
##         44   0.8656 0.8374    0.10104 0.12448         
##         45   0.8723 0.8456    0.09840 0.12150         
##         46   0.8806 0.8566    0.08104 0.09835         
##         47   0.8656 0.8376    0.10104 0.12446         
##         48   0.8656 0.8376    0.10104 0.12446         
##         49   0.8714 0.8450    0.09465 0.11703         
##         50   0.8718 0.8453    0.09882 0.12192         
##         51   0.8656 0.8376    0.10104 0.12446         
##         52   0.8648 0.8369    0.09733 0.12005         
##         53   0.8806 0.8564    0.08104 0.09838         
##         54   0.8790 0.8536    0.10021 0.12377         
##         55   0.8656 0.8376    0.10104 0.12446         
##         56   0.8719 0.8452    0.09931 0.12257         
##         57   0.8714 0.8448    0.09465 0.11703         
##         58   0.8714 0.8455    0.09465 0.11532         
##         59   0.8648 0.8369    0.09733 0.12005         
##         60   0.8648 0.8376    0.09733 0.11843         
##         61   0.8786 0.8532    0.09613 0.11900         
##         62   0.8576 0.8284    0.08852 0.10962         
##         63   0.8648 0.8376    0.09733 0.11843         
##         64   0.8648 0.8369    0.09733 0.12005         
##         65   0.8786 0.8539    0.09613 0.11725         
##         66   0.8719 0.8452    0.09931 0.12257         
##         67   0.8786 0.8533    0.09613 0.11898         
##         68   0.8719 0.8452    0.09931 0.12257         
##         69   0.8786 0.8540    0.09613 0.11723         
##         70   0.8719 0.8452    0.09931 0.12257         
##         71   0.8786 0.8532    0.09613 0.11900         
## 
## The top 5 variables (out of 15):
##    LTD, FTFM, LA, FTDM, FDFM
```

```r
## these are the predictors in the optimal subset 

optimal_subset_rfe_annual <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_annual
```

```
##           Features
## 1              LTD
## 2             FTFM
## 3               LA
## 4             FTDM
## 5             FDFM
## 6             FRDM
## 7            WPRMF
## 8          L_Aarea
## 9             FDDM
## 10            WPTB
## 11 L_NightRespArea
## 12            FRFM
## 13        L_LipidC
## 14       L_LaminaT
## 15             FDA
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S10.svg",
       dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S11.svg",
       dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
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
```

```
## Random Forest 
## 
## 136 samples
##  15 predictor
##   7 classes: 'H_annuus', 'H_argophyllus', 'H_debili_ssp_tardiflorus', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_petiolari_ssp_petiolaris', 'H_praeco_ssp_runyonii' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 122, 121, 121, 123, 123, 122, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9146154  0.8985945
##    8    0.9140659  0.8974689
##   15    0.8825641  0.8594599
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
```

```
## Stochastic Gradient Boosting 
## 
## 136 samples
##  15 predictor
##   7 classes: 'H_annuus', 'H_argophyllus', 'H_debili_ssp_tardiflorus', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_petiolari_ssp_petiolaris', 'H_praeco_ssp_runyonii' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 122, 121, 121, 123, 123, 122, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.9281868  0.9140864
##   4                  1000     0.9348535  0.9218639
##   6                   600     0.9281868  0.9140105
##   6                  1000     0.9348535  0.9219150
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at a
##  value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 1000, interaction.depth = 4, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
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


saveWidget(plot,"Annual3d.html")
```

