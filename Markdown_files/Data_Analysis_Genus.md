

```r
#### GENUS ###

rm(list = ls())


packages <- list("tidyverse","caret","ggplot2","randomForest",
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
```

```r
### reading in data #### 

train_imputed <- read.csv("train_imputed.csv")

test_imputed <- read.csv("test_imputed.csv")

#### change the character into a factor (Species column)

train_imputed$Species <- factor(train_imputed$Species)

test_imputed$Species <- factor(test_imputed$Species)

### 

##### ### Computing Variable Importance by GINI ### 
set.seed(1234)

colnames(train_imputed)
```

```
##  [1] "Species"         "SPAD"            "L_Aarea"         "L_Amass"         "L_Con"           "L_Ci"           
##  [7] "L_iWUE"          "L_NightRespArea" "L_NightRespmass" "LA"              "L_Peri"          "L_Circ"         
## [13] "LS"              "LMA"             "L_FreshMass"     "L_DryMass"       "LWC"             "LDMC"           
## [19] "L_MidribT"       "L_LaminaT"       "L_Tough"         "L_life"          "LN"              "LD15C"          
## [25] "LPNUE"           "LC"              "LD13C"           "LCN"             "LP"              "Leaf_N_P"       
## [31] "L_VeinD"         "L_T_activity"    "LTD"             "L_LipidC"        "L_AshC"          "WPTB"           
## [37] "WPBMF"           "WPSMF"           "WPLMF"           "WPRMF"           "WPFB"            "WPFF"           
## [43] "FPFM"            "FDFM"            "FRFM"            "FTFM"            "FPDM"            "FDDM"           
## [49] "FRDM"            "FTDM"            "FPWC"            "FDWC"            "FRWC"            "FTWC"           
## [55] "P_D"             "P_T"             "D_T"             "R_T"             "FRL"             "FRW"            
## [61] "FDD"             "FDC"             "FTD"             "FTC"             "FTA"             "FDA"            
## [67] "FPA"             "FRN"             "FPD"             "FAIR"            "FPAF"            "FDAF"
```

```r
Rf_genus <- randomForest(Species~.,data = train_imputed)

importance_by_gini <- varImp(Rf_genus)

importance_by_gini <- data.frame(Features = row.names(importance_by_gini),
                                 Overall = importance_by_gini$Overall)


## making a ggplot ### 


ggplot(data = importance_by_gini,
              aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Importance of each Variable as per GINI") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
ggsave("Figure 3a.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,"Gini_Importance_Genus.csv",row.names = FALSE)


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
##          1   0.1666 0.1321    0.04555 0.04805         
##          2   0.4056 0.3801    0.05015 0.05177         
##          3   0.6058 0.5886    0.03906 0.04041         
##          4   0.7272 0.7157    0.04194 0.04376         
##          5   0.7781 0.7687    0.04031 0.04220         
##          6   0.8004 0.7919    0.03522 0.03675         
##          7   0.8283 0.8209    0.05285 0.05534         
##          8   0.8390 0.8321    0.04966 0.05162         
##          9   0.8428 0.8361    0.03462 0.03602         
##         10   0.8310 0.8238    0.03666 0.03809         
##         11   0.8308 0.8237    0.04627 0.04808         
##         12   0.8385 0.8316    0.04571 0.04755         
##         13   0.8424 0.8356    0.03221 0.03347         
##         14   0.8425 0.8358    0.03842 0.03989         
##         15   0.8621 0.8562    0.04356 0.04534         
##         16   0.8547 0.8485    0.04653 0.04841         
##         17   0.8704 0.8649    0.03709 0.03850         
##         18   0.8626 0.8567    0.03710 0.03854         
##         19   0.8679 0.8623    0.04613 0.04798         
##         20   0.8720 0.8665    0.03226 0.03353         
##         21   0.8622 0.8564    0.03296 0.03420         
##         22   0.8756 0.8703    0.03357 0.03490         
##         23   0.8701 0.8645    0.03701 0.03842         
##         24   0.8700 0.8645    0.03730 0.03877         
##         25   0.8759 0.8706    0.02969 0.03079         
##         26   0.8775 0.8723    0.02887 0.02991         
##         27   0.8731 0.8676    0.02752 0.02866         
##         28   0.8831 0.8781    0.02996 0.03121         
##         29   0.8751 0.8697    0.03154 0.03284         
##         30   0.8810 0.8759    0.02239 0.02327         
##         31   0.8868 0.8820    0.02936 0.03058         
##         32   0.8825 0.8775    0.03167 0.03299         
##         33   0.8866 0.8818    0.03021 0.03146         
##         34   0.8905 0.8858    0.02844 0.02962         
##         35   0.8963 0.8918    0.03457 0.03602         
##         36   0.8946 0.8901    0.03052 0.03184         
##         37   0.8859 0.8810    0.03886 0.04051         
##         38   0.8827 0.8777    0.03680 0.03834         
##         39   0.8962 0.8917    0.03945 0.04107         
##         40   0.8902 0.8855    0.03544 0.03701         
##         41   0.9019 0.8977    0.04547 0.04750         
##         42   0.8940 0.8894    0.04016 0.04191         
##         43   0.8980 0.8936    0.03726 0.03890         
##         44   0.8937 0.8892    0.04366 0.04557         
##         45   0.8938 0.8892    0.03848 0.04018         
##         46   0.8961 0.8916    0.03629 0.03789         
##         47   0.8959 0.8914    0.04014 0.04189         
##         48   0.8941 0.8895    0.04361 0.04549         
##         49   0.8875 0.8827    0.04542 0.04742         
##         50   0.8944 0.8898    0.04004 0.04177         
##         51   0.8896 0.8849    0.04610 0.04811         
##         52   0.8996 0.8953    0.04016 0.04192         
##         53   0.9112 0.9073    0.04141 0.04322        *
##         54   0.8978 0.8934    0.03787 0.03949         
##         55   0.9053 0.9012    0.03751 0.03915         
##         56   0.9073 0.9033    0.03159 0.03296         
##         57   0.9070 0.9030    0.04570 0.04769         
##         58   0.8975 0.8931    0.04106 0.04286         
##         59   0.9034 0.8992    0.03902 0.04072         
##         60   0.9089 0.9049    0.04227 0.04410         
##         61   0.9112 0.9073    0.04141 0.04323         
##         62   0.9051 0.9010    0.04397 0.04587         
##         63   0.8991 0.8948    0.04344 0.04528         
##         64   0.9093 0.9054    0.03663 0.03825         
##         65   0.9052 0.9012    0.04263 0.04452         
##         66   0.9050 0.9009    0.04220 0.04402         
##         67   0.9052 0.9011    0.03971 0.04146         
##         68   0.9070 0.9030    0.04498 0.04695         
##         69   0.9111 0.9073    0.03884 0.04054         
##         70   0.9071 0.9031    0.03387 0.03535         
##         71   0.9071 0.9031    0.04012 0.04185         
## 
## The top 5 variables (out of 53):
##    L_Circ, LTD, WPFF, LA, WPFB
```

```r
## these are the predictors in the optimal subset 

optimal_subset_rfe_genus <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_genus
```

```
##        Features
## 1        L_Circ
## 2           LTD
## 3          WPFF
## 4            LA
## 5          WPFB
## 6          FRFM
## 7   L_FreshMass
## 8     L_DryMass
## 9          FTFM
## 10         FDFM
## 11          LWC
## 12         LDMC
## 13         FTDM
## 14        WPBMF
## 15         FDDM
## 16          FDA
## 17        LD13C
## 18          FDC
## 19       L_life
## 20         FDAF
## 21          FDD
## 22         FRDM
## 23           LS
## 24          FRN
## 25         FPAF
## 26    L_LaminaT
## 27        WPRMF
## 28        WPSMF
## 29      L_Tough
## 30         FAIR
## 31         FPFM
## 32         WPTB
## 33         FPDM
## 34       L_Peri
## 35      L_Aarea
## 36          FRW
## 37     L_LipidC
## 38          P_T
## 39        L_Con
## 40          FRL
## 41          FPD
## 42          FTD
## 43          FTA
## 44         SPAD
## 45          FPA
## 46          P_D
## 47           LC
## 48          FTC
## 49 L_T_activity
## 50           LN
## 51          LCN
## 52         FRWC
## 53        WPLMF
```

```r
#### Importance of each variable 

Rfe_Imp_Genus <- data.frame(varImp(features_rfe_gini))

Rfe_Imp_Genus <- data.frame(Features = rownames(Rfe_Imp_Genus),
                            Overall = Rfe_Imp_Genus$Overall)

Rfe_Imp_Genus_best_subset <- Rfe_Imp_Genus %>%
                             dplyr::filter(Features %in% optimal_subset_rfe_genus$Features)


### plotting the variation of accuracy with the removal of variables
ggplot(features_rfe_gini)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
ggplot(data = Rfe_Imp_Genus_best_subset,
                          aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
  geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
  coord_flip() + 
  theme_bw() + theme(legend.position = "none") + 
  ggtitle("Optimal subset of features") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
ggsave("Figure 4a.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Genus_best_subset,"Rfe_Genus_best_subset.csv",row.names = FALSE)


##### 
## Now removing the features outside of the optimal subset ### 
## from both train and test 

###### train

train_optimal <- train_imputed %>% 
                 dplyr::select(Species,optimal_subset_rfe_genus$Features)

            
### test

test_optimal <- test_imputed %>% 
                 dplyr::select(Species,optimal_subset_rfe_genus$Features)





###########################
#### BORUTA ######
##### FINDING THE STRONGEST, WEAKLY RELEVANT AND REDUNDANT VARIABLES ###

Boruta_genus <- Boruta(Species ~., train_optimal)

Boruta_genus
```

```
## Boruta performed 13 iterations in 25.88915 secs.
##  53 attributes confirmed important: FAIR, FDA, FDAF, FDC, FDD and 48 more;
##  No attributes deemed unimportant.
```

```r
## Putting the importance decisions in a nice table ### 

Boruta_feature_analysis <- data.frame(attStats(Boruta_genus)) 

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
  ggtitle("Strongly important variables") +
  theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
ggsave("Figure 5a.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,"Boruta_Genus.csv",row.names = FALSE)


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

Rf <- train(Species~.,data=train_boruta,
            method="rf",trControl=params,
            verbose=F)

Rf
```

```
## Random Forest 
## 
## 513 samples
##  53 predictor
##  28 classes: 'H_agrestis', 'H_angustifolius', 'H_annuus', 'H_argophyllus', 'H_atrorubens', 'H_carnosus', 'H_cusickii', 'H_debili_ssp_tardiflorus', 'H_divaricatus', 'H_floridanus', 'H_giganteus', 'H_grosseserratus', 'H_heterophyllus', 'H_longifolius', 'H_maximiliani', 'H_microcephalus', 'H_mollis', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_occidentali_ssp_occidentalis', 'H_petiolari_ssp_petiolaris', 'H_porteri', 'H_praeco_ssp_runyonii', 'H_radula', 'H_salicifolius', 'H_silphioides', 'H_verticillatus', 'P_tenuifolius' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 458, 458, 462, 460, 466, 466, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.8850345  0.8800660
##   27    0.9006593  0.8963965
##   53    0.9009244  0.8966507
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 53.
```

```r
p_rf_boruta <- predict(Rf,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,"per_class_metrics_RF_genus.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                        apply(per_class_metrics_RF_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,"RF_Macro_averaged_metrics_genus.csv")


#### GBM ### 

grid <- expand.grid(n.trees=c(600,1000),
                    interaction.depth=c(4,6),
                    shrinkage=0.1,
                    n.minobsinnode=10)

params <- trainControl(method = "cv",
                       number = 10)

set.seed(1234)
gbm <- train(Species~., data=train_boruta,
                    method="gbm",trControl=params,
                    verbose=F,tuneGrid=grid)

gbm
```

```
## Stochastic Gradient Boosting 
## 
## 513 samples
##  53 predictor
##  28 classes: 'H_agrestis', 'H_angustifolius', 'H_annuus', 'H_argophyllus', 'H_atrorubens', 'H_carnosus', 'H_cusickii', 'H_debili_ssp_tardiflorus', 'H_divaricatus', 'H_floridanus', 'H_giganteus', 'H_grosseserratus', 'H_heterophyllus', 'H_longifolius', 'H_maximiliani', 'H_microcephalus', 'H_mollis', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_occidentali_ssp_occidentalis', 'H_petiolari_ssp_petiolaris', 'H_porteri', 'H_praeco_ssp_runyonii', 'H_radula', 'H_salicifolius', 'H_silphioides', 'H_verticillatus', 'P_tenuifolius' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 458, 458, 462, 460, 466, 466, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.8732507  0.8677353
##   4                  1000     0.8361417  0.8289926
##   6                   600     0.8635705  0.8576415
##   6                  1000     0.8342688  0.8269329
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at
##  a value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 600, interaction.depth = 4, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
p_gbm <- predict(gbm,test_boruta)

c_gbm <- confusionMatrix(p_gbm,test_boruta$Species)


##############
### Extracting data from the confusion matrix 

###############    #### Gradient Boosting ######## 

per_class_metrics_GBM_boruta <- data.frame(c_gbm$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_GBM_boruta,"per_class_metrics_GBM_genus.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,"GBM_Macro_averaged_metrics_genus.csv")

### Model comparison plots 

Model_list <- resamples(list(RF=Rf,GBM=gbm))


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

plot <- plot_ly(df,x= ~L_Circ,y= ~LTD,z= ~LA, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Circularity'),
                 yaxis = list(title = 'Leaf Trichome Density'),
                 zaxis = list(title = 'Leaf Area'))
  )

plot
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors

## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
saveWidget(plot,"Genus3d.html")
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors

## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors
```

