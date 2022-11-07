

```r
#### GENUS ###

rm(list = ls())


packages <- list("tidyverse","here","caret","ggplot2","randomForest",
                 "plotly","Boruta","htmlwidgets")


### Loading all packages ### 
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
#### change the character into a factor (Species column)

train_imputed$Species <- factor(train_imputed$Species)

test_imputed$Species <- factor(test_imputed$Species)

### 

##### ### Computing Variable Importance by GINI ### 
set.seed(1234)

colnames(train_imputed)
```

```
##  [1] "Species"         "SPAD"            "L_Aarea"         "L_Amass"         "L_Con"           "L_Ci"            "L_iWUE"         
##  [8] "L_NightRespArea" "L_NightRespmass" "LA"              "L_Peri"          "L_Circ"          "LS"              "LMA"            
## [15] "L_FreshMass"     "L_DryMass"       "LWC"             "LDMC"            "L_MidribT"       "L_LaminaT"       "L_Tough"        
## [22] "L_life"          "LN"              "LD15C"           "LPNUE"           "LC"              "LD13C"           "LCN"            
## [29] "LP"              "Leaf_N_P"        "L_VeinD"         "L_T_activity"    "LTD"             "L_LipidC"        "L_AshC"         
## [36] "WPTB"            "WPBMF"           "WPSMF"           "WPLMF"           "WPRMF"           "WPFB"            "WPFF"           
## [43] "FPFM"            "FDFM"            "FRFM"            "FTFM"            "FPDM"            "FDDM"            "FRDM"           
## [50] "FTDM"            "FPWC"            "FDWC"            "FRWC"            "FTWC"            "P_D"             "P_T"            
## [57] "D_T"             "R_T"             "FRL"             "FRW"             "FDD"             "FDC"             "FTD"            
## [64] "FTC"             "FTA"             "FDA"             "FPA"             "FRN"             "FPD"             "FAIR"           
## [71] "FPAF"            "FDAF"
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S6.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### exporting the dataframe containing the variable importance of each feature

write.csv(importance_by_gini,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Gini_Importance_Genus.csv",
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
##          1   0.1803 0.1464    0.05945 0.06142         
##          2   0.4074 0.3822    0.04834 0.05078         
##          3   0.5969 0.5797    0.03964 0.04120         
##          4   0.6987 0.6860    0.04557 0.04742         
##          5   0.7519 0.7414    0.05223 0.05456         
##          6   0.7852 0.7761    0.04505 0.04710         
##          7   0.8022 0.7937    0.05022 0.05250         
##          8   0.8066 0.7984    0.04744 0.04956         
##          9   0.8201 0.8124    0.05627 0.05869         
##         10   0.8145 0.8067    0.05793 0.06040         
##         11   0.8090 0.8009    0.04344 0.04529         
##         12   0.8140 0.8061    0.04517 0.04706         
##         13   0.8217 0.8142    0.04532 0.04717         
##         14   0.8075 0.7994    0.04873 0.05063         
##         15   0.8311 0.8241    0.05028 0.05227         
##         16   0.8373 0.8304    0.04483 0.04676         
##         17   0.8478 0.8414    0.03338 0.03485         
##         18   0.8409 0.8342    0.03707 0.03865         
##         19   0.8457 0.8393    0.03917 0.04082         
##         20   0.8539 0.8477    0.03943 0.04120         
##         21   0.8596 0.8536    0.03562 0.03718         
##         22   0.8602 0.8543    0.03642 0.03794         
##         23   0.8600 0.8541    0.03381 0.03534         
##         24   0.8636 0.8579    0.04202 0.04387         
##         25   0.8599 0.8540    0.02964 0.03097         
##         26   0.8599 0.8539    0.03722 0.03881         
##         27   0.8715 0.8660    0.02780 0.02905         
##         28   0.8718 0.8663    0.04348 0.04542         
##         29   0.8715 0.8660    0.03713 0.03877         
##         30   0.8780 0.8728    0.04678 0.04881         
##         31   0.8813 0.8762    0.03594 0.03747         
##         32   0.8741 0.8688    0.04231 0.04417         
##         33   0.8836 0.8786    0.04298 0.04486         
##         34   0.8873 0.8825    0.04708 0.04912         
##         35   0.8911 0.8864    0.04495 0.04690         
##         36   0.8854 0.8805    0.03435 0.03589         
##         37   0.8888 0.8841    0.03620 0.03786         
##         38   0.8871 0.8822    0.04074 0.04257         
##         39   0.8930 0.8884    0.04124 0.04304         
##         40   0.8971 0.8927    0.04104 0.04286         
##         41   0.8933 0.8887    0.04171 0.04354         
##         42   0.8914 0.8868    0.04908 0.05123         
##         43   0.8933 0.8887    0.04093 0.04275         
##         44   0.8914 0.8867    0.04355 0.04547         
##         45   0.8990 0.8947    0.03963 0.04135         
##         46   0.8971 0.8927    0.04219 0.04400         
##         47   0.8953 0.8908    0.03824 0.03992         
##         48   0.8915 0.8868    0.04271 0.04456         
##         49   0.8895 0.8848    0.04417 0.04614         
##         50   0.8970 0.8926    0.04583 0.04786         
##         51   0.8933 0.8887    0.04179 0.04368         
##         52   0.8971 0.8927    0.04086 0.04263         
##         53   0.8990 0.8947    0.03877 0.04047         
##         54   0.8893 0.8846    0.05171 0.05398         
##         55   0.8951 0.8907    0.04579 0.04783         
##         56   0.8932 0.8887    0.04201 0.04386         
##         57   0.8931 0.8885    0.04018 0.04197         
##         58   0.8991 0.8947    0.04553 0.04755         
##         59   0.8992 0.8949    0.03601 0.03758         
##         60   0.8969 0.8925    0.04081 0.04264         
##         61   0.8932 0.8886    0.04546 0.04751         
##         62   0.8891 0.8844    0.04290 0.04483         
##         63   0.8972 0.8928    0.04419 0.04617         
##         64   0.8913 0.8867    0.03988 0.04166         
##         65   0.9011 0.8969    0.03762 0.03928         
##         66   0.8934 0.8888    0.04235 0.04427         
##         67   0.9034 0.8993    0.04073 0.04249        *
##         68   0.9009 0.8967    0.03332 0.03484         
##         69   0.8956 0.8911    0.04835 0.05054         
##         70   0.8994 0.8951    0.04474 0.04673         
##         71   0.8934 0.8888    0.04644 0.04850         
## 
## The top 5 variables (out of 67):
##    L_Circ, LTD, WPFF, LA, WPFB
```

```r
## these are the predictors in the optimal subset 

optimal_subset_rfe_genus <- data.frame(Features = predictors(features_rfe_gini))

optimal_subset_rfe_genus
```

```
##           Features
## 1           L_Circ
## 2              LTD
## 3             WPFF
## 4               LA
## 5             WPFB
## 6      L_FreshMass
## 7             FRFM
## 8             FTFM
## 9        L_DryMass
## 10            FTDM
## 11            FDFM
## 12             LWC
## 13            LDMC
## 14          L_life
## 15             FDD
## 16           LD13C
## 17            FRDM
## 18             FDA
## 19             FDC
## 20             FRN
## 21            FPFM
## 22            FDDM
## 23            FPAF
## 24            FDAF
## 25           WPBMF
## 26            FAIR
## 27           WPSMF
## 28              LS
## 29           WPRMF
## 30            FPDM
## 31         L_Aarea
## 32       L_LaminaT
## 33             P_T
## 34          L_Peri
## 35         L_Tough
## 36              LN
## 37             P_D
## 38             FRW
## 39            WPTB
## 40             LCN
## 41             FPD
## 42        L_LipidC
## 43            SPAD
## 44             FTC
## 45             FRL
## 46           L_Con
## 47             FTA
## 48             FTD
## 49 L_NightRespArea
## 50             FPA
## 51           WPLMF
## 52             LMA
## 53            FRWC
## 54       L_MidribT
## 55 L_NightRespmass
## 56              LC
## 57    L_T_activity
## 58         L_Amass
## 59             R_T
## 60            FPWC
## 61           LD15C
## 62            FTWC
## 63             D_T
## 64            FDWC
## 65         L_VeinD
## 66           LPNUE
## 67          L_AshC
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S7.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
### writing out the optimal number of features in a dataframe ###

write.csv(Rfe_Imp_Genus_best_subset,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Rfe_Genus_best_subset.csv",
          row.names = FALSE)


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
## Boruta performed 13 iterations in 20.37031 secs.
##  67 attributes confirmed important: D_T, FAIR, FDA, FDAF, FDC and 62 more;
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
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S8.svg",
       dpi = 300)
```

```
## Saving 7 x 7 in image
```

```r
####### exporting the strongly relevant features as deemed by Boruta 

write.csv(Boruta_feature_analysis,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Genus.csv",
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

Rf <- train(Species~.,data=train_boruta,
            method="rf",trControl=params,
            verbose=F)

Rf
```

```
## Random Forest 
## 
## 513 samples
##  67 predictor
##  28 classes: 'H_agrestis', 'H_angustifolius', 'H_annuus', 'H_argophyllus', 'H_atrorubens', 'H_carnosus', 'H_cusickii', 'H_debili_ssp_tardiflorus', 'H_divaricatus', 'H_floridanus', 'H_giganteus', 'H_grosseserratus', 'H_heterophyllus', 'H_longifolius', 'H_maximiliani', 'H_microcephalus', 'H_mollis', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_occidentali_ssp_occidentalis', 'H_petiolari_ssp_petiolaris', 'H_porteri', 'H_praeco_ssp_runyonii', 'H_radula', 'H_salicifolius', 'H_silphioides', 'H_verticillatus', 'P_tenuifolius' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 458, 458, 462, 460, 466, 466, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.8929700  0.8882994
##   34    0.8873520  0.8824625
##   67    0.8850562  0.8800352
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
```

```r
p_rf_boruta <- predict(Rf,test_boruta)

c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)



##############
### Extracting data from the confusion matrix 

###############    #### Random Forest ######## 

per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)

### writing this dataframe out in a nice excel sheet ### 

write.csv(per_class_metrics_RF_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_RF_genus.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                        apply(per_class_metrics_RF_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_Rf,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/RF_Macro_averaged_metrics_genus.csv")


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
##  67 predictor
##  28 classes: 'H_agrestis', 'H_angustifolius', 'H_annuus', 'H_argophyllus', 'H_atrorubens', 'H_carnosus', 'H_cusickii', 'H_debili_ssp_tardiflorus', 'H_divaricatus', 'H_floridanus', 'H_giganteus', 'H_grosseserratus', 'H_heterophyllus', 'H_longifolius', 'H_maximiliani', 'H_microcephalus', 'H_mollis', 'H_neglectus', 'H_niveu_ssp_tephrodes', 'H_occidentali_ssp_occidentalis', 'H_petiolari_ssp_petiolaris', 'H_porteri', 'H_praeco_ssp_runyonii', 'H_radula', 'H_salicifolius', 'H_silphioides', 'H_verticillatus', 'P_tenuifolius' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 458, 458, 462, 460, 466, 466, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   4                   600     0.8633665  0.8575269
##   4                  1000     0.8450132  0.8382634
##   6                   600     0.8562133  0.8501393
##   6                  1000     0.8299896  0.8227519
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at a
##  value of 10
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

write.csv(per_class_metrics_GBM_boruta,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/per_class_metrics_GBM_genus.csv")

#### Macro averaged metrics ### Random_forest ########

Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                          apply(per_class_metrics_GBM_boruta[-1],2,mean))


### Write it out in a nice excel sheet ### 

write.csv(Macro_averaged_metrics_GBM,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/GBM_Macro_averaged_metrics_genus.csv")

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


saveWidget(plot,"Genus3d.html")
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors

## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors
```

