

```r
####  

rm(list = ls())

packages <- list("tidyverse","naniar","ggplot2","randomForest","caret","readxl")


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
```

```r
Sunflower_traits <- read_excel("CombinedTraitDataset_ForSam_12.20.20.xlsx")

colnames(Sunflower_traits)
```

```
##  [1] "Year"                                                 "Species"                                             
##  [3] "Pop"                                                  "Number"                                              
##  [5] "PlantID"                                              "Block"                                               
##  [7] "Leaf Chlorophyll Content (SPAD)"                      "Leaf Aarea (µmolCO2 m-2 s-1)"                        
##  [9] "Leaf Amass (nmolCO2 g-1 s-1)"                         "Leaf Conductance (molH2O m-2 s-1)"                   
## [11] "Leaf Ci (µmolCO2 molair-1)"                           "Leaf iWUE"                                           
## [13] "Leaf NightResparea (µmolCO2 m-2 s-1)"                 "Leaf NightRespmass (nmolCO2 g-1 s-1)"                
## [15] "Leaf Area (cm2)"                                      "Leaf Perimeter (cm)"                                 
## [17] "Leaf Circularity"                                     "Leaf Solidity"                                       
## [19] "LMA (g/m2)"                                           "Leaf Fresh Mass(g)"                                  
## [21] "Leaf Dry Mass (g)"                                    "Leaf Water Content (gH20/g dry mass)"                
## [23] "LDMC (g dry mass/g fresh mass)"                       "Leaf Midrib Thickness(mm)"                           
## [25] "Leaf Lamina Thickness(mm)"                            "Leaf Toughness (g/mm2)"                              
## [27] "Leaf Lifespan (100%)"                                 "Leaf N (%)"                                          
## [29] "Leaf D15N"                                            "Leaf PNUE (nmolCO2 gN-1 s-1)"                        
## [31] "Leaf C (%)"                                           "Leaf D13C"                                           
## [33] "Leaf C:N Ratio"                                       "Leaf P (%)"                                          
## [35] "Leaf N:P Ratio"                                       "Leaf Vein Density (mm/mm2)"                          
## [37] "Leaf Tannin Activity (%)"                             "Leaf TrichomeDensity (# per cm2)"                    
## [39] "Leaf Lipid Content (%)"                               "Leaf Ash Content (%)"                                
## [41] "Whole Plant Total Biomass (g)"                        "Whole Plant Belowground Mass Fraction"               
## [43] "Whole Plant Stem Mass Fraction"                       "Whole Plant Leaf Mass Fraction"                      
## [45] "Whole Plant Reproductive Mass Fraction"               "Whole Plant First Bud (Julian day)"                  
## [47] "Whole Plant First Flower (Julian day)"                "Flower Petals Fresh Mass (g)"                        
## [49] "Flower Disc Fresh Mass (g)"                           "Flower Receptacle Fresh Mass (g)"                    
## [51] "Flower Total Fresh Mass (g)"                          "Flower Petals Dry Mass (g)"                          
## [53] "Flower Disc Dry Mass (g)"                             "Flower Receptacle Dry Mass (g)"                      
## [55] "Flower Total Dry Mass (g)"                            "Flower Petal Water Content (g H20 / g dry mass)"     
## [57] "Flower Disc Water Content (g H20 / g dry mass)"       "Flower Receptacle Water Content (g H20 / g dry mass)"
## [59] "Flower Total Water Content (g H20 / g dry mass)"      "Flower Dry Mass Investment Ratio (Petals:Disc)"      
## [61] "Flower Petal Dry Mass Fraction (Petal/Total)"         "Flower Disc Dry Mass Fraction (Disc/Total)"          
## [63] "Flower Receptacle Dry Mass Fraction (Recep/Total)"    "Flower Ray Length (cm)"                              
## [65] "Flower Ray Width (cm)"                                "Flower Disc Diameter (cm)"                           
## [67] "Flower Disc Circumference (cm)"                       "Flower Total Diameter (cm)"                          
## [69] "Flower Total Circumference (cm)"                      "Flower Total Area (cm2)"                             
## [71] "Flower Disc Area (cm2)"                               "Flower Petal Area (cm2)"                             
## [73] "Flower Ray Number"                                    "Flower Petal Density (#/cm disc circumference)"      
## [75] "Flower Area Investment Ratio (Petals:Disc)"           "Flower Petal Area Fraction (Petal/Total)"            
## [77] "Flower Disc Area Fraction (Disc/Total)"
```

```r
#### removing the columns which we don't need ### 

Sunflower_traits <- Sunflower_traits %>% 
  dplyr::select(-c(Year,PlantID,Block,Number))


### now renaming the columns ### 

New_names <- c("Species","Pop","SPAD","L_Aarea","L_Amass","L_Con","L_Ci",
               "L_iWUE","L_NightRespArea","L_NightRespmass","LA",
               "L_Peri","L_Circ","LS","LMA","L_FreshMass","L_DryMass",
               "LWC","LDMC","L_MidribT","L_LaminaT","L_Tough","L_life",
               "LN","LD15C","LPNUE","LC","LD13C","LCN","LP","Leaf_N_P",
               "L_VeinD","L_T_activity","LTD","L_LipidC","L_AshC","WPTB",
               "WPBMF","WPSMF","WPLMF","WPRMF","WPFB","WPFF","FPFM","FDFM",
               "FRFM","FTFM","FPDM","FDDM","FRDM","FTDM","FPWC","FDWC","FRWC",
               "FTWC","P_D","P_T","D_T","R_T","FRL","FRW","FDD","FDC","FTD",
               "FTC","FTA","FDA","FPA","FRN","FPD","FAIR","FPAF","FDAF")


Old_names <- colnames(Sunflower_traits)


#### renaming the names of the dataframe ##### 

Sunflower_traits <- Sunflower_traits %>% rename_at(vars(Old_names), ~ New_names)

### First changing H. agrestis, H. argophyllus to H_agrestis, H_argophyllus 

Sunflower_traits$Species <- gsub(". ","_",Sunflower_traits$Species)

### converting every ".", into NA in the trait columns 

Sunflower_traits[Sunflower_traits == "."] <- NA

### 

total_cells <- prod(dim(Sunflower_traits %>% select(-Species))) 

missing_cells <- sum(is.na(Sunflower_traits %>% select(-Species)))

# calculating percentage of missing values
percentage_missing <- (missing_cells * 100 )/(total_cells)

percentage_missing 
```

```
## [1] 15.13657
```

```r
### VIsualizing the missing data ## by variables  

gg_miss_var(Sunflower_traits)
```

```
## Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please use `guide = "none"` instead.
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
ggsave("Figure S1.svg",dpi=300)
```

```
## Saving 7 x 7 in image
```

```
## Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please use `guide = "none"` instead.
```

```r
##### Converting the Species and Pop column to a factor ###

Sunflower_traits$Species <- factor(Sunflower_traits$Species) 

Sunflower_traits$Pop <- factor(Sunflower_traits$Pop) 

### converting all other columns into a numeric column ##

Sunflower_traits <- Sunflower_traits %>%
  mutate_if(is.character,as.numeric)

### Divide the dataset into a training and test set ###


s <- createDataPartition(y=Sunflower_traits$Species, p=0.70, list = F)
train <- Sunflower_traits[s,]
test <- Sunflower_traits[-s,]

#### removing the Pop column from both train and test putting it in a different dataframe ###

pop_train <- data.frame(Pop = train$Pop)

train <- train %>% dplyr::select(-Pop)

test <- test %>% dplyr::select(-Pop)


### Now impute using random forest ### 
set.seed(1234)
train_imputed <- rfImpute(Species~.,train)
```

```
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:   8.77%  0.00%  0.00%  5.88%  5.88% 23.53%  0.00%  0.00%  0.00%  0.00%  5.88% 23.53%  5.88% 11.76%  0.00% 11.76% 21.74% 11.76% 47.06%  0.00% 16.67% 23.53%  5.88% 11.76%  2.94%  0.00%  0.00% 25.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:   9.16%  0.00%  0.00%  5.88% 11.76% 17.65%  0.00%  0.00%  0.00% 20.00% 11.76% 17.65%  5.88%  5.88%  0.00% 11.76% 21.74%  5.88% 29.41%  0.00% 16.67% 23.53%  5.88% 11.76%  8.82%  0.00%  6.06% 33.33%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  10.33%  5.88%  0.00%  5.88%  5.88% 35.29%  0.00%  0.00%  0.00% 20.00% 17.65% 41.18%  5.88% 11.76%  0.00% 11.76% 13.04%  5.88% 29.41%  0.00% 16.67% 17.65%  5.88%  5.88% 11.76%  5.88%  6.06% 25.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:   9.36%  5.88%  0.00%  5.88%  5.88% 23.53%  0.00%  0.00%  0.00% 20.00%  5.88% 23.53%  5.88%  5.88%  0.00% 17.65% 21.74%  5.88% 35.29%  0.00%  8.33% 17.65%  5.88% 11.76% 11.76%  0.00%  3.03% 33.33%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  10.14%  0.00%  0.00%  5.88%  5.88% 17.65%  0.00%  0.00%  0.00% 10.00% 23.53% 35.29%  5.88%  5.88%  0.00% 17.65% 21.74%  5.88% 41.18%  0.00%  8.33% 23.53%  5.88% 11.76% 11.76%  0.00%  6.06% 25.00%  0.00%
```

```r
set.seed(1234)
test_imputed <- rfImpute(Species~.,test)
```

```
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  20.49%  0.00% 14.29%  7.14%  0.00% 57.14%  0.00% 33.33%  0.00%100.00% 42.86%  0.00% 14.29% 16.67% 28.57% 42.86% 11.11% 33.33% 83.33% 16.67% 40.00% 42.86%  0.00%  0.00%  0.00%  0.00% 14.29%100.00% 14.29%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  22.44%  0.00% 14.29%  7.14% 14.29% 71.43%  0.00% 33.33% 14.29% 66.67% 42.86% 14.29% 28.57% 33.33%  0.00% 28.57% 11.11% 16.67% 83.33% 16.67% 60.00% 71.43%  0.00% 28.57%  0.00%  0.00%  7.14%100.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  20.98%  0.00% 28.57%  0.00% 28.57% 57.14%  0.00% 33.33% 14.29%100.00% 42.86%  0.00% 14.29% 16.67% 14.29% 42.86% 11.11% 33.33% 83.33% 16.67% 40.00% 42.86%  0.00%  0.00%  7.14%  0.00%  7.14%100.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  21.95%  0.00% 14.29%  0.00% 14.29% 71.43%  0.00% 16.67% 14.29% 66.67% 57.14% 14.29% 28.57% 16.67% 14.29% 42.86% 11.11% 33.33% 83.33% 16.67% 60.00% 57.14%  0.00%  0.00%  7.14%  0.00%  7.14%100.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  24.39%  0.00% 14.29%  7.14% 14.29% 85.71%  0.00% 16.67% 14.29%100.00% 57.14%  0.00% 42.86% 16.67%  0.00% 71.43% 11.11% 33.33% 83.33% 16.67% 40.00% 71.43%  0.00%  0.00%  7.14%  0.00% 14.29%100.00%  0.00%
```

```r
###### writing this out ####

write.csv(train_imputed,"train_imputed.csv",row.names = FALSE)

write.csv(test_imputed,"test_imputed.csv",row.names=FALSE)

## adding the pop column to the training data and calling it Sunflower_train###

Sunflower_train <- data.frame(Pop = pop_train$Pop,
                              train_imputed)

### write this dataframe out to analyze variance partitioning #### 

write.csv(Sunflower_train,"Sunflower_train.csv",row.names = FALSE)
```

