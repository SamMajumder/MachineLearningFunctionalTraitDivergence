

```r
####  

rm(list = ls())

packages <- list("here","tidyverse","naniar","ggplot2","randomForest","caret","readxl")


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
Sunflower_traits <- read_excel(here("Datasets and Tables","CombinedTraitDataset_ForSam_12.20.20.xlsx"))


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
#### Missing values in the data

Missing_values <-Sunflower_traits %>%
                 gather(key = "key", value = "val") %>%
                 mutate(is.missing = is.na(val)) %>%
                 group_by(key, is.missing) %>%
                 summarise(num.missing = n()) %>%
                 filter(is.missing==T) %>%
                 select(-is.missing) %>%
                 arrange(desc(num.missing)) 
```

```
## `summarise()` has grouped output by 'key'. You can override using the `.groups` argument.
```

```r
## Now make a plot ## number of missing values for specific traits ###

ggplot(data = Missing_values,
       aes(x=reorder(key,num.missing), y = num.missing, fill = key)) +
       geom_bar(stat = "identity") + 
       labs(x= "Features", y= "Total number of missing values") +
       coord_flip() + 
       theme_bw() + 
       theme(legend.position = "none") + 
       ggtitle("Missing Values in the dataset") +
       theme(text = element_text(size = 10)) 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
### Percent missing for each trait ###

Missing_percent <- Sunflower_traits %>%
                  gather(key = "key", value = "val") %>%
                  mutate(isna = is.na(val)) %>%
                  group_by(key) %>%
                  mutate(total = n()) %>%
                  group_by(key, total, isna) %>%
                  summarise(num.isna = n()) %>%
                  mutate(pct = num.isna / total * 100) %>%
                  mutate(Type = case_when(isna == "FALSE" ~ "Not Missing",
                                          isna == "TRUE" ~ "Missing"))
```

```
## `summarise()` has grouped output by 'key', 'total'. You can override using the `.groups` argument.
```

```r
ggplot(Missing_percent, aes(fill=Type, y=pct, x=key)) + 
  geom_bar(position='stack', stat='identity') +
  labs(x = "Variable", y = "Percent Missing") +
  coord_flip() +
  ggtitle("Percent Missing Value from each trait") +
  theme(text = element_text(size = 10))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
here("Figures")
```

```
## [1] "C:/Users/samba/Documents/Chapter_1_Analysis/Figures"
```

```r
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S1.svg",
       dpi=300)
```

```
## Saving 7 x 7 in image
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

train <- train %>% dplyr::select(-c(Pop))

test <- test %>% dplyr::select(-c(Pop))


### Now impute using random forest ### 
set.seed(1234)
train_imputed <- rfImpute(Species~.,train)
```

```
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  10.53%  0.00%  0.00%  5.88%  0.00% 29.41%  0.00%  6.67%  0.00% 20.00% 17.65% 17.65% 17.65%  0.00%  5.88% 11.76% 30.43% 23.53% 52.94%  0.00% 25.00% 17.65%  0.00% 11.76%  0.00%  0.00%  6.06% 16.67%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  10.33%  0.00%  0.00%  5.88%  0.00% 17.65%  5.88%  6.67%  5.88% 20.00% 11.76% 23.53% 17.65%  0.00%  0.00% 17.65% 26.09% 17.65% 47.06%  5.88% 16.67% 23.53%  0.00% 11.76%  0.00%  0.00%  9.09% 16.67%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  10.33%  0.00%  0.00%  5.88%  5.88% 17.65%  5.88%  6.67%  0.00% 20.00% 11.76% 17.65% 17.65%  0.00%  5.88% 23.53% 30.43% 17.65% 29.41%  5.88%  8.33% 17.65%  0.00% 11.76%  2.94%  0.00% 12.12% 25.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  10.14%  0.00%  0.00%  5.88%  0.00% 11.76%  0.00%  6.67%  0.00% 20.00% 11.76% 17.65% 17.65%  0.00%  0.00% 17.65% 21.74% 17.65% 41.18%  5.88% 16.67% 23.53%  0.00% 11.76%  8.82%  0.00%  9.09% 33.33%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  11.31%  0.00%  5.88%  5.88%  0.00% 17.65%  0.00%  6.67%  0.00% 20.00%  5.88% 23.53% 17.65%  5.88%  0.00% 23.53% 30.43% 17.65% 35.29%  5.88% 16.67% 17.65%  0.00% 11.76%  8.82%  0.00%  9.09% 50.00%  0.00%
```

```r
set.seed(1234)
test_imputed <- rfImpute(Species~.,test)
```

```
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  21.46% 14.29%  0.00%  0.00% 28.57% 71.43%  0.00%  0.00%  0.00% 66.67% 71.43% 42.86% 14.29% 33.33% 14.29% 42.86% 33.33% 33.33% 83.33%  0.00% 40.00% 57.14% 14.29%  0.00%  0.00%  0.00%  0.00% 50.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  21.46%  0.00%  0.00%  0.00% 14.29% 57.14%  0.00% 16.67%  0.00%100.00% 57.14% 14.29% 14.29% 33.33% 14.29% 42.86% 33.33% 33.33% 66.67%  0.00% 60.00% 71.43% 28.57%  0.00%  0.00%  0.00%  0.00%100.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  21.46% 28.57%  0.00%  0.00% 28.57% 57.14%  0.00% 16.67%  0.00% 66.67% 42.86% 28.57% 14.29% 33.33% 28.57% 42.86% 22.22% 33.33% 66.67% 16.67% 60.00% 71.43% 14.29%  0.00%  0.00%  0.00%  0.00% 50.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  23.41% 28.57%  0.00%  0.00% 14.29% 71.43%  0.00% 33.33%  0.00%100.00% 57.14% 28.57% 14.29% 33.33% 14.29% 57.14% 22.22% 33.33% 83.33% 16.67% 60.00% 42.86% 14.29%  0.00%  7.14%  0.00%  0.00% 75.00%  0.00%
## ntree      OOB      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24     25     26     27     28
##   300:  20.98% 28.57%  0.00%  0.00%  0.00% 57.14%  0.00% 16.67%  0.00%100.00% 42.86% 42.86% 14.29% 33.33% 14.29% 42.86% 33.33% 16.67% 66.67% 16.67% 60.00% 57.14% 14.29%  0.00%  7.14%  0.00%  0.00% 50.00%  0.00%
```

```r
###### writing this out ####

here("Datasets and Tables")
```

```
## [1] "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables"
```

```r
write.csv(train_imputed,"C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/train_imputed.csv",
          row.names = FALSE)

write.csv(test_imputed,"C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/test_imputed.csv",
          row.names=FALSE)

## adding the pop column to the training data and calling it Sunflower_train###

Sunflower_train <- data.frame(Pop = pop_train$Pop,
                              train_imputed)

### write this dataframe out to analyze variance partitioning #### 

write.csv(Sunflower_train,"C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Sunflower_train.csv",
          row.names = FALSE)
```

