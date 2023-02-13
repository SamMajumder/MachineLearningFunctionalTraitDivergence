
####  

rm(list = ls())

packages <- list("here","tidyverse","naniar","ggplot2","randomForest","caret","readxl")


lapply(packages, require,character.only=T)

Sunflower_traits <- read_excel(here("Datasets and Tables","CombinedTraitDataset_ForSam_12.20.20.xlsx"))


#### removing the columns which we don't need ### 

Sunflower_traits <- Sunflower_traits %>% 
  dplyr::select(-c(Year,PlantID,Block,Number))


### now renaming the columns ### 

New_names <- c("Species","Pop","SPAD","L_Aarea","L_Amass","L_Con","L_Ci",
               "L_iWUE","L_NightRespArea","L_NightRespmass","LA",
               "L_Peri","L_Circ","LS","LMA","L_FreshMass","L_DryMass",
               "LWC","LDMC","L_MidribT","L_LaminaT","L_Tough","L_life",
               "LN","LD15N","LPNUE","LC","LD13C","LCN","LP","Leaf_N_P",
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

#### Missing values in the data 
## #### Missing values in the data
## credit for this workflow goes to James Laufer's blog ## 
## https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html#:~:text=You%20can%20use%20the%20gather,where%20the%20value%20is%20missing.


Missing_values <-Sunflower_traits %>%
                 gather(key = "key", value = "val") %>%
                 mutate(is.missing = is.na(val)) %>%
                 group_by(key, is.missing) %>%
                 summarise(num.missing = n()) %>%
                 filter(is.missing==T) %>%
                 select(-is.missing) %>%
                 arrange(desc(num.missing)) 


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
  


ggplot(Missing_percent, aes(fill=Type, y=pct, x=key)) + 
  geom_bar(position='stack', stat='identity') +
  labs(x = "Variable", y = "Percent Missing") +
  coord_flip() +
  ggtitle("Percent Missing Value from each trait") +
  theme(text = element_text(size = 10))


here("Figures")

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S1.svg",
       dpi=300)


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

set.seed(1234)
test_imputed <- rfImpute(Species~.,test)

###### writing this out ####

here("Datasets and Tables")

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

