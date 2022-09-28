
####  

rm(list = ls())

packages <- list("tidyverse","naniar","ggplot2","randomForest","caret","readxl")


lapply(packages, require,character.only=T)


Sunflower_traits <- read_excel("CombinedTraitDataset_ForSam_12.20.20.xlsx")

colnames(Sunflower_traits)


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

### VIsualizing the missing data ## by variables  

gg_miss_var(Sunflower_traits)

ggsave("Figure S1.svg",dpi=300)


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

set.seed(1234)
test_imputed <- rfImpute(Species~.,test)

###### writing this out ####

write.csv(train_imputed,"train_imputed.csv",row.names = FALSE)

write.csv(test_imputed,"test_imputed.csv",row.names=FALSE)

## adding the pop column to the training data and calling it Sunflower_train###

Sunflower_train <- data.frame(Pop = pop_train$Pop,
                              train_imputed)

### write this dataframe out to analyze variance partitioning #### 

write.csv(Sunflower_train,"Sunflower_train.csv",row.names = FALSE)

