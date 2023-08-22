
rm(list = ls())

options(scipen=999)

packages <- list("tidyverse","ggplot2","here","lme4","svglite")


lapply(packages, require,character.only=T)

Sunflower_train <- read_csv(here("Datasets and Tables","Sunflower_train.csv"))

### converting the population and the species column to factor 

Sunflower_train <- Sunflower_train %>%
                   mutate_if(is.character,as.factor)



### Isolating the Species and the Population columns as separte vectors

Species <- Sunflower_train$Species

Pop <- Sunflower_train$Pop




#### A function to fit a mixed model on the entire dataset and
###  calculate the variance partitioning scores 

mixed_model <- function(x){
  model <- lmer(x~(1|Species/Pop))
  vc <- data.frame(VarCorr(model))
  variance <- vc %>% select(grp,vcov)
  Total_variance <- sum(variance$vcov)
  Population <- (variance[1,2]/Total_variance) * 100
  Species <- (variance[2,2]/Total_variance) * 100
  Residual <- (variance[3,2]/Total_variance) * 100
  results <- rbind(Population,Species,Residual)
  results <- data.frame(Variance = results)
  return(results)
}

##### Applying this function on the whole dataset 
### the result returns a list 

Variance_Species_pop <- Sunflower_train[,-c(1:2)] %>% 
                        map(mixed_model)

### converting this list into a dataframe

Variance_Species_pop <- data.frame(Variances = 
                                   do.call(rbind,Variance_Species_pop))
                       

#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ##

Traits <- rep(colnames(Sunflower_train[-c(1:2)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=71)

### Putting the variance values, trait names and levels in one dataframe ## 

Variance_table_1 <- data.frame(Variance = Variance_Species_pop$Variance,
                               Traits = Traits,
                               Levels = Levels)

### Making a plot to view the variance partitioning ###

ggplot(Variance_table_1, aes(fill=Levels, y=Variance, x=Traits)) + 
  geom_bar(position='stack', stat='identity') +
  labs(x = "Traits", y = "Estimated relative Variance (%)") +
  ggtitle("Estimated relative variation in percent at the genus level") +
  theme(axis.text.x = element_text(size = 10,angle = 90,vjust = 0.5,hjust = 1)) 
  
 

here("Figures")

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S2.svg",
       dpi=300)



Variance_table_1 <- Variance_table_1 %>% pivot_wider(names_from = Levels,
                                                     values_from = Variance)



## Arranging the Species variance values in a descending order ##

Variance_table_1 <- Variance_table_1 %>% arrange(desc(Species))


here("Dataset and Tables")

### exporting the dataframe containing the variance partitioning values at the genus level

write.csv(Variance_table_1,
          "C:/Users/samba/Documents/MachineLearningFunctionalTraitDivergence/Dataset and Tables/Variance_partitioning_Genus.csv",
          row.names = FALSE)




#################### 
####### Exploring variance partitioning values at the clade level ###
###############

### PERENNIAL #### 

###### Perennial Species Names ##### 

Perennials <- c("H_salicifolius","H_maximiliani","H_giganteus",
                "H_verticillatus","H_grosseserratus","H_divaricatus",
                "H_microcephalus","H_cusickii")


##### Subset the dataframe to only contain the perennial species ## 

Perennial_Sunflowers <- Sunflower_train %>% filter(Species %in% Perennials)

##### 

### Isolating the Species and the Population columns as separate vectors ##
## This must be done before applying the function ###

Species <- Perennial_Sunflowers$Species

Pop <- Perennial_Sunflowers$Pop

#### Calculate Variance partitioning ###

Variance_Species_pop_Perennials <- Perennial_Sunflowers[,-c(1:2)] %>% 
                        map(mixed_model)

### converting this list into a dataframe

Variance_Species_pop_Perennials <- data.frame(Variances = 
                                     do.call(rbind,
                                             Variance_Species_pop_Perennials))


#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ## Same as Genus level 


Traits <- rep(colnames(Sunflower_train[-c(1:2)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=71)

### Putting the variance values, trait names and levels in one dataframe ## 

Variance_table_2 <- data.frame(Variance = Variance_Species_pop_Perennials$Variance,
                               Traits = Traits,
                               Levels = Levels)

### Making a plot to view the variance partitioning ###

ggplot(Variance_table_2, aes(fill=Levels, y=Variance, x=Traits)) + 
  geom_bar(position='stack', stat='identity') +
  labs(x = "Traits", y = "Estimated relative Variance (%)") +
  ggtitle("Estimated relative variation in percent at the Perennial level") +
  theme(axis.text.x = element_text(size = 10,angle = 90,vjust = 0.5,hjust = 1)) 



here("Figures")

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S3.svg",
       dpi=300)



Variance_table_2 <- Variance_table_2 %>% pivot_wider(names_from = Levels,
                                                     values_from = Variance)



## Arranging the Species variance values in a descending order ##

Variance_table_2 <- Variance_table_2 %>% arrange(desc(Species))


here("Dataset and Tables")

### exporting the dataframe containing the variance partitioning values at the genus level

write.csv(Variance_table_2,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Variance_partitioning_perennial.csv",
          row.names = FALSE)




#### ANNUAL ####

### Annual species names  ###


###### FILTER OUT BY ANNUAL SPECIES ##### 

Annuals <- c("H_praeco_ssp_runyonii","H_debili_ssp_tardiflorus",
             "H_neglectus","H_petiolari_ssp_petiolaris",
             "H_niveu_ssp_tephrodes","H_annuus","H_argophyllus")



##### keeping only the annuals  ### 
Annual_Sunflowers <- Sunflower_train %>% filter(Species %in% Annuals)


### Isolating the Species and the Population columns as separate vectors ##
## This must be done before applying the function ###

Species <- Annual_Sunflowers$Species

Pop <- Annual_Sunflowers$Pop

#### Calculate Variance partitioning ###

Variance_Species_pop_Annuals <- Annual_Sunflowers[,-c(1:2)] %>% 
                                map(mixed_model)

### converting this list into a dataframe

Variance_Species_pop_Annuuals <- data.frame(Variances = 
                                                do.call(rbind,
                                                        Variance_Species_pop_Annuals))


#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ## Same as Genus level 

Traits <- rep(colnames(Sunflower_train[-c(1:2)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=71)

### Putting the variance values, trait names and levels in one dataframe ## 

Variance_table_3 <- data.frame(Variance = Variance_Species_pop_Annuuals$Variance,
                               Traits = Traits,
                               Levels = Levels)

### Making a plot to view the variance partitioning ###

ggplot(Variance_table_3, aes(fill=Levels, y=Variance, x=Traits)) + 
  geom_bar(position='stack', stat='identity') +
  labs(x = "Traits", y = "Estimated relative Variance (%)") +
  ggtitle("Estimated relative variation in percent at the Annual level") +
  theme(axis.text.x = element_text(size = 10,angle = 90,vjust = 0.5,hjust = 1)) 



here("Figures")

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S4.svg",
       dpi=300)



Variance_table_3 <- Variance_table_3 %>% pivot_wider(names_from = Levels,
                                                     values_from = Variance)



## Arranging the Species variance values in a descending order ##

Variance_table_3 <- Variance_table_3 %>% arrange(desc(Species))


here("Dataset and Tables")

### exporting the dataframe containing the variance partitioning values at the genus level

write.csv(Variance_table_3,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Variance_partitioning_Annual.csv",
          row.names = FALSE)




####### SOUTHEASTERN PERENNIAL ###

### Southeastern perennial species names ### 

Southeastern_perennials <- c("H_carnosus","H_atrorubens","H_radula",
                             "H_silphioides","H_floridanus","H_heterophyllus",
                             "H_longifolius","H_angustifolius")


##### keeping only the Southeastern perennials  ### 
Southeastern_perennial_sunflowers <- Sunflower_train %>% 
                                     filter(Species %in% 
                                              Southeastern_perennials)


### Isolating the Species and the Population columns as separate vectors ##
## This must be done before applying the function ###

Species <- Southeastern_perennial_sunflowers$Species

Pop <- Southeastern_perennial_sunflowers$Pop

#### Calculate Variance partitioning ###

Variance_Species_pop_southeastern_perennials <- Southeastern_perennial_sunflowers[,-c(1:2)] %>% 
                                                map(mixed_model)

### converting this list into a dataframe

Variance_Species_pop_southeastern_perennials <- data.frame(Variances = 
                                              do.call(rbind,
                                                      Variance_Species_pop_southeastern_perennials))


#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ## Same as Genus level 

Traits <- rep(colnames(Sunflower_train[-c(1:2)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=71)

### Putting the variance values, trait names and levels in one dataframe ## 

Variance_table_4 <- data.frame(Variance = Variance_Species_pop_southeastern_perennials$Variance,
                               Traits = Traits,
                               Levels = Levels)

### Making a plot to view the variance partitioning ###

ggplot(Variance_table_4, aes(fill=Levels, y=Variance, x=Traits)) + 
  geom_bar(position='stack', stat='identity') +
  labs(x = "Traits", y = "Estimated relative Variance (%)") +
  ggtitle("Estimated relative variation in percent at the southeastern perennial level") +
  theme(axis.text.x = element_text(size = 10,angle = 90,vjust = 0.5,hjust = 1)) 



here("Figures")

ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S5.svg",
       dpi=300)



Variance_table_4 <- Variance_table_4 %>% pivot_wider(names_from = Levels,
                                                     values_from = Variance)



## Arranging the Species variance values in a descending order ##

Variance_table_4 <- Variance_table_4 %>% arrange(desc(Species))


here("Dataset and Tables")

### exporting the dataframe containing the variance partitioning values at the genus level

write.csv(Variance_table_4,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Variance_partitioning_southeastern_perennials.csv",
          row.names = FALSE)





