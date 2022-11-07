

```r
rm(list = ls())

options(scipen=999)

packages <- list("tidyverse","ggplot2","here","lme4","svglite")


lapply(packages, require,character.only=T)
```

```
## Loading required package: lme4
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

```
## Loading required package: svglite
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
```

```r
Sunflower_train <- read_csv(here("Datasets and Tables","Sunflower_train.csv"))
```

```
## Rows: 513 Columns: 73
```

```
## ── Column specification ──────────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): Pop, Species
## dbl (71): SPAD, L_Aarea, L_Amass, L_Con, L_Ci, L_iWUE, L_NightRespArea, L_NightRespmass, LA, L_Peri, L_Circ, LS, LMA, L_FreshM...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
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

Variance_Species_pop <- Sunflower_train[,-c(1:3)] %>% 
                        map(mixed_model)
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

```r
### converting this list into a dataframe

Variance_Species_pop <- data.frame(Variances = 
                                   do.call(rbind,Variance_Species_pop))
                       

#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ##

Traits <- rep(colnames(Sunflower_train[-c(1:3)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=70)

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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
here("Figures")
```

```
## [1] "C:/Users/samba/Documents/Chapter_1_Analysis/Figures"
```

```r
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S2.svg",
       dpi=300)
```

```
## Saving 7 x 7 in image
```

```r
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

Variance_Species_pop_Perennials <- Perennial_Sunflowers[,-c(1:3)] %>% 
                        map(mixed_model)
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

```r
### converting this list into a dataframe

Variance_Species_pop_Perennials <- data.frame(Variances = 
                                     do.call(rbind,
                                             Variance_Species_pop_Perennials))


#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ## Same as Genus level 


Traits <- rep(colnames(Sunflower_train[-c(1:3)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=70)

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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
here("Figures")
```

```
## [1] "C:/Users/samba/Documents/Chapter_1_Analysis/Figures"
```

```r
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S3.svg",
       dpi=300)
```

```
## Saving 7 x 7 in image
```

```r
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

Variance_Species_pop_Annuals <- Annual_Sunflowers[,-c(1:3)] %>% 
                                map(mixed_model)
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| =
## 0.00289434 (tol = 0.002, component 1)
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.0045198
## (tol = 0.002, component 1)
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
### converting this list into a dataframe

Variance_Species_pop_Annuuals <- data.frame(Variances = 
                                                do.call(rbind,
                                                        Variance_Species_pop_Annuals))


#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ## Same as Genus level 

Traits <- rep(colnames(Sunflower_train[-c(1:3)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=70)

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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
here("Figures")
```

```
## [1] "C:/Users/samba/Documents/Chapter_1_Analysis/Figures"
```

```r
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S4.svg",
       dpi=300)
```

```
## Saving 7 x 7 in image
```

```r
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

Variance_Species_pop_southeastern_perennials <- Southeastern_perennial_sunflowers[,-c(1:3)] %>% 
                                                map(mixed_model)
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

```r
### converting this list into a dataframe

Variance_Species_pop_southeastern_perennials <- data.frame(Variances = 
                                              do.call(rbind,
                                                      Variance_Species_pop_Annuals))


#### extracting all the trait names in this dataset ##
## and repeating each name 3 times ## Same as Genus level 

Traits <- rep(colnames(Sunflower_train[-c(1:3)]),each=3)

### These are the groups for which the variances were computed ###

Levels <- rep(c("Population","Species","Residuals"),times=70)

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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
here("Figures")
```

```
## [1] "C:/Users/samba/Documents/Chapter_1_Analysis/Figures"
```

```r
ggsave("C:/Users/samba/Documents/Chapter_1_Analysis/Figures/Figure S5.svg",
       dpi=300)
```

```
## Saving 7 x 7 in image
```

