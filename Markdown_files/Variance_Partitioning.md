

```r
rm(list = ls())

packages <- list("tidyverse","ggplot2","lme4","svglite")


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
```

```r
Sunflower_train <- read.csv("Sunflower_train.csv")

### converting the population and the species column to factor 

Sunflower_train <- Sunflower_train %>%
                   mutate_if(is.character,as.factor)


LTD_mixed_model <- lmer(LTD~ (1|Species/Pop),data = Sunflower_train)
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
L_Circ_mixed_model <- lmer(L_Circ ~ (1|Species/Pop),data = Sunflower_train)

L_Area_mixed_model <- lmer(LA~(1|Species/Pop),data = Sunflower_train)


summary(LTD_mixed_model)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: LTD ~ (1 | Species/Pop)
##    Data: Sunflower_train
## 
## REML criterion at convergence: 5567.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -7.2116 -0.2975 -0.0662  0.2118  9.2081 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  Pop:Species (Intercept)     0      0.00  
##  Species     (Intercept) 16022    126.58  
##  Residual                 2374     48.72  
## Number of obs: 513, groups:  Pop:Species, 83; Species, 28
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)    82.90      24.02    3.45
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

```r
all_var_LTD <- 0+16274+2276

Pop_Ltd_var <- (0/all_var_LTD) * 100

Species_Ltd_var <- (16274/all_var_LTD) * 100


summary(L_Circ_mixed_model)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: L_Circ ~ (1 | Species/Pop)
##    Data: Sunflower_train
## 
## REML criterion at convergence: -1293.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.3139 -0.5296 -0.0879  0.4885  5.0916 
## 
## Random effects:
##  Groups      Name        Variance  Std.Dev.
##  Pop:Species (Intercept) 0.0004988 0.02233 
##  Species     (Intercept) 0.0212321 0.14571 
##  Residual                0.0033710 0.05806 
## Number of obs: 513, groups:  Pop:Species, 83; Species, 28
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  0.30660    0.02778   11.04
```

```r
all_var_L_Circ <- 0.0007041+0.0214819+0.0030185

Pop_L_Circ_var <- (0.0007041/all_var_L_Circ) * 100

Species_L_Circ_var <- (0.0214819/all_var_L_Circ) * 100


summary(L_Area_mixed_model)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: LA ~ (1 | Species/Pop)
##    Data: Sunflower_train
## 
## REML criterion at convergence: 4647
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.4387 -0.3664 -0.0546  0.3883  6.8084 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  Pop:Species (Intercept)  50.41    7.10   
##  Species     (Intercept) 816.17   28.57   
##  Residual                392.62   19.81   
## Number of obs: 513, groups:  Pop:Species, 83; Species, 28
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   46.078      5.532    8.33
```

```r
all_var_L_Area <- 98.53 + 772.62 + 376.84

Pop_L_Area_var <- (98.53/all_var_L_Area) * 100

Species_L_Area_var <- (772.62/all_var_L_Area) * 100


Variance_table <- data.frame(Traits = c("Leaf trichome density","Leaf trichome density",
                                        "Leaf Circularity","Leaf Circularity",
                                        "Leaf Area","Leaf Area"),
                             Groups = c("Species","Pop","Species","Pop",
                                        "Species","Pop"))
                            


Variances <- rbind(Species_Ltd_var,Pop_Ltd_var,
                   Species_L_Circ_var,Pop_L_Circ_var,
                   Species_L_Area_var,Pop_L_Area_var)

Variance_table <- cbind(Variance_table,Variances)


# Plot stack bar chart

ggplot(Variance_table, aes(fill=Groups, y=Variances, x=Traits)) + 
  geom_bar(position='stack', stat='identity') +
  labs(x = "Traits", y = "Estimated relative Variance (%)") +
  ggtitle("Estimated relative variation in percent") +
  theme(text = element_text(size = 10))  
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
ggsave("Figure S2.svg",dpi = 300)
```

```
## Saving 7 x 7 in image
```

