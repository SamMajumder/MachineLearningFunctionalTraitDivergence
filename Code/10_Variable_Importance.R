
rm(list = ls())

packages <- c("here","tidyverse","RColorBrewer")

lapply(packages, require, character.only =T)

########### GINI IMPURITY (Ranking all traits) ###
###### Loading the Gini Impurity data ###
### Genus, Large Perennials, southeastern perennials and annuals ###

Gini_Genus <- read.csv(here("Datasets and Tables",
                            "Gini_Importance_Genus.csv")) %>% 
                  mutate(`Phylogenetic Level` = "Genus") %>%
                  mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))

Gini_Perennial <- read.csv(here("Datasets and Tables",
                            "Gini_Importance_Perennials.csv")) %>% 
                          mutate(`Phylogenetic Level` = "Large Perennial") %>%
              mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))

Gini_Annual <- read.csv(here("Datasets and Tables",
                                "Gini_Importance_Annual.csv")) %>% 
                      mutate(`Phylogenetic Level` = "Annual") %>%
              mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))

Gini_southeastern_perennial <- read.csv(here("Datasets and Tables",
                             "Gini_Importance_southeastern.csv")) %>% 
                      mutate(`Phylogenetic Level` = "Southeastern") %>%
             mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))



####
#### Making the importance plots ####
# GENUS ### 

Gini_Genus$Trait_type <- factor(Gini_Genus$Trait_type, 
                          levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = Gini_Genus,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



### LARGE PERENNIAL ###

Gini_Perennial$Trait_type <- factor(Gini_Perennial$Trait_type, 
                                levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = Gini_Perennial,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



### ANNUAL ###

Gini_Annual$Trait_type <- factor(Gini_Annual$Trait_type, 
                                    levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = Gini_Annual,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))


### SOUTHEASTERN PERENNIAL ##


Gini_southeastern_perennial$Trait_type <- factor(Gini_southeastern_perennial$Trait_type, 
                                 levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = Gini_southeastern_perennial,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



Gini_Importance <- rbind(Gini_Genus,Gini_Perennial,Gini_Annual,
                         Gini_southeastern_perennial) %>% 
                                               mutate(Groups = 'All')

###################
### NOW MAKING THE SAME VARIABLE IMPORTANCE PLOTS USING THE OPTIMAL SUBSET OF TRAITS ##
#####################

###### Loading the RFE data ###
### Genus, Large Perennials, southeastern perennials and annuals ###

RFE_Genus <- read.csv(here("Datasets and Tables",
                            "Rfe_Genus_best_subset.csv")) %>% 
  mutate(`Phylogenetic Level` = "Genus") %>%
  mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))

RFE_Perennial <- read.csv(here("Datasets and Tables",
                                "Rfe_Perennial_best_subset.csv")) %>% 
  mutate(`Phylogenetic Level` = "Large Perennial") %>%
  mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))

RFE_Annual <- read.csv(here("Datasets and Tables",
                             "Rfe_Annual_best_subset.csv")) %>% 
  mutate(`Phylogenetic Level` = "Annual") %>%
  mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))

RFE_southeastern_perennial <- read.csv(here("Datasets and Tables",
                                             "Rfe_southeastern_best_subset.csv")) %>% 
                             mutate(`Phylogenetic Level` = "Southeastern") %>%
              mutate(Trait_type = case_when(str_detect(Features,"^L") ~ "Leaf",
                                str_detect(Features, "SPAD") ~ "Leaf",
                                str_detect(Features, "^W") ~ "Whole Plant",
                                str_detect(Features, "^F") ~ "Flower",
                                str_detect(Features, "P_D") ~ "Flower",
                                str_detect(Features, "P_T") ~ "Flower",
                                str_detect(Features, "D_T") ~ "Flower",
                                str_detect(Features, "R_T") ~ "Flower"))



####
#### Making the importance plots ####
# GENUS ### 

RFE_Genus$Trait_type <- factor(RFE_Genus$Trait_type, 
                                levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = RFE_Genus,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



### LARGE PERENNIAL ###

RFE_Perennial$Trait_type <- factor(RFE_Perennial$Trait_type, 
                                    levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = RFE_Perennial,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



### ANNUAL ###

RFE_Annual$Trait_type <- factor(RFE_Annual$Trait_type, 
                                 levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = RFE_Annual,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))


### SOUTHEASTERN PERENNIAL ##


RFE_southeastern_perennial$Trait_type <- factor(RFE_southeastern_perennial$Trait_type, 
                                                 levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = RFE_southeastern_perennial,
       aes(x=reorder(Features,Overall), 
           y = Overall, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



RFE_Importance <- rbind(RFE_Genus,RFE_Perennial,RFE_Annual,
                         RFE_southeastern_perennial) %>% 
                                      mutate(Groups = 'Optimal Subset')



################
### MOST DIVERGENT TRAITS ###
################ 

###################
### NOW MAKING THE SAME VARIABLE IMPORTANCE PLOTS FOR THE MOST DIVERGENT TRAITS ##
#####################

###### Loading the BORUTA data ###
### Genus, Large Perennials, southeastern perennials and annuals ###

Boruta_Genus <- read.csv(here("Datasets and Tables",
                           "Boruta_Genus.csv")) %>% 
  mutate(`Phylogenetic Level` = "Genus") %>%
  mutate(Trait_type = case_when(str_detect(Feature,"^L") ~ "Leaf",
                                str_detect(Feature, "SPAD") ~ "Leaf",
                                str_detect(Feature, "^W") ~ "Whole Plant",
                                str_detect(Feature, "^F") ~ "Flower",
                                str_detect(Feature, "P_D") ~ "Flower",
                                str_detect(Feature, "P_T") ~ "Flower",
                                str_detect(Feature, "D_T") ~ "Flower",
                                str_detect(Feature, "R_T") ~ "Flower"))

Boruta_Perennial <- read.csv(here("Datasets and Tables",
                               "Boruta_Perennials.csv")) %>% 
  mutate(`Phylogenetic Level` = "Large Perennial") %>%
  mutate(Trait_type = case_when(str_detect(Feature,"^L") ~ "Leaf",
                                str_detect(Feature, "SPAD") ~ "Leaf",
                                str_detect(Feature, "^W") ~ "Whole Plant",
                                str_detect(Feature, "^F") ~ "Flower",
                                str_detect(Feature, "P_D") ~ "Flower",
                                str_detect(Feature, "P_T") ~ "Flower",
                                str_detect(Feature, "D_T") ~ "Flower",
                                str_detect(Feature, "R_T") ~ "Flower"))

Boruta_Annual <- read.csv(here("Datasets and Tables",
                            "Boruta_Annual.csv")) %>% 
  mutate(`Phylogenetic Level` = "Annual") %>%
  mutate(Trait_type = case_when(str_detect(Feature,"^L") ~ "Leaf",
                                str_detect(Feature, "SPAD") ~ "Leaf",
                                str_detect(Feature, "^W") ~ "Whole Plant",
                                str_detect(Feature, "^F") ~ "Flower",
                                str_detect(Feature, "P_D") ~ "Flower",
                                str_detect(Feature, "P_T") ~ "Flower",
                                str_detect(Feature, "D_T") ~ "Flower",
                                str_detect(Feature, "R_T") ~ "Flower"))

Boruta_southeastern_perennial <- read.csv(here("Datasets and Tables",
                                            "Boruta_southeastern_perennials.csv")) %>% 
  mutate(`Phylogenetic Level` = "Southeastern") %>%
  mutate(Trait_type = case_when(str_detect(Feature,"^L") ~ "Leaf",
                                str_detect(Feature, "SPAD") ~ "Leaf",
                                str_detect(Feature, "^W") ~ "Whole Plant",
                                str_detect(Feature, "^F") ~ "Flower",
                                str_detect(Feature, "P_D") ~ "Flower",
                                str_detect(Feature, "P_T") ~ "Flower",
                                str_detect(Feature, "D_T") ~ "Flower",
                                str_detect(Feature, "R_T") ~ "Flower"))



####
#### Making the importance plots ####
# GENUS ### 

Boruta_Genus$Trait_type <- factor(Boruta_Genus$Trait_type, 
                               levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 


colnames(Boruta_Genus)

ggplot(data = Boruta_Genus,
       aes(x=reorder(Feature,meanImp), 
           y = meanImp, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



### LARGE PERENNIAL ###

Boruta_Perennial$Trait_type <- factor(Boruta_Perennial$Trait_type, 
                                   levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = Boruta_Perennial,
       aes(x=reorder(Feature,meanImp), 
           y = meanImp, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



### ANNUAL ###

Boruta_Annual$Trait_type <- factor(Boruta_Annual$Trait_type, 
                                levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = Boruta_Annual,
       aes(x=reorder(Feature,meanImp), 
           y = meanImp, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))


### SOUTHEASTERN PERENNIAL ##


Boruta_southeastern_perennial$Trait_type <- factor(Boruta_southeastern_perennial$Trait_type, 
                                                levels = c("Leaf","Whole Plant","Flower"))

#### Before we create the plot let us extract some colorblind friendly colors ###
## from RColorBrewer ###

display.brewer.all(colorblindFriendly = T) ### color friendly palettes

display.brewer.pal(8, 'Set2') ## displaying the colors


brewer.pal(8, 'Set2') ## displaying the hex codes 

ggplot(data = Boruta_southeastern_perennial,
       aes(x=reorder(Feature,meanImp), 
           y = meanImp, 
           fill = Trait_type)) +
  geom_bar(stat = "identity",
           color ="black") + 
  scale_fill_manual(values = c("#A6D854",
                               "#E5C494",
                               "#FFD92F"),
                    name= 'Trait type') +
  labs(x= "Traits",
       y= "Variable Importance (Mean Decrease of Gini Impurity)") +
  coord_flip() + 
  theme_bw() + theme(legend.position = c(0.78, 0.15),
                     legend.background = element_rect(fill = "white", 
                                                      color = "black")) +
  theme(text = element_text(size = 10))



Boruta_Importance <- rbind(Boruta_Genus,Boruta_Perennial,Boruta_Annual,
                        Boruta_southeastern_perennial) %>% 
  mutate(Groups = 'Most Divergent')




######### Exporting the files containing the combined GINI, RFE and Boruta ##

saveRDS(Gini_Importance,"Gini_Importance_combined.RDS")

saveRDS(RFE_Importance,"Optimal_subset.RDS")

saveRDS(Boruta_Importance,"Most_Divergent.RDS")






