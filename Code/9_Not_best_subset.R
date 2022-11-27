
rm(list=ls())

library(tidyverse)
library(here)

### Not part of best subset at the Genus Level ###

Gini_genus <- read_csv(here("Datasets and Tables", "Gini_Importance_Genus.csv"))

Gini_Rfe <- read_csv(here("Datasets and Tables","Rfe_Genus_best_subset.csv"))

Not_best_subset_genus <- anti_join(Gini_genus, Gini_Rfe, 
                                   by = c("Features" = "Features"))



#### Not part of the best subset at the Annual Level ###

Gini_annual <- read_csv(here("Datasets and Tables", "Gini_Importance_Annual.csv"))

Annual_Rfe <- read_csv(here("Datasets and Tables","Rfe_Annual_best_subset.csv"))                                                

Not_best_subset_annual <- anti_join(Gini_annual, Annual_Rfe, 
                                   by = c("Features" = "Features"))



###### Not part of the best subset at the Perennial Level ##

Gini_perennial <- read_csv(here("Datasets and Tables", "Gini_Importance_Perennials.csv"))

Perennials_Rfe <- read_csv(here("Datasets and Tables","Rfe_Perennial_best_subset.csv"))

Not_best_subset_perennial <- anti_join(Gini_perennial,Perennials_Rfe,
                                       by = c("Features"="Features"))


###### Not part of best subset at the sutheastern perennial level ##

Gini_southeastern <- read_csv(here("Datasets and Tables", "Gini_Importance_southeastern.csv"))


Southeastern_Rfe <- read_csv(here("Datasets and Tables","Rfe_southeastern_best_subset.csv"))


Not_best_subset_southeastern <- anti_join(Gini_southeastern,Southeastern_Rfe,
                                       by = c("Features"="Features"))


### export these files as csv files ###

write.csv(Not_best_subset_genus,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Not_best_subset_genus.csv",
          row.names = FALSE)


write.csv(Not_best_subset_annual,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Not_best_subset_annual.csv",
          row.names = FALSE)


write.csv(Not_best_subset_perennial,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Not_best_subset_perennial.csv",
          row.names = FALSE)



write.csv(Not_best_subset_southeastern,
          "C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Not_best_subset_southeastern.csv",
          row.names = FALSE)
















