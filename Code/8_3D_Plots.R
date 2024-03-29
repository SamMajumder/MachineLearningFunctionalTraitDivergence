rm(list = ls())

packages <- list("tidyverse","plotly","here","htmlwidgets")


lapply(packages, require,character.only=T)


### 3D PLOTS ### GENUS ###

### reading in data #### 

train_imputed <- read_csv(here("Datasets and Tables","train_imputed.csv"))


Imp_features <- read.csv(here("Datasets and Tables",
                              "Rfe_Genus_best_subset.csv"))

### only keeping the strongly divergent traits 
train_rfe <- train_imputed %>% 
                      dplyr::select(Species,Imp_features$Features)


### 3D PLOTLY PLOTS ###

plot <- plot_ly(train_rfe,x= ~L_Circ,y= ~LA,z= ~WPFF, color = ~Species
) %>%
  add_markers() %>%
  layout(title = "Divergent traits at the Genus Level",
    scene = list(xaxis = list(title = 'Leaf Circularity'),
                 yaxis = list(title = 'Leaf Area (cm<sup>2</sup>)'),
                 zaxis = list(title = 'Whole Plant First Flower (Julian Day)'))
)

plot


saveWidget(plot,"Genus3d.html")

saveRDS(plot,"Genus_3d.RDS")


##### 3D PLOTS ### ANNUALS ###

Imp_features_Annuals <- read.csv(here("Datasets and Tables",
                                "Rfe_Annual_best_subset.csv")) 


###### FILTER OUT BY ANNUAL SPECIES ##### 

Annuals <- c("H_praeco_ssp_runyonii","H_debili_ssp_tardiflorus",
             "H_neglectus","H_petiolari_ssp_petiolaris",
             "H_niveu_ssp_tephrodes","H_annuus","H_argophyllus")



##### keeping only the annuals  ### 
train_imputed_annuals <- train_imputed %>% filter(Species %in% Annuals)


### only keeping the strongly divergent traits
train_rfe_Annuals <- train_imputed_annuals %>% 
                        dplyr::select(Species,Imp_features_Annuals$Features)

table(train_rfe_Annuals$Species)


plot_annuals <- plot_ly(train_rfe_Annuals,x= ~LTD,y= ~LA,z= ~FDFM, color = ~Species
) %>%
  add_markers() %>%
  layout(title = "Divergent traits at the Annual Level",
    scene = list(xaxis = list(title = 'Leaf Trichome Density'),
                 yaxis = list(title = 'Leaf Area (cm<sup>2</sup>)'),
                 zaxis = list(title = 'Flower disc fresh mass (g)'))
  )


plot_annuals


saveWidget(plot_annuals,"Annual3d.html")


saveRDS(plot_annuals,"Annual_3d.RDS")

####### 3D PLOTs #### LARGE PERENNIALS
###### #############

Imp_features_Perennials <- read.csv(here("Datasets and Tables",
                                    "Rfe_Perennial_best_subset.csv")) 

###### Only keep the perennial species ##### 

Perennials <- c("H_salicifolius","H_maximiliani","H_giganteus",
                "H_verticillatus","H_grosseserratus","H_divaricatus",
                "H_microcephalus","H_cusickii")



##### keeping only the annuals  ### 
train_imputed_perennials <- train_imputed %>% filter(Species %in% Perennials)

### only keeping the most strongly divergent traits 
train_boruta_Perennials <- train_imputed_perennials %>% 
  dplyr::select(Species,Imp_features_Perennials$Feature)

table(train_boruta_Perennials$Species)



plot_perennials <- plot_ly(train_boruta_Perennials,x= ~LD13C,y= ~L_Circ,z= ~LA, color = ~Species
) %>%
  add_markers() %>%
  layout(title = "Divergent traits at the Perennial Level",
    scene = list(xaxis = list(title = 'LD13C'),
                 yaxis = list(title = 'Leaf Circularity'),
                 zaxis = list(title = 'Leaf Area'))
  )

plot_perennials  

saveWidget(plot_perennials,"Perennials3d.html") 

saveRDS(plot_perennials,"Perennials_3d.RDS")

############
### 3D PLOTS ## SOUTHEASTERN PERENNIALS ###
###### 

Imp_features_southeastern <- read.csv(here("Datasets and Tables",
                                      "Rfe_southeastern_best_subset.csv"))

###### FILTER OUT BY SOUTHEASTERN PERENNUIALS SPECIES ##### 

Southeastern_perennials <- c("H_carnosus","H_atrorubens","H_radula",
                             "H_silphioides","H_floridanus","H_heterophyllus",
                             "H_longifolius","H_angustifolius")


##### keeping only the Southeastern perennials  ### 

train_imputed_southeastern <- train_imputed %>% filter(Species %in% Southeastern_perennials)

### only keeping the most strongly divergent traits 
train_rfe_southeastern <- train_imputed_southeastern %>% 
                                   dplyr::select(Species,Imp_features_southeastern$Features)


table(train_rfe_southeastern$Species)


plot_southeastern <- plot_ly(train_rfe_southeastern,x= ~L_Circ,y= ~LA,z= ~LDMC, color = ~Species
) %>%
  add_markers() %>%
  layout(title = "Divergent traits at the Southeastern perennial Level",
    scene = list(xaxis = list(title = 'Leaf Circularity'),
                 yaxis = list(title = 'Leaf Area cm<sup>2</sup>'),
                 zaxis = list(title = 'LDMC (g dry mass/g fresh mass)'))
  )


plot_southeastern

saveWidget(plot_southeastern,"Southeastern3d.html") 

saveRDS(plot_southeastern,"Southeastern_3d.RDS")





