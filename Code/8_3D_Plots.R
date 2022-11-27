rm(list = ls())

packages <- list("tidyverse","plotly","here","htmlwidgets")


lapply(packages, require,character.only=T)


### 3D PLOTS ### GENUS ###

### reading in data #### 

train_imputed <- read_csv(here("Datasets and Tables","train_imputed.csv"))


Imp_features <- read.csv("C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Genus.csv")

### only keeping the strongly divergent traits 
train_boruta <- train_imputed %>% 
  dplyr::select(Species,Imp_features$Feature)


### 3D PLOTLY PLOTS ###

plot <- plot_ly(train_boruta,x= ~L_Circ,y= ~LA,z= ~WPSMF, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Leaf Circularity'),
                 yaxis = list(title = 'Leaf Area'),
                 zaxis = list(title = 'Whole Plant Stem Mass Fraction'))
)

plot


saveWidget(plot,"Genus3d.html")

##### 3D PLOTS ### ANNUALS ###

Imp_features_Annuals <- read.csv("C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Annual.csv") 


###### FILTER OUT BY ANNUAL SPECIES ##### 

Annuals <- c("H_praeco_ssp_runyonii","H_debili_ssp_tardiflorus",
             "H_neglectus","H_petiolari_ssp_petiolaris",
             "H_niveu_ssp_tephrodes","H_annuus","H_argophyllus")



##### keeping only the annuals  ### 
train_imputed_annuals <- train_imputed %>% filter(Species %in% Annuals)


### only keeping the strongly divergent traits
train_boruta_Annuals <- train_imputed_annuals %>% 
  dplyr::select(Species,Imp_features_Annuals$Feature)


plot_annuals <- plot_ly(train_boruta_Annuals,x= ~WPTB,y= ~L_NightRespArea,z= ~L_LaminaT, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Whole Plant Total Biomass'),
                 yaxis = list(title = 'Leaf Night Respiration Area'),
                 zaxis = list(title = 'Leaf Lamina Thickness'))
  )


plot_annuals


saveWidget(plot_annuals,"Annual3d.html")


####### 3D PLOTs #### LARGE PERENNIALS
###### #############

Imp_features_Perennials <- read.csv("C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_Perennials.csv") 

###### Only keep the perennial species ##### 

Perennials <- c("H_salicifolius","H_maximiliani","H_giganteus",
                "H_verticillatus","H_grosseserratus","H_divaricatus",
                "H_microcephalus","H_cusickii")



##### keeping only the annuals  ### 
train_imputed_perennials <- train_imputed %>% filter(Species %in% Perennials)

### only keeping the most strongly divergent traits 
train_boruta_Perennials <- train_imputed_perennials %>% 
  dplyr::select(Species,Imp_features_Perennials$Feature)


plot_perennials <- plot_ly(train_boruta_Perennials,x= ~LD13C,y= ~L_Circ,z= ~LA, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'LD13C'),
                 yaxis = list(title = 'Leaf Circularity'),
                 zaxis = list(title = 'Leaf Area'))
  )

plot_perennials  

saveWidget(plot_perennials,"Perennials3d.html") 


############
### 3D PLOTS ## SOUTHEASTERN PERENNIALS ###
###### 

Imp_features_southeastern <- read.csv("C:/Users/samba/Documents/Chapter_1_Analysis/Datasets and Tables/Boruta_southeastern_perennials.csv")

###### FILTER OUT BY SOUTHEASTERN PERENNUIALS SPECIES ##### 

Southeastern_perennials <- c("H_carnosus","H_atrorubens","H_radula",
                             "H_silphioides","H_floridanus","H_heterophyllus",
                             "H_longifolius","H_angustifolius")


##### keeping only the Southeastern perennials  ### 

train_imputed_southeastern <- train_imputed %>% filter(Species %in% Southeastern_perennials)

### only keeping the most strongly divergent traits 
train_boruta_southeastern <- train_imputed_southeastern %>% 
  dplyr::select(Species,Imp_features_southeastern$Feature)



plot_southeastern <- plot_ly(train_boruta_southeastern,x= ~L_Circ,y= ~LA,z= ~LC, color = ~Species
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Leaf Circularity'),
                 yaxis = list(title = 'Leaf Area'),
                 zaxis = list(title = 'Leaf Conductivity'))
  )


plot_southeastern

saveWidget(plot_southeastern,"Southeastern3d.html") 






