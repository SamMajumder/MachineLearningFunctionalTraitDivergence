
rm(list = ls())

packages <- list("tidyverse","ggplot2","lme4","svglite")


lapply(packages, require,character.only=T)


Sunflower_train <- read.csv("Sunflower_train.csv")

### converting the population and the species column to factor 

Sunflower_train <- Sunflower_train %>%
                   mutate_if(is.character,as.factor)


LTD_mixed_model <- lmer(LTD~ (1|Species/Pop),data = Sunflower_train)

L_Circ_mixed_model <- lmer(L_Circ ~ (1|Species/Pop),data = Sunflower_train)

L_Area_mixed_model <- lmer(LA~(1|Species/Pop),data = Sunflower_train)


summary(LTD_mixed_model)

all_var_LTD <- 0+16274+2276

Pop_Ltd_var <- (0/all_var_LTD) * 100

Species_Ltd_var <- (16274/all_var_LTD) * 100


summary(L_Circ_mixed_model)

all_var_L_Circ <- 0.0007041+0.0214819+0.0030185

Pop_L_Circ_var <- (0.0007041/all_var_L_Circ) * 100

Species_L_Circ_var <- (0.0214819/all_var_L_Circ) * 100


summary(L_Area_mixed_model)

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


ggsave("Figure S2.svg",dpi = 300)












