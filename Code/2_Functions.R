#### GENUS ###



### 
### writing a function that will compute the gini importance values and ##
### put them in a dataframe as well as create a barplot for comparasion purposes
## the function will also output the values as excel tables 

Gini <- function(df){
  set.seed(1234) ## this is important to get consistent results ##
  Rf_genus <- randomForest(Species~.,data = df)
  
  importance_by_gini <- varImp(Rf_genus)
  
  importance_by_gini <- data.frame(Features = row.names(importance_by_gini),
                                   Overall = importance_by_gini$Overall)
  
  
  ## making a ggplot ### 
  
  
  p <- ggplot(data = importance_by_gini,
         aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
    geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
    coord_flip() + 
    theme_bw() + theme(legend.position = "none") + 
    ggtitle("Importance of each Variable as per GINI") +
    theme(text = element_text(size = 10))   
  

  list = list("plot"= p,"Gini importance table" = importance_by_gini) 
  return(list) 

}



#######
## now lets write a function that identifies the optimum subset of traits##
## plots their respective importance values on a relative scale ##
## and outputs a datadframe containing the importance values and exports them
#########

RFE <- function(df){
  set.seed(1234) ### we need to do this to get consistent results
  features_rfe_gini <- rfe(Species~.,data = df,
                           sizes=subsets,rfeControl=params_rfe)
  
  ## these are the predictors in the optimal subset 
  
  optimal_subset_rfe_genus <- data.frame(Features = predictors(features_rfe_gini))
  
  #### Importance of each variable 
  
  Rfe_Imp_Genus <- data.frame(varImp(features_rfe_gini))
  
  Rfe_Imp_Genus <- data.frame(Features = rownames(Rfe_Imp_Genus),
                              Overall = Rfe_Imp_Genus$Overall)
  
  Rfe_Imp_Genus_best_subset <- Rfe_Imp_Genus %>%
    dplyr::filter(Features %in% optimal_subset_rfe_genus$Features)
  
  
  ### plotting the variation of accuracy with the removal of variables
  p_1 <- ggplot(features_rfe_gini)  
  
  p_2 <- ggplot(data = Rfe_Imp_Genus_best_subset,
         aes(x=reorder(Features,Overall), y = Overall, fill = Features)) +
    geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
    coord_flip() + 
    theme_bw() + theme(legend.position = "none") + 
    ggtitle("Optimal subset of features") +
    theme(text = element_text(size = 10))  
  
  list <- list("Plot of variation of accuracy with the removal of variables" = p_1,
               "Importance plot" = p_2,
               "optimal subset" = optimal_subset_rfe_genus,
               "Best subset along with their importance values" = Rfe_Imp_Genus_best_subset
               ) 
  return(list)
  

}


####################
#### Now writing a function that will evaluate the strongly divergent traits ##
#### output the tables containing the importance values for each variables ###

Boruta_analysis <- function(df){
  set.seed(1234) ### this is important to get consistent results 
  
  Boruta_genus <- Boruta(Species ~., df)
  
  # Putting the importance decisions in a nice table ### 
  
  Boruta_feature_analysis <- data.frame(attStats(Boruta_genus)) 
  
  ### Making the colnames into row names ###
  
  Boruta_feature_analysis$Feature <- rownames(Boruta_feature_analysis)
  
  ### Subsetting the dataset based on the strongly relevant traits ## 
  ## i.e., Confirmed ###
  
  Boruta_feature_analysis <- Boruta_feature_analysis %>% 
    dplyr::filter(decision == "Confirmed")
  
  
  p<- ggplot(data = Boruta_feature_analysis,
         aes(x=reorder(Feature,meanImp), y = meanImp, fill = Feature)) +
    geom_bar(stat = "identity") + labs(x= "Features", y= "Variable Importance") +
    coord_flip() + 
    theme_bw() + theme(legend.position = "none") + 
    ggtitle("Strongly important variables") +
    theme(text = element_text(size = 10))   
    
    list <- list("Importance plot Boruta" = p,
         "Table with strongly divergent traits along with thier importances" = Boruta_feature_analysis)  
  
    return(list)
}



#####################
## MODELING ####
################

Random_forest_analysis <- function(df){
  set.seed(1234)
  Rf <- train(Species~.,data=df,
              method="rf",trControl=params,
              verbose=F)
  p_rf_boruta <- predict(Rf,test_boruta)
  
  c_rf_boruta <- confusionMatrix(p_rf_boruta,test_boruta$Species)
  ##############
  ### Extracting data from the confusion matrix 
  ###############    #### Random Forest ######## 
  
  per_class_metrics_RF_boruta <- data.frame(c_rf_boruta$byClass)
  #### Macro averaged metrics ### Random_forest ########

  Macro_averaged_metrics_Rf <- data.frame(Macro_averaged_metrics =
                                            apply(per_class_metrics_RF_boruta[-1],2,mean))
  
  list <- list("Rf" = Rf,"Confusion Matrix" = c_rf_boruta, 
               "Per class Metrics" = per_class_metrics_RF_boruta,
               "Macro averaged metrics" = Macro_averaged_metrics_Rf)
  
  return(list)
  
}



Gbm_analysis <- function(df) {
  set.seed(1234)
  gbm <- train(Species~., data=df,
               method="gbm",trControl=params,
               verbose=F,tuneGrid=grid)
  
  p_gbm <- predict(gbm,test_boruta)
  
  c_gbm <- confusionMatrix(p_gbm,test_boruta$Species)
  
  ##############
  ### Extracting data from the confusion matrix 
  ###############    #### Gradient Boosting ######## 
  
  per_class_metrics_GBM_boruta <- data.frame(c_gbm$byClass)
  
  #### Macro averaged metrics ### Random_forest ########
  
  Macro_averaged_metrics_GBM <- data.frame(Macro_averaged_metrics =
                                             apply(per_class_metrics_GBM_boruta[-1],2,mean))
  
  list <- list("GBM" = gbm,"Confusion Matrix" = c_gbm, 
               "Per class Metrics" = per_class_metrics_GBM_boruta,
               "Macro averaged metrics" = Macro_averaged_metrics_GBM)
  
  return(list)
  
}




