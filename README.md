# 🌱 Machine Learning Functional Trait Divergence

## 📌 Overview
This repository contains datasets, code, and tables used in the analysis of functional trait divergence using a machine learning approach. The study focuses on the diversification of functional traits in specific biological entities, with detailed analysis and findings presented in the associated document.

## 📂 Repository Structure

- **Datasets and Tables**: 📊 Contains data tables generated during the analyses and the datsets used in this study. Here is the full list
  - **CombinedTraitDataset_ForSam_12.20.20** Functional trait data from twenty-eight diploid wild Helianthus species.
  - **train_imputed** This data was used to perform descriptive modeling and was used to train machine learning classifiers.
  - **test_imputed** This data was used to validate the predictive capabilities of the machine learning classifiers trained on the training data.
  - **Sunflower_train** Dataset used to perform univariate variance partition analysis, this is the same dataset as train_imputed.
  - **Not_best_subset_genus** Traits that were not part of the optimal subset of relevant traits at the genus.
  - **Not_best_subset_annual** Traits that were not part of the optimal subset of relevant traits at the annual.
  - **Not_best_subset_perennial** Traits that were not part of the optimal subset of relevant traits at the perennial.
  - **Not_best_subset_southeastern** Traits that were not part of the optimal subset of relevant traits at the southeastern perennial.
  - **Variance_partitioning_Genus** Variance partitioned between species, population, and corresponding residuals at the genus level.
  - **Variance_partitioning_Annual** Variance partitioned between species, population, and corresponding residuals at the annual clade level.
  - **Variance_partitioning_Perennial** Variance partitioned between species, population, and corresponding residuals at the perennial clade level.
  -  **Variance_partitioning_southastern_perennial** Variance partitioned between species, population, and corresponding residuals at the southeastern perennial clade level.
  - **Gini_Importance_Genus** Relative importance of each trait at the genus level calculated by Gini impurity by fitting a random forest model to the training data.
  - **Gini_Importance_Annual** Relative importance of each trait at the annual level calculated by Gini impurity by fitting a random forest model to the training data.
  - **Gini_Importance_Perennial** Relative importance of each trait at the perennial level calculated by Gini impurity by fitting a random forest model to the training data.
  - **Gini_Importance_southeastern** Relative importance of each trait at the southeastern perennial calculated by Gini impurity by fitting a random forest model to the training data.
  - **Rfe_Genus_best_subset** Optimal subset of ecologically relevant traits at a genus level, identified by recursive feature elimination.
  - **Rfe_Perennial_best_subset** Optimal subset of ecologically relevant traits at a perennial level, identified by recursive feature elimination.
  - **Rfe_Annual_best_subset** Optimal subset of ecologically relevant traits at an annual level, identified by recursive feature elimination.
  - **Rfe_southeastern_best_subset** Optimal subset of ecologically relevant traits at the southeastern perennial level, identified by recursive feature elimination.
  - **RF_Macro_averaged_metrics_genus** Metrics quantifying Random Forest performance at the genus level.
  - **RF_Macro_averaged_metrics_annual** Metrics quantifying Random Forest performance at the annual clade level.  
  - **RF_Macro_averaged_metrics_perennial** Metrics quantifying Random Forest performance at the large perennial clade level.  
  - **RF_Macro_averaged_metrics_southeastern** Metrics quantifying Random Forest performance at the southeastern perennial clade level.  
  - **per_class_metrics_RF_genus** Class based performance of Random Forest model. This table shows the ability of the model to detect each species at the genus level.
  - **per_class_metrics_RF_annual** Class based performance of Random Forest model. This table shows the ability of the model to detect each species at the annual level.
  - **per_class_metrics_RF_annual** Class based performance of Random Forest model. This table shows the ability of the model to detect each species at the annual clade level.
  - **per_class_metrics_RF_perennial** Class based performance of Random Forest model. This table shows the ability of the model to detect each species at the large perennial clade level.
  - **per_class_metrics_RF_southeastern** Class based performance of Random Forest model. This table shows the ability of the model to detect each species at the southeastern clade level.
  - **GBM_Macro_averaged_metrics_genus** Metrics quantifying Gradient Boosting Machine performance at the genus level.
  - **GBM_Macro_averaged_metrics_annual** Metrics quantifying Gradient Boosting Machine performance at the annual level.
  - **GBM_Macro_averaged_metrics_perennial** Metrics quantifying Gradient Boosting Machine performance at the large perennial level.  
  - **GBM_Macro_averaged_metrics_southeastern** Metrics quantifying Gradient Boosting Machine performance at the southeastern perennial level.
  - **per_class_metrics_GBM_genus** Class based performance of Gradient Boosting model. This table shows the ability of the model to detect each species at the genus level.
  - **per_class_metrics_GBM_annual** Class based performance of Gradient Boosting model. This table shows the ability of the model to detect each species at the annual clade level.
  - **per_class_metrics_GBM_perennial** Class based performance of Gradient Boosting model. This table shows the ability of the model to detect each species at the large perennial clade level.
  - **per_class_metrics_GBM_annual** Class based performance of Gradient Boosting model. This table shows the ability of the model to detect each species at the annual clade level.
  - **per_class_metrics_GBM_southeastern** Class based performance of Gradient Boosting model. This table shows the ability of the model to detect each species at the southeastern clade level.
  - **SupplementData** Contains all supplementary data tables pertaining to this study
- **Figures**: 🖼️ Visual representations and plots related to the study. This is the folder where the figures generated during the analyses will be saved upon running the code.
  - **Figure 1** Complete workflow of the entire analysis procedure. The data was divided into a training and a test dataset by random sampling, whereby 70% of the data was used for training and 30% was used for testing. Missing data was imputed using a random forest algorithm using the R function rfImpute from the package randomForest. A random forest classifier was applied to the imputed training data (Train Imputed) and the traits were ranked based on Gini Importance. A recursive feature elimination method was implemented on the imputed training data and the optimal subset of ecologically relevant traits for species diversification was identified. The traits that were not in the optimal subset were excluded from both the training (Train optimal) and the test dataset (Test optimal). Two predictive models were trained on this reduced training dataset (Train optimal). One predictive model was built using the random forest (RF) classifier while the other one was built using the gradient boosting machine (GBM). These two models were validated using the test dataset (Test optimal) and compared with each other using metrics like overall accuracy, precision, recall and the F1 score.

- **Code Files**: 💻 Various R and HTML files for data processing, analysis, and visualization. Notable files include:
  - `1_Data_preparation.R` : Script to clean and preprocess the dataset
  - `2_Functions.R` : Custom functions created to streamline the various data cleaning and modeling procedures.  
  - `3_Data_Analysis_Genus.R` : Script for performing feature selection, model training and model validation on the training and the test datasets at the genus level.
  - `4_Data_Analysis_Annuals.R` : Script for performing feature selection, model training and model validation on the training and the test datasets at the annual clade level.
  - `5_4_Data_Analysis_Perennials.R` : Script for performing feature selection, model training and model validation on the training and the test datasets at the large perennial clade level.
  - `6_Data_Analysis_Southeastern.R` : Script for performing feature selection, model training and model validation on the training and the test datasets at the southeastern clade level.
  - `7_Variance_Partitioning.R` : Script for performing variance partitioning analysis at the genus and the clade levels.
  - `8_3D_Plots.R` : Script to create 3D plots at genus and clade levels
  - `9_Not_best_subset.R` : Script for ascertaining the traits which are not part of the optimal subset at the genus and clade level.
  - `10_Variable_Importance.R` : Script for creating variable importance plots.

## 🔍 Key Findings

- **Trait Importance for Species Classification:** The study identified key traits for accurate species classification at both genus and clade levels. Important traits include leaf size, shape, trichome density, and whole plant reproductive phenology. Traits like leaf nutrient chemistry, gas exchange, and floral morphology were generally less important.

- **Differences in Trait Importance Across Scales:** There were notable differences in the importance of various traits when comparing genus and clade levels. For instance, leaf lifespan was more significant at the genus level, while traits like leaf area and photosynthetic rate varied in importance across different clades.

- **Identification of Strongly Divergent Traits:** Using Recursive Feature Elimination (RFE), the study pinpointed a core set of ecophysiological traits that are highly divergent across species. These include leaf size, shape, trichome density, and aspects of whole plant reproductive phenology and biomass.

- **Model Performance in Species Classification:** Classification models, specifically Random Forest (RF) and Gradient Boosting Machine (GBM), were used to validate the findings. These models showed high overall accuracy (ranging from 91.3% to 96.3%) in classifying species based on the identified traits, demonstrating the effectiveness of these traits in species differentiation.

## 📜 License
This project is licensed under the MIT License.

## 🤝 Contributing
For any contributions or suggestions, please open an issue or submit a pull request.
