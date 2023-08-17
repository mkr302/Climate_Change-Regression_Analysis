# Climate Change - Regression Analysis & Hyperparameter Optimization
  
## Abstract

This project investigates the intricate chain of effects associated with global warming, focusing on the correlation between greenhouse gas emissions and historical temperature variations, as well as how temperature changes influence rainfall patterns. The study quantified greenhouse gas emissions, temperature, and rainfall using well-established methodologies. Findings revealed strong relationships between temperature anomalies and mean CO2 ppm (Model I), significant effects of temperature anomalies and mean CO2 ppm on mean crop yield (Model II), and the importance of mean temperature in predicting average standardized precipitation (Model III). Moreover, Model IV showed substantial impacts of mean temperature and average standardized precipitation on mean crop yield, with hyperparameter optimization enhancing model performance. These insights contribute valuable understanding of global warming's impact on different aspects and its societal implications.  

## Problem Statement

The project delves into the intricate chain of effects linked to global warming, with a technical focus on investigating the correlation between greenhouse gas emissions and historical temperature variations. Additionally, we aim to examine how these temperature changes influence rainfall patterns. By gaining valuable insights into the broader societal impact of global warming research, we aim to contribute significantly to the understanding and implications of this critical environmental issue.
  
## Initial Hypothesis

Our preliminary hypothesis suggests that rising CO2 emissions will lead to higher temperatures, increased rainfall, and reduced crop yields. To support this hypothesis with empirical evidence, we aim to establish robust correlations and statistical significance between the independent variables (CO2 and temperature) and the dependent variables (rainfall and crop yield). This will provide a more balanced and objective assessment of the global warming phenomenon. 

## Key Findings

• **Model I: Temperature_anomaly ~ mean_co2_ppm**
 - Coefficient for mean_co2_ppm: 0.0084357 (p < 0.0000000000000002)
 - Adjusted R-squared: 0.8385
 - Evaluation metrics: MSE: 0.01272262, RMSE: 0.1127946, MAE: 0.1024163
 - Strong and significant relationship between mean_co2_ppm and Temperature_anomaly.
 
• **Model II: mean_crop_yield ~ Temperature_anomaly + mean_co2_ppm**
 - Adjusted R-squared: 0.8944, Deviance ratio: 0.8991113
 - Approximately 89.44% of variation in mean_crop_yield explained by predictors.
 - Hyperparameter optimization using Ridge regression with lambda=1.
 - Statistically significant model, p-value: 0.04073.
 
• **Model III: Avg_Standardized_Precipitation ~ mean_temperature + reference_temperature_global + Temperature_anomaly + mean_co2_ppm**
 - Adjusted R-squared: 0.2948, F-statistic: 4.136 (p-value: 0.04073)
 - Coefficients: mean_temperature: -0.176951138, Avg_Standardized_Precipitation: -860.4407
 - Hyperparameter optimization using Ridge regression with lambda=0.11.
 - Statistically significant relationship with mean_temperature.
 
• **Model IV: mean_crop_yield ~ mean_temperature + reference_temperature_global + Temperature_anomaly + mean_co2_ppm + Avg_Standardized_Precipitation**
 - Coefficients: mean_temperature: -210.6340, Avg_Standardized_Precipitation: -860.4407
 - p-values indicate statistical significance for mean_temperature and Avg_Standardized_Precipitation.
 - Adjusted R-squared: 0.9603, Deviance ratio: 0.9512085
 - Approximately 96.03% of variation in mean_crop_yield explained by predictors.
 - Hyperparameter optimization using Ridge regression with lambda=123.5.
 - Model shows a good fit to the data.

## Conclusion

Through linear regression and ridge regression using cross-validation/grid search, we analyzed multiple models. Model I revealed a strong relationship between Temperature_anomaly and mean_co2_ppm. Model II indicated significant effects of Temperature_anomaly and mean_co2_ppm on mean_crop_yield. Model III highlighted the importance of mean_temperature in predicting Avg_Standardized_Precipitation. Lastly, Model IV showcased the substantial impact of mean_temperature and Avg_Standardized_Precipitation on mean_crop_yield, with hyperparameter optimization further improving model performance across all cases.

  
## References

Trenberth, K. E. (2011). Changes in precipitation with climate change. CLIMATE RESEARCH,
    47: 123–138, 2011. https://doi.org/10.3354/cr00953
    
Tabari, H. (2020). Climate change impact on flood and extreme precipitation increases with
    water availability. Scientific Reports 10, 13768 (2020). https://doi.org/10.1038/s41598-020-70816-2
    
Goyal, A. (2023). Climate Insights Dataset - Exploring the Impact of Climate Change: A
    Comprehensive Dataset on Temperature. Kaggle.
    https://www.kaggle.com/datasets/goyaladi/climate-insights-dataset?select=climate_change_data.csv
    
Devastator. (2022). Crop Production & Climate Change - Explore the Relationship between Crop
    Production and Climate Change over time. Kaggle. https://www.kaggle.com/datasets/thedevastator/the-relationship-between-crop-production-and-cli
    
Qian, E. (2019). Twitter Climate Change Sentiment Dataset - 44k tweets pertaining to climate
    change. Kaggle. https://www.kaggle.com/datasets/edqian/twitter-climate-change-sentiment-dataset
    
Berkeley Earth (2017). Earth Surface Temperature data (1780-2013). Kaggle.
    https://www.kaggle.com/datasets/berkeleyearth/climate-change-earth-surface-temperature-data/discussion?select=GlobalLandTemperaturesByCountry.csv
    
Cavender-Bares, K. (2013). Sentiment analysis of Climate change. Data.World.
    https://data.world/crowdflower/sentiment-of-climate-change?ref=hackernoon.com
