# Happiness Report Analysis

## Overview
This R script performs basic statistical analysis on the *Happiness Report* dataset. It explores various variables contributing to happiness levels, applies linear and non-linear models to the data, and provides insights into the relationships between the factors. The goal is to understand how different socio-economic, environmental, and health factors influence happiness.

## Features
- Reads and loads the *Happiness Report* dataset.
- Performs basic descriptive statistics (mean, median, standard deviation, etc.).
- Visualizes the distribution of key variables using various plots (e.g., histograms, boxplots).
- Fits linear regression models to identify predictors of happiness.
- Applies non-linear models (e.g., generalized additive models) to capture more complex relationships.
- Provides summary statistics and model evaluation metrics.

## Input
The script assumes the input dataset is in CSV format, containing columns such as:
- **Country**: Name of the country.
- **Happiness Score**: Happiness index for the country.
- **GDP per Capita**: Gross Domestic Product per capita.
- **Social Support**: Level of social support in the country.
- **Life Expectancy**: Life expectancy in the country.
- **Freedom**: Freedom to make life choices.
- **Generosity**: Measure of generosity in the country.
- **Perceptions of Corruption**: The perceived level of corruption in the country.

Make sure your dataset follows a similar structure for the script to work correctly.

## Models
### 1. Linear Regression
- A linear regression model is used to predict the happiness score based on various predictors (e.g., GDP, social support, life expectancy, etc.).
  
### 2. Non-Linear Models
- A generalized additive model (GAM) is used to model non-linear relationships between the predictors and the happiness score.

## Technologies Used
- **R**: The programming language used to perform the analysis.
- **ggplot2**: For data visualization (histograms, boxplots, etc.).
- **stats**: For linear regression and statistical tests.
- **mgcv**: For fitting non-linear models (GAMs).

## Usage
1. Ensure that you have the necessary libraries installed:
   ```R
   install.packages(c("ggplot2", "mgcv"))
