# Property Price Prediction using XGBoost

This project utilizes the `XGBoost` machine learning algorithm to predict property prices based on various features. The pipeline includes data preprocessing, feature engineering, and model evaluation.

---

## Table of Contents
- [Overview](#overview)
- [Dataset](#dataset)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Results](#results)
- [Acknowledgments](#acknowledgments)

---

## Overview
This repository implements a machine learning model to predict property prices using the XGBoost algorithm. Key features include:
- Preprocessing of property datasets.
- Handling categorical and numerical variables.
- Cross-validation with k-fold techniques.
- Hyperparameter tuning for better predictions.

---

## Dataset
The project uses a dataset containing property features like:
- `Luas Tanah`, `Luas Bangunan`, `Harga`
- Number of rooms, garage, ownership type, and others.

Make sure to upload the following dataset files:
1. **Copy of Merge Data Properti - Sheet1.csv** (Main dataset)
2. **Data Testing Properti.csv** (Test dataset)

---

## Installation
To run the code, ensure the following dependencies are installed:
- `R` (v4.3.1 or higher)
- Required Packages: 
  ```r
  install.packages(c("xgboost", "caret", "mltools", "ggplot2", "data.table"))
  ```

---

## Usage
### 1. Clone this repository
```bash
git clone https://github.com/RaffiArdhiN/Property_Price_Prediction_XGBoost.git
cd Property_Price_Prediction_XGBoost
```

### 2. Open and Run the R Script
- Open `model.R` in RStudio or VSCode.
- Ensure the dataset files are located in the same directory as the script.
- Run the script to preprocess data, train the model, and evaluate predictions.

### 3. Preprocessing Steps
- Replace spaces in column names with underscores.
- Convert text data to uppercase for consistency.
- Encode categorical variables using one-hot encoding.

### 4. Plots Generated
During the execution of the script, the following plots are generated:
- `train_plot.png`: Comparison between actual and predicted prices for the training set.
- `test1_plot.png`: Comparison between actual and predicted prices for the first test dataset.
- `test2_plot.png`: Comparison between actual and predicted prices for the second test dataset.

These plots are saved in the same directory as the script and provide insights into model performance.

---

## Project Structure
```
.
├── model.R                           # R script for data preprocessing and model training
├── Copy of Merge Data Properti - Sheet1.csv   # Main dataset
├── Data Testing Properti.csv         # Test dataset
├── train_plot.png                    # Plot for training set results
├── test1_plot.png                    # Plot for test set 1 results
├── test2_plot.png                    # Plot for test set 2 results
└── README.md                         # Project documentation
```

---

## Results
### RMSE for k-Fold Cross Validation
Below are RMSE values obtained from each fold of the cross-validation:
- [1] 1510044210, 3322630879, 3802210710, 1528702937, 4945731882, 2240249870, 1751618255, 2240672508, 3360616472, 11832249769

### Final Ensemble Predictions RMSE
- Training Set: `992256319`
- Test Set 1: `3758348020`
- Test Set 2: `4072324683`

---

## Acknowledgments
Thanks to the open-source community for developing the tools and libraries used in this project, particularly `XGBoost` and `caret` for enabling advanced machine learning workflows in R.