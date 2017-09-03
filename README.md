# Forecasting U.S. Inflation Using Machine Learning

This project investigates the application of machine learning methods in forecasting
the rate of CPI inflation in the U.S. by comparing the empirical performance of
six supervised learning algorithms: Ridge Regression, KNN, SVR, Random Forest,
Gradient Boosting Machines (GBMs) and LSTMs. The input features are lagged measures
of inflation, unemployment and percentage change in crude oil price.

A rolling window of data and walk forward cross-validation are used to pro-
duce one and four-quarters-ahead forecasts on the period 1998Q1 to 2017Q1, fol-
lowing out-of-sample forecasting methods.

As a means of comparison against econometric approaches to forecasting in-
flation, forecasts made in practice by human experts is used as the benchmark. We
found the best machine learning algorithms marginally outperform human experts
for four-quarters-ahead forecasts but not for one-quarter-ahead forecasts.

## Prerequisites

The code uses packages and libraries for R (version 3.4.1) and Python (version 2.7 or 3.5). For R the following libraries are required:
* MASS
* e1071
* caret
* randomForest
* xgboost

For Python the following packages are required:
* TensorFlow (version r1.0)
* NumPy
* Pandas
* SciPy
* jupyter

## Where everything is

### data

This folder contains the data used to train and test the algorithms.

### code

The folder ```code``` contains all the scripts. Within it you'll find ```1Q_trainer.R``` and ```4Q_trainer.R```.
These two R scripts train Ridge Regression, KNN, SVR and Random Forest models for 1Q-ahead and 4Q-ahead respectively.
You'll also find ```gbm_trainer_1q.R``` and ```gbm_trainer_4q.R``` scripts.
These two scripts train the GBM models for 1Q-ahead and 4Q-ahead respectively.

Lastly, you'll find a folder ```lstm``` which contains two folders ```1Q``` and ```4Q```.
These two folders contain ipython notebooks of all the LSTM models.

### models

The folder ```models``` contain two R workspace files named ```rr_svr_knn_rf``` and ```gbm```.
These two contain the trained models of Ridge Regression, SVR, KNN and Random Forest models and the trained GBM models respectively.

Furthermore you'll find the folder ```lstm```. This folder contain the saved validated models of all LSTM variations. For instructions on how to load these files, refer to the TensorFlow API Documentation (https://www.tensorflow.org/api_docs/python/tf/train/Saver).

