# Predicting Commuter Transportation Choices (Python)

# import packages into the workspace for this program
from __future__ import division, print_function
import numpy as np
import pandas as pd
import statsmodels.api as sm

# read data from comma-delimited text file... create DataFrame object
sydney = pd.read_csv("sydney.csv")

# check input DataFrame
print(sydney)

# dictionary object to convert string to binary integer 
response_to_binary = {'TRAIN':1, 'CAR':0}

y = sydney['choice'].map(response_to_binary)
cartime = sydney['cartime']
carcost = sydney['carcost']
traintime = sydney['traintime']
traincost = sydney['traincost']

# define design matrix for the linear predictor
Intercept = np.array([1] * len(y))
x = np.array([Intercept, cartime, carcost, traintime, traincost]).T

# generalized linear model for logistic regression
logistic_regression = sm.GLM(y, x, family=sm.families.Binomial())
sydney_fit = logistic_regression.fit()
print(sydney_fit.summary())

sydney['train_prob'] = sydney_fit.predict(linear = False)

# function to convert probability to choice prediction
def prob_to_response(response_prob, cutoff):
    if(response_prob > cutoff):
        return('TRAIN')
    else:
        return('CAR')
            
# add binary predictions to DataFrame sydney using cutoff value for the case
sydney['choice_pred'] = \
    sydney['train_prob'].apply(lambda d: prob_to_response(d, cutoff = 0.50))
    
# evaluate performance of logistic regression model    
# obtain confusion matrix and proportion of observations correctly predicted    
cmat = pd.crosstab(sydney['choice_pred'], sydney['choice']) 
a = float(cmat.ix[0,0])
b = float(cmat.ix[0,1])
c = float(cmat.ix[1,0]) 
d = float(cmat.ix[1,1])
n = a + b + c + d
predictive_accuracy = (a + d)/n  

print(cmat)
print('\n Percentage Correctly Predicted',\
     round(predictive_accuracy, 3), "\n")