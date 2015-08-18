# Predicting Customer Retention (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for text processing and machine learning
import pandas as pd  # DataFrame structure and operations
import numpy as np  # arrays and numerical processing
import matplotlib.pyplot as plt  # 2D plotting
import statsmodels.api as sm  # logistic regression
import statsmodels.formula.api as smf  # R-like model specification
import patsy  # translate model specification into design matrices
from sklearn import svm  # support vector machines
from sklearn.ensemble import RandomForestClassifier  # random forests
from sklearn.naive_bayes import GaussianNB  # naive Bayes

# import user-defined module
import evaluate_classifier as eval

# read in comma-delimited text file and create data frame
# there are blank character fields for missing data
# read them as character fields initially
att = pd.read_csv("att.csv")
print(att.head())

# select variables of interest for modeling
attprelim = pd.DataFrame(att, columns = ['pick','usage','reachout','card'])

# use dictionary objects for mapping to 0/1 binary codes
pick_to_binary = {'ATT' : 0, 'OCC' : 1}
attprelim['pick'] = attprelim['pick'].map(pick_to_binary)
yes_to_binary = {'NO' : 0, 'YES' : 1}
attprelim['reachout'] = attprelim['reachout'].map(yes_to_binary)
attprelim['card'] = attprelim['card'].map(yes_to_binary)

# work with complete cases only
attwork = attprelim.dropna()
print(attwork.head())

# specify form of predictive model
attmodel = 'pick ~ usage + reachout + card'

# convert R-like formula into design matrix needed for statsmodels        
y,x = patsy.dmatrices(attmodel,\
    attwork, return_type = 'dataframe')    

# --------------------------------------
# Logistic regression method
# --------------------------------------
my_logit_model = sm.Logit(y,x)
# fit the model to the full data set
my_logit_model_fit = my_logit_model.fit()
print(my_logit_model_fit.summary())

# predicted probability of switching to OCC
attwork['pred_logit_prob'] = my_logit_model_fit.predict(linear = False)

# map from probability to ATT (0) or OCC (1)
def prob_to_pred(x):
    if(x > 0.5):
        return(1)
    else:
        return(0)

attwork['pred_logit'] =\
    attwork['pred_logit_prob'].apply(lambda d: prob_to_pred(d))

print('\n Logistic Regression Performance\n',\
    'Percentage of Choices Correctly Classified:',\
    100 * round(eval.evaluate_classifier(attwork['pred_logit'],\
    attwork['pick'])[4], 3),'\n')

# --------------------------------------
# Support vector machines
# --------------------------------------
my_svm = svm.SVC()  
my_svm_fit = my_svm.fit(x, np.ravel(y))
attwork['pred_svm_binary'] = my_svm_fit.predict(x)

print('\n Support Vector Machine Performance\n',\
    'Percentage of Choices Correctly Classified:',\
    100 * round(eval.evaluate_classifier(attwork['pred_svm_binary'],\
    attwork['pick'])[4], 3),'\n')

# --------------------------------------
# Random forests
# --------------------------------------
# for reproducibility set random number seed with random_state
my_rf_model = RandomForestClassifier(n_estimators = 10, random_state = 9999)
my_rf_model_fit = my_rf_model.fit(x, np.ravel(y))
attwork['pred_rf_binary'] = my_rf_model_fit.predict(x)

print('\n Random Forest Performance\n',\
    'Percentage of Choices Correctly Classified:',\
    100 * round(eval.evaluate_classifier(attwork['pred_rf_binary'],\
    attwork['pick'])[4], 3),'\n')

# --------------------------------------
# Naive Bayes
# --------------------------------------
my_nb_model = GaussianNB()
my_nb_model_fit = my_nb_model.fit(x, np.ravel(y))
attwork['pred_nb_binary'] = my_nb_model_fit.predict(x)

print('\n Naive Bayes Performance\n',\
    'Percentage of Choices Classified:',\
    100 * round(eval.evaluate_classifier(attwork['pred_rf_binary'],\
    attwork['pick'])[4], 3),'\n')

# for training and test, utilize scikit-laearn cross-validation




