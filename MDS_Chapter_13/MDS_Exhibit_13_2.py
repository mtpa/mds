# Restaurant Site Selection (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for analysis and modeling
import pandas as pd  # data frame operations
import numpy as np  # arrays and math functions
import statsmodels.api as sm  # statistical models (including regression)
import statsmodels.formula.api as smf  # statistical models (including regression)

# read data for Studenmund's Restaurants
# creating data frame restdata
restdata = pd.read_csv('studenmunds_restaurants.csv')

# print the first five rows of the data frame
print(pd.DataFrame.head(restdata)) 

# specify regression model
my_model = str('sales ~ competition + population + income')

# fit the model to the data
my_model_fit = smf.ols(my_model, data = restdata).fit()
# summary of model fit to the training set
print(my_model_fit.summary())
# predictions from the model fit to the data for current stores
restdata['predict_sales'] = my_model_fit.fittedvalues

# compute the proportion of response variance accounted for
print('\nProportion of Test Set Variance Accounted for: ',\
    round(np.power(restdata['sales'].corr(restdata['predict_sales']),2),3))

# define DataFrame of sites for new restaurants
sites_data = {'sales': [0,0,0],
             'competition': [2, 3, 5],
             'population': [50000, 200000, 220000],
             'income': [25000, 22000, 19000]}

sites = pd.DataFrame(sites_data)

# obtain predicted sales for the new restaurants
# rounding to the nearest dollar
sites['sales_pred'] = my_model_fit.predict(sites)
print('\nNew sites with predicted sales', sites, '\n')


