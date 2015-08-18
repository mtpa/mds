# Analysis for a Field Test of Laundry Soaps (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function
from future_builtins import ascii, filter, hex, map, oct, zip

# import packages into the namespace for this program
import numpy as np
import pandas as pd
import statsmodels.api as sm

# first import data from the comma-delimited file soaps.csv
# of individual observations (households) in the field test
# the response variable relates to brand choice
# choice is factor/binary response variable (M or X)
# coded as a 0/1 binary variable named response
# the explanatory variables are factors (categorical variables):
#   wtemp = water temperature with levels LOW OR HIGH
#   wtype = water type with levels SOFT, MEDIUM, OR HARD
#   muser = is user of brand M with labels NO OR YES

# read individual household data for field experiment
soaps = pd.read_csv("soaps.csv")

# check the pandas DataFrame object
print(soaps.head())
print(soaps.tail())

# -------------------------
# Define the Design Matrix
# -------------------------
Intercept = np.array([1] * len(soaps))

# use dictionary objects for mapping to 0/1 binary codes
muser_to_binary = {'NO' : 0, 'YES' : 1}
YESmuser = np.array(soaps['muser'].map(muser_to_binary))

wtemp_to_binary = {'LOW' : 0, 'HIGH' : 1}
HIGHwtemp = np.array(soaps['wtemp'].map(wtemp_to_binary))

medium_wtype_to_binary = {'SOFT' : 0, 'MEDIUM' : 1, 'HARD': 0}
MEDIUMwtype = np.array(soaps['wtype'].map(medium_wtype_to_binary))

hard_wtype_to_binary = {'SOFT' : 0, 'MEDIUM' : 0, 'HARD': 1}
HARDwtype = np.array(soaps['wtype'].map(hard_wtype_to_binary))

# define two-way interation terms
YESmuser_HIGHwtemp = YESmuser * HIGHwtemp
YESmuser_MEDIUMwtype = YESmuser * MEDIUMwtype
YESmuser_HARDwtype = YESmuser * HARDwtype
HIGHwtemp_MEDIUMwtype = HIGHwtemp * MEDIUMwtype
HIGHwtemp_HARDwtype = HIGHwtemp * HARDwtype

# define three-way interation terms
YESmuser_HIGHwtemp_MEDIUMwtype = YESmuser * HIGHwtemp * MEDIUMwtype
YESmuser_HIGHwtemp_HARDwtype = YESmuser * HIGHwtemp * HARDwtype

# specify complete experimental design with interactions
Design_Matrix = np.array([Intercept,\
    YESmuser,\
    HIGHwtemp,\
    MEDIUMwtype,\
    HARDwtype,\
    YESmuser_HIGHwtemp,\
    YESmuser_MEDIUMwtype,\
    YESmuser_HARDwtype,\
    HIGHwtemp_MEDIUMwtype,\
    HIGHwtemp_HARDwtype,\
    YESmuser_HIGHwtemp_MEDIUMwtype,\
    YESmuser_HIGHwtemp_HARDwtype]).T

# define the binary response variable
Response = np.array(soaps['response'])

# -------------------------
# Fit Model to Data
# -------------------------    
# fit the complete model using a generalized linear model
glm_binom = sm.GLM(Response, Design_Matrix,\
    family=sm.families.Binomial())
res = glm_binom.fit()
print(res.summary())

