# Identifying Customer Targets (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for text processing and machine learning
import pandas as pd  # DataFrame structure and operations
import numpy as np  # arrays and numerical processing
import matplotlib.pyplot as plt  # 2D plotting
import statsmodels.api as sm  # logistic regression
import statsmodels.formula.api as smf  # R-like model specification
import patsy  # translate model specification into design matrices

# import user-defined module
import evaluate_classifier as eval

# read in comma-delimited text file and create data frame
# there are blank character fields for missing data
# read them as character fields initially
bank = pd.read_csv('bank.csv', sep = ';')
print(bank.head())

# define jobtype variable
job_to_jobtype = {'admin.':'White Collar',\
    'entrepreneur':'White Collar',\
    'management':'White Collar',\
    'self-employed':'White Collar',\
    'blue-collar':'Blue Collar',\
    'services':'Blue Collar',\
    'technician':'Blue Collar'}
bank['jobtype'] = bank['job'].map(job_to_jobtype)
bank['jobtype'] = bank['jobtype'].fillna('Other/Unknown')

# set marital variable
marital_to_label = {'divorced':'Divorced',\
    'married':'Married',\
    'single':'Single'}
bank['marital'] = bank['marital'].map(marital_to_label)
bank['marital'] = bank['marital'].fillna('Unknown')

# set education variable
education_to_label = {'primary':'Primary',\
    'secondary':'Secondary',\
    'tertiary':'Tertiary'}
bank['education'] = bank['education'].map(education_to_label)
bank['education'] = bank['education'].fillna('Unknown')

# set no/yes variable labels
noyes_to_label = {'no':'No', 'yes':'Yes'}
bank['default'] = bank['default'].map(noyes_to_label)
bank['default'] = bank['default'].fillna('No')

bank['housing'] = bank['housing'].map(noyes_to_label)
bank['housing'] = bank['housing'].fillna('No')

bank['loan'] = bank['loan'].map(noyes_to_label)
bank['loan'] = bank['loan'].fillna('No')

# code response as binary variable
noyes_to_binary = {'no':0, 'yes':1}
bank['response'] = bank['response'].map(noyes_to_binary)
bank['response'] = bank['response'].fillna('No')

# work only with bank clients who are being approached for the first time  
filter = bank['pdays'].map(lambda d: d == -1)

# apply the filter and select columns needed for targeting model
bankwork = pd.DataFrame(bank[filter], columns = ['response','age','jobtype',\
    'education',  'marital', 'default', 'balance', 'housing', 'loan'])
print(bankwork.head()) 
print(bankwork.shape)   

# examine descriptive statistics and frequency tables for variables in model
print(bankwork.describe())
print('\njobtype:\n',bankwork['jobtype'].value_counts())
print('\nmarital:\n',bankwork['marital'].value_counts())
print('\neducation:\n',bankwork['education'].value_counts())
print('\ndefault:\n',bankwork['default'].value_counts())
print('\nhousing:\n',bankwork['housing'].value_counts())
print('\nloan:\n',bankwork['loan'].value_counts())

# examine means of continuous explanatory variables by response
print(bankwork.pivot_table(['age'], index = ['response']))
print(bankwork.pivot_table(['balance'], index = ['response']))

# baseline response rate computed (will be used later)
filter_took_offer = bankwork['response'].map(lambda d: d == 1)
baseline_response_rate = len(bankwork[filter_took_offer]) / len(bankwork)
print('\nBaseline proportion of clients responding to offer: ',\
    round(baseline_response_rate,5), '\n')

# examine proportion responding across levels 
# of categorical variables
print(bankwork.pivot_table(['response'], index = ['jobtype']))
print(bankwork.pivot_table(['response'], index = ['education']))
print(bankwork.pivot_table(['response'], index = ['marital']))
print(bankwork.pivot_table(['response'], index = ['default']))
print(bankwork.pivot_table(['response'], index = ['housing']))
print(bankwork.pivot_table(['response'], index = ['loan']))

# specify model for logisitc regression
bank_spec = 'response ~ age + jobtype + education + marital +\
    default + balance + housing + loan'

# ----------------------------------
# fit logistic regression model 
# ----------------------------------
# convert R-like formula into design matrix needed for statsmodels        
y,x = patsy.dmatrices(bank_spec, bankwork, return_type = 'dataframe')    

my_logit_model = sm.Logit(y,x)
# fit the model to the full data set
my_logit_model_fit = my_logit_model.fit()
print(my_logit_model_fit.summary())

# predicted probability of reponding to the offer
bankwork['pred_logit_prob'] = my_logit_model_fit.predict(linear = False)

# map target from probability cutoff specified
def prob_to_pred(x, cutoff):
    if(x > cutoff):
        return(1)
    else:
        return(0)

# try cutoff set at 0.50
bankwork['pred_logit_50'] =\
    bankwork['pred_logit_prob'].\
    apply(lambda d: prob_to_pred(d, cutoff = 0.50))    
print('\nConfusion matrix for 0.50 cutoff\n',\
    pd.crosstab(bankwork.pred_logit_50, bankwork.response, margins = True))    
# cutoff 0.50 does not work for targeting... all predictions 0 or No    

# try cutoff set at 0.10
bankwork['pred_logit_10'] =\
    bankwork['pred_logit_prob'].\
    apply(lambda d: prob_to_pred(d, cutoff = 0.10))    
print('\nConfusion matrix for 0.10 cutoff\n',\
    pd.crosstab(bankwork.pred_logit_10, bankwork.response, margins = True)) 

print('\n Logistic Regression Performance (0.10 cutoff)\n',\
    'Percentage of Targets Correctly Classified:',\
    100 * round(eval.evaluate_classifier(bankwork['pred_logit_10'],\
    bankwork['response'])[4], 3),'\n')

# direct calculation of lift 
# decile labels from highest to lowest 
decile_label = []
for i in range(10):
    decile_label.append('Decile_'+str(10 - i))
# draws on baseline response rate computed earlier    
def lift(x):
    return(x / baseline_response_rate)

prediction_deciles = pd.qcut(bankwork.pred_logit_prob, 10, labels = decile_label)
decile_groups = bankwork.response.groupby(prediction_deciles)
print(decile_groups.mean())
lift_values = decile_groups.mean() / baseline_response_rate
print('\nLift Chart Values by Decile:\n', lift_values, '\n')

