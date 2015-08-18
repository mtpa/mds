# Identifying Consumer Segments (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for multivariate analysis
import pandas as pd  # DataFrame structure and operations
import numpy as np  # arrays and numerical processing
from sklearn.cluster import KMeans  # cluster analysis by partitioning
from sklearn.metrics import silhouette_score as silhouette_score

# read data from comma-delimited text file... create DataFrame object
bank = pd.read_csv('bank.csv', sep = ';')
print(bank.head)  # check the structure of the data frame
print(bank.shape)
# look at the list of column names
list(bank.columns.values)

# examine the demographic variable age
print(bank['age'].unique())
print(bank['age'].value_counts(sort = True))
print(bank['age'].describe())  

# examine the demographic variable job
print(bank['job'].unique())
print(bank['job'].value_counts(sort = True))
print(bank['job'].describe())  

# define job indicator variables
job_indicators = pd.get_dummies(bank['job'], prefix = 'job')
print(job_indicators.head())
bank = bank.join(job_indicators)
bank['whitecollar'] = bank['job_admin.'] + bank['job_management'] + \
    bank['job_entrepreneur'] + bank['job_self-employed']
bank['bluecollar'] = bank['job_blue-collar'] + bank['job_services'] + \
    bank['job_technician'] + bank['job_housemaid']

# examine the demographic variable marital
print(bank['marital'].unique())
print(bank['marital'].value_counts(sort = True))
print(bank['marital'].describe())  

# define marital indicator variables
marital_indicators = pd.get_dummies(bank['marital'], prefix = 'marital')
print(marital_indicators.head())
bank = bank.join(marital_indicators)
bank['divorced'] = bank['marital_divorced']
bank['married'] = bank['marital_married']

# examine the demographic variable education
print(bank['education'].unique())
print(bank['education'].value_counts(sort = True))
print(bank['education'].describe())  

# define education indicator variables
education_indicators = pd.get_dummies(bank['education'], prefix = 'education')
print(education_indicators.head())
bank = bank.join(education_indicators)
bank['primary'] = bank['education_primary']
bank['secondary'] = bank['education_secondary']
bank['tertiary'] = bank['education_tertiary']

print(bank.head)  # check the structure of the data frame
print(bank.shape)
# look at the list of column names
list(bank.columns.values)

# select/filter for cases never previously contacted by sales
bank_selected = bank[bank['previous'] == 0]
print(bank_selected.shape)
    
# select subset of variables needed for cluster analysis and post-analysis
bankfull = pd.DataFrame(bank_selected, \
    columns = ['response', 'age', 'whitecollar', 'bluecollar', 
               'divorced', 'married',
               'primary', 'secondary', 'tertiary'])

# examine the structure of the full bank DataFrame
print(bankfull.head)  # check the structure of the data frame
print(bankfull.shape)
# look at the list of column names
list(bankfull.columns.values)

# select subset of variables for input to cluster analysis
data_for_clustering = pd.DataFrame(bank_selected, \
    columns = ['age', 'whitecollar', 'bluecollar', 
               'divorced', 'married',
               'primary', 'secondary', 'tertiary'])
               
# convert to matrix/numpy array for input to cluster analysis
data_for_clustering_matrix = data_for_clustering.as_matrix()               
  
# investigate alternative numbers of clusters using silhouette score
silhouette_value = []
k = range(2,21)  # look at solutions between 2 and 20 clusters
for i in k:
    clustering_method = KMeans(n_clusters = i, random_state = 9999)
    clustering_method.fit(data_for_clustering_matrix)
    labels = clustering_method.predict(data_for_clustering_matrix)
    silhouette_average = silhouette_score(data_for_clustering_matrix, labels)
    silhouette_value.append(silhouette_average)    
              
# highest silhouette score is for two clusters
# so we use that clustering solution here   
clustering_method = KMeans(n_clusters = 2, random_state = 9999)
clustering_method.fit(data_for_clustering_matrix)
labels = clustering_method.predict(data_for_clustering_matrix)

# add cluster labels to bankfull and review the solution 
bankfull['cluster'] = labels

# pivot table and cross-tabulation examples
bankfull.pivot_table(rows = ['cluster'])  
pd.crosstab(bankfull.cluster, bankfull.bluecollar, margins = True)

# groupby example
segments = bankfull.groupby('cluster')
segments.describe()

