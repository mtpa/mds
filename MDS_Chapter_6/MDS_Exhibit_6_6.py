# Using Activities Market Baskets for Product Positioning (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for multivariate analysis
import numpy as np  # arrays and numerical processing
import matplotlib.pyplot as plt  # 2D plotting
import pandas as pd

# alternative distance metrics for multidimensional scaling
from sklearn.metrics import euclidean_distances 
from sklearn.metrics.pairwise import linear_kernel as cosine_distances
from sklearn.metrics.pairwise import manhattan_distances as manhattan_distances

from sklearn import manifold  # multidimensional scaling

# read data from comma-delimited text file... create DataFrame object
wisconsin_dells_data_frame = pd.read_csv('wisconsin_dells.csv')
print(wisconsin_dells_data_frame.head)  # check the structure of the data frame
print(wisconsin_dells_data_frame.shape)

binary_variable_names = ['shopping','antiquing',     
'scenery','eatfine','eatcasual','eatfamstyle','eatfastfood','museums',       
'indoorpool','outdoorpool','hiking','gambling','boatswim','fishing',       
'golfing','boattours','rideducks','amusepark','minigolf','gocarting',     
'waterpark','circusworld','tbskishow','helicopter','horseride','standrock',     
'outattract','nearbyattract','movietheater','concerttheater','barpubdance',
'shopbroadway','bungeejumping']

# let's focus on activities data
dells_activities_data_frame_preliminary = \
    pd.DataFrame(wisconsin_dells_data_frame, columns = binary_variable_names)

# remove any records with missing data
dells_activities_data_frame = dells_activities_data_frame_preliminary.dropna()
print(dells_activities_data_frame.shape)

# use dictionary object for mapping the response/target variable
activity_to_binary = {'NO' : 0, 'YES' : 1, '': 0}
for iname in binary_variable_names:
    dells_activities_data_frame[iname] = \
        dells_activities_data_frame[iname].map(activity_to_binary)
print(dells_activities_data_frame[0:10])  # examine the first 10 rows of data    
 
# convert DataFrame to numpy array representation of activities matrix                   
activities_binary_matrix = dells_activities_data_frame.as_matrix().transpose() 
print(type(activities_binary_matrix))
print(activities_binary_matrix.shape)

# compute distance matrix
distance_matrix = manhattan_distances(activities_binary_matrix)
print(distance_matrix.shape)

# apply the multidimensional scaling algorithm and plot the map
mds_method = manifold.MDS(n_components = 2, random_state = 9999,\
    dissimilarity = 'precomputed')
mds_fit = mds_method.fit(distance_matrix)  
mds_coordinates = mds_method.fit_transform(distance_matrix) 
                                                                                                                                  
activity_names = ['Shopping', 'Antiquing',     
'Site Seeing', 'Fine Dining', 'Casual Dining', 
'Family Style Dining', 'Fast Food Dining', 'Museums',       
'Indoor Pool', 'Outdoor Pool', 'Hiking', 'Gambling', 
'Boating/Swimming', 'Fishing', 'Golfing', 'Boat Tours', 
'Ride the Ducks', 'Amusement Park', 'Minigolf', 'Go-carting',     
'Waterpark', 'Circus World', 'Tommy Bartlett Ski Show', 
'Helicopter Rides', 'Horseback Riding', 'Stand Rock',     
'Outdoor Attractions', 'Nearby Attractions', 
'Movie Theater', 'Concert Theater', 'Bar/Pub Dancing',
'Shop Broadway', 'Bungee Jumping']
   
# plot mds solution in two dimensions using activity labels
# defined by multidimensional scaling
plt.figure()
plt.scatter(mds_coordinates[:,0],mds_coordinates[:,1],\
    facecolors = 'none', edgecolors = 'none')  # points in white (invisible)
labels = activity_names
for label, x, y in zip(labels, mds_coordinates[:,0], mds_coordinates[:,1]):
    plt.annotate(label, (x,y), xycoords = 'data')
plt.xlabel('First Dimension')
plt.ylabel('Second Dimension')    
plt.show()
plt.savefig('fig_positioning_products_mds_dells_python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='landscape', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)          
    





