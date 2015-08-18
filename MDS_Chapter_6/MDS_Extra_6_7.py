# Hierarchical Clustering of Activities (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for multivariate analysis
import numpy as np  # arrays and numerical processing
import matplotlib.pyplot as plt  # 2D plotting
import pandas as pd  # DataFrame work
from scipy.cluster.hierarchy import ward, dendrogram  # clustering

# alternative distance metrics for multidimensional scaling
from sklearn.metrics import euclidean_distances 
from sklearn.metrics.pairwise import linear_kernel as cosine_distances
from sklearn.metrics.pairwise import manhattan_distances as manhattan_distances

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

linkage_matrix = ward(distance_matrix) 
fig, ax = plt.subplots(figsize=(15, 20)) # set size
ax = dendrogram(linkage_matrix, orientation="right", labels=activity_names)

plt.tick_params(\
    axis = 'x',          # changes apply to the x-axis
    which = 'both',      # both major and minor ticks are affected
    bottom = 'off',      # ticks along the bottom edge are off
    top = 'off',         # ticks along the top edge are off
    labelbottom = 'off')

plt.tight_layout()  # show plot with tight layout

# route figure to external file
plt.savefig('plot_hierarchical_clustering_solution.png', dpi = 200) 