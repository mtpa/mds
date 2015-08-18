# Product Positioning of Movies (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for multivariate analysis
import numpy as np  # arrays and numerical processing
import scipy
import matplotlib.pyplot as plt  # 2D plotting

# alternative distance metrics for multidimensional scaling
from sklearn.metrics import euclidean_distances 
from sklearn.metrics.pairwise import linear_kernel as cosine_distances
from sklearn.metrics.pairwise import manhattan_distances as manhattan_distances

from sklearn import manifold  # multidimensional scaling

# These are the original data from one respondent
# Pairs of movies are judged on their similarity
# Smaller numbers are more similar to one another
# Zero on the diagonal means no difference
#  0  5  8  3  7     Bonnie and Clyde    
#  5  0  2  4  6     The Conversation
#  8  2  0  9  1     The French Connection
#  3  4  9  0 10     Hoosiers
#  7  6  1 10  0     Unforgiven

# define a numpy array for these data
distance_matrix = np.array([[0,  5,  8,  3,  7],
    [5,  0,  2,  4,  6],
    [8,  2,  0,  9,  1],
    [3,  4,  9,  0, 10],
    [7,  6,  1, 10,  0]])
    
# check to see that the distance structure has been entered correctly
print(distance_matrix)    
print(type(distance_matrix))

# apply the multidimensional scaling algorithm and plot the map
mds_method = manifold.MDS(n_components = 2, random_state = 9999,\
    dissimilarity = 'precomputed')
mds_fit = mds_method.fit(distance_matrix)  
mds_coordinates = mds_method.fit_transform(distance_matrix) 
                                                                                                                                  
movie_label = ['Bonnie and Clyde', 'The Conversation',
    'The French Connection', 'Hoosiers', 'Unforgiven']
    
# plot mds solution in two dimensions using movie labels
# defined by multidimensional scaling
plt.figure()
plt.scatter(mds_coordinates[:,0],mds_coordinates[:,1],\
    facecolors = 'none', edgecolors = 'none')  # points in white (invisible)
labels = movie_label
for label, x, y in zip(labels, mds_coordinates[:,0], mds_coordinates[:,1]):
    plt.annotate(label, (x,y), xycoords = 'data')
plt.xlabel('First Dimension')
plt.ylabel('Second Dimension')    
plt.show()
plt.savefig('fig_positioning_products_mds_movies_python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='landscape', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)          
    





