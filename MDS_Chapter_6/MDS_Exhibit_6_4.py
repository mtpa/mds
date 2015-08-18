# Multidimensional Scaling Demonstration: US Cities (Python)

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

# These are the original data from a chart of U.S. airline miles.
# Atla Chic  Den Hous   LA Miam   NY SanF Seat WaDC
#    0  587 1212  701 1936  604  748 2139 2182  543     Atlanta    
#  587    0  920  940 1745 1188  713 1858 1737  597     Chicago
# 1212  920    0  878  831 1726 1631  949 1021 1494     Denver
#  701  940  878    0 1374  968 1420 1645 1891 1220     Houston
# 1936 1745  831 1374    0 2339 2451  347  959 2300     Los.Angeles
#  604 1188 1726  968 2339    0 1092 2594 2734  923     Miami
#  748  713 1631 1420 2451 1092    0 2571 2408  205     New.York
# 2139 1858  949 1645  347 2594 2571    0  678 2442     San.Francisco
# 2182 1737 1021 1891  959 2734 2408  678    0 2329     Seattle
#  543  597 1494 1220 2300  923  205 2442 2329    0     Washington.D.C.

# we enter these into a distance matrix for multidimensional scaling 
# defining a numpy array for these data
distance_matrix = \
np.array([[ 0,  587, 1212,  701, 1936,  604,  748, 2139, 2182,  543],         
    [587,    0,  920,  940, 1745, 1188,  713, 1858, 1737,  597],     
    [1212,  920,    0,  878,  831, 1726, 1631,  949, 1021, 1494],     
    [701,  940,  878,    0, 1374,  968, 1420, 1645, 1891, 1220],     
    [1936, 1745,  831, 1374,    0, 2339, 2451,  347,  959, 2300],     
    [604, 1188, 1726,  968, 2339,    0, 1092, 2594, 2734,  923],     
    [748,  713, 1631, 1420, 2451, 1092,    0, 2571, 2408,  205],     
    [2139, 1858,  949, 1645,  347, 2594, 2571,    0,  678, 2442],     
    [2182, 1737, 1021, 1891,  959, 2734, 2408,  678,    0, 2329],     
    [543,  597, 1494, 1220, 2300,  923,  205, 2442, 2329,    0]])
    
# check to see that the distance structure has been entered correctly
print(distance_matrix)    
print(type(distance_matrix))

# apply the multidimensional scaling algorithm and plot the map
mds_method = manifold.MDS(n_components = 2, random_state = 9999,\
    dissimilarity = 'precomputed')
mds_fit = mds_method.fit(distance_matrix)  
mds_coordinates = mds_method.fit_transform(distance_matrix) 
                                                                                                                                  
city_label = ['Atlanta', 'Chicago', 'Denver', 'Houston', 'Los Angeles',
    'Miami', 'New York', 'San Francisco', 'Seattle', 'Washington D.C.']
    
# plot mds solution in two dimensions using city labels
# defined by multidimensional scaling
plt.figure()
plt.scatter(mds_coordinates[:,0],mds_coordinates[:,1],\
    facecolors = 'none', edgecolors = 'none')  # points in white (invisible)
labels = city_label
for label, x, y in zip(labels, mds_coordinates[:,0], mds_coordinates[:,1]):
    plt.annotate(label, (x,y), xycoords = 'data')
plt.xlabel('First Dimension')
plt.ylabel('Second Dimension')    
plt.show()
plt.savefig('fig_positioning_products_mds_cities_python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='landscape', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)          
    





