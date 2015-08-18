# Defining and Visualizing a Small World Network (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# load package into the workspace for this program
import networkx as nx
import matplotlib.pyplot as plt  # 2D plotting
import numpy as np

# generate small-world network with n nodes
# k is number of nearby nodes to which each node is connected
# and p probability of rewiring from nearby node to random node
small_world = nx.watts_strogatz_graph(n = 100, k = 3, p = 0.25, seed = None)

# create an adjacency matrix object for the line network
# use nodelist argument to order the rows and columns
small_world_mat = nx.adjacency_matrix(small_world)
# print(small_world_mat)  # undirected networks are symmetric

# examine alternative layouts for plotting the small_world 
# plot the network/graph with default layout 
fig = plt.figure()
nx.draw_networkx(small_world, node_size = 200, node_color = 'yellow')
plt.show()

# spring layout
fig = plt.figure()
nx.draw_networkx(small_world, node_size = 200, node_color = 'yellow',\
    pos = nx.spring_layout(small_world))
plt.show()

# circlular layout
fig = plt.figure()
nx.draw_networkx(small_world, node_size = 200, node_color = 'yellow',\
    pos = nx.circular_layout(small_world))
plt.show()

# shell/concentric circles layout
fig = plt.figure()
nx.draw_networkx(small_world, node_size = 200, node_color = 'yellow',\
    pos = nx.shell_layout(small_world))
plt.show()

# Gephi provides interactive network plots 
# dump the graph object in GraphML format for input to Gephi
# and for initial network structure in pynetsim
nx.write_graphml(small_world,'small_world_network.graphml')
