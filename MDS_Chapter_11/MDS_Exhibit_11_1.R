# Network Models and Measures (R)

# bring in packages we rely upon for work in predictive analytics
library(igraph)  # network/graph models and methods
library(lattice)  # statistical graphics

# load correlation heat map utility
load(file = "correlation_heat_map.RData")

# note. evcent() often produces warning about lack of convergence
# we will ignore these warnings in this demonstration program
options(warn = -1)

# number of iterations for the statistical simulations
NITER <- 100

# user-defined function to compute centrality index correlations
get_centrality_matrix <- function(graph_object) {
    adjacency_mat <- as.matrix(get.adjacency(graph_object))
    node_degree <- degree(graph_object)
    node_betweenness <- betweenness(graph_object)
    node_closeness <- closeness.estimate(graph_object, 
        mode = "all", cutoff = 0)
    node_evcent <- evcent(graph_object)$vector
    centrality <- cbind(node_degree, node_betweenness,
    node_closeness, node_evcent)
        colnames(centrality) <-  c("Degree", "Betweenness", 
            "Closeness", "Eigenvector")
    return(cor(centrality))
    }
       
# ---------------------------------
# Random Graphs   
# ---------------------------------
# show the plot of the first random graph model
set.seed(1)
# generate random graph with 50 nodes and 100 links/edges 
random_graph <- erdos.renyi.game(n = 50, type = "gnm", p.or.m = 100)  
pdf(file = "fig_network_random_graph.pdf", width = 5.5, height = 5.5)
plot(random_graph, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, edge.color = "black",
    layout = layout.circle, edge.curved = TRUE)  
dev.off()

# express adjacency matrix in standard matrix form
random_graph_mat <- as.matrix(get.adjacency(random_graph))

# verify that the network has one hundred links/edges
print(sum(degree(random_graph))/2)

aggregate_degree <- NULL  # initialize collection of node degree values
correlation_array <- array(NA, dim = c(4, 4, NITER))  # initialize array
for (i in 1:NITER) {
    set.seed(i)
    random_graph <- erdos.renyi.game(n = 50, type = "gnm", p.or.m = 100)  
    aggregate_degree <- c(aggregate_degree, 
        degree(random_graph))
    correlation_array[,,i] <- get_centrality_matrix(random_graph)
    }   
average_correlation <- matrix(NA, nrow = 4, ncol = 4, 
    dimnames = list(c("Degree", "Betweenness", "Closeness", "Eigenvector"), 
        c("Degree", "Betweenness", "Closeness", "Eigenvector")))  
for (i in 1:4) 
    for(j in 1:4)
        average_correlation[i, j] <- mean(correlation_array[i, j, ])

pdf(file = "fig_network_random_graph_heat_map.pdf", width = 11, 
  height = 8.5)  
correlation_heat_map(cormat = average_correlation)  
dev.off()    

# create data frame for node degree distribution
math_model <- rep("Random Graph", rep = length(aggregate_degree))
random_graph_degree_data_frame <- data.frame(math_model, aggregate_degree)

# ---------------------------------
# Small-World Networks
# ---------------------------------
# example of a small-world network (no random links)
set.seed(1)
# one-dimensional small-world model with 10 nodes, 
# links to additional adjacent nodes in a lattice 
# (nei = 1 implies degree = 2 for all nodes prior to rewiring)
# rewiring probability of 0.00... no rewiring
small_world_network_prelim <- watts.strogatz.game(dim = 1, size = 10, 
    nei = 1, p = 0.00, loops = FALSE, multiple = FALSE)
# remove any multiple links/edges    
small_world_network_prelim <- simplify(small_world_network_prelim)   
# express adjacency matrix in standard matrix form
# show that each node has four links
print(degree(small_world_network_prelim))
# verify that the network has one hundred links/edges
print(sum(degree(small_world_network_prelim))/2)

pdf(file = "fig_network_small_world_nei_1.pdf", width = 5.5, height = 5.5)
plot(small_world_network_prelim, vertex.size = 25, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, edge.color = "black",
    layout = layout.circle, edge.curved = TRUE)  
dev.off()

# another of a small-world network (no random links)
set.seed(1)
# one-dimensional small-world model with 10 nodes, 
# links to additional adjacent nodes in a lattice 
# (nei = 2 implies degree = 2 for all nodes prior to rewiring)
# rewiring probability of 0.00... no rewiring
small_world_network_prelim <- watts.strogatz.game(dim = 1, size = 10, 
    nei = 2, p = 0.00, loops = FALSE, multiple = FALSE)
# remove any multiple links/edges    
small_world_network_prelim <- simplify(small_world_network_prelim)   
# express adjacency matrix in standard matrix form
# show that each node has four links
print(degree(small_world_network_prelim))
# verify that the network has one hundred links/edges
print(sum(degree(small_world_network_prelim))/2)

pdf(file = "fig_network_small_world_nei_2.pdf", width = 5.5, height = 5.5)
plot(small_world_network_prelim, vertex.size = 25, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, edge.color = "black",
    layout = layout.circle, edge.curved = TRUE)  
dev.off()

# yet another of a small-world network (no random links)
set.seed(1)
# one-dimensional small-world model with 10 nodes, 
# links to additional adjacent nodes in a lattice 
# (nei = 3 implies degree = 6 for all nodes prior to rewiring)
# rewiring probability of 0.00... no rewiring
small_world_network_prelim <- watts.strogatz.game(dim = 1, size = 10, 
    nei = 3, p = 0.00, loops = FALSE, multiple = FALSE)
# remove any multiple links/edges    
small_world_network_prelim <- simplify(small_world_network_prelim)   
# express adjacency matrix in standard matrix form
# show that each node has four links
print(degree(small_world_network_prelim))
# verify that the network has one hundred links/edges
print(sum(degree(small_world_network_prelim))/2)

pdf(file = "fig_network_small_world_nei_3.pdf", width = 5.5, height = 5.5)
plot(small_world_network_prelim, vertex.size = 25, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, edge.color = "black",
    layout = layout.circle, edge.curved = TRUE)  
dev.off()


# rewire a selected proportion of the links to get small world model
set.seed(1)
small_world_network <- watts.strogatz.game(dim = 1, size = 50, nei = 2, 
    p = 0.2, loops = FALSE, multiple = FALSE)
# remove any multiple links/edges    
small_world_network <- simplify(small_world_network)   
# express adjacency matrix in standard matrix form
# show that each node has four links
print(degree(small_world_network))
# verify that the network has one hundred links/edges
print(sum(degree(small_world_network))/2)

pdf(file = "fig_network_small_world.pdf", width = 5.5, height = 5.5)
plot(small_world_network, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, edge.color = "black",
    layout = layout.circle, edge.curved = TRUE)  
dev.off()

aggregate_degree <- NULL  # initialize collection of node degree values
correlation_array <- array(NA, dim = c(4, 4, NITER))  # initialize array
for (i in 1:NITER) {
    set.seed(i)
    small_world_network <- watts.strogatz.game(dim = 1, size = 50, nei = 1, 
         p = 0.2, loops = FALSE, multiple = FALSE) 
    aggregate_degree <- c(aggregate_degree, 
        degree(small_world_network))
    correlation_array[,,i] <- get_centrality_matrix(small_world_network)
    }   
average_correlation <- matrix(NA, nrow = 4, ncol = 4, 
    dimnames = list(c("Degree", "Betweenness", "Closeness", "Eigenvector"), 
        c("Degree", "Betweenness", "Closeness", "Eigenvector")))  
for (i in 1:4) 
    for(j in 1:4)
        average_correlation[i, j] <- mean(correlation_array[i, j, ])

pdf(file = "fig_network_small_world_correlation_heat_map.pdf", width = 11, 
  height = 8.5)  
correlation_heat_map(cormat = average_correlation)  
dev.off()    

# create data frame for node degree distribution
math_model <- rep("Small-World Network", rep = length(aggregate_degree))
small_world_degree_data_frame <- data.frame(math_model, aggregate_degree)

# ---------------------------------
# Scale-Free Networks
# ---------------------------------
# show the plot of the first scale-free network model
set.seed(1)
# directed = FALSE to generate an undirected graph
# fifty nodes to be consistent with the models above
scale_free_network <- barabasi.game(n = 50, m = 2, directed = FALSE)
# remove any multiple links/edges    
scale_free_network <- simplify(scale_free_network)    
pdf(file = "fig_network_scale_free.pdf", width = 5.5, height = 5.5)
plot(scale_free_network, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, edge.color = "black",
    layout = layout.circle, edge.curved = TRUE)  
dev.off()

# express adjacency matrix in standard matrix form
scale_free_network_mat <- as.matrix(get.adjacency(scale_free_network))

# note that this model yields a graph with almost 100 links/edges
print(sum(degree(scale_free_network))/2)

aggregate_degree <- NULL  # initialize collection of node degree values
correlation_array <- array(NA, dim = c(4, 4, NITER))  # initialize array
for (i in 1:NITER) {
    set.seed(i)
    scale_free_network <- barabasi.game(n = 50, m = 2, directed = FALSE)
    # remove any multiple links/edges    
     scale_free_network <- simplify(scale_free_network)   
    aggregate_degree <- c(aggregate_degree, 
        degree(scale_free_network))
    correlation_array[,,i] <- get_centrality_matrix(scale_free_network)
    }   
average_correlation <- matrix(NA, nrow = 4, ncol = 4, 
    dimnames = list(c("Degree", "Betweenness", "Closeness", "Eigenvector"), 
        c("Degree", "Betweenness", "Closeness", "Eigenvector")))  
for (i in 1:4) 
    for(j in 1:4)
        average_correlation[i, j] <- mean(correlation_array[i, j, ])

pdf(file = "fig_network_scale_free_correlation_heat_map.pdf", width = 11, 
  height = 8.5)  
correlation_heat_map(cormat = average_correlation)  
dev.off()    

# create data frame for node degree distribution
math_model <- rep("Preferential Attachment Network", 
    rep = length(aggregate_degree))
scale_free_degree_data_frame <- data.frame(math_model, aggregate_degree)

# ---------------------------------
# Compare Degree Distributions
# ---------------------------------
plotting_data_frame <- rbind(scale_free_degree_data_frame,
    small_world_degree_data_frame, 
    random_graph_degree_data_frame)

# use lattice graphics to compare degree distributions  

pdf(file = "fig_network_model_degree_distributions.pdf", width = 8.5, 
  height = 11)  
lattice_object <- histogram(~aggregate_degree | math_model, 
    plotting_data_frame, type = "density", 
    xlab = "Node Degree", layout = c(1,3))
print(lattice_object)    
dev.off() 

# Suggestions for the student. 
# Experiment with the three models, varying the numbers of nodes
# and methods for constructing links between nodes. Try additional
# measures of centrality to see how they relate to the four measures
# we explored in this program.  Explore summary network measures
# of centrality and connectedness to see how they relate across 
# networks generated from random graph, preferential attachment,
# and small-world models.

