# Product Positioning of Movies (R)

library(MASS) # need MASS package for isoMDS() function for non-metric MDS

# We define a utility function for converting a distance structure 
# to a distance matrix as required for some routines and 
# for printing of the complete matrix for visual inspection.
make.distance_matrix <- function(distance_structure)
{ n <- attr(distance_structure, "Size")
  full <- matrix(0,n,n)
  full[lower.tri(full)] <- distance_structure
  full+t(full)
}

# These are the original data from one respondent
# Pairs of movies are judged on their similarity
# Smaller numbers are more similar to one another
# Zero on the diagonal means no difference
#  0  5  8  3  7     Bonnie and Clyde    
#  5  0  2  4  6     The Conversation
#  8  2  0  9  1     The French Connection
#  3  4  9  0 10     Hoosiers
#  7  6  1 10  0     Unforgiven

# We enter these into a distance structure as required for various 
# distance-based routines. That is, we enter the upper triangle 
# of the distance matrix as a single vector of distances.  
distance_structure <- as.single(c(5, 8, 3, 7,     
                                  2, 4, 6,    
                                  9, 1,  
                                  10))    
                                 
# We also provide a character vector of movie names.
movie.names <- c("Bonnie and Clyde", "The Conversation",
    "The French Connection", "Hoosiers", "Unforgiven")

attr(distance_structure, "Size") <- length(movie.names)  # set size attribute 

# We can check to see that the distance structure has been entered correctly
# by converting the distance structure to a distance matrix using the utility 
# function make.distance_matrix, which we had defined above.
distance_matrix <- unlist(make.distance_matrix(distance_structure))
cat("\n",  "Distance Matrix for Five Movies", "\n")
print(distance_matrix)

# apply the multidimensional scaling algorithm and plot the map
nonmetric.mds.solution <- isoMDS(distance_matrix, k=2, trace = FALSE)
First.Dimension <- nonmetric.mds.solution$points[,1]
Second.Dimension <- nonmetric.mds.solution$points[,2]

pdf(file = "fig_positioning_products_movies_mds.pdf",
    width = 8.5, height=8.5)  # opens pdf plotting device
# use par(mar = c(bottom, left, top, right)) to set up margins on the plot 
par(mar=c(7.5, 7.5, 7.5, 5))

# We set up the plot but do not plot points... we use names for points.
plot(First.Dimension,Second.Dimension,xlim=c(-6,6),ylim=c(-6,6),type="n") 
# We plot the movie names in the locations where points normally go.
text(First.Dimension, Second.Dimension, labels=movie.names, offset = 0.0)
dev.off()  # closes the pdf plotting device

# Suggestions for the student: Here is another set of movies. 
# These movies are similar in having simple two-word titles: "The ______."
# Again, pairs of movies are judged on their similarity.
# Candidate Godfather Hustler Passenger Stranger 
#  0  6  2  4  9     Candidate    
#  6  0  5  7 10     Godfather
#  2  5  0  3  8     Hustler
#  4  7  3  0  1     Passenger
#  9 10  8  1  0     Stranger
#
# Try your own set of five movies.
#
# Modify the program to accommodate larger sets of movies.


