# Using Activities Market Baskets for Product Positioning (R)

library(MASS)  # MASS package for isoMDS() function for non-metric MDS
library(cluster)  # cluster analysis algorithms 
library(wordcloud)  # provides function textplot() for non-overlapping text

# We define a utility function for converting a distance structure 
# to a distance matrix as required for some routines and 
# for printing of the complete matrix for visual inspection.
make.distance_matrix <- function(distance_structure)
{ n <- attr(distance_structure, "Size")
  full <- matrix(0,n,n)
  full[lower.tri(full)] <- distance_structure
  full+t(full)
}

wisconsin_dells_data_frame <- read.csv("wisconsin_dells.csv") # read in data 
print(str(wisconsin_dells_data_frame))  # show structure of data frame

binary_variable_names <- c("shopping","antiquing",     
"scenery","eatfine","eatcasual","eatfamstyle","eatfastfood","museums",       
"indoorpool","outdoorpool","hiking","gambling","boatswim","fishing",       
"golfing","boattours","rideducks","amusepark","minigolf","gocarting",     
"waterpark","circusworld","tbskishow","helicopter","horseride","standrock",     
"outattract","nearbyattract","movietheater","concerttheater","barpubdance",
"shopbroadway","bungeejumping")

binary_activity_data_frame <- 
    wisconsin_dells_data_frame[,binary_variable_names]

# ifelse() converts YES/NO to 1/0 as needed for distance calculations
# then we form a matrix and take the transpose of the matrix
# which will be needed to compute distances between activities
binary_activity_matrix <- 
    t(as.matrix(ifelse((binary_activity_data_frame == "YES"), 1, 0)))

# We compute the distance structure using the dist() function 
# We specify that we want to use a distance metric for binary input data
# this takes the number of occurrences of zero and one or one and zero 
# divided by the number of times at least one variable has a one
# The distance structure is for pairs of activities in this case study.
# A distance structure is needed for many distance-guided routines.
distance_structure <- dist(binary_activity_matrix, method="binary")

# To see this structure in the form of a distance matrix 
# we can run the utility function for converting a distance structure 
# to a distance matrix, which is make.distance_matrix
distance_matrix <- unlist(make.distance_matrix(distance_structure))
cat("\n", "Distance Matrix for Activities","\n")
print(str(distance_matrix))

# use par(mar = c(bottom, left, top, right)) to set up margins on the plot 
par(mar=c(7.5, 7.5, 7.5, 5))

# The non-metric solution for this case was not especially useful,
# so we use metric multidimensional scaling for this problem.
mds_solution <- cmdscale(distance_structure, k=2, eig=T)
First_Dimension <- mds_solution$points[,1]
Second_Dimension <- mds_solution$points[,2]
plot(First_Dimension,Second_Dimension, type="n")
# setting xpd = TRUE allows text labels to extend beyond plotting region
text(First_Dimension, Second_Dimension, labels=binary_variable_names,
    offset=0, xpd=TRUE)

# let's plot all of the activities on a single plot using the
# textplot() utility function from the wordcloud package
# to avoid overlapping text strings... and for presentation to
# management, let's spell out the activities by name
activity_names <- c("Shopping", "Antiquing",     
"Site Seeing", "Fine Dining", "Casual Dining", 
"Family Style Dining", "Fast Food Dining", "Museums",       
"Indoor Pool", "Outdoor Pool", "Hiking", "Gambling", 
"Boating/Swimming", "Fishing", "Golfing", "Boat Tours", 
"Ride the Ducks", "Amusement Park", "Minigolf", "Go-carting",     
"Waterpark", "Circus World", "Tommy Bartlett Ski Show", 
"Helicopter Rides", "Horseback Riding", "Stand Rock",     
"Outdoor Attractions", "Nearby Attractions", 
"Movie Theater", "Concert Theater", "Bar/Pub Dancing",
"Shop Broadway", "Bungee Jumping")

pdf(file = "fig_positioning_products_dells_mds.pdf",
    width=8.5,height=8.5) # opens pdf plotting device
textplot(x = mds_solution$points[,1], 
    y = mds_solution$points[,2],
    words = activity_names, 
    show.lines = FALSE,
    xlim = range(mds_solution$points[,1]), 
    ylim = range(mds_solution$points[,2]),
    xlab = "First Dimension",
    ylab = "Second Dimension")
dev.off()  # closes the pdf plotting device

# Suggestions for the student:
# Try alternative distance metrics to see how these change
# the multidimensional solution. Try reflection of the map
# and roation of the map to see if the resulting visualization
# may be more useful for management. Try nom-metric as well
# as metric MDS to see how the solutions compare.
# Having identified bungee jumping as an entertainment
# activity distinct from the set of other Dells activities,
# develop a classification model from visitor group demographics
# to find/target the bungee jumpers. Try alternative methods
# for this, including traditional and machine learning methods.

