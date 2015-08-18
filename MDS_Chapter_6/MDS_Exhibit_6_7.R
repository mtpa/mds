# Hierarchical Clustering of Activities (R)

library(cluster)  # cluster analysis algorithms 

# We define a utility function for converting a distance structure. 

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

# We compute the distance structure using the dist() function. 
distance_structure <- dist(binary_activity_matrix, method="binary")

# To see this structure in the form of a distance matrix 
# we can run the utility function for converting a distance structure 
# to a distance matrix, which is make.distance_matrix
distance_matrix <- unlist(make.distance_matrix(distance_structure))
print(str(distance_matrix))

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

rownames(distance_matrix) <- activity_names
colnames(distance_matrix) <- activity_names
print(str(distance_matrix))

clustering_solution <- agnes(distance_matrix, diss = TRUE, 
    metric = "manhattan", method = "average") 

pdf(file = "fig_positioning_products_dells_clustering.pdf",
  width = 11, height = 8.5)
plot(clustering_solution)
dev.off()

# Suggestions for the student:
# Try other clustering solutions by varying the distance metric
# and hierarchical clustering algorithms.