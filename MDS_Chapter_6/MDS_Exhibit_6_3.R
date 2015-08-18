# Multidimensional Scaling Demonstration: US Cities (R)

library(MASS) # need MDS functions

# We define a utility function for converting a distance structure 
# to a distance matrix as required for some routines and 
# for printing of the complete matrix for visual inspection.
make.distance.matrix <- function(distance.structure)
{ n <- attr(distance.structure, "Size")
  full <- matrix(0,n,n)
  full[lower.tri(full)] <- distance.structure
  full+t(full)
}

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

# We enter these into a distance structure as required for various 
# distance-based routines. That is, we enter the upper triangle 
# of the distance matrix as a single vector of distances.  
distance.structure <- 
    as.single(c(587, 1212,  701, 1936,  604,  748, 2139, 2182,  543,      
                      920,  940, 1745, 1188,  713, 1858, 1737,  597,     
                            878,  831, 1726, 1631,  949, 1021, 1494,     
                                 1374,  968, 1420, 1645, 1891, 1220,     
                                       2339, 2451,  347,  959, 2300,     
                                             1092, 2594, 2734,  923,    
                                                   2571, 2408,  205,     
                                                          678, 2442,     
                                                               2329))    
                                 
# We also provide a character vector of city names.
city.names <- c("Atlanta", "Chicago", "Denver", "Houston", "LA",
    "Miami","NY","SanFran","Seattle","WashDC")

attr(distance.structure, "Size") <- length(city.names)  # set size attribute 

# We can check to see that the distance structure has been entered correctly
# by converting the distance structure to a distance matrix 
# using the utility function make.distance.matrix, which we had defined
distance.matrix <- unlist(make.distance.matrix(distance.structure))
cat("\n","Distance Matrix for U.S. Airline Miles","\n")
print(distance.matrix)

# apply the multidimensional scaling algorithm and plot the map
mds.solution <- cmdscale(distance.structure, k=2, eig=T)
First.Dimension <- mds.solution$points[,1]
Second.Dimension <- mds.solution$points[,2]

pdf(file = "plot_metric_mds_airline_miles.pdf",
    width=11, height=8.5) # opens pdf plotting device

# use par(mar = c(bottom, left, top, right)) to set up margins on the plot 
par(mar=c(7.5, 7.5, 7.5, 5))

# We set up the plot but do not plot points... use names for points.
plot(First.Dimension,Second.Dimension,type="n") # first page of pdf plots
# We plot the city names in the locations where points normally go.
text(First.Dimension,Second.Dimension,labels=city.names,offset = 0.0)
title("Multidimensional Scaling of U.S. Airline Miles (First Draft)") 

# a review of the plot shows that the horizontal dimension should be reflected
# multiply the first dimension by -1 to get closer to desired map of the US
First.Dimension <- mds.solution$points[,1] * -1
Second.Dimension <- mds.solution$points[,2]
plot(First.Dimension,Second.Dimension,type="n")  # second page of pdf plots
text(First.Dimension,Second.Dimension,labels=city.names,offset = 0.0)
title("Multidimensional Scaling of U.S. Airline Miles (Second Draft)") 

# a review of the plot shows vertical dimension should also be reflected
# so we multiply the first and second dimensions by -1 
# this gives us the desired map of the US
First.Dimension <- mds.solution$points[,1] * -1
Second.Dimension <- mds.solution$points[,2] * -1
plot(First.Dimension,Second.Dimension,type="n")  # third page of pdf plots
text(First.Dimension,Second.Dimension,labels=city.names,offset = 0.0)
title("Multidimensional Scaling of U.S. Airline Miles") 
dev.off()  # closes the pdf plotting device

# Suggestions for the student: Try another geographic map.
# Try using distance in highway miles in place of distance in miles by air.
# Try non-metric MDS in place of metric MDS. See if there is a difference
# in the plotted solutions.