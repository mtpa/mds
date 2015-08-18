# Identifying Consumer Segments (R)

# call in R packages for use in this study
library(lattice)  # multivariate data visualization
library(vcd)  # data visualization for categorical variables
library(cluster)  # cluster analysis methods

# read bank data into R, creating data frame bank
# note that this is a semicolon-delimited file
bank <- read.csv("bank.csv", sep = ";", stringsAsFactors = FALSE)

# examine the structure of the bank data frame
print(str(bank))
print(head(bank))

print(table(bank$job , useNA = c("always")))
print(table(bank$marital , useNA = c("always")))
print(table(bank$education , useNA = c("always")))
print(table(bank$default , useNA = c("always")))
print(table(bank$housing , useNA = c("always")))
print(table(bank$loan , useNA = c("always")))

# Type of job (admin., unknown, unemployed, management,
# housemaid, entrepreneur, student, blue-collar, self-employed,
# retired, technician, services)
# put job into three major categories defining the factor variable jobtype
# the "unknown" category is how missing data were coded for job... 
# include these in "Other/Unknown" category/level
white_collar_list <- c("admin.","entrepreneur","management","self-employed")  
blue_collar_list <- c("blue-collar","services","technician")
bank$jobtype <- rep(3, length = nrow(bank))
bank$jobtype <- ifelse((bank$job %in% white_collar_list), 1, bank$jobtype) 
bank$jobtype <- ifelse((bank$job %in% blue_collar_list), 2, bank$jobtype) 
bank$jobtype <- factor(bank$jobtype, levels = c(1, 2, 3), 
    labels = c("White Collar", "Blue Collar", "Other/Unknown"))
with(bank, table(job, jobtype, useNA = c("always")))  # check definition   

# define binary indicator variables as numeric 0/1 variables
bank$whitecollar <- ifelse((bank$jobtype == "White Collar"), 1, 0)
bank$bluecollar <- ifelse((bank$jobtype == "Blue Collar"), 1, 0)
with(bank, print(table(whitecollar, bluecollar)))  # check definition
with(bank, print(table(jobtype)))  # check definition

# define factor variables with labels for plotting and binary factors
bank$marital <- factor(bank$marital, 
    labels = c("Divorced", "Married", "Single"))
    
# define binary indicator variables as numeric 0/1 variables
bank$divorced <- ifelse((bank$marital == "Divorced"), 1, 0)
bank$married <- ifelse((bank$marital == "Married"), 1, 0)    
with(bank, print(table(divorced, married)))  # check definition
with(bank, print(table(marital)))  # check definition    
        
bank$education <- factor(bank$education, 
    labels = c("Primary", "Secondary", "Tertiary", "Unknown"))
# define binary indicator variables as numeric 0/1 variables
bank$primary <- ifelse((bank$education == "Primary"), 1, 0)
bank$secondary <- ifelse((bank$education == "Secondary"), 1, 0)        
bank$tertiary <- ifelse((bank$education == "Tertiary"), 1, 0)     
with(bank, print(table(primary, secondary, tertiary)))  # check definition
with(bank, print(table(education)))  # check definition    
                
# client experience variables will not be useful for segmentation 
# but can be referred to after segments have been defined
bank$default <- factor(bank$default, labels = c("No", "Yes"))
bank$housing <- factor(bank$housing, labels = c("No", "Yes"))
bank$loan <- factor(bank$loan, labels = c("No", "Yes"))
bank$response <- factor(bank$response, labels = c("No", "Yes"))
    
# select subset of cases never perviously contacted by sales
# keeping variables needed for cluster analysis and post-analysis
bankfull <- subset(bank, subset = (previous == 0),
    select = c("response", "age", "jobtype", "marital", "education", 
               "default", "balance", "housing", "loan", 
               "whitecollar", "bluecollar", "divorced", "married",
               "primary", "secondary", "tertiary"))

# examine the structure of the full bank data frame
print(str(bankfull))
print(head(bankfull))

# select subset of variables for input to cluster analysis
data_for_clustering <- subset(bankfull,
    select = c("age", 
               "whitecollar", "bluecollar", 
               "divorced", "married",
               "primary", "secondary", "tertiary"))    

# -----------------------------------------------------
# clustering solutions (min_clusters to max_clusters)
# this step may take 10 minutes or more to complete
# -----------------------------------------------------
# set file for graphical output from the clustering solutions
pdf(file = "fig_finding_new_customers_cluster_search.pdf",
        width = 8.5, height = 11)
min_clusters <- 2
max_clusters <- 20
# evaluate alternative numbers of clusters/segments
# we use the average silhouette width as a statistical criterion
evaluation_vector <- NULL  # initialize evaluation vector 
# selected algorithm is pam (partitioning around medoids)
# with so many binary variables, manhattan distances seemed 
# to work better than Euclidean distances
for (number_of_clusters in min_clusters:max_clusters) {
    try_clustering <- pam(data_for_clustering, k = number_of_clusters,
        metric = "manhattan", stand = TRUE)
    evaluation_vector <- rbind(evaluation_vector,
        data.frame(number_of_clusters, 
            average_silhouette_width = 
                try_clustering$silinfo$avg.width))
    # show plot for this clustering solution
    plot(try_clustering)  # add this clustering solution to results file         
    }        
dev.off()  # close the pdf results file for the clustering solution    
# examine the cluster solution results, 
# look for average silhouette width > 0.5
# look for last big jump in average silhoutte width

print(evaluation_vector) 

# provide a single summary plot for the clustering solutions
pdf(file = "fig_finding_new_customers_cluster_summary.pdf",
        width = 8.5, height = 8.5)
with(evaluation_vector, plot(number_of_clusters, 
    average_silhouette_width))
dev.off()  # close summary results file

# -----------------------------------------------------
# select clustering solution and examine it
# -----------------------------------------------------
# examine the seven-cluster solution in more detail
seven_cluster_solution <- pam(data_for_clustering, k = 8,
        metric = "manhattan", stand = TRUE)
pdf(file = "fig_finding_new_customers_seven_cluster_solution.pdf",
    width = 8.5, height = 8.5)
plot(seven_cluster_solution)
dev.off()
# from the silhouette plot, the first five of the seven
# clusters appear to be large and well-defined

# add the cluster membership information and select first five
bankfull$cluster <- seven_cluster_solution$clustering
bankpart <- subset(bankfull, subset = (cluster < 6))
bankpart$cluster <- factor(bankpart$cluster,
    labels = c("A", "B", "C", "D", "E"))

# look at demographics across the clusters/segments
# -----------------
# age  Age in years
# -----------------
# examine relationship between age and response to promotion
with(bankpart, print(by(age, cluster, mean)))

pdf(file = "fig_finding_new_customers_age_lattice.pdf",     
    width = 8.5, height = 11)
lattice_plot_object <- histogram(~age | cluster, data = bankpart,
    type = "density", 
    xlab = "Age of Bank Client", layout = c(1,5))
print(lattice_plot_object)  # responders tend to be older
dev.off()
               
# -----------------------------------------------------------
# education
# Level of education (unknown, secondary, primary, tertiary)
# -----------------------------------------------------------
with(bankpart, print(table(cluster, education)))

# ---------------------------------------------------------------
# job status using jobtype
# White Collar: admin., entrepreneur, management, self-employed  
# Blue Collar: blue-collar, services, technician
# Other/Unknown
# ---------------------------------------------------------------
with(bankpart, print(table(cluster, jobtype)))

# ----------------------------------------------
# marital status
# Marital status (married, divorced, single)
# [Note: ``divorced'' means divorced or widowed]
# ----------------------------------------------
with(bankpart, print(table(cluster, marital)))

# look at bank client history across the clusters/segments

# -----------------------------------------
# default  Has credit in default? (yes, no)
# -----------------------------------------
with(bankpart, print(table(cluster, default)))

# ------------------------------------------
# balance  Average yearly balance (in Euros)
# ------------------------------------------
with(bankpart, print(by(balance, cluster, mean)))
pdf(file = "fig_finding_new_customers_blance_lattice.pdf",     
    width = 8.5, height = 11)
lattice_plot_object <- histogram(~balance | cluster, data = bankpart,
    type = "density", xlab = "Age of Bank Client", 
    layout = c(1,5))
print(lattice_plot_object)  # responders tend to be older
dev.off()

# ------------------------------------
# housing  Has housing loan? (yes, no)
# ------------------------------------
with(bankpart, print(table(cluster, housing)))

# ----------------------------------
# loan  Has personal loan? (yes, no)
# ----------------------------------
with(bankpart, print(table(cluster, loan)))

# ----------------------------------------------------
# response  Response to term deposit offer (yes, no)
# ----------------------------------------------------
with(bankpart, print(table(cluster, response)))
pdf(file = "fig_finding_new_customers_response_mosaic.pdf", 
    width = 8.5, height = 8.5)
mosaic( ~ response + cluster, data = bankpart,
  labeling_args = list(set_varnames = c(response = "Response to Term Deposit Offer", 
  cluster = "Segment Membership")),
  highlighting = "response",
  highlighting_fill = c("cornsilk","violet"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()

# compute percentage of yes responses to term deposit offer
response_table <- table(bankpart$cluster, bankpart$response)
cat("\nPercentage Responses\n")
for (i in 1:5) 
     cat("\n", toupper(letters[i]), 
         round(100 * response_table[i,2] / 
             sum(response_table[i,]), digits = 1))

# note the percentage of the customers receiving offers
# for the first time falling into each of the clusters/segments
# A = 1 ... E = 5 ...
print(round(100 * table(bankfull$cluster) / nrow(bankfull), digits = 1))

# Suggestions for the student:
# Try alternative clustering methods, using various
# distance measures and clustering algorithms.
# Try your hand at slecting a best clustering solution
# and interpreting the solution for bank managers.
# Could your clustering solution be useful in 
# target marketing?


