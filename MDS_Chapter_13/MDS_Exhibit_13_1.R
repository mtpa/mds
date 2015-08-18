# Restaurant Site Selection (R)

# bring packages into workspace
library(car)  # regression tools
library(lattice)  # needed for correlation _heat_map function

load("correlation_heat_map.RData")  # from R utility programs

# read data for Studenmund's Restaurants
# creating data frame restdata
restdata <- read.csv("studenmunds_restaurants.csv", header = TRUE)

# examine the data frame
print(str(restdata))
print(restdata)

# compute summary statistics
print(summary(restdata))

# exploratory data analysis... graphics for discovery 
# cumulative distribution function of the sales response
pdf(file = "fig_selecting_sites_hist.pdf", 
    width = 8.5, height = 8.5)
with(restdata, hist(sales/1000,
    xlab="Sales (thousands)",
    ylab="Frequency", 
    main = "", las = 1))
dev.off()

pdf(file = "fig_selecting_sites_cdf.pdf", 
    width = 8.5, height = 8.5)
with(restdata, plot(sort(sales/1000), 
    (1:length(sales))/length(sales),
    type="s", ylim=c(0,1), las = 1, 
    xlab="Restaurants Ordered by Sales (thousands)",
    ylab="Proportion of Restaurants with Lower Sales"))
dev.off()

# scatter plot matrix with simple linear regression
# models and lowess smooth fits for variable pairs
pdf(file = "fig_selecting_sites_scatter_plot_matrix.pdf", 
    width = 8.5, height = 8.5)
pairs(restdata,
    panel = function(x, y) {
        points(x, y)
        abline(lm(y ~ x), lty = "solid", col = "red")
        lines(lowess(x, y))
        }
    )
dev.off()     

# correlation heat map
pdf(file = "fig_selecting_sites_correlation_heat_map.pdf", 
    width = 8.5, height = 8.5)
restdata_cormat <- 
    cor(restdata[,c("sales","competition","population","income")])    
correlation_heat_map(cormat = restdata_cormat)
dev.off()

# specify regression model
restdata_model <- {sales ~ competition + population + income}

# fit linear regression model
restdata_fit <- lm(restdata_model, data = restdata)

# report fitted linear model
print(summary(restdata_fit))

# examine multicollinearity across explanatory variables
# ensure that all values are low (say, less than 4)
print(vif(restdata_fit))

# default residuals plots. . . diagnostic graphics 
pdf(file = "fig_selecting_sites_residuals.pdf", 
    width = 8.5, height = 8.5)
par(mfrow=c(2,2),mar=c(4, 4, 2, 2))
plot(restdata_fit)  
dev.off()

# define data frame of sites for new restaurants
sites <- data.frame(sales = c(NA, NA, NA),
    competition = c(2, 3, 5),
    population = c(50000, 200000, 220000),
    income = c(25000, 22000, 19000))

# obtain predicted sales for the new restaurants
# rounding to the nearest dollar
sites$predicted_sales <- round(predict(restdata_fit, newdata = sites),0)
print(sites)

# Suggestions for the student: Employ alternative methods of regression
# to predict sales response. Compare results with those obtained from
# ordinary least squares regression. Examine the out-of-sample predictive 
# power of models within a cross-validation framework.
# Having predicted sales for a cross-sectional/site selection problem,
# try a time series forecasting problem, working with one of the 
# cases provided for this purpose: Lydia Pinkham's Medicine Company or
# Wisconsin Lottery Sales.
