# Predicting Commuter Transportation Choices (R)

library(lattice)  # multivariate data visualization

load("correlation_heat_map.RData")  # from R utility programs

# read data from comma-delimited text file... create data frame object
sydney <- read.csv("sydney.csv")
names(sydney) <- 
    c("Car_Time", "Car_Cost", "Train_Time", "Train_Cost", "Choice")
plotting_data_frame <- sydney[, 1:4]

# scatter plot matrix with simple linear regression
# models and lowess smooth fits for variable pairs
pdf(file = "fig_predicting_choice_scatter_plot_matrix.pdf", 
    width = 8.5, height = 8.5)
pairs(plotting_data_frame,
    panel = function(x, y) {
        points(x, y, cex = 0.5)
        abline(lm(y ~ x), lty = "solid", col = "red")
        lines(lowess(x, y))
        }
    )
dev.off()  

# correlation heat map for the explanatory variables
pdf(file = "fig_predicting_choice_correlation_heat_map.pdf",
    width = 8.5, height = 8.5)
sydney_cormat <- 
    cor(sydney[, c("Car_Time", "Car_Cost", "Train_Time", "Train_Cost")])
correlation_heat_map(sydney_cormat)
dev.off()

# specify and fit logistic regression model
sydney_model <- {Choice ~ Car_Time + Car_Cost + Train_Time + Train_Cost}
sydney_fit <- glm(sydney_model, family=binomial, data=sydney)
print(summary(sydney_fit))
print(anova(sydney_fit, test="Chisq"))

# compute predicted probability of taking the train 
sydney$Predict_Prob_TRAIN <- predict.glm(sydney_fit, type = "response") 

pdf(file = "fig_predicting_choice_density_evaluation.pdf", 
    width = 8.5, height = 8.5)
plotting_object <- densityplot( ~ Predict_Prob_TRAIN | Choice, 
               data = sydney, 
               layout = c(1,2), aspect=1, col = "darkblue", 
               plot.points = "rug",
               strip=function(...) strip.default(..., style=1),
               xlab="Predicted Probability of Taking Train") 
print(plotting_object) 
dev.off()

# predicted car-or-train choice using 0.5 cut-off
sydney$Predict_Choice <- ifelse((sydney$Predict_Prob_TRAIN > 0.5), 2, 1)
sydney$Predict_Choice <- factor(sydney$Predict_Choice,
    levels = c(1, 2), labels = c("CAR", "TRAIN"))
    
confusion_matrix <- table(sydney$Predict_Choice, sydney$Choice)
cat("\nConfusion Matrix (rows = Predicted Choice, columns = Actual Choice\n")
print(confusion_matrix)
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
                        sum(confusion_matrix)                                              
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))

# How much lower would train ticket prices have to be to increase
# public transportation usage (TRAIN) by 10 percent?
train_cost_vector <- 
     seq(min(sydney$Train_Cost), max(sydney$Train_Cost), length=1000)
beta.vector <- sydney_fit$coefficients
train_probability_vector <- numeric(1000)
for (i in 1:1000) {
       X.vector <- c(1, mean(sydney$Car_Time), mean(sydney$Train_Time),
           mean(sydney$Car_Cost), train_cost_vector[i])
       train_probability_vector[i] <- 
           exp(X.vector %*% beta.vector)/
               (1 + exp(X.vector %*% beta.vector))
       } 

# currently 150 out of 333 commuters (45 percent) use the train       
# determine price required for 55 percent of commuters to take the train 
# this is the desired quota set by public administrators
index <- 1  # beginning index for search
while (train_probability_vector[index] > 0.55) index <- index + 1
Solution_Price <- train_cost_vector[index]
cat("\nSolution Price: ", Solution_Price)

Current_Mean_Price <- mean(sydney$Train_Cost)

# how much do administrators need to lower prices?
# use greatest integer function to ensure quota is exceeded
Cents_Lower <- ceiling(Current_Mean_Price - Solution_Price)
cat("\nLower prices by ", Cents_Lower, "cents\n")
 
pdf(file = "fig_predicting_choice_ticket_price_solution.pdf", 
    width = 8.5, height = 8.5) 
plot(train_cost_vector, train_probability_vector,
     type="l",ylim=c(0,1.0), las = 1, 
     xlab="Cost of Taking the Train (in cents)",
     ylab="Estimated Probability of Taking the Train")

# plot current average train ticket price as vertical line     
abline(v = Current_Mean_Price, col = "red", lty = "solid", lwd = 2)    
abline(v = Solution_Price, col = "blue", lty = "dashed", lwd = 2)

legend("topright", legend = c("Current Mean Train Ticket Price", 
        paste("Solution Price (", Cents_Lower, " Cents Lower)", sep = "")), 
    col = c("red", "blue"), pch = c(NA, NA), lwd = c(2, 2),
    border = "black", lty = c("solid", "dashed"), cex = 1.25)
dev.off()       
       
# Suggestions for the student: 
# How much lower must train fares be to encourage more than 60 percent 
# of Sydney commuters to take the train? What about car costs? How much
# of a tax would public administrators have to impose in order to have
# a comparable effect to train ticket prices?
# Evaluate the logistic regression model in terms of its out-of-sample
# predictive accuracy (using multi-fold cross-validation, for example).
# Try alternative classification methods such as tree-structured 
# classification and support vector machines. Compare their predictive
# performance to that of logistic regression in terms of percentage
# of accurate prediction and other measures of classification performance.
       


