# Predicting Customer Retention (R)

library(lattice)  # lattice plot
library(vcd)  # mosaic plots
library(gam)  # generalized additive models for probability smooth
library(rpart)  # tree-structured modeling
library(e1071)  # support vector machines
library(randomForest)  # random forests
library(nnet)  # neural networks
library(rpart.plot)  # plot tree-structured model information
library(ROCR)  # ROC curve objects for binary classification 

# user-defined function for plotting ROC curve using ROC objects from ROCR
plot_roc <- function(train_roc, train_auc, test_roc, test_auc) {
    plot(train_roc, col = "blue", lty = "solid", main = "", lwd = 2,
        xlab = "False Positive Rate",
        ylab = "True Positive Rate")
        plot(test_roc, col = "red", lty = "dashed", lwd = 2, add = TRUE)
        abline(c(0,1))
       # Draw a legend.
       train.legend <- paste("Training AUC = ", round(train_auc, digits=3))
       test.legend <- paste("Test AUC =", round(test_auc, digits=3))
       legend("bottomright", legend = c(train.legend, test.legend),
           lty = c("solid", "dashed"), lwd = 2, col = c("blue", "red"))
    }       

# read in comma-delimited text file and create data frame
# there are blank character fields for missing data
# read them as character fields initially
att <- read.csv("att.csv", stringsAsFactors = FALSE)
print(str(att))

# convert blank character fields to missing data codes
att[att == ""] <- NA

# convert character fields to factor fields 
att$pick <- factor(att$pick)
att$income <- factor(att$income)
att$moves <- factor(att$moves)
att$age <- factor(att$age)
att$education <- factor(att$education)
att$employment <- factor(att$employment)
att$nonpub <- factor(att$nonpub)
att$reachout <- factor(att$reachout)
att$card <- factor(att$card)

# check revised structure of att data frame
print(str(att))

# select usage and AT&T marketing plan factors
attwork <- subset(att, select = c("pick", "usage", "reachout", "card"))
attwork <- na.omit(attwork)

# listwise case deletion for usage and marketing factors
attwork <- na.omit(attwork)
print(summary(attwork))

# provide overview of data
print(summary(att))

# -----------------
# usage and pick
# -----------------
# examine relationship between age and response to promotion
pdf(file = "fig_retaining_customers_usage_lattice.pdf", 
    width = 8.5, height = 8.5)
lattice_plot_object <- histogram(~usage | pick, data = att,
    type = "density", xlab = "Telephone Usage (Minutes per Month)", 
    layout = c(1,2))
print(lattice_plot_object)  # switchers tend to have lower usage
dev.off()

att_gam_model <- gam(pick == "OCC"  ~ s(usage), family=binomial,data=att) 

# probability smooth for usage and switching
pdf(file = "fig_retaining_customers_usage_probability_smooth.pdf", 
    width = 8.5, height = 8.5)
plot(att$usage, att$pick == "OCC", type="n", 
     ylim=c(-0.1,1.1), yaxt="n", 
     ylab="Estimated Probability of Switching", 
     xlab="Telephone Usage (Minutes per Month)") 
     axis(2, at=c(0,.5,1)) 
     points(jitter(att$usage), 
     att$pick=="OCC",pch="|") 
     o <- order(att$usage) 
     lines(att$usage[o],fitted(att_gam_model)[o]) 
dev.off()

# -----------------
# reachout and pick
# -----------------

# create a mosaic plot in using vcd package
pdf(file = "fig_retaining_customers_reachout_mosaic.pdf", 
    width = 8.5, height = 8.5)
mosaic( ~ pick + reachout, data = attwork,
  labeling_args = list(set_varnames = c(pick = "Service Provider Choice", 
  reachout = "AT&T Reach Out America Plan")),
  highlighting = "reachout",
  highlighting_fill = c("cornsilk","violet"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()

# create a mosaic plot in using vcd package
pdf(file = "fig_retaining_customers_card_mosaic.pdf", 
    width = 8.5, height = 8.5)
mosaic( ~ pick + card, data = attwork,
  labeling_args = list(set_varnames = c(pick = "Service Provider Choice", 
  card = "AT&T Credit Card")),
  highlighting = "card",
  highlighting_fill = c("cornsilk","violet"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()

# ----------------------------------
# fit logistic regression model 
# ----------------------------------
att_spec <- {pick ~ usage + reachout + card}
att_fit <- glm(att_spec, family=binomial, data=attwork)
print(summary(att_fit))
print(anova(att_fit, test="Chisq"))

# compute predicted probability of switching service providers 
attwork$Predict_Prob_Switching <- predict.glm(att_fit, type = "response") 

pdf(file = "fig_retaining_customers_log_reg_density_evaluation.pdf", 
    width = 8.5, height = 8.5)
plotting_object <- densityplot( ~ Predict_Prob_Switching | pick, 
               data = attwork, 
               layout = c(1,2), aspect=1, col = "darkblue", 
               plot.points = "rug",
               strip=function(...) strip.default(..., style=1),
               xlab="Predicted Probability of Switching") 
print(plotting_object) 
dev.off()

# use a 0.5 cut-off in this problem
attwork$Predict_Pick <- 
    ifelse((attwork$Predict_Prob_Switching > 0.5), 2, 1)
attwork$Predict_Pick <- factor(attwork$Predict_Pick,
    levels = c(1, 2), labels = c("AT&T", "OCC"))  
confusion_matrix <- table(attwork$Predict_Pick, attwork$pick)
cat("\nConfusion Matrix (rows=Predicted Service Provider,",
   "columns=Actual Service Provider\n")
print(confusion_matrix)
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
                        sum(confusion_matrix)                                              
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))
# mosaic rendering of the classifier with 0.10 cutoff
with(attwork, print(table(Predict_Pick, pick, useNA = c("always"))))
pdf(file = "fig_retaining_customers_confusion_mosaic_50_percent.pdf", 
    width = 8.5, height = 8.5)
mosaic( ~ Predict_Pick + pick, data = attwork,
  labeling_args = list(set_varnames = 
  c(Predict_Pick = 
      "Predicted Service Provider (50 percent cut-off)",
       pick = "Actual Service Provider")),
  highlighting = c("Predict_Pick", "pick"),
  highlighting_fill = c("green","cornsilk","cornsilk","green"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()


# -----------------------------------------
# example of tree-structured classification 
# -----------------------------------------
att_tree_fit <- rpart(att_spec, data = attwork, 
    control = rpart.control(cp = 0.0025))
# plot classification tree result from rpart
pdf(file = "fig_retaining_customers_tree_classifier.pdf", 
    width = 8.5, height = 8.5)
prp(att_tree_fit, main="",
    digits = 3,  # digits to display in terminal nodes
    nn = TRUE,  # display the node numbers
    branch = 0.5,  # change angle of branch lines
    branch.lwd = 2,  # width of branch lines
    faclen = 0,  # do not abbreviate factor levels
    trace = 1,  # print the automatically calculated cex
    shadow.col = 0,  # no shadows under the leaves
    branch.lty = 1,  # draw branches using dotted lines
    split.cex = 1.2,  # make the split text larger than the node text
    split.prefix = "is ",  # put "is" before split text
    split.suffix = "?",  # put "?" after split text
    split.box.col = "blue",  # lightgray split boxes (default is white)
    split.col = "white",  # color of text in split box 
    split.border.col = "blue",  # darkgray border on split boxes
    split.round = .25)  # round the split box corners a tad
dev.off()

# ---------------------------------------------
# example of random forest model for importance
# ---------------------------------------------
# fit random forest model to the training data
set.seed (9999)  # for reproducibility
attwork_rf_fit <- randomForest(att_spec, data = attwork, 
   mtry=3, importance=TRUE, na.action=na.omit) 
# check importance of the individual explanatory variables 
pdf(file = "fig_retaining_customers_random_forest_importance.pdf", 
width = 11, height = 8.5)
varImpPlot(attwork_rf_fit, main = "", pch = 20, cex = 1.25)
dev.off()

# ------------------------------------------------------------
# training-and-test for evaluating alternative modeling methods 
# ------------------------------------------------------------
set.seed(2020)
partition <- sample(nrow(attwork)) # permuted list of row index numbers
attwork$group <- ifelse((partition < nrow(attwork)/(3/2)),1,2)
attwork$group <- factor(attwork$group, levels=c(1,2), 
    labels=c("TRAIN","TEST"))
train <- subset(attwork, subset = (group == "TRAIN"), 
    select = c("pick", "usage", "reachout", "card"))
test <- subset(attwork, subset = (group == "TEST"), 
    select = c("pick", "usage", "reachout", "card"))
# ensure complete data in both partitions
train <- na.omit(train)
test <- na.omit(test)
# check partitions for no-overlap and correct pick frequencies
if(length(intersect(rownames(train), rownames(test))) != 0) 
    print("\nProblem with partition")  
print(table(attwork$pick))
print(table(test$pick)) 
print(table(train$pick))  

# --------------------------------------
# Logistic regression training-and-test
# auc = area under ROC curve
# --------------------------------------
# fit logistic regression model to the training set 
train_lr_fit <- glm(att_spec, family=binomial, data=train)
train$lr_predict_prob <- predict(train_lr_fit, type = "response")
train_lr_prediction <- prediction(train$lr_predict_prob, train$pick)
train_lr_auc <- as.numeric(performance(train_lr_prediction, "auc")@y.values)
# use model fit to training set to evaluate on test data
test$lr_predict_prob <- as.numeric(predict(train_lr_fit, newdata = test, 
    type = "response"))
test_lr_prediction <- prediction(test$lr_predict_prob, test$pick)
test_lr_auc <- as.numeric(performance(test_lr_prediction, "auc")@y.values)

# ----------------------------------------
# ROC for logistic regression
# ----------------------------------------
train_lr_roc <- performance(train_lr_prediction, "tpr", "fpr")
test_lr_roc <- performance(test_lr_prediction, "tpr", "fpr")
pdf(file = "fig_retaining_customers_logistic_regression_roc.pdf", 
    width = 8.5, height = 8.5)
plot_roc(train_roc = train_lr_roc, 
    train_auc = train_lr_auc, 
    test_roc = test_lr_roc, 
    test_auc = test_lr_auc)       
dev.off()    
    
# ----------------------------------------
# Support vector machine training-and-test
# ----------------------------------------
set.seed (9999)  # for reproducibility
# determine tuning parameters prior to fitting model to training set
train_svm_tune <- tune(svm, att_spec, data = train,
                   ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
                   tunecontrol = tune.control(sampling = "fix"))
# fit the support vector machine to the training set using tuning parameters
train_svm_fit <- svm(att_spec, data = train, 
    cost = train_svm_tune$best.parameters$cost, 
    gamma=train_svm_tune$best.parameters$gamma, 
    probability = TRUE)  
train_svm_predict <- predict(train_svm_fit, train, probability = TRUE)
train$svm_predict_prob <- attr(train_svm_predict, "probabilities")[,1]
train_svm_prediction <- prediction(train$svm_predict_prob, train$pick)
train_svm_auc <- as.numeric(performance(train_svm_prediction, "auc")@y.values)
# use model fit to training data to evaluate on test set
test_svm_predict <- predict(train_svm_fit, test, probability = TRUE)
test$svm_predict_prob <- attr(test_svm_predict, "probabilities")[,1]
test_svm_prediction <- prediction(test$svm_predict_prob, test$pick)
test_svm_auc <- as.numeric(performance(test_svm_prediction, "auc")@y.values)

# --------------------------------------------
# ROC for support vector machines
# --------------------------------------------
train_svm_roc <- performance(train_svm_prediction, "tpr", "fpr")
test_svm_roc <- performance(test_svm_prediction, "tpr", "fpr")
pdf(file = "fig_retaining_customers_support_vector_machine_roc.pdf", 
    width = 8.5, height = 8.5)
plot_roc(train_roc = train_svm_roc, 
    train_auc = train_svm_auc, 
    test_roc = test_svm_roc, 
    test_auc = test_svm_auc)    
dev.off()

# ----------------------------------------
# Random forest training-and-test
# ----------------------------------------
set.seed (9999)  # for reproducibility
train_rf_fit <- randomForest(att_spec, data = train, 
   mtry=3, importance=FALSE, na.action=na.omit) 
train$rf_predict_prob <- as.numeric(predict(train_rf_fit, type = "prob")[,2])
train_rf_prediction <- prediction(train$rf_predict_prob, train$pick)
train_rf_auc <- as.numeric(performance(train_rf_prediction, "auc")@y.values)
# use model fit to training set to evaluate on test data
test$rf_predict_prob <- as.numeric(predict(train_rf_fit, newdata = test, 
    type = "prob")[,2])
test_rf_prediction <- prediction(test$rf_predict_prob, test$pick)
test_rf_auc <- as.numeric(performance(test_rf_prediction, "auc")@y.values)

# --------------------------------------------
# ROC for random forest
# --------------------------------------------
train_rf_roc <- performance(train_rf_prediction, "tpr", "fpr")
test_rf_roc <- performance(test_rf_prediction, "tpr", "fpr")
pdf(file = "fig_retaining_customers_random_forest_roc.pdf", 
    width = 8.5, height = 8.5)
plot_roc(train_roc = train_rf_roc, 
    train_auc = train_rf_auc, 
    test_roc = test_rf_roc, 
    test_auc = test_rf_auc)  
dev.off()

# ----------------------------------------
# Naive Bayes training-and-test
# ----------------------------------------
set.seed (9999)  # for reproducibility
train_nb_fit <- naiveBayes(att_spec, data = train) 
train$nb_predict_prob <- as.numeric(predict(train_nb_fit, newdata = train,
    type = "raw")[,2])
train_nb_prediction <- prediction(train$nb_predict_prob, train$pick)
train_nb_auc <- as.numeric(performance(train_nb_prediction, "auc")@y.values)
# use model fit to training set to evaluate on test data
test$nb_predict_prob <- as.numeric(predict(train_nb_fit, newdata = test, 
    type = "raw")[,2])
test_nb_prediction <- prediction(test$nb_predict_prob, test$pick)
test_nb_auc <- as.numeric(performance(test_nb_prediction, "auc")@y.values)

# --------------------------------------------
# ROC for Naive Bayes
# --------------------------------------------
train_nb_roc <- performance(train_nb_prediction, "tpr", "fpr")
test_nb_roc <- performance(test_nb_prediction, "tpr", "fpr")
pdf(file = "fig_retaining_customers_naive_bayes_roc.pdf", 
    width = 8.5, height = 8.5)
plot_roc(train_roc = train_nb_roc, 
    train_auc = train_nb_auc, 
    test_roc = test_nb_roc, 
    test_auc = test_nb_auc)    
dev.off()

# ----------------------------------------
# Neural Network training-and-test
# ----------------------------------------
set.seed (9999)  # for reproducibility
train_nnet_fit <- nnet(att_spec, data = train, size = 3, decay = 0.0,
    probability = TRUE, trace = FALSE) 
train$nnet_predict_prob <- as.numeric(predict(train_nnet_fit, newdata = train))
train_nnet_prediction <- prediction(train$nnet_predict_prob, train$pick)
train_nnet_auc <- as.numeric(performance(train_nnet_prediction, "auc")@y.values)
# use model fit to training set to evaluate on test data
test$nnet_predict_prob <- as.numeric(predict(train_nnet_fit, newdata = test))
test_nnet_prediction <- prediction(test$nnet_predict_prob, test$pick)
test_nnet_auc <- as.numeric(performance(test_nnet_prediction, "auc")@y.values)

# --------------------------------------------
# ROC for Neural Network
# --------------------------------------------
train_nnet_roc <- performance(train_nnet_prediction, "tpr", "fpr")
test_nnet_roc <- performance(test_nnet_prediction, "tpr", "fpr")
pdf(file = "fig_retaining_customers_neural_network_roc.pdf", 
    width = 8.5, height = 8.5)
plot_roc(train_roc = train_nnet_roc, train_auc = train_nnet_auc, 
    test_roc = test_nnet_roc, test_auc = test_nnet_auc)   
dev.off()

# Suggestions for the student: Build a more complete model for customer
# retention using additional information in the AT&T Choice Study.
# Take care to deal with missing data issues in customer demographics, 
# perhaps using technologies for missing data imputation. 
# Employ additional classification methods and evaluate them using
# multi-fold cross-validation, another training-and-test regimen.

