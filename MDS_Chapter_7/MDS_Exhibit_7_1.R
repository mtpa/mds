# Analysis for a Field Test of Laundry Soaps (R)

library(ggplot2)  # graphics for interaction plotting

# set contrast for tests of significance with factors
options(contrasts=c(factor = "contr.treatment", ordered="contr.poly"))

# first import data from the comma-delimited file gsoaps.csv
# the response variable relates to brand choice
# choice is factor/binary response variable (M or X)
# the explanatory variables are factors (categorical variables):
#   wtemp = water temperature with levels LOW OR HIGH
#   wtype = water type with levels SOFT, MEDIUM, OR HARD
#   muser = is user of brand M with labels NO OR YES

# read grouped frequecy data file
gsoaps <- read.csv("gsoaps.csv")

# convert to individual observations as required for logistic regression
indices <- rep(1:nrow(gsoaps),gsoaps$freq)
soaps <- gsoaps[indices,-1]
soaps <- data.frame(soaps, row.names=NULL)

# check the data frame
print(str(soaps))
print(head(soaps))
print(tail(soaps))
with(soaps, print(table(wtemp, wtype, muser, choice)))

# specify complete experimental design with interactions
soaps_model <- choice ~ muser * wtemp * wtype 
     
# fit the complete model     
soaps_fit <- glm(soaps_model, family = binomial, data=soaps)
     
# summary of fit
print(soaps_fit)

# analysis of deviance for experimental factors
# providing likelihood ratio chi-square tests of each effect
print(anova(soaps_fit, test="Chisq"))  

# -----------------------------
# Interaction Plotting
# -----------------------------
# code the choice as a 0/1 binary variable 1 = Brand X  
soaps$response <- ifelse((soaps$choice == "X"), 1, 0)

# compute choice share cell means for use in interaction plot
response_mean <- aggregate(response ~ muser * wtemp,
  data = soaps, mean)

# generate an interaction plot for brand X choice as a percentage
pdf(file = "fig_developing_new_products_soaps_interaction_plot.pdf", 
    width = 7, height = 5)
interaction_plot <- ggplot(response_mean, 
  aes(x = muser, y = 100*response, 
  group = wtemp, fill = wtemp)) +  
  geom_line(linetype = "solid", size = 1, colour = "black") +
  geom_point(size = 4, shape = 21) +
  ylab("Preference for Soap X (percentage)") +
  xlab("Currently Uses Soap M") +
  labs(fill = "Water Temperature") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
print(interaction_plot)
dev.off()

# write individual field test data to comma-delimited file
write.csv(soaps, file = "soaps.csv", row.names = FALSE)

# Suggestions for the student:
# Show that other effects in the experiment are of lesser
# importance by making additional interaction plots.
# Try alternative modeling methods for examining these
# field test data. Log-linear models are one possibility.
