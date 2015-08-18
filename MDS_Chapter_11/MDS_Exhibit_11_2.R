# Analysis of Agent-Based Simulation (R)

# install necessary packages

library(ggplot2)  # for pretty plotting

# -------------------------------
# Simulation study background
# (same as under Python exhibit)
# -------------------------------
# note. it is possible to run NetLogo simulations within R 
# using the RNetLogo package from Thiele (2014)

# -----------------------------
# Analysis of Deviance
# -----------------------------
options(warn = -1)  # drop glm() warnings about non-integer responses

# read in summary results and code the experimental factors
virus <- read.csv("virus_results.csv")

# define factor variables
virus$Connectivity <- factor(virus$degree, 
  levels = c(3, 5), labels = c("LOW", "HIGH")) 
virus$Susceptibility <- factor(virus$spread, 
  levels = c(5, 10), labels = c("LOW", "HIGH"))
  
virus$Market_Share <- virus$infected

# show the mean proportions by cell in the 2x2 design
with(virus, print(by(Market_Share, Connectivity * Susceptibility, mean)))

# generalized linear model for response variable that is a proportion
virus_fit <- glm(Market_Share ~ Connectivity + Susceptibility + 
  Connectivity:Susceptibility,
  data = virus, family = binomial(link = "logit"))
print(summary(virus_fit))  

# analysis of deviance 
print(anova(virus_fit, test="Chisq"))  

# compute market share cell means for use in interaction plot
virus_means <- aggregate(Market_Share ~ Connectivity * Susceptibility,
  data = virus, mean)

# -----------------------------
# Interaction Plotting
# -----------------------------
# generate an interaction plot for market share as a percentage
interaction_plot <- ggplot(virus_means, 
  aes(x = Connectivity, y = 100*Market_Share, 
    group = Susceptibility, fill = Susceptibility)) +  
  geom_line(linetype = "solid", size = 1, colour = "black") +
  geom_point(size = 4, shape = 21) +
  ylab("Market Share (Percentage)") + 
  ggtitle("Network Interaction Effects in Innovation") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
print(interaction_plot)


