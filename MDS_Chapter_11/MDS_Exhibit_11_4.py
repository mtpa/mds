# Analysis of Agent-Based Simulation (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function
from future_builtins import ascii, filter, hex, map, oct, zip

# import packages into the namespace for this program
import numpy as np
import pandas as pd
import statsmodels.api as sm

# -----------------------------
# Simulation study background
# -----------------------------
# an agent-based simulation was run with NetLogo, a public-domain
# program available from Northwestern University

# added one line of code to the Virus on a Network program:

if ticks = 200 [stop]

# this line was added to stop the simulation at exactly 200 ticks
# the line was added to the <to go> code block as shown here:
# 
# to go
#   if all? turtles [not infected?]
#     [ stop ]
#   ask turtles
#   [
#      set virus-check-timer virus-check-timer + 1
#      if virus-check-timer >= virus-check-frequency
#        [ set virus-check-timer 0 ]
#   ]
#   if ticks = 200 [stop]
#   spread-virus
#   do-virus-checks
#   tick
# end

# the simulation stops if no nodes/turtles were infected 
# or if the simulation reaches 200 ticks

# To see the results of the simulation at 200 ticks, we route the simulation
# world to a file using the GUI File/Export/Export World  
# this gives an a comma-delimited text file of the status of the network 
# at 200 ticks. Specifically, we enter the following Excel command into 
# cell D1 of the results spreadsheet to compute the proportion of nodes 
# infected:   = COUNTIF(N14:N163, TRUE)/M10

# NetLogo turtle infected status values were given in cells N14 through N163. 
# The detailed results of the simulation runs or trials are shown in the files 
# <trial01.csv> through <trial20.csv> under the directory NetLogo_results

# this particular experiment, has average connectivity or node degree 
# at 3 or 5 and the susceptibility or virus spread chance to 5 or 10 percent. 
# we have a completely crossed 2 x 2 design with 5 replications of each cell
# that is, we run each treatment combination 5 times, 20 independent 
# observations or trials. for each trial, we note the percentage of infected 
# nodes after 200 ticks---this is the response variable
# results are summarized in the comma-delimited file <virus_results.csv>. 

# -----------------------------
# Analysis of Deviance
# -----------------------------

# read in summary results and code the experimental factors
virus = pd.read_csv("virus_results.csv")

# check input DataFrame
print(virus)

Intercept = np.array([1] * len(virus))

# use dictionary object for mapping to 0/1 binary codes
degree_to_binary = {3 : 0, 5 : 1}
Connectivity = np.array(virus['degree'].map(degree_to_binary))

# use dictionary object for mapping to 0/1 binary codes
spread_to_binary = {5 : 0, 10 : 1}
Susceptibility = np.array(virus['spread'].map(spread_to_binary))

Connectivity_Susceptibility = Connectivity * Susceptibility

Design_Matrix = np.array([Intercept, Connectivity, Susceptibility, Connectivity_Susceptibility]).T

print(Design_Matrix)

Market_Share = np.array(virus['infected'])

# generalized linear model for a response variable that is a proportion
glm_binom = sm.GLM(Market_Share, Design_Matrix, family=sm.families.Binomial())
res = glm_binom.fit()
print(res.summary())