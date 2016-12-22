############################################################# 
## Stat 202A - Homework 4
## Author: 
## Date : 
## Description: This script implements stagewise regression
## (epsilon boosting)
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change the working directory
## in your code. If you do, I will be unable to grade your 
## work since Python will attempt to change my working directory
## to one that does not exist.
#############################################################

######################################
## Function 1: Stagewise regression ##
######################################

import numpy as np

def swRegression(X, Y, numIter = 3000, epsilon = 0.0001):
  
    # Perform stagewise regression (epsilon boosting) of Y on X
    # 
    # X: Matrix (np.array) of explanatory variables.
    # Y: Response vector (np.array)
    # numIter: Number of iterations ("T" in class notes)
    # epsilon: Update step size (should be small)
    #
    # Returns a matrix (np.array) containing the stepwise 
    # solution vector for each iteration

    #######################
    ## FILL IN WITH CODE ##
    #######################
    n, p = X.shape
    
    beta = np.zeros(p)
    db = np.zeros(p)
    
    beta_all = np.zeros((p,numIter))
    R = np.copy(Y)
    
    for t in range(numIter):
        for j in range(p):
            db[j] = sum(R * X[:,j])
        
        j = np.argmax(abs(db))
        beta[j] = beta[j] + db[j] * epsilon
        R = R - X[:,j] * db[j] * epsilon
        beta_all[:,t] = beta

    ## Function should output the matrix (np.array) beta_all, the 
    ## solution to the stagewise regression problem.
    ## beta_all is p x numIter
    return beta_all