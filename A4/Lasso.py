#############################################################
## Stat 202A - Homework 3
## Author:
## Date :
## Description: This script implements the lasso
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names,
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to
## double-check your work, but MAKE SURE TO COMMENT OUT ALL
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change the working directory anywhere
## in your code. If you do, I will be unable to grade your
## work since Python will attempt to change my working directory
## to one that does not exist.
#############################################################

import numpy as np

#####################################
## Function 1: Lasso solution path ##
#####################################

def myLasso(X, Y, lambda_all):

    # Find the lasso solution path for various values of
    # the regularization parameter lambda.
    #
    # X: Array of explanatory variables.
    # Y: Response array
    # lambda_all: Array of regularization parameters. Make sure
    # to sort lambda_all in decreasing order for efficiency.
    #
    # Returns an array containing the lasso solution
    # beta for each regularization parameter.
    
    # reverse sort lambda_all
    lambda_all = sorted(lambda_all, reverse=True)
    
    p = X.shape[1]
    L = len(lambda_all)
    
    # Constants
    T = 10
    

    # Beta
    beta = np.zeros(p)
    beta_all = np.zeros((p,L))

    R = np.copy(Y)
    ss = np.zeros(p)
    for j in range(p):
        ss[j] = sum(np.square(X[:,j]))
    
    for l in range(L):
        lam = lambda_all[l]
    
        for t in range(T):
            for j in range(p):
                db = sum(R * X[:,j]) / ss[j]
                b = beta[j] + db
                b = np.sign(b) * max(0, abs(b) - lam / ss[j])
                db = b - beta[j]
                R = R - X[:,j] * db
                beta[j] = b
                
        beta_all[:,l] = beta

    ## Function should output the array beta_all, the
    ## solution to the lasso regression problem for all
    ## the regularization parameters.
    ## beta_all is p x length(lambda_all)
    return(beta_all)