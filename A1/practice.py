"""
#############################################################
## Stat 202A - Practice assignment
## Author: Pranav Thulasiram Bhat (704741684)
## Date : 09/22/2016
## Description: Practice completing a python script and
## uploading it to CCLE.
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names,
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to
## double-check your work, but MAKE SURE TO COMMENT OUT ALL
## OF YOUR EXAMPLES BEFORE SUBMITTING.
#############################################################
"""

import numpy as np

################
## Function 1 ##
################

def piFun(x):

    """ This function takes the input (x), multiplies it by
    3, then adds pi.
    .......
    INPUTS:
    x: A real number
    """

    #######################################
    ## FILL IN THE BODY OF THIS FUNCTION ##
    #######################################

    y = x * 3 + np.pi

    ## Function returns output y, a real number
    return(y)


################
## Function 2 ##
################

def cosFun(x):

    """ This function takes the cosine of the input (x), then adds 0.5 to the
    result.
    .......
    INPUTS:
    x: A vector of reals
    """

    #######################################
    ## FILL IN THE BODY OF THIS FUNCTION ##
    #######################################

    y = np.cos(x) + 0.5

    ## Function returns output y, a vector of reals
    return(y)


########################################################
## Optional examples (comment out before submitting!) ##
########################################################

# piFun(3)
# piFun(-2)
# cosFun([-10, 10])
