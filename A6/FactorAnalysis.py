#############################################################
## Stat 202A - Homework 5
## Author: Pranav Thulasiram Bhat
## Date : 11/03/2016
## Description: This script implements factor analysis and
## matrix completion
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


def mySweep(A, m):
    """
    Perform a SWEEP operation on A with the pivot element A[m,m].

    :param A: a square matrix.
    :param m: the pivot element is A[m, m].
    :returns a swept matrix. Original matrix is unchanged.
    """

    ## No need to change anything here
    B = np.copy(A)
    n = B.shape[0]
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if i!=k and j!=k:
                    B[i,j] = B[i,j] - B[i,k]*B[k,j] / B[k,k]
        for i in range(n):
            if i!=k:
                 B[i,k] = B[i,k] / B[k,k]
        for j in range(n):
            if j!=k:
                B[k,j] = B[k,j] / B[k,k]
        B[k,k] = -1/B[k,k]

    return(B)


def factorAnalysis(n = 10, p = 5, d = 2, sigma = 1, nIter = 1000):

    """
    Perform factor analysis on simulated data.
    Simulate data X from the factor analysis model, e.g.
    X = Z_true * W.T + epsilon
    where W_true is p * d loading matrix (numpy array), Z_true is a n * d matrix
    (numpy array) of latent factors (assumed normal(0, I)), and epsilon is iid
    normal(0, sigma^2) noise. You can assume that W_true is normal(0, I)

    :param n: Sample size.
    :param p: Number of variables
    :param d: Number of latent factors
    :param sigma: Standard deviation of noise
    :param nIter: Number of iterations
    """

    ## FILL CODE HERE
    W_true = np.random.standard_normal(size=(p, d))
    Z_true = np.random.standard_normal(size=(d, n))
    epsilon = np.random.standard_normal(size=(p, n)) * sigma
    X = W_true.dot(Z_true) + epsilon

    sq = 1.
    XX = X.dot(X.T)
    w = np.random.standard_normal(size=(p, d)) * 0.1

    for it in range(nIter):
        A = np.vstack((
            np.hstack((w.T.dot(w) / sq + np.identity(d), w.T / sq)),
            np.hstack((w / sq, np.identity(p)))
        ))
        AS = mySweep(A, d)
        alpha = AS[:d, d:(d + p + 1)]
        D = -AS[:d, :d]
        Zh = alpha.dot(X)
        ZZ = Zh.dot(Zh.T) + D * n

        B = np.vstack((
            np.hstack((ZZ, Zh.dot(X.T))),
            np.hstack((X.dot(Zh.T), XX))
        ))
        BS = mySweep(B, d)
        w = BS[:d, d : (d + p + 1)].T
        sq = np.mean(np.diagonal(BS[d : (d + p + 1), d : (d + p + 1)])) / n

    ## Return the p * d np.array w, the estimate of the loading matrix
    return(w)


def matrixCompletion(n = 200, p = 100, d = 3, sigma = 0.1, nIter = 100,
                     prob = 0.2, lam = 0.1):

    """
    Perform matrix completion on simulated data.
    Simulate data X from the factor analysis model, e.g.
    X = Z_true * W.T + epsilon
    where W_true is p * d loading matrix (numpy array), Z_true is a n * d matrix
    (numpy array) of latent factors (assumed normal(0, I)), and epsilon is iid
    normal(0, sigma^2) noise. You can assume that W_true is normal(0, I)

    :param n: Sample size.
    :param p: Number of variables
    :param d: Number of latent factors
    :param sigma: Standard deviation of noise
    :param nIter: Number of iterations
    :param prob: Probability that an entry of the matrix X is not missing
    :param lam: Regularization parameter
    """

    ## FILL CODE HERE
    W_true = np.random.standard_normal(size=(p,d))
    Z_true = np.random.standard_normal(size=(d,n))
    epsilon = np.random.standard_normal(size=(p,n)) * sigma

    X = W_true.dot(Z_true) + epsilon
    R = np.random.uniform(size=(p,n)) < prob

    W = np.random.standard_normal(size=(p,d)) * 0.1
    Z = np.random.standard_normal(size=(d,n)) * 0.1

    for it in range(nIter):
        for i in range(n):
            WW = (W.T).dot(np.diag(R[:,i])).dot(W) + lam * np.identity(d)
            WX = (W.T).dot(np.diag(R[:,i])).dot(X[:,i])

            A = np.vstack((
                np.column_stack((WW, WX)),
                np.hstack((WX.T, 0))
            ))
            AS = mySweep(A, d)
            Z[:,i] = AS[:d, d]

        for j in range(p):
            ZZ = Z.dot(np.diag(R[j,:])).dot(Z.T) + lam * np.identity(d)
            ZX = Z.dot(np.diag(R[j,:])).dot(X[j,:])

            B = np.vstack((
                np.column_stack((ZZ, ZX)),
                np.hstack((ZX.T, 0))
            ))
            BS = mySweep(B, d)
            W[j,:] = BS[:d, d]

        sd1 = np.sqrt(np.sum(R * np.square(X-W.dot(Z))) / np.sum(R))
        sd0 = np.sqrt(np.sum((1.0 - R) * np.square(X - W.dot(Z))) / np.sum(1.0 - R))

    ## Return estimates of Z and W (both numpy arrays)
    return Z, W

###########################################################
### Optional examples (comment out before submitting!!) ###
###########################################################

# print(factorAnalysis())
# print(matrixCompletion())
