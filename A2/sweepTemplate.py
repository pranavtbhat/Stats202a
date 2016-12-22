"""
Stat 202A - Homework 1
Author: Pranav Thulasiram Bhat
Date : 10/01/2016
Description: This script implements the sweep operator as
well as Gauss-Jordan elimination in both plain and
vectorized form

INSTRUCTIONS: Please fill in the missing lines of code
only where specified. Do not change function names,
function inputs or outputs. You can add examples at the
end of the script to double-check your work, but MAKE
SURE TO COMMENT OUT ALL OF YOUR EXAMPLES BEFORE SUBMITTING.

Also, whenever I refer to matrix in this python code,
I am basically referring to an array (np.array)

Very important: Do not change the working directory in your code.
If you do, I will be unable to grade your
work since Python will attempt to change my working directory
to one that does not exist.
"""

# Import numpy so we can use mathematical objects
# If you need other libraries go ahead and load them as well
import numpy as np


""" Problem 1 """


""" Function 1: Sweep operation """


def mySweep(A, m):
    """
    Perform a SWEEP operation on A with the pivot element A[m,m].

    A: a square matrix.
    m: the pivot element is A[m, m].
    : Returns a swept matrix. Original matrix is unchanged.
    """

    # Copy matrix A into B so that A is unchangeed.
    B = np.copy(A)

    """ Fill in the body of this function! """
    n = B.shape[1]

    for k in range(0, m):
        for i in range(0, n):
            for j in range(0, n):
                # Sweep for i,j != k
                if i != k and j != k:
                    B[i, j] = B[i, j] - (B[i, k] * B[k, j]) / B[k, k]

        for i in range(0, n):
            # Sweep for i != k
            if i != k:
                B[i, k] = B[i, k] / B[k, k]

        for j in range(0, n):
            # Sweep for j != k
            if j != k:
                B[k, j] = B[k, j] / B[k, k]

        B[k, k] = -1 / B[k, k]

    """ Returns swept matrix B. Output should be an array / np.array /
    numpy.ndarray, basically an array of some sort"""
    return B


""" Function 2: Use sweep operation to find determinant """


def myDet(A):
    """
    Compute the determinant of A using the sweep operation.

    A: a square matrix.
    : Returns the determinant of A.
    """

    # Copy matrix A into B so that A is unchangeed.
    B = np.copy(A)

    """ Fill in the body of this function! """
    n = B.shape[1]
    det = 1

    for k in range(0, n):
        det = det * B[k, k]
        for i in range(0, n):
            for j in range(0, n):
                # Sweep for i,j != k
                if i != k and j != k:
                    B[i, j] = B[i, j] - (B[i, k] * B[k, j]) / B[k, k]

        for i in range(0, n):
            # Sweep for i != k
            if i != k:
                B[i, k] = B[i, k] / B[k, k]

        for j in range(0, n):
            # Sweep for j != k
            if j != k:
                B[k, j] = B[k, j] / B[k, k]

        B[k, k] = -1 / B[k, k]

    """ Returns the determinant of A. This should be a number,
     e.g. of class numpy.float """
    return(det)


""" Problem 2 """


""" Function 3: Elementwise version of Gauss Jordan """


def myGaussJordan(A, m):
    """ Perform Gauss Jordan elimination on A.
    A: a square matrix.
    m: Number of diagonal elements to loop through.
    """

    """FILL IN THE BODY OF THIS FUNCTION """
    n = A.shape[1]
    B = np.concatenate((A, np.identity(n)), axis=1)

    for k in range(0, m):
        a = B[k, k]

        # Divide kth row by a
        for j in range(0, n * 2):
            B[k, j] = B[k, j] / a

        # Remove constants in kth column
        for i in range(0, n):
            if i != k:
                b = B[i, k]
                for j in range(0, n * 2):
                    B[i, j] = B[i, j] - B[k, j] * b

    """ Function returns the matrix (aka array) B """
    return B


""" Function 4: Vectorized version of Gauss Jordan """


def myGaussJordanVec(A, m):
    """ Perform Gauss Jordan elimination on A.
    A: a square matrix.
    m: Number of diagonal elements to loop through.
    """

    """FILL IN THE BODY OF THIS FUNCTION """
    n = A.shape[1]
    B = np.concatenate((A, np.identity(n)), axis=1)

    for k in range(0, m):
        a = B[k, k]

        # Divide kth row by a
        B[k, :] = B[k, :] / a

        # Remove constants in kth column
        for i in range(0, n):
            if i != k:
                B[i, :] = B[i, :] - B[k, :] * B[i, k]

    """ Function returns the matrix (aka array) B """
    return B


""" Optional examples (comment out before submitting!) """
## A = np.array([[1, 2, 3], [7, 11, 13], [17, 21, 23]], dtype = float).T
