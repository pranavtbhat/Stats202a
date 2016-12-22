/*
####################################################
## Stat 202A - Homework 6
## Author: 
## Date : 
## Description: This script implements sweep and QR
## operations in Rcpp
####################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
 */ 

# include <Rcpp.h>
using namespace Rcpp;


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
Sign function for later use 
~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export]]
double signC(double d){
  return d<0?-1:d>0? 1:0;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
Problem 1: Sweep operator 
~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export]]
NumericMatrix mySweepC(const NumericMatrix B, int m){
  
  // See R code from previous assignment for description
  // of inputs and outputs. Note the "const" in front 
  // of NumericMatrix B; this is so you don't accidentally
  // change B inside your code.
  
  /* Fill in code below */
  
  
  
  
  // Return swept matrix A
  return(A);
  
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Problem 2: QR decomposition 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */  


// [[Rcpp::export]]
List myQRC(const NumericMatrix A){ 
  
  // A is the input matrix with dimension n x m.
  
  /* Fill in code below */
  
  
  
  
  // Return a list with two named objects, Q and R
  output["Q"] = Q.t();
  output["R"] = R;
  return(output);
  
  
}