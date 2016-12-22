# setwd("Academic/Stats202a/Assignments/Final/")
source("Final_Project.R")

plotGraph <- function(acc_train, acc_test){
  par(mfrow=c(1,2))
  plot(
    1 : length(acc_train), 
    acc_train, 
    xlab='Iteration', 
    ylab='Train Accuracy', 
    xlim=c(0,400),
    ylim=c(0,1)
  )
  
  plot(
    1 : length(acc_test), 
    acc_test, 
    xlab='Iteration', 
    ylab='Test Accuarcy',
    xlim=c(0,400),
    ylim=c(0,1)
    )  
}

test_accuracy <- function(f){
  acc_train = NULL
  acc_test = NULL
  
  num_iter = 10
  for(i in 1 : num_iter){
    print(i)
    output = f(X_train, Y_train, X_test, Y_test)
    
    if(is.null(acc_train)){
      acc_train = rep(0, length(output$acc_train))
    }
    acc_train = acc_train + output$acc_train

    if(is.null(acc_test)){
      acc_test = rep(0, length(output$acc_test))
    }
    acc_test = acc_test + output$acc_test
  }
  
  plotGraph(acc_train / num_iter, acc_test / num_iter)
}

test_accuracy(myLogistic)

# output_NN       = my_NN(X_train, Y_train, X_test, Y_test)
# plotGraph(output_NN$acc_train, output_NN$acc_test)


# output_SVM      = my_SVM(X_train, Y_train, X_test, Y_test)
# plotGraph(output_SVM$acc_train, output_SVM$acc_tes)


# output_adaboost = myAdaboost(X_train, Y_train, X_test, Y_test)
# plotGraph(output_adaboost$acc_train, output_adaboost$acc_tes)

# output_logistic  = myLogistic(X_train, Y_train, X_test, Y_test)
# plotGraph(output_logistic$acc_train, output_logistic$acc_tes)