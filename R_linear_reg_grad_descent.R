factor_method = function(train_data, train_labels){
     cost <- function(train_data, train_labels, theta) {
          sum( (train_data %*% theta - train_labels)^2 ) / (2*length(train_labels))
     }

     # learning rate and iteration limit
     learning_rate <- 0.01
     num_runs <- 1000

     # keep history
     cost_mat <- double(num_runs)
     theta_mat <- list(num_runs)

     # initialize coefficients
     theta <- matrix(c(0,0), nrow=2)

     # add a column of 1's for the intercept coefficient
     train_data <- cbind(1, train_data)

     # gradient descent
     for (i in 1:num_runs) {
     error <- (train_data %*% theta - train_labels)
     gradient <- t(train_data) %*% error / length(train_labels)
     theta <- theta - learning_rate * gradient
     cost_mat[i] <- cost(train_data, train_labels, theta)
     theta_mat[[i]] <- theta
}