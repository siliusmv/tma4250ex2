
# Monte Carlo simulation of a Neumann-Scott model
clusterMC <- function(args){

  # lambda, sigma, area, childIntensity

  lambda_m <- args$lambda_m
  sigma <- args$sigma
  area <- args$area
  lambda_c <- args$lambda_c


  events <- NULL

  # Do we need to multiply lambda with size of area?????
  k_m <- rpois(n = 1, lambda = lambda_m)

  mothers <- matrix(0, k_m, 2)
  mothers[, 1] <- runif(k_m) * (area[2] - area[1]) + area[1]
  mothers[, 2] <- runif(k_m) * (area[4] - area[3]) + area[3]

  num_child <- rpois(n = k_m, lambda = lambda_c)

  cov_mat <- diag(sigma^2, 2)

  for(i in 1:k_m){
    for(j in 1:num_child[i]){
      x <- generateMultinormal(mu = mothers[i, ], cov_mat = cov_mat)
      if(isInside(x, area)){
        events <- rbind(events, t(x))
      }
    }
  }
  events <- list(x = events[, 1], y = events[, 2], area = area)
  return(events)
}


isInside <- function(x, area){
  x[1] > area[1] && x[1] < area[2] && x[2] > area[3] && x[2] < area[4]
}

# Generate samples from a multinormal distribution with
# mean mu and variance cov_mat
generateMultinormal <- function(mu, cov_mat){

  # Compute the Cholesky factor. Pivot in order to find cholesky
  # decomposition for positive semi definite matrix
  l <- chol(cov_mat, pivot = TRUE)
  pivot <- attr(l, "pivot")
  l <- l[, order(pivot)]

  # n samples from normal distribution N(mu, cov_mat)
  samples <- t(l) %*% rnorm(dim(cov_mat)[1]) + mu

  return(samples)

}
