

phiFunc <- function(phi_0, phi_1, tau_0, tau){

  # if(tau > tau_0){
  #   return(phi_0 * exp(-phi_1 * (tau - tau_0)))
  # }
  #
  # return(phi_0)

  nonpositive <- ifelse(test = (tau <= 0),
                    yes = TRUE,
                    no = FALSE)

  res <- ifelse(test = (nonpositive == FALSE),
                yes = phi_0,
                no = 0)

  res <- res * ifelse(test = (tau > tau_0),
                      yes = exp(-phi_1 * (tau - tau_0)),
                      no = 1)

  return(res)
}



repulsiveMC <- function(args){

  phi_0 <- args$phi_0
  phi_1 <- args$phi_1
  tau_0 <- args$tau_0
  k <- args$k
  area <- args$area
  acceptanceFunc <- args$acceptanceFunc
  num_iter <- args$num_iter
  jumps <- args$jumps

  data <- array(0, c(k, 2, (num_iter / jumps) + 1))

  events_mat <- matrix(0, k, 2)
  events_mat[, 1] <- runif(k) * (area[2] - area[1]) + area[1]
  events_mat[, 2] <- runif(k) * (area[4] - area[3]) + area[3]

  data[, , 1] <- events_mat

  for(i in 1:num_iter){

    u <- ceiling(runif(1) * k)

    x_new <- runif(1) * (area[2] - area[1]) + area[1]
    y_new <- runif(1) * (area[4] - area[3]) + area[3]

    alpha <- acceptanceFunc(events_mat = events_mat,
                            new_coords = c(x_new, y_new),
                            i = u,
                            k = k,
                            phi_0 = phi_0,
                            phi_1 = phi_1,
                            tau_0 = tau_0,
                            phiFunc = phiFunc)

    v <- runif(1)
    if(v < alpha){
      events_mat[u, ] <- c(x_new, y_new)
    }

    if(!(i %% jumps)){
      data[, , (i / jumps) + 1] <- events_mat
    }


  }

  return(data)
}



acceptanceRepulsive <- function(events_mat, new_coords, i, k, phi_0, phi_1, tau_0, phiFunc){

  old_coords <- events_mat[i, ]

  tau_new <- sqrt((new_coords[1] - events_mat[, 1])^2 +
                    (new_coords[2] - events_mat[, 2])^2)

  tau_old <- sqrt((old_coords[1] - events_mat[, 1])^2 +
                    (old_coords[2] - events_mat[, 2])^2)

  # Remove point nr. i
  tau_new <- tau_new[-i]
  tau_old <- tau_old[-i]


  new_sum <- sum(phiFunc(phi_0, phi_1, tau_0, tau_new))
  old_sum <- sum(phiFunc(phi_0, phi_1, tau_0, tau_old))

  # for(j in 1:k){
  #   if(j != i){
  #
  #     tau_new <- dist(rbind(new_coords, events_mat[j, ]))
  #     tau_old <- dist(rbind(old_coords, events_mat[j, ]))
  #
  #     new_sum <- new_sum + phiFunc(phi_0, phi_1, tau_0, tau_new)
  #     old_sum <- old_sum + phiFunc(phi_0, phi_1, tau_0, tau_old)
  #
  #   }
  # }

  # FEIL????
  log_res <- old_sum - new_sum

  if(log_res > 0){
    log_res <- 0
  }

  return(exp(log_res))

}


evaluateMinDist <- function(data){

  min_dist <- vector("numeric", dim(data)[3])

  for(i in 1:dim(data)[3]){
    min_dist[i] <- min(dist(data[, , i]))
  }

  return(min_dist)
}

evaluateMaxDist <- function(data){

  max_dist <- vector("numeric", dim(data)[3])

  for(i in 1:dim(data)[3]){
    max_dist[i] <- max(dist(data[, , i]))
  }

  return(max_dist)
}

evaluateMeanMinDist <- function(data){

  n <- dim(data)[1]
  mean_min_dist <- vector("numeric", dim(data)[3])

  for(i in 1:dim(data)[3]){
    distances <- as.matrix(dist(data[, , i])) + diag(Inf, n)
    min_distances <- apply(distances, 2, min)
    mean_min_dist[i] <- mean(min_distances)
  }

  return(mean_min_dist)
}

evaluateMeanMaxDist <- function(data){

  n <- dim(data)[1]
  mean_max_dist <- vector("numeric", dim(data)[3])

  for(i in 1:dim(data)[3]){
    distances <- as.matrix(dist(data[, , i])) + diag(-Inf, n)
    max_distances <- apply(distances, 2, max)
    mean_max_dist[i] <- mean(max_distances)
  }

  return(mean_max_dist)
}



testBurnIn <- function(evalFunc, args, num_test, title = NULL){

  eval_mat <- NULL

  for(i in 1:num_test){

    data <-  repulsiveMC(args)

    eval_vec <- evalFunc(data)

    eval_mini_mat <- cbind(eval_vec, 1:length(eval_vec)) %>% merge(as.character(i))

    eval_mat <- rbind(eval_mat, eval_mini_mat)

  }

  eval_mat <- data.frame(eval_mat)
  names(eval_mat) <- c("val", "iter", "run")

  gg <- ggplot(data = eval_mat) +
    geom_line(aes(y = val, x = iter, col = run)) +
    labs(x = "Sample nr.") +
    scale_color_discrete(guide = FALSE)

  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }

  gg
}


generateRepulsivePois <- function(args){

  all_data <- repulsiveMC(args)
  final_step <- all_data[, , dim(all_data)[3]]

  res <- list(x = final_step[, 1],
              y = final_step[, 2],
              area = args$area)

  return(res)
}
