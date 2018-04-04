# Plot a point process
plotProcess <- function(data, title = NULL, jitter = FALSE){

  dat <- tibble(x = data$x, y = data$y)

  gg <- ggplot(data = dat, aes(x = x, y = y))

  if(jitter){
    gg <- gg + geom_jitter(width = 0.03, height = 0.03)
  } else{
    gg <- gg + geom_point()
  }

  if(!is.null(data$area)){
    gg <- gg +
      xlim(data$area["xl"], data$area["xu"]) +
      ylim(data$area["yl"], data$area["yu"])
  }

  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }

  return(gg)
}

# Plot an L-function along with the dashed line y = x
plotLFunc <- function(data, title = NULL, add_line = TRUE){

  dat <- tibble(x = data$x, y = data$y)

  gg <- ggplot(data = dat) +
    geom_line(aes(x = x, y = y)) +
    labs(x = "t", y = "L")
  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }
  if(add_line){
    line_x <- ggplot_build(gg)$data[[1]]$x
    gg <- gg +
      geom_line(aes(x = line_x, y = line_x), linetype = "dashed")
  }

  return(gg)
}

# Simulate realisations from some model and compute confidence-interval for the
# L-function. Display along with the empirical L-function of some point process
testModel <- function(data, num_real = 100, alpha = .1, simulatePois, args, title = NULL){

  if(!is.null(data$area)){
    area_size <- (data$area[2] - data$area[1]) *
      (data$area[4] - data$area[3])
  } else{
    area_size <- 1
  }

  all_data <- NULL
  all_data_mat <- NULL

  for(i in 1:num_real){
    pois <- simulatePois(args)
    poisL <- Kfn(pois, area_size)

    all_data_mat <- cbind(all_data_mat, poisL$y)
  }

  realL <- Kfn(data, area_size)
  plot_data <- tibble(x = realL$x, y = realL$y)

  var_vec <- apply(all_data_mat, 1, var)
  mean_vec <- apply(all_data_mat, 1, mean)

  z <- qnorm(1 - (alpha / 2))
  lower <- mean_vec - z * sqrt(var_vec)
  upper <- mean_vec + z * sqrt(var_vec)

  plot_data <- mutate(plot_data, lower = lower, upper = upper)

  gg <- ggplot(data = plot_data) +
    geom_line(aes(x = x, y = y)) +
    geom_line(aes(x = x, y = upper), linetype = "dashed") +
    geom_line(aes(x = x, y = lower), linetype = "dashed") +
    labs(x = "t", y = "L")
  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }

  return(gg)

}


# Simulate events from a homogenuous Poisson process
# With a known number of events in a given area.
simulateHomoPois <- function(args){

  n <- args$n
  area <- args$area

  x_coord <- runif(n) * (area[2] - area[1]) + area[1]
  y_coord <- runif(n) * (area[4] - area[3]) + area[3]

  res <- list(x = x_coord, y = y_coord, area = area)

  return(res)
}


