
cells_data <- ppinit("cells.dat")

plotProcess <- function(data, title){

  dat <- data.frame(data$x, data$y)
  names(dat) <- c("x", "y")

  gg <- ggplot(data = dat) +
    geom_point(aes(x = x, y = y)) +
    xlim(data$area["xl"], data$area["xu"]) +
    ylim(data$area["yl"], data$area["yu"]) +
    labs(title = title)

  return(gg)
}

plotLFunc <- function(data, title){

  dat <- data.frame(data$x, data$y)
  names(dat) <- c("x", "y")

  gg <- ggplot(data = dat) +
    geom_line(aes(x = x, y = y)) +
    labs(title = title)

  return(gg)
}


testIfPois <- function(data, num_real, alpha = .1){

  area_size <- (data$area[2] - data$area[1]) * (data$area[4] - data$area[3])
  n <- length(data$x)

  lambda <- n / area_size

  all_data <- NULL
  all_data_mat <- NULL

  for(i in 1:num_real){
    pois <- simulatePois(n = n, area = data$area)
    poisL <- Kfn(pois, area_size)

    # pois_data <- cbind(poisL$x, poisL$y)
    # pois_data <- cbind(pois_data, as.character(i))
    # all_data <- rbind(all_data, pois_data)

    all_data_mat <- cbind(all_data_mat, poisL$y)
  }

  # all_data <- data.frame(all_data)
  # names(all_data) <- c("x", "y", "run")

  realL <- Kfn(data, area_size)
  plot_data <- data.frame(cbind(realL$x, realL$y))
  names(plot_data) <- c("x", "y")

  var_vec <- apply(all_data_mat, 1, var)
  mean_vec <- apply(all_data_mat, 1, mean)

  z <- qnorm(1 - (alpha / 2))
  lower <- mean_vec - z * sqrt(var_vec)
  upper <- mean_vec + z * sqrt(var_vec)

  plot_data$lower <- lower
  plot_data$upper <- upper

  gg <- ggplot(data = plot_data) +
    geom_line(aes(x = x, y = y)) +
    geom_line(aes(x = x, y = upper), linetype = "dashed") +
    geom_line(aes(x = x, y = lower), linetype = "dashed")

  return(gg)

}



simulatePois <- function(n, area){

  x_coord <- runif(n) * (area[2] - area[1]) + area[1]
  y_coord <- runif(n) * (area[4] - area[3]) + area[3]

  res <- list(x = x_coord, y = y_coord, area = area)

  return(res)
}