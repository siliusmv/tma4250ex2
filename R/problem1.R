
plotProcess <- function(data, title = NULL){

  dat <- tibble(x = data$x, y = data$y)

  gg <- ggplot(data = dat) +
    geom_point(aes(x = x, y = y))

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

plotLFunc <- function(data, title = NULL){

  dat <- tibble(x = data$x, y = data$y)

  gg <- ggplot(data = dat) +
    geom_line(aes(x = x, y = y)) +
    labs(x = TeX("$\\tau$"), y = TeX("$\\mathbf{L}$"))
  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }

  return(gg)
}


testIfPois <- function(data, num_real = 100, alpha = .1, simulatePois, args, title = NULL){

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
    geom_line(aes(x = x, y = lower), linetype = "dashed")
  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }

  return(gg)

}



simulateHomoPois <- function(args){

  n <- args$n
  area <- args$area

  x_coord <- runif(n) * (area[2] - area[1]) + area[1]
  y_coord <- runif(n) * (area[4] - area[3]) + area[3]

  res <- list(x = x_coord, y = y_coord, area = area)

  return(res)
}


