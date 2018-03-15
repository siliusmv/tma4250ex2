library(MASS)
library(spatial)

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

# Dafuck?
# val <- Kfn(data, 10)
