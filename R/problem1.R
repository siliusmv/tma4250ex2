



plotProcess <- function(data){

  dat <- data.frame(data$x, data$y)
  names(dat) <- c("x", "y")

  gg <- ggplot(data = dat) +
    geom_point(aes(x = x, y = y)) +
    xlim(data$area["xl"], data$area["xu"]) +
    ylim(data$area["yl"], data$area["yu"])

  return(gg)
}


# Dafuck?
val <- Kfn(data, 10)
