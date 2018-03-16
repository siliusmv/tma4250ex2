

repulsiveMC <- function(args){

  phi_0 <- args$phi_0
  phi_1 <- args$phi_1
  tau_0 <- args$tau_0
}

phiFunc <- function(phi_0, phi_1, tau_0, tau){

  negative <- ifelse(test = tau <0,
                    yes = TRUE,
                    no = FALSE)

  res <- ifelse(test = (negative == FALSE),
                yes = phi_0,
                no = 0)

  res <- res * ifelse(test = (tau > tau_0),
                      yes = exp(-phi_1 * (tau - tau_0)),
                      no = 1)

  return(res)
}
