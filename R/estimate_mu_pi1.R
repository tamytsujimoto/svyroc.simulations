#' Title
#'
#' @param weight
#'
#' @return

estimate_mu_pi1 <- function(weight){

  N_hat <- sum(weight)
  mu_pi1_hat <- sum(weight^2)/N_hat - 1

  return(mu_pi1_hat)

}
