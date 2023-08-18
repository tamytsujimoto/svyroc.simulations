#' Generate finite population for binormal model for ROC curve
#'
#' Function to generate marker X ~ D + W1 + W2 + W1:D + W2:D + e,
#' where e ~ N(0, sigma) and (W1, W2, W3, W4) ~ MNV(0, I)
#'
#' @param N: Finite population size
#' @param p1: disease/positive label proportion
#' @param coef_marker: 4-dimentional vector with coefficient for binormal model
#' @param sigma:  standard deviation (sd) of binormal model
#' @param coef_missing:  4-dimentional vector with coefficient for missing component
#' @param cluster_size: vector containing cluster sizes
#' @param tau: sd of random effects for cluster sampling
#'
#' @return Data frame containing:
#' X: marker generated using binormal model
#' D: disease/label
#' W1-W4: normally distributed covariates (W1 and W2 associated with X)

generate_pop <- function(N,
                         p1,
                         coef_marker,
                         sigma,
                         coef_missing = NULL,
                         cluster_size = NULL,
                         tau = 1) {

  # Generating disease indicator
  D <- rbinom(N, 1, p1)

  # Generating auxiliary variable
  W1 <- rnorm(N)
  W2 <- rnorm(N)
  W3 <- rnorm(N)
  W4 <- rnorm(N)

  # Generating marker
  X <-
    coef_marker[1] +
    coef_marker[2]*D +
    coef_marker[3]*W1 +
    coef_marker[3]*W2 +
    coef_marker[4]*W1*D +
    coef_marker[4]*W2*D +
    + rnorm(N, sd = sigma)

  # Unclustered population
  pop <- data.frame(X = X,
                    D = D,
                    W1 = W1,
                    W2 = W2,
                    W3 = W3,
                    W4 = W4)

  # Clustered population
  if(!is.null(cluster_size)){

    # Determining number of clusters
    N_cluster <- length(cluster_size)

    # Generating random effect
    random_eff <- purrr::map2_dfr(rnorm(N_cluster, sd = tau),
                                  cluster_size,
                                  .f = function(x, y) data.frame(random_eff = rep(x = x, times = y)),
                                  .id = 'cluster_id')

    pop <-
      pop %>%
      dplyr::mutate(cluster_id = as.numeric(random_eff$cluster_id),
             X = X + random_eff$random_eff)

  }

  # Generating missing
  if(!is.null(coef_missing)){

    pop <-
      pop %>%
      dplyr::mutate(aux =
               coef_missing[1] +
               coef_missing[2]*D +
               coef_missing[3]*W1 +
               coef_missing[3]*W2 +
               coef_missing[4]*W1*D +
               coef_missing[4]*W2*D,
             phi = exp(aux)/(1+exp(aux)),
             R = rbinom(N, 1, phi),
             X = ifelse(R == 0, NA, X)) %>%
      dplyr::select(-c(aux, phi, R))
  }

  return(pop)

}
