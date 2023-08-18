#' Generate stratified finite population with binormal model
#'
#' Function to generate a stratified finite population with independent observations
#' generated from a binormal model for the ROC curve.
#' n_strata: Number of strata
#'
#' @param N: Finite population size
#' @param freq_strata: vector (of size n_strata) containing the distribution of strata in the finite population
#' @param p1_strata: vector (of size n_strata) containing the disease/positive label proportion in each stratum
#' @param coef_marker_strata: list (of size n_strata) of 4-dimentional vector with coefficient for binormal model for each stratum
#' @param sigma_strata: vector (of size n_strata) containing standard deviation (sd) of binormal model in each stratum
#' @param coef_missing_strata: list (of size n_strata) 4-dimentional vector with coefficient for missing component for each stratum
#' @param cluster_size_strata: list (of size n_strata) of vectors containing cluster sizes
#' @param tau: sd of random effects for cluster sampling
#'
#' @return A dataframe with generated diagnostic test X, the disease indicator D and the strata identifier

generate_pop_strat <- function(N,
                               freq_strata,
                               p1_strata,
                               coef_marker_strata, # list of coefficients
                               sigma_strata,
                               coef_missing_strata, # list of coefficients missing
                               cluster_size_strata, # list of cluster sizes
                               tau = 1) {

  strata_size <- ceiling(N*freq_strata)
  N_strata <- length(freq_strata)

  pop_strat <-
    purrr::pmap_dfr(list(as.list(strata_size),
                         as.list(p1_strata),
                         coef_marker_strata,
                         as.list(sigma_strata),
                         coef_missing_strata,
                         cluster_size_strata,
                         as.list(rep(tau, N_strata))), # List of parameters for each strata
                    .f = generate_pop,
                    .id = 'strata') %>%
    dplyr::mutate(strata = as.numeric(strata))

  return(pop_strat)
}
