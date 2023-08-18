#' Function to run the simulation for Stratified Simple Random Sample (SSRS)
#'
#' @param N_pop: Number of finite populations to be generated
#' @param pop_size: Finite population size
#' @param param_pop: list of population parameters
#' @param cluster_size_strata: list (of size n_strata) of vectors containing cluster sizes
#' @param N_sample: Number of samples to be drawn using SRS
#' @param sample_size: Size of each sample
#' @param pscore_formula: (For IPW methods) formula for propensity score
#' @param ci: (default = FALSE) is true, asymptotic confidence interval is computed
#' @param grid: grid for the false positive rate
#'
#' @return

simulation_ssrs <- function(N_pop,
                            pop_size,
                            param_pop,
                            cluster_size_strata,
                            N_sample,
                            sample_size,
                            pscore_formula,
                            ci = FALSE,
                            grid = seq(0, 1, by=0.1)){

  # Generating N_pop stratified finite populations
  pop <-
    replicate(N_pop,
              generate_pop_strat(N = pop_size,
                                 freq_strata = param_pop$freq_strata,
                                 p1_strata = param_pop$p1_strata,
                                 coef_marker_strata = param_pop$coef_marker_strata,
                                 sigma_strata = param_pop$sigma_strata,
                                 coef_missing_strata = param_pop$coef_missing_strata,
                                 cluster_size_strata = cluster_size_strata),
              simplify = FALSE)

  # Computing finite population ROC
  tpr_pop_ipw <-
    pop %>%
    purrr::map(~survey::svydesign(id =~1, weights =~1, data = .x)) %>%
    purrr::map_dfr(svyroc_ipw,
                   grid = all_of(grid),
                   pscore_formula = ~D + W1 + W2 + D:W1 + D:W2,
                   .id = 'population') %>%
    dplyr::select(population, fpr, tpr_pop_ipw = tpr)

  tpr_pop_cc <-
    pop %>%
    purrr::map(~survey::svydesign(id =~1, weights =~1, data = .x)) %>%
    purrr::map_dfr(svyroc_complete,
                   grid = all_of(grid),
                   .id = 'population') %>%
    dplyr::select(population, fpr, tpr_pop_cc = tpr)

  # # For each population, generating N_sample samples and computing ROC
  sim <-
    pop %>%
    purrr::map_dfr(simulate_msample_ssrs,
                   N_sample = N_sample,
                   sample_size = sample_size,
                   grid = grid,
                   ci = ci,
                   pscore_formula = pscore_formula,
                   .id = 'population') %>%
    dplyr::left_join(tpr_pop_ipw, by = c('fpr', 'population')) %>%
    dplyr::left_join(tpr_pop_cc, by = c('fpr', 'population'))

  return(sim)

}
