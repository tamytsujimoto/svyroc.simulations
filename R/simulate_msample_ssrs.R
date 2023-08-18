#' Title
#'
#' @param pop
#' @param N_sample
#' @param sample_size
#' @param grid
#' @param ci
#' @param pscore_formula
#'
#' @return

simulate_msample_ssrs <- function(pop,
                                  N_sample,
                                  sample_size,
                                  grid,
                                  ci,
                                  pscore_formula) {

  # Generating and stacking samples
  sim <-
    replicate(N_sample,
              simulate_sample_ssrs(pop = pop,
                                   sample_size = sample_size,
                                   grid = grid,
                                   ci = ci,
                                   pscore_formula = pscore_formula),
              simplify = FALSE) %>%
    bind_rows(.id = 'sample')

  return(sim)

}
