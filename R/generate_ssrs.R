#' Title
#'
#' @param pop_strat
#' @param sample_size
#'
#' @return

generate_ssrs <- function(pop_strat,
                          sample_size){

  # Determining sample size per strata
  N <- dim(pop_strat)[1]
  N_strata <- length(table(pop_strat$strata))
  sample_size_strata <- ceiling(sample_size/N_strata)

  # Nesting population by strata
  pop_nested <-
    pop_strat %>%
    dplyr::group_by(strata) %>%
    tidyr::nest()

  # Sampling sample_size_strata observations from each strata
  sample <-
    purrr::pmap(list(pop_nested$data,
                     replicate(N_strata, sample_size_strata, simplify = FALSE)),
         .f = generate_srs) %>%
    dplyr::bind_rows(.id = 'strata') %>%
    dplyr::mutate(freq_strata = pop_size/N) %>%
    dplyr::rename(strata_size = pop_size)

  return(sample)
}
