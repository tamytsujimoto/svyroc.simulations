#' Title
#'
#' @param pop
#' @param sample_size
#' @param pscore_formula
#' @param ci
#' @param grid
#'
#' @return

simulate_sample_ssrs <- function(pop,
                                 sample_size,
                                 pscore_formula,
                                 ci,
                                 grid) {

  # Generating and stacking samples
  sample_stack <-
    purrr::map(as.list(sample_size),
               .f = generate_ssrs,
               pop_strat = pop)

  # Computing COMPLETE ROC for samples
  roc_comp <-
    sample_stack %>%
    purrr::map(~survey::svydesign(id =~1, strata =~strata, fpc = ~strata_size, weights = ~weight, data = .x)) %>%
    purrr::map_dfr(svyroc_complete,
                   grid = grid,
                   ci = ci,
                   .id = 'scenario') %>%
    dplyr::rename(tpr_comp = tpr, cutoff_comp = cutoff)

  # Computing UNWEIGHTED ROC for samples
  roc_unw <-
    sample_stack %>%
    purrr::map(~survey::svydesign(id =~1, strata =~strata, weights = ~1, data = .x)) %>%
    purrr::map_dfr(svyroc_unw,
                   grid = grid,
                   ci = ci,
                   .id = 'scenario') %>%
    dplyr::select(fpr, scenario, tpr_unw = tpr, var_unw)

  # Computing WEIGHTED ROC (NOT SURVEY!) for samples
  roc_wt <-
    sample_stack %>%
    purrr::map(~survey::svydesign(id =~1, strata =~strata, fpc = ~strata_size, weights = ~weight, data = .x)) %>%
    purrr::map_dfr(svyroc_unw,
                   grid = grid,
                   ci = ci,
                   .id = 'scenario') %>%
    dplyr::select(fpr, scenario, tpr_wt = tpr, var_wt = var_unw)

  # Computing IPW ROC for samples
  roc_ipw <-
    sample_stack %>%
    purrr::map(~survey::svydesign(id =~1, strata =~strata, fpc = ~strata_size, weights = ~weight, data = .x)) %>%
    purrr::map_dfr(svyroc_ipw,
                   pscore_formula = pscore_formula,
                   grid = grid,
                   ci = ci,
                   .id = 'scenario') %>%
    dplyr::rename(tpr_ipw = tpr, cutoff_ipw = cutoff)

  # Computing BINORMAL ROC for samples
  # roc_binormal <-
  #   sample_stack %>%
  #   purrr::map(~survey::svydesign(id =~1, strata =~strata, fpc = ~strata_size, weights = ~weight, data = .x)) %>%
  #   purrr::map_dfr(svyroc_binormal,
  #                  grid = grid,
  #                  roc_ci = ci,
  #                  .id = 'scenario') %>%
  #   dplyr::mutate(fpr = round(fpr, 1))

  roc <-
    roc_comp %>%
    dplyr::left_join(roc_unw, by = c('scenario', 'fpr')) %>%
    dplyr::left_join(roc_wt, by = c('scenario', 'fpr')) %>%
    dplyr::left_join(roc_ipw, by = c('scenario', 'fpr'))
    # dplyr::left_join(roc_binormal, by = c('scenario', 'fpr'))

  return(roc)
}
