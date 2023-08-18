#' Title
#'
#' @param roc_data
#' @param design
#' @param grid
#'
#' @return

roc_var_svy <- function(roc_data,
                        design,
                        grid = NULL){

  if(is.null(grid)) grid <- seq(0,1,.01)

  roc_svy <-
    roc_data %>%
    dplyr::mutate(grid = cut(fpr, c(-Inf,grid), include.lowest = TRUE, labels = grid)) %>%
    dplyr::group_by(grid) %>%
    dplyr::summarise(fpr = max(fpr),
                     tpr = max(tpr),
                     cutoff = min(cutoff))

  c <- roc_svy$cutoff
  fp <- roc_svy$fpr
  tp <- roc_svy$tpr

  n <- dim(design$variables)[1]
  wt <- 1/(design$prob)
  mu_pi1 <- estimate_mu_pi1(wt)
  p1 <- survey::svymean(~D, design)[1]
  sampling_frac <- n/sum(wt)

  dsgnD1 <- subset(design, D == 1)
  dsgnD0 <- subset(design, D == 0)

  f0 =
    survey::svysmooth(~X, design = dsgnD0)$X %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(aux = cut(x, c(-Inf, c), include.lowest = TRUE)) %>%
    dplyr::group_by(aux) %>%
    dplyr::filter(row_number() == n()) %>%
    dplyr::select(aux, f0 = y)

  f1 =
    survey::svysmooth(~X, design = dsgnD1)$X %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(aux = cut(x, c(-Inf, c), include.lowest = TRUE)) %>%
    dplyr::group_by(aux) %>%
    dplyr::filter(row_number() == n()) %>%
    dplyr::select(aux, f1 = y)

  ratio <-
    f1 %>%
    dplyr::full_join(f0, by = 'aux') %>%
    dplyr::mutate(ratio = f1/f0)

  roc_svy <-
    roc_svy %>%
    dplyr::mutate(aux = cut(cutoff, c(-Inf,cutoff), include.lowest = TRUE)) %>%
    dplyr::left_join(ratio, by = "aux") %>%
    dplyr::filter(fpr > 0 & fpr < 1) %>%
    dplyr::mutate(
      ratio = zoo::na.approx(ratio, na.rm = FALSE),
      vsvy_sup = (1/n)*(sampling_frac*(1+mu_pi1))*((1-p1)^(-1)*ratio^2*fpr*(1-fpr) + p1^(-1)*tpr*(1-tpr)),
      vsvy_fin = (1/n)*(sampling_frac*(mu_pi1))*((1-p1)^(-1)*ratio^2*fpr*(1-fpr) + p1^(-1)*tpr*(1-tpr))) %>%
    dplyr::select(-c(fpr, aux, f1, f0, ratio)) %>%
    dplyr::rename(fpr = grid) %>%
    dplyr::mutate(fpr = as.numeric(as.character(fpr)))

  return(roc_svy)

}
