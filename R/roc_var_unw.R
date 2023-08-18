#' Title
#'
#' @param roc_data
#' @param design
#' @param grid
#'
#' @return

roc_var_unw <- function(roc_data,
                        design,
                        grid){

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

  dsgnD1 <- subset(design, D == 1)
  dsgnD0 <- subset(design, D == 0)
  p1 <- survey::svymean(~D, design)[1]
  n <- dim(design$variables)[1]

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
    full_join(f0, by = 'aux') %>%
    mutate(ratio = f1/f0)

  roc_var <-
    roc_svy %>%
    dplyr::mutate(aux = cut(cutoff, c(-Inf,cutoff), include.lowest = TRUE)) %>%
    dplyr::left_join(ratio, by = "aux") %>%
    dplyr::filter((fpr > 0 & tpr < 1) | (tpr == 1 & !is.na(ratio))) %>%
    dplyr::mutate(
      ratio = zoo::na.approx(ratio),
      var_unw = (1/n)*((1-p1)^(-1)*ratio^2*fpr*(1-fpr) + p1^(-1)*tpr*(1-tpr))
    ) %>%
    dplyr::select(-c(fpr, aux, f1, f0, ratio)) %>%
    dplyr::rename(fpr = grid) %>%
    dplyr::mutate(fpr = as.numeric(as.character(fpr)))

  return(roc_var)

}
