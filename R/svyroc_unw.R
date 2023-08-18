#' Title
#'
#' @param design
#' @param grid
#' @param ci
#'
#' @return

svyroc_unw <- function(design,
                       grid = NULL,
                       ci = FALSE) {

  df_roc <- svyroc_complete(design, grid = grid)

  # CONFIDENCE INTERVAL - WEIGHTED

  if(ci) {

    var_unw <-
      roc_var_unw(df_roc, design, grid) %>%
      dplyr::select(fpr, var_unw)

    df_roc <-
      df_roc %>%
      dplyr::left_join(var_unw, by = 'fpr')

  }

  if(ci == FALSE & !is.null(grid)) {

    df_roc <-
      df_roc %>%
      dplyr::mutate(grid = cut(fpr, c(-Inf,grid), include.lowest = TRUE, labels = grid)) %>%
      dplyr::group_by(grid) %>%
      dplyr::summarise(fpr = round(max(fpr),1),
                       tpr = max(tpr),
                       cutoff = min(cutoff)) %>%
      dplyr::select(-grid)

  }

  return(df_roc)
}
