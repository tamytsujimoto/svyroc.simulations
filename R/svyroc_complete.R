#' Title
#'
#' @param design
#' @param grid
#' @param ci
#'
#' @return

svyroc_complete <- function(design,
                            grid = NULL,
                            ci = FALSE) {

  data <- as.data.frame(design$variables)
  D <- data$D
  X <- data$X

  wt <- 1/design$prob
  wt <- wt*(!is.na(X))

  X.order <- order(X, decreasing = TRUE, na.last = NA)

  TTT <- X[X.order]
  TPF <- cumsum(wt[X.order]*(D[X.order] == 1))/sum(wt*(D == 1))
  FPF <- cumsum(wt[X.order]*(D[X.order] == 0))/sum(wt*(D == 0))

  dups <- rev(duplicated(rev(TTT)))
  tp <- TPF[!dups]
  fp <- FPF[!dups]
  cutoffs <- TTT[!dups]

  tp <- ifelse(tp > 1, 1, tp)
  fp <- ifelse(fp > 1, 1, fp)

  df_roc <-
    data.frame(fpr = c(0,fp),
               tpr = c(0,tp),
               cutoff = c(Inf, cutoffs))

  # CONFIDENCE INTERVAL - WEIGHTED

  if(ci) {

    var_wt <-
      roc_var_svy(df_roc, design, grid)

    df_roc <- var_wt

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
