#' Title
#'
#' @param design
#' @param pscore_formula
#' @param grid
#' @param ci
#'
#' @return

svyroc_ipw <- function(design,
                       pscore_formula,
                       grid = NULL,
                       ci = FALSE) {

  design_R <- update(design, R = ifelse(is.na(X), 0, 1))
  data <- design_R$variables
  wt <- 1/design_R$prob
  D <- data$D
  X <- data$X
  R <- data$R
  X.order <- order(X, decreasing = TRUE, na.last = NA)

  if(sum(is.na(X)) > 0){

    form <- as.formula(paste0("R ",paste(pscore_formula, collapse = " ")))
    pscore_model <- survey::svyglm(form, design = design_R, family = quasibinomial)
    pscore <- 1/as.vector(predict(pscore_model, type = "response"))

  } else pscore <- rep(1, length(X))

  TTT <- X[X.order]
  TPF <- cumsum(wt[X.order]*pscore[X.order]*R[X.order]*(D[X.order] == 1))/sum(wt*(D == 1)*pscore*(R == 1))
  FPF <- cumsum(wt[X.order]*pscore[X.order]*R[X.order]*(D[X.order] == 0))/sum(wt*(D == 0)*pscore*(R == 1))

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
      roc_var_svy_ipw(df_roc, design_R, pscore, grid)

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
