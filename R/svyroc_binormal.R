
svyroc_binormal <- function(design,
                            grid,
                            roc_ci = FALSE){

  binormal_fit <- svyroc_binormal_fit(design)
  a <- binormal_fit$fit[1,1]
  b <- binormal_fit$fit[2,1]

  #Computing ROC curve for grid if specified
  if(!is.null(grid)){

    a_vec = rep(a, length(grid))
    b_vec = rep(b, length(grid))
    binormal_roc <- pnorm(a_vec + b_vec*qnorm(grid))

    df_roc <- data.frame(fpr = grid, tpr_binorm = binormal_roc)

    if(roc_ci == TRUE){

      # Compute variance
      roc_vcov <- svyroc_binormal_var(a, b, grid, binormal_fit$vcov)
      n <- dim(design$variables)[1]
      wt <- 1/(design$prob)
      sampling_frac <- n/sum(wt)

      df_roc <-
        df_roc %>%
        dplyr::mutate(var_binorm = (1/n)*sampling_frac*roc_vcov)

    }

    return(roc = df_roc)
  }

}
