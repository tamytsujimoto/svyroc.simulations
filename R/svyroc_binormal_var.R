

svyroc_binormal_var <- function(a, b, fpr, roc_binormal_cov){

  gradient <- function(x1, x2, t){

    length_t <- length(t)
    x1_vec = rep(x1, length_t)
    x2_vec = rep(x2, length_t)

    var <- as.vector(
      c(dnorm(x1_vec + x2_vec*qnorm(t)),
        dnorm(x1_vec + x2_vec*qnorm(t))*qnorm(t)))

    return(matrix(var, nrow = length_t, 2))
  }

  roc_gradient <- gradient(a,b,fpr)
  roc_var <- diag(roc_gradient %*% roc_binormal_cov %*% t(roc_gradient))

  return(roc_var)

}
