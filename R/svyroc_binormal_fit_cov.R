


svyroc_binormal_fit_cov <- function(vcov_mean0,
                                    vcov_mean1,
                                    vcov_var0,
                                    vcov_var1){

  gradient <- function(x1, x2, x3, x4){

    return(
      matrix(
        c(-x4^(-0.5), x4^(-0.5), 0, -0.5*(x2-x1)*x4^(-1.5),
          0, 0, 0.5*x3^(-0.5)*x4^(-0.5), -0.5*x3^(0.5)*x4^(-1.5)),
        nrow = 2,
        byrow = TRUE))


  }

  vcov_input <- diag(c(vcov_mean0, vcov_mean1, vcov_var0, vcov_var1))
  binormal_gradient <- gradient(vcov_mean0,
                                vcov_mean1,
                                vcov_var0,
                                vcov_var1)

  binormal_cov <- binormal_gradient %*% vcov_input %*% t(binormal_gradient)

  return(binormal_cov)

}
