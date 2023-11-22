
svyroc_binormal_fit <- function(design) {

  # Estimate mean and variance for marker according to D
  fit_mean0 <- survey::svymean(~X, subset(design, D == 0))
  fit_mean1 <- survey::svymean(~X, subset(design, D == 1))

  fit_var0 <- survey::svyvar(~X, subset(design, D == 0))
  fit_var1 <- survey::svyvar(~X, subset(design, D == 1))

  # Computing Binormal ROC curve parameters
  mean0 <- fit_mean0[1]
  mean1 <- fit_mean1[1]

  sd0 <- sqrt(fit_var0[1])
  sd1 <- sqrt(fit_var1[1])

  a = (mean1-mean0)/sd1
  b = sd0/sd1

  # Computing covariance matrix for (a, b)
  vcov <- svyroc_binormal_fit_cov(vcov(fit_mean0),
                                  vcov(fit_mean1),
                                  vcov(fit_var0),
                                  vcov(fit_var1))

  # Binormal ROC curve
  binormal_fit <- matrix(matrix(c(a,b,sqrt(diag(vcov)))),
                         ncol = 2,
                         dimnames = list(c("Intercept", "Slope"),
                                         c("Estimate", "SE")))

  return(list(fit = binormal_fit, vcov = vcov))

}
