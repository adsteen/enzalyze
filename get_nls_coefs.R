get_nls_coefs <- function(x) {
  # x is a nls fit
  coeffs <- summary(x)$coefficients
  data.frame(k=coeffs[1, 1],
             k.se=coeffs[1, 2],
             recalcitrant=coeffs[2, 1],
             recalcitrant.se=coeffs[2, 2])
}