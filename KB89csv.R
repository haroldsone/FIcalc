KB89csv <- function(WtPctNaCl){
  psi <- log(WtPctNaCl + 1)
  csv <- 3.106 - 0.5967 * psi - 0.01066 * psi^2 + 0.01267 * psi^3
}