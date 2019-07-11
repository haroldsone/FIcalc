## Knight and Bodnar (1989) equation to calculate the critical pressure

KB89ct <- function(WtpctNaCl){
  Tcrit <- 374.1 + 8.8 * WtpctNaCl + 0.1771 * WtpctNaCl^2 - 0.02113 * WtpctNaCl^3 + 0.0007334 * WtpctNaCl^4
}
