## Knight and Bodnar (1989) equation to calculate the critical pressure

KB89cp <- function(Tcrit){
  Pcrit <- 2094 - 20.56 * Tcrit + 0.06896 * Tcrit^2 - 0.00008903 * Tcrit^3 + 0.00000004214 * Tcrit^4
  }