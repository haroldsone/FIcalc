## function to calculate Pressure given Temperature and Salinity using equation 2 from Atkinson (2002)
## use in temperature range -300 to 484 C
## input Temperature(T) in celcius and salinity(S) in weight percent NaCl

## It is unclear if coefficient b60 in table 3 should be used for the final portion of the equation (ie, "b61*T6*X1") or if b61 is 0, 0 is used here

A02e2 <- function(T, S){
  b200 <- -7.25091694178014
  b201 <- 52.7356253268118
  b202 <- -95.139589091999
  b203 <- 237.001333358832
  b204 <- -270.853923550159
  b206 <- -645.843502315573
  b207 <- 411.125432162485
  b210 <- 2.91847163119225
  b211 <- -30.0742277771278
  b212 <- 41.0055099275695
  b213 <- -82.6121950337547
  b214 <- 86.8864476861164
  b215 <- 85.0303689746524
  b216 <- -24.1431916305905
  b220 <- -0.28727704099067
  b221 <- 5.98143068258883
  b222 <- -5.87588111777108
  b223 <- 7.89995198785004
  b224 <- -10.886118260654
  b230 <- 0.00898343968706551
  b231 <- -0.436696998416016
  b232 <- 0.27654361988714
  b243 <- -0.0074586977134027
  b251 <- 0.000828114542697018
  b260 <- 0.0000015228510009732
  b261 <- 0
  xr <- S/100
  tr <- (T+273.15)/100
  P = 10^(b200 + b201*xr + b202*xr^2 + b203*xr^3 + b204*xr^4 + b206*xr^6 + b207*xr^7 + b210*tr + b220*tr^2 + b230*tr^3 + b260*tr^6 + b211*tr*xr + b212*tr*xr^2 + b213*tr*xr^3 + b214*tr*xr^4 + b215*tr*xr^5 + b216*tr*xr^6 + b221*tr^2*xr + b222*tr^2*xr^2 + b223*tr^2*xr^3 + b224*tr^2*xr^4 + b231*tr^3*xr + b232*tr^3*xr^2 + b243*tr^4*xr^3 + b251*tr^5*xr + b261*tr^6*xr)
}