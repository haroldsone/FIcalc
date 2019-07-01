library("tidyverse")
library("readxl")
#Need to address (figure out) how to be selective. The If, and For loops are only using one option or the other.

#read the data table into info
info <- read_excel("data-raw/FIdata.xlsx",
                   range = cell_cols("A:D")) # import all records in columns A to C

#calculate the salinity using equation 3 from Hall, Sterner and Bodnar (1988), Economic Geology, Vol. 83, pp. 197-202

#setting up some constants
s <- info[4]
c <- 0.4597 + 0.144*s 
names(c) <- "c"
e <- 0.0002227 + 0.0001999*s + (0.00004633*s^2) + (0.0001123*s^3)
names(e) <- "e"

a <- c/e
names(a) <- "a"
b <- info[2]/e
names(b) <- "b"

d <- -b/2
names(d) <- "d"
f <- b^2/4
names(f) <- "f"
g <- a^3/27
names(g) <- "g"
h <- sqrt(f + g)
names(h) <- "h"

p <- d + h
names(p) <- "p"

ifelse(p < 0, r <- abs(p)^0.33333, r <- p^0.33333)
# if (p<0) {
#   r <- abs(p)^0.33333
# } else {
#   r <- p^0.33333
# }
names(r) <- "r"

q <- d-h

ifelse(q < 0, s <- -(abs(q))^0.33333, s <- p^0.33333)
# if (q<0) {
#   s <- -(abs(q))^0.33333
# } else {
#   s <- p^0.33333
# }
names(s) <- "s"

# r <- p^0.33333
# names(r) <- "r"
# 
# s <- p^0.33333
# names(p) <- "p"

info["WtPctNaCl"] <- r+s

rm(a, b, c, d, e, f, g, h, p, q, r, s)

#calculate the density using molality (m) into equation 22 from Zhang and Frantz (1987), Chemical Geology, vol. 64, pg. 335-350

#calculate molality
t <- 18.01534*info[5]/(18.01534*info[5]+58.4428*(100-info[5]))
m <- 55.55*t/(1-t) #equation 17

info["molality"] <- m

#constants
g1 <- 1.0014
g2 <- -0.00022323
g3 <- -0.0000013472
g4 <- -0.0000000046597
g5 <- 0.023547
g6 <- 0.0045636
g7 <- 0.00048805
g8 <- -0.00006498
g9 <- -0.000053074
g10 <- 0.000001009

#calculate density
ifelse(m<5, info["density"] <- (g1 + g5*m + g6*m^2 + g7*m^3) + (g2 + g8*m + g9*m^2) * info[3] + (g3 + g10*m) * info[3]^2 + g4*info[3]^3, "NA")

rm(m, t, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10)

#Calculate pressure along L-V curve at Th
T <- info[3]
tr <- T/100

info["PatTh"] <- "NA"

ifelse(T > -21.2 & T < 180.01, info[8] <- exp(-5.38 + 0.0688*T - 0.000208*T^2 + 0.000000296*T^3), info[8] <- -135.99 + 304.37*tr - 236.18*tr^2 + 78.625*tr^3 - 10.094*tr^4 + 0.4244*tr^5)
# for (var in T) {
#   if(var > -21.2 & var < 180.01) {
#   info[8] <- exp(-5.38 + 0.0688*T - 0.000208*T^2 + 0.000000296*T^3)
# } else { info[8] <- -135.99 + 304.37*tr - 236.18*tr^2 + 78.625*tr^3 - 10.094*tr^4 + 0.4244*tr^5
#}
#}

info["Tcrit"] <- 374.1 + 8.8*info[5] + 0.1771*info[5]^2 - 0.02113*info[5]^3 + 0.0007334*info[5]^4

info["Pcrit"] <- 2094 - 20.56*info[9] + 0.06896*info[9]^2 - 0.00008903*info[9]^3 + 0.00000004214*info[9]^4

rm(T, tr)

print ("CAUTION, FOR FI WITH MOLALITY > 5, DENSITY VALUES ARE INCORRECT")

