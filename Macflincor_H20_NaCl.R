library("tidyverse")
library("readxl")
library("magrittr")
library("openxlsx")

#read the data table into info
info <- read_excel("data-raw/FIdata.xlsx",
                   range = cell_cols("A:D")) # import all records in columns A to C

#calculate the salinity using equation 3 from Hall, Sterner and Bodnar (1988), Economic Geology, Vol. 83, pp. 197-202

#setting up some constants
NK <- (info[4])
c <- (0.4597 + 0.144*NK)
# names(c) <- "c"
e <- (0.0002227 + 0.0001999*NK + (0.00004633*NK^2) + (0.0001123*NK^3))
# names(e) <- "e"

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

p <- as.numeric(unlist(d + h))

r <-  ifelse(p <= 0, abs(p)^0.33333, p^0.33333)

q <- as.numeric(unlist(d-h))

s <- ifelse(q <= 0, -(abs(q))^0.33333, p^0.33333)

info["WtPctNaCl"] <- r+s

rm(NK, a, b, c, d, e, f, g, h, p, q, r, s)


#calculate the density using molality (m) into equation 22 from Zhang and Frantz (1987), Chemical Geology, vol. 64, pg. 335-350


#calculate molality
t <- 18.01534*info[5]/(18.01534*info[5]+58.4428*(100-info[5]))
m <- 55.55*t/(1-t) #equation 17

info["molality"] <- m


#calculate the density

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

T <- as.numeric(unlist(info[3]))
densML5 <- as.numeric(unlist((g1 + g5*m + g6*m^2 + g7*m^3) + (g2 + g8*m + g9*m^2) * T + (g3 + g10*m) * T^2 + g4*T^3))

options(infoble.print_max = Inf)

info %<>% mutate(
  dens = ifelse(molality <= 5, densML5, NA))

rm(T, densML5, m, t, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10)

#Calculate pressure along L-V curve at Th
T <- as.numeric(unlist(info[3]))
tr <- T/100
x1 <- as.numeric(unlist(exp(-5.38 + 0.0688*T - 0.000208*T^2 + 0.000000296*T^3)))

x2 <- as.numeric(unlist(-135.99 + 304.37*tr - 236.18*tr^2 + 78.625*tr^3 - 10.094*tr^4 + 0.4244*tr^5))
                 
info %<>% mutate(
  PatTh = ifelse(T > -21.2 & T < 180.01, x1, x2))

rm(x1, x2)


#Calculate the critical T,P for the FI system
info["Tcrit"] <- 374.1 + 8.8*info[5] + 0.1771*info[5]^2 - 0.02113*info[5]^3 + 0.0007334*info[5]^4

info["Pcrit"] <- 2094 - 20.56*info[9] + 0.06896*info[9]^2 - 0.00008903*info[9]^3 + 0.00000004214*info[9]^4

rm(T, tr)

#calculate an isochore using Bodnar and Vityk (1994)
Th <- as.numeric(unlist(info[3]))
S <- as.numeric(unlist(info[5]))
as <- as.numeric(unlist(18.28 + 1.4413 * S + 0.0047241 * S^2 - 0.0024213 * S^3 + 0.000038064 * S^4))
bs <- as.numeric(unlist( 0.019041 - 0.015268 * S + 0.000566012 * S^2 - 0.0000042329 * S^3 - 0.000000030354 * S^4))
cs <- as.numeric(unlist( -0.00015988 + 0.000036892 * S - 0.0000019473 * S^2 + 0.000000041674 * S^3 - 0.00000000033008 * S^4))

info["dpdtT"] <- as.numeric(unlist(as + bs * Th + cs * Th^2))
PatTh <- as.numeric(unlist(info[8]))
dpdt <- as.numeric(unlist(info[9]))
rm(S, as, bs, cs)

#plot isochore using the geom_segment in ggplot2

y2 <- as.numeric(unlist((dpdt * (1000 - Th) + PatTh)))
x2 <- as.numeric(unlist(1000))

#TODO: create data for 0-5 wt%, 5-10%, etc.
info %<>% mutate(
  y2 = ifelse(WtPctNaCl < 5, )
)
ggplot() + 
  geom_segment(data=info, mapping=aes(x=Th, y=PatTh, xend=x2, yend=y2), size=0.1, color="red") 

write.xlsx(info, file = "HNdata.xlsx")

