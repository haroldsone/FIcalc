library("tidyverse")
library("readxl")

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

if p<0 {
  r <- abs(p)^0.33333
  else r <- p^0.33333
}
name(r) <- "r"

q <- d-h

if q<0 {
  s <- -(abs(q))^0.33333
  else s <- p^0.33333
}
names(s) <- "s"

# r <- p^0.33333
# names(r) <- "r"
# 
# s <- p^0.33333
# names(p) <- "p"

info["WtPctNaCl"] <- r+s

rm(a, b, c, d, e, f, g, h, p, q, r, s)
