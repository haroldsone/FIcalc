BV94ims <- function(T) {
#function to calculate the salinity of a fluid from ice melting temperature in degrees celcius (after Bodnar and Vityk (1994))
a <- (1.78)
b <- (0.0442)
c <- (0.000557)
iT <- (T*-1)
Swp = 0.00 + (a * iT) - (b * iT^2) + (c * iT^3)
print ( Swp)
}

BV94hds <- function(T) {
  #function to calculate the salinity of a fluid from halite dissolution temperature in degrees celcius (after Bodnar and Vityk (1994))
a <- (26.242)
b <- (0.4928)
c <- (1.42)
d <- (0.223)
e <- (0.04129)
f <- (0.006295)
g <- (0.001967)
h <- (0.00011112)
iT <- (T/100)
Swp = a+b*iT+c*iT^2-d*iT^3+e*iT^4+f*iT^5-g*iT^6+h*iT^7
print ( Swp)
}

BV94isoch <- function(Swp, Th) {
  #function to calculate the isochore for a H2O-NaCl fluid using the salinity in weight percent NaCl and the homogenization temperature in degrees celcius
as <- (18.28+1.4413*Swp+0.0047241*Swp^2-0.0024213*Swp^3+0.000038064*Swp^4)
bs <- (0.019041-0.015268*Swp+0.000566012*Swp^2-0.0000042329*Swp^3-0.000000030354*Swp^4)
cs <- (-0.00015988+0.000036892*Swp-0.0000019473*Swp^2+0.000000041674*Swp^3-0.00000000033008*Swp^4)
Slope = (as+bs*Th+cs*Th^2)
print ( Slope)
}
