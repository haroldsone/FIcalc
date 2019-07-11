library("ggplot2")

LVcurvLT180 <- data.frame("T" = 1:180, "P" = NA)

#calculate pressure along L-V curve at Th (taken from Macflincor)
LVcurvLT180["P"] <- exp(-5.38+0.0688*LVcurvLT180[1]-0.000208*LVcurvLT180[1]^2+0.000000296*LVcurvLT180[1]^3)#for #Th between -21.2 and 180.01

LVcurvGT180 <- data.frame("T" = 180:374, "P" = NA)

LVcurvGT180["P"] <- -135.99+304.37*(LVcurvGT180["T"]/100)+236.18*(LVcurvGT180["T"]/100)^2+78.625*(LVcurvGT180["T"]/100)^3-10.094*(LVcurvGT180["T"]/100)^4+0.4244*(LVcurvGT180["T"]/100)^5 #For T >180.01

ggplot(LVcurvLT180, aes(x=T, y=P, group=1)) + 
  geom_line()+
  geom_point()

ggplot(LVcurvGT180, aes(x=T, y=P, group=1)) + 
  geom_line()+
  geom_point()