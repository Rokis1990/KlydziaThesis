#test
#Penn World Tables
# https://econometricswithr.wordpress.com/tag/growth-accounting/

install.packages("pwt8")
library(pwt8)
data(pwt8.1) #neveikia
pwtData 

i <- "AUS"
dat<-pwt8.1[pwt8.1$isocode==i & pwt8.1[,"year"]>=1960 & pwt8.1[,"year"]<=2000,]

dy <- diff(log(dat[,"rgdpna"]/dat[,"emp"]))*100
dk <- diff(log(dat[,"rkna"]/dat[,"emp"]))*100
a <- 1-dat[-1,"labsh"]


dtfp <- dy-a*dk
data.frame("country"=i,"g"=mean(dy),"tfp"=mean(dtfp),"k"=mean(a*dk),"tfp.share"=mean(dtfp)/mean(dy),"capital.share"=mean(dk)/mean(dy))

