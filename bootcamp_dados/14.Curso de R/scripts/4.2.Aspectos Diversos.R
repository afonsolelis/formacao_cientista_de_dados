#Professor Fernando Amaral

getwd()
setwd("c:\\dados")


quit()


class(iris)


iristeste = iris
save(iristeste, file="iristeste.Rdata")
rm(iristeste)
iristeste
     
load(file="iristeste.Rdata")
iristeste
     

x = c(12,34,56,79)
y = c(1,6,9,14)
plot(x,y)