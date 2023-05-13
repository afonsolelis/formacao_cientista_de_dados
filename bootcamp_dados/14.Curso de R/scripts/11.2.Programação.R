#Professor Fernando Amaral
a = 10
b = 20


if (a > 10)
{
  print("A é maior")
}else
{
  print("A não é maior") 
}


x = ifelse(a > 10,"A é maior","A não é maior")
x


for (i in 1:10) {
  print(i)
  }
  

a = 1
while(a <= 10)
{
  print(a)
  a = a+1
}


parouimpar <- function(x) {
  
  return(ifelse(x%%2==0,"Par","impar"))
  
}

parouimpar(5)
parouimpar(12)
