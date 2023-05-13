#Professor Fernando Amaral

#vetores
X  <- c(1,2,3,4,5,6)  
X 
X[1]
X[1] <- 10
X[1]

#outros tiposo de vetores
Y = c("a","b","c","d")
Y
Z = c(1L,2L,3L)
Z


#matrizes
VADeaths
colnames(VADeaths)
rownames(VADeaths)

#só coluna 1
VADeaths[,1]

#so linha 1
VADeaths[1,]

#linhas 1 até 3
VADeaths[1:3,]

#data frame
longley
#funcina como matrize
longley[,1:3]
#acessar coluna com $
longley$Unemployed
#ou nome
longley['Unemployed']

#listas
ability.cov 
#acessando elementos
ability.cov$cov
ability.cov[1]
#verificando que são objetos diferentes
class( ability.cov$cov)
class( ability.cov$center)
#acesando elemento dentro da lista
ability.cov$cov[,1:3]

#fatores
state.region



