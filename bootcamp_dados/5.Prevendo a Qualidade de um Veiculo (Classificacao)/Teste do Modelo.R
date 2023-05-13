#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(e1071)

carros = read.csv(file.choose(),sep=",")
amostra = sample(2,1728,replace = T, prob = c(0.7,03))
carrostreino = carros[amostra==1,]
carrosteste = carros[amostra==2,]
modelo = naiveBayes(class ~ ., carrostreino)

predicao = predict(modelo, carrosteste)
confusao = table(carrosteste$class, predicao)
taxaacerto = (confusao[1] + confusao[6] + confusao[11] + confusao[16] ) / sum(confusao) 