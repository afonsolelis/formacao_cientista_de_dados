#Professor Fernando Amaral

#texto
x = read.csv(file.choose(),header = TRUE, sep = ";")
x

#odbc
install.packages("RODBC")
library(RODBC)
conexao <- odbcDriverConnect('driver={SQL Server};server=DESKTOP-UD9RQJ9\\SQLEXPRESS;database=VENDAS;trusted_connection=true')

resultado <- sqlQuery(conexao, " select * from dbo.vendas ")

resultado

odbcClose(conexao)

#planilha
install.packages("xlsx")
library(xlsx)
dados = read.xlsx(file.choose(),sheetIndex = 1)
dados

#arff
install.packages("foreign")
library(foreign)
dados = read.arff(file.choose())
dados
