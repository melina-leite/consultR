#####################################
# Material de acompanhamento Aula 1 #
####################################
# Por: Melina Leite

## Para ver como citar o R em um trabalho:
# no corpo do texto pode-se colocar 
#"... as análises estatísticas foram realizadas no R (V.3.2.2 R Core Team 2015)"
#"no ambiente R (R Core Team 2015)"
citation()

###########
# Parte 1 #
###########

## O R como calculadora
2+5           #Soma
30*7          #Multiplicação
10/2          #Divisão
10^2          #Potência
sqrt(4)       #Raiz quadrada
log(100,10)   #Log na base 10

## Precedência de operações e parênteses
2*4^3 - 1
2*4^(3 - 1)
(2*4)^3 - 1
(2*4)^(3 - 1)

## Outras funções matemáticas
factorial(12)
exp(1)
round(1.987645,2)
abs(-1)

## Sintaxe básica
#função(argumento1=valor1, argumento2=valor2, ...)
log(x=100, base=10)
log(100,10)

## Obtendo ajuda!
#texto de ajuda
help(mean)
?mean
help.search("linear regression")
??"linear regression"

help.start()
#Argumentos de uma função  
args(lm)

## Instalando pacotes
#Diretamente pelo console:
install.packages("vegan")
#Carregando o pacote instalado*
library(vegan)
#* é sempre bom deixar no início do seu script o carregamento dos pacotes que você vai usar

## Como criar um objeto:
#Objeto <- expressão
objeto<-10+2        #Criando
objeto              #"Chamando" o objeto no R
objeto*10           #Multiplica por 10, mas não salva o resultado
objeto              #Veja como ele continua igual
objeto<-objeto*10   #Agora sim, salva o resultado da operação
objeto              #Agora sim ele salvou a modificação

## Outra forma de criar objetos
#Objeto = Expressão
objeto = 10+2
objeto
area = c(100,235,449, 98, 147)
area


## Fazendo operações com objetos
a<- sqrt(4)
a
b<- 5
a*b
c<- c(2,4,6)
c*b

## Um erro comum
#Funções também são objetos, se você digita uma função sem os parênteses, o R vai exibir o conteúdo da função
help

## Linguagem orientada a objeto
area = c(100,235,449, 98, 147, 214, 346, 89)
riqueza <- c(56,62,70,33,49,67,71,45)
summary(area)
summary(riqueza)

## Linguagem orientada a objeto
modelo1 <- lm(area~riqueza)
summary(modelo1)

## ls e rm: listar e remover objetos
ls()
rm(objeto)
ls()

## Qual o diretório de trabalho atual?
getwd()

##Mudando o diretório de trabalho
setwd( "") #entre as aspas você coloca o caminho do diretório que você está trabalhando

## Classes de objetos
class(area)

x <- c("TRUE","FALSE","TRUE","TRUE")
class(x)

x<- as.logical(x)
class(x)

class(modelo1)
names(modelo1)
#A classe 'lm' contém outros objetos, como os vetores de valores previstos e de resíduos, a fórmula do modelo ajustado e muito mais.

## Verificação e coerção de classes
#is.[classe] e as.[classe]
is.character(area)
is.logical(x)
y<- as.factor(x)
is.factor(y)

## Classe de datas
copa.70 <- "21/06/70"
copa.94 <- "17/07/94"
copa.70 - copa.94 # a gente consegue diminuir dois objetos da classe 'character'?
class(copa.70) 
class(copa.94)
copa.70 <- as.Date(copa.70,format="%d/%m/%y")
copa.94 <- as.Date(copa.94,format="%d/%m/%y")
copa.94 - copa.70


## Classe de fatores
sexo <- rep(c("F","M"),each=9)
sexo
class(sexo)
sexo <- as.factor(sexo)
sexo

## Níveis de um fator
levels(sexo)
#Contagem de observações:
table(sexo)

## Operações com um vetor
#Todas as operações aplicadas a um vetor são aplicadas a cada um de seus elementos
a <- seq(from=0,to=8,by=2)
a
2*a
sqrt(a)


## Operações entre vetores
#Pareando os elementos
a = seq(0,8,2)
b = c(1,15,18,3,6)
a
b
a+b
a^(1/b)

## A regra da ciclagem:
#Os elementos do vetor mais curto são repetidos sequencialmente até que a operação seja aplicada a todos os elementos do vetor mais longo
b <- c(0,0,0,0,1,1,1,1)
c <- c(1,2,3)
b*c # preste atenção na mensagem de aviso!

## Comprimento de vetores
length(a)
length(b)
length(a)/length(b)

## Funções estatísticas
sum(a)
mean(a)
var(a)
min(a)
max(a)
#Estas funções operam sobre TODO o vetor, e não elemento a elemento.
summary(a)

## Outra funções que operam sobre todo o vetor
a
cumsum(a) #percebeu a diferença?
d<- c(23,10,56,33,8)
sort(d)
sort(cumsum(a), decreasing=T)

###########
# Parte 2 #
###########

## Leitura de arquivos texto
trapa <- read.table(file="trapalhoes.txt", header=T, sep="\n")
trapa <- read.table(file="trapalhoes.csv", header=T, sep=";")
trapa
class(trapa)
dim(trapa)

## Verifique sempre sua tabela depois de importar
trapa2<-  read.table(file="trapalhoes.csv", header=T, sep=",")
trapa2
dim(trapa2) # opa! qual foi o problema aqui?

## Vendo a estrutura dos dados importados
dim(trapa)
colnames(trapa)
rownames(trapa)
summary(trapa)

## Outros argumentos da função read.table 'as.is=TRUE'
trapa3 <- read.table(file="trapalhoes.csv", header=T, sep=";", as.is=TRUE)
str(trapa3) #colocando 'as.is=TRUE' o que acontece de diferente?

## Outros argumentos da função read.table 'row.names='
trapa4 <- read.table(file="trapalhoes.csv", header=T, sep=";", row.names=1)
trapa4
str(trapa4)
rownames(trapa4)

## Olhando a tabela de dados
head(trapa, n=2L) #n=2L significa que você quer ver as 2 primeiras linhas, o default da função é 6L
tail(trapa, n=2L)

## Mudando nomes de colunas
colnames(trapa)
colnames(trapa) <- c("cod", "nas", "est", "viv", "alt")
trapa
colnames(trapa) <-colnames(trapa3)#voltando com os nomes nas colunas em extenso

## Mudando nomes de linhas
rownames(trapa)
rownames(trapa) <- c("trap1","trap2","trap3", "trap4")
trapa
rownames(trapa) <- paste("trapa",1:4,sep='')
help(paste)
trapa

## Vendo cada coluna do data frame $
# Uso do $ para selecionar colunas no data frame
trapa$codinome
trapa$altura
trapa$estado

## Criando nova coluna
#Uso do $ para criar nova coluna
trapa$idade <- 2015 - trapa$nas
trapa
#Cuidado para não escolher um nome de coluna já existente!
  
## Criando uma nova coluna a partir de outra
# Criando uma coluna do log de uma variável
trapa$log.alt <- log(trapa$alt)

## Substituindo valores nas colunas
trapa$vivo
trapa$vivo <- c("TRUE","TRUE", "FALSE", "FALSE")
trapa

## Criando matrizes
matrix(1:12, nrow=4, ncol=3)
matrix(1:12, nrow=4, ncol=3, byrow=T)

## Convetendo data frame para matriz
a<- data.frame(1:4,2:5,3:6)
class(a)
a<-as.matrix(a)
class(a)
a

## Matrizes
ilhas <- matrix(seq(from=1,to=80,by=2),ncol=8)
ilhas

## Mudando nomes de colunas e linhas de matrizes
colnames(ilhas)<- paste("ilha",1:8)
rownames(ilhas)<- paste("sp",1:5)
ilhas

## Listas - contém objetos de qualquer classe
a<-c(3,2,4,5,2,2)
area = c(100,235,449, 98, 147, 214, 346, 89)
riqueza <- c(56,62,70,33,49,67,71,45)
modelo1 <- lm(area~riqueza)
minha.lista <- list(um.vetor=a,um.data.frame=trapa,um.modelo=modelo1)

minha.lista

## Selecionando um elemento da lista
minha.lista$um.vetor
minha.lista$um.modelo

## Operações com vetores lógicos
altura <- c(1.85,1.78,1.92,1.63,1.81,1.55)
sexo <- c(rep("M",3),rep("F",3))
altura; sexo
altura >1.80
machos.altos = altura>1.80 & sexo=="M"
machos.altos


## Operações com vetores lógicos
#">" maior que   
#"<" menor que  
#"==" igual a  
#"!=" diferente de  
#"&" e  
#"|" ou 

## Operações com vetores lógicos
notas <- c(6,5.1,6.8,2.8,6.1,9.0,4.3,10.4,6.0,7.9,8.9,6.8,9.8,4.6,11.3,8.0,6.7,4.5)
notas
#Quantos aprovados?
notas>=5
sum(notas>=5)

## Vetores lógicos em matrizes
ilhas= matrix(round(sample(c(runif(36,0,6),rep(0,4)))),ncol=8) #quanta coisa! vamos destrinchar:
l<-rep(0,4)
m<-runif(36,0,6) #veja o help desta função
n<-sample(c(m,l)) #veja o help desta função
o<-round(n) #veja o help desta função
matrix(round(n), ncol=8)

colnames(ilhas)<- paste("ilha",1:8)
rownames(ilhas)<- paste("sp",1:5)
ilhas
ilhas.vf = ilhas>0 
ilhas.vf

## Extraindo informação da matriz
# Quantas espécies por ilha?
apply(X=ilhas.vf, MARGIN=2,FUN=sum )
#  Quantas ilhas por espécie?
apply(X=ilhas.vf, MARGIN=1,FUN=sum )

## Indexação de vetores usando [ ] para isolar elementos
a
a[1]
a[5]
a[1:2]
a[c(1,3,5)]

## Indexação operações lógicas em vetores
peso<- c(80,100,115,70,65,50)
altura
peso
sexo

machos.altos = altura>1.80 & sexo=="M"
peso[machos.altos]

## Indexação [linha,coluna] em data frames
dat<-data.frame(sexo,altura,peso)
dat
dat[1,]
dat[,2]
dat[1,1]
dat[3,3]

## Indexação operações lógicas em data frames
dat
dat$peso
dat$peso[dat$sexo=="M"]
dat$peso[dat$sexo=="M"& dat$altura>1.80]

## Indexação [linha,coluna] em matrizes
ilhas[1,]
ilhas[,4]

ilhas[1:3,4:8]

## Indexação data frame [linha,coluna]
dat
dat[dat$sexo=="M",3]
dat[dat$sexo=="M"& dat$altura>1.80, 3]

## Indexação com atribuição - Alteração de subconjuntos
trapa<-trapa[,1:4]
rownames(trapa) <-rep(1:4)
trapa
trapa[trapa$vivo=="TRUE", ]
trapa[trapa$vivo=="TRUE",2]<-c(2015,2014)
trapa
  
## Exportando dados write.table()
write.table(trapa,"trapa.csv")
write.csv(trapa,"trapa.csv")
