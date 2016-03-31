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

2+5           # Soma
30*7          # Multiplicação
10/2          # Divisão
10^2          # Potência
log(100,10)   # Log na base 10
exp(1)        # n. euler elevado a 1
round(1.987645,2) # arredondamento
abs(-1)       # número absoluto
```

## Precedência de operações e parênteses


2*4^3-1
2*4^(3-1)
(2*4)^3-1
(2*4)^(3-1)

## Como criar um objeto

conta <- 10+2  # Criando
conta          # "Chamando"
conta*10   # x 10
conta      # ?
conta2 <- conta*10   # novo
conta2   #Agora sim salvou a operação

## Fazendo operações com objetos 

a <- sqrt(4)
a
b <- 5
b
a*b
d <- c(2,4,6)
d
d*b

## Funções!

area <- c(100,235,449, 98, 147, 214, 346, 89)
riqueza <- c(56,62,70,33,49,67,71,45)

plot(x=area, y=riqueza, log="xy")

plot(area, riqueza) # omitindo argumentos

log(x=100, base=10)

log(100,10) # omitindo argumentos

## Obtendo ajuda!

#Abrindo página geral
help.start()

#Busca de termos no help
help.search("linear regression")
??"linear regression"

#Ajuda para funções específicas
help(mean)
?mean

#Argumentos da função
args(mean)

#Exemplo da função
example(mean)

## Instalando pacotes

install.packages("vegan")

## Carregando o pacote instalado*

library(vegan)

## Linguagem orientada a objeto

area <- c(100, 235, 449, 98, 147, 214, 346, 89)
riqueza <- c(56, 62 ,70, 33, 49, 67, 71, 45)

summary(area)
summary(riqueza)


## Qual o diretório de trabalho atual?
  
getwd()

## Mudando o diretório de trabalho

setwd( "coloque aqui o caminho")


## ls e rm: listar e remover objetos

ls()

rm(objeto) #para remover o objeto de nome objeto

rm(list=ls()) # para remover todos os objetos na área de trabalho

## Classes de objetos

class(area)

x <- c("TRUE","FALSE","TRUE","TRUE")
class(x)

x <- as.logical(x)
class(x)

## Classe fatores

sexo <- rep(c("F","M"),each=9)
sexo
class(sexo)
sexo <- as.factor(sexo) # mudando a classe
sexo

## Níveis de um fator

levels(sexo)

# Contagem de observações
table(sexo)

## Operações com um vetor

# Todas as operações aplicadas a um vetor são aplicadas a cada um de seus elementos

a <- seq(from=0,to=8,by=2)
a
2*a
sqrt(a)

## Operações entre vetores
a <- seq(0,8,2)
b <- c(1,15,18,3,6)
a
b
a+b
a^(1/b)

## Comprimento de vetores
  
length(a)
length(b)

length(a)/length(b)

## Funções estatísticas

# Estas funções operam sobre TODO o vetor, e não elemento a elemento.
sum(a)
mean(a)
var(a)
min(a)
max(a)

summary(a)


#Leitura e Manipulação de Dados

## Importando dados

# arquivo txt com separação tabular
trapa <- read.table(file="trapalhoes.txt", header=T, sep="\n")

#arquivo csv com separação por , ou ;
trapa <- read.table(file="trapalhoes.csv", header=T, sep=";")
trapa

class(trapa) # vendo a classe da tabela importada


## Leitura de dados

# Cuidado com separador decimal!
# read.table(..., dec = ".") 
# read.table(..., dec=",")

# Tipos de separador de colunas
# read.table(..., sep=";") #csv Brasil
# read.table(..., sep="\n") #tabulação
# read.table(..., sep=",") #csv internacional


## SEMPRE verifique sua tabela depois de importar
  
#Onde está o erro?
trapa2<-  read.table(file="../data/trapalhoes.csv", header=T, sep=",")
trapa2
dim(trapa2)

## Vendo a estrutura dos dados importados

dim(trapa)
colnames(trapa)
rownames(trapa)

str(trapa)
# Colunas com caracteres foram convertidas para **factor**

## Vendo a estrutura dos dados importados
  
summary(trapa)

## Olhando a tabela de dados

head(trapa, n=2L) #ver as 2 primeiras linhas
tail(trapa, n=2L) # ver as 3 últimas linhas


## Selecionando colunas no data frame $

trapa$codinome
trapa$altura
trapa$estado

## Criando nova coluna
# Uso do $ para criar nova coluna
trapa$idade <- 2016 - trapa$nas
trapa

# Cuidado para não escolher um nome de coluna já existente!
  
## Criando uma nova coluna a partir de outra
  
trapa$log.alt <- log(trapa$alt)
trapa

## Substituindo valores nas colunas

trapa$vivo
trapa$vivo <- c("TRUE","TRUE", "FALSE", "FALSE")
trapa


# Operações com vetores lógicos e indexação

## Operações com vetores lógicos {.flexbox .vcenter}

# ">" maior que   

# "<" menor que  

# "==" igual a  

# "!=" diferente de  

# "&" e  

# "|" ou 

## Operações com vetores lógicos

altura <- c(1.85,1.78,1.92,1.63,1.81,1.55)
sexo <- c(rep("M",3),rep("F",3))

altura; sexo
altura > 1.80
machos.altos <- altura > 1.80 & sexo =="M"
machos.altos

## Operações com vetores lógicos
  
notas <- c(6,5.1,6.8,2.8,6.1,9.0,4.3,10.4,6.0,7.9,8.9,6.8,9.8,4.6,11.3,8.0,6.7,4.5)
notas

# Quantos aprovados?
notas >= 5
sum(notas >= 5) # número de aprovados

## Vetores lógicos em matrizes


ilhas= matrix(round(sample(c(runif(36,0,6),rep(0,4)))),ncol=8)
colnames(ilhas)<- paste("ilha",1:8)
rownames(ilhas)<- paste("sp",1:5)

ilhas
ilhas.vf = ilhas > 0  ; ilhas.vf

## Extraindo informação da matriz

# Quantas espécies por ilha?
apply(X=ilhas.vf, MARGIN=2,FUN=sum )

?apply 

## Extraindo informação da matriz

# Quantas ilhas por espécie?
apply(X=ilhas.vf, MARGIN=1,FUN=sum )

## Indexação de vetores usando [ ] para isolar elementos
a
a[1]
a[5]
a[1:2]
a[c(1,3,5)]

## Indexação [linha,coluna] em data frames

altura <- c(1.85,1.78,1.92,1.63,1.81,1.55)
sexo <- c(rep("M",3),rep("F",3))
peso<- c(80,100,115,70,65,50)
dat<-data.frame(sexo,altura,peso)
dat

dat[1,]
dat[,2]
dat[1,1]
dat[3,3]

## Indexação operações lógicas em data frames

dat$peso
dat$peso[dat$sexo == "M"]

dat$peso[dat$sexo == "M" & dat$altura > 1.80]

## Exportando dados write.table() {.columns-2}
  
write.table(trapa,"trapa.csv")

write.csv(trapa,"trapa.csv")


## Erros mais comuns no R

# Nomes de objetos e funções errados  

dados <- c(23, 10, 45)

dadoz
ls()

dados

## Um erro comum
# Funções também são objetos, se você digita uma função sem os parênteses, o R vai exibir o conteúdo da função

help # o que aparece no console?
help( ) # e agora?

## Fechar parênteses

plot(dados$cor,dados$peso


## Símbolos inesperados

# Esqueceu a vírgula separando os argumentos?  
#  Colocou espaço entre os nomes de um objeto?  

aula.pratica<-c(10,8,5,9)

plot(dadoscor dados$peso)

aula pratica
aula.pratica

## Como lidar com os erros?
     
# Leia atentamente as mensagens de erro  
     
# Acostume-se com as mensagens mais comuns  
     
# Se não entender, use o google!  
       
# Perceba a diferença entre mensagem de erro e `warning()`
