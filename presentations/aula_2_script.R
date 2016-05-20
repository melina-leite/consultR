#####################################
# Material de acompanhamento Aula 2 #
####################################
# Por: Melina Leite

# Mais um pouco sobre manipulação e verificação de dados

## Importando dados - matrizes

ilhas <- read.csv2("ilhas.csv",header=T,row.names = 1)
ilhas<-as.matrix(ilhas)
ilhas
 

## Totais marginais em matrizes
# Abundância por espécie
 
rowSums(ilhas) 
 
#Abundância por ilha
 
colSums(ilhas)
 

## Totais marginais: a função apply
# Abundância média por espécie
 
apply(X=ilhas, MARGIN=1, FUN=mean)
 
# Número de espécies por ilha
 
ilhas.ocor <- ilhas > 0
apply(ilhas.ocor, 2, sum)
 

## Importando data frame
 
sps <- read.csv2("sps.csv",header=T, dec=".")
sps
 

## Estrutura dados
 
str(sps)
 

## Sumário dados
 
summary(sps)


## Consertando erros
   
sps[ sps$dieta=="fruto" , ]
sps[9,4] <- "frutos"
sps[9,]
 

## Consertando erros  
# Mas a variável continua tendo 4 níveis...
 
sps$dieta
sps$dieta<- as.character(sps$dieta)
sps$dieta<- as.factor(sps$dieta)
sps$dieta

#ou
sps$dieta <- factor(sps$dieta, levels=c("frutos", "insetos", "nectar" ))



## Inserindo variáveis
# Abundância total das espécies da planilha ilhas:
   
rowSums(ilhas)
sps$abund.tot <- rowSums(ilhas)
head(sps)
 


## Ordenação de data frames  

# Ordenando `sps` por peso:
   
sps[ order(sps$peso) , ]
 
## Estatística descritiva  
 
max(sps$compr.asa)
min(sps$compr.corpo)

range(sps$peso)

mean(sps$peso)
sd(sps$peso)
var(sps$peso)

median(sps$peso)
quantile(sps$peso)
 


## Quantis  

valores <- c(15,5,3,8,10,2,7,11,12)
 

valores.ord<- valores[order(valores)]
valores.ord
 
valores.ord[5]

median(valores)

quantile(valores)
 


## Estatística descritiva: contagens  

 
table(sps$cor)

table(sps$dieta)

table(sps$cor, sps$dieta)
 


## Totais marginais: a função `tapply`
# Comprimento médio do corpo por dieta
 
tapply(X = sps$compr.corpo, INDEX = sps$dieta, FUN = mean)
 
# Peso máximo por cor e dieta
 
tapply(sps$peso, list(sps$cor, sps$dieta), max)
 


## `aggregate` em data frames
# abundância média por dieta e cor
 
aggregate(formula = abund.tot ~ cor + dieta, data = sps, FUN = mean)
 

## aggregate em data frames
# abundância e peso médio por cor e dieta
 
aggregate(cbind(abund.tot, peso) ~ cor + dieta, sps, mean)
 


#Gráficos no R

## O quarteto de Anscombe
 
ans<-anscombe
ans[12,] <-apply(ans,2,mean)
ans[13,] <-apply(ans,2,sd)
rownames(ans)[12]<-"media"
rownames(ans)[13]<-"sd"
round(ans,2)
 
## O quarteto de Anscombe
 
lim.x <- c(4,19)
lim.y <- c(3,13)

# parâmetros gráficos que serão vistos mais abaixo
par(mfrow=c(2,2) ,pch=16, mar=c(3,4,2,2), las=1, mgp=c(1.5,0.5,0))

plot(anscombe$y1 ~ anscombe$x1, 
     col="red", 
     xlab="x1", ylab="y1", 
     xlim=lim.x, ylim=lim.y)
abline(a=3,b=0.5,col="blue")

mtext("média = 7.5, sd = 1.94, correlação=0.81, y = 3 + 0.5x", 
      3, line=.8, cex=1.4, at=15)

plot(anscombe$y2 ~ anscombe$x2,
     col="red",
     xlab="x2", ylab="y2", 
     xlim=lim.x, ylim=lim.y)
abline(a=3,b=0.5,col="blue")

plot(anscombe$y3 ~ anscombe$x3,
     col="red", 
     xlab="x3", ylab="y3", 
     xlim=lim.x, ylim=lim.y)
abline(a=3,b=0.5,col="blue")

plot(anscombe$y4 ~ anscombe$x4, 
     col="red",
     xlab="x4", ylab="y4", 
     xlim=lim.x, ylim=lim.y)
abline(a=3,b=0.5,col="blue")
 
# apagando os gráficos e limpando os parâmetros gráficos de par()
dev.off()
  
# Gráficos exploratórios: Uma variável
  
# Gráficos de barra

t.dieta <- table(sps$dieta)
barplot(t.dieta, 
        ylab="número de espécies", 
        main="Dieta das espécies")
 

  
## Gráficos de barra 
    
t.dieta2 <- table(sps$cor, sps$dieta)
cores<-c("yellow3", "darkblue","brown", "darkgreen")
 
barplot(t.dieta2, legend=T, col=cores)

barplot(t.dieta2, beside = T, legend=T, col=cores)
 

## Dados dos pardais

pardais <- read.table("Sparrows.txt",header=T)
head(pardais)
 

## Gráficos de pontos: Pesos de pardais

dotchart(pardais$Wt)
axis(2)
 

## Gráficos de pontos condicional

 
dotchart(pardais$Wt, 
         groups =pardais$Sex)


### Histogramas: Peso dos animais

hist(pardais$Wt)
 

## Histogramas condicionais: lattice

# carregando o pacote lattice para gráficos condicionais
library(lattice)
 
histogram(~Wt|Sex, data= pardais)
 

## Histogramas condicionais: lattice 
  
histogram(~Wt|Species, 
          data = pardais,
          layout = c(1,2),
          strip=F,
          strip.left=T)
 


## Boxplots  

boxplot(pardais$Wingcrd,
        xlab="wingcrd")

boxplot(pardais$Tarsus,
        xlab="Tarsus")


## Boxplots condicionais

 
boxplot(Wt~Sex, 
        main="peso de pardais",
        data=pardais)
 


## Gráficos quantil-quantil

qqnorm(pardais$Culmen)
qqline(pardais$Culmen)
 

## Gráficos quantil-quantil

#simulando dados normais
normais <- rnorm(n=101,mean = 50, sd = 2)

qqnorm(normais)
qqline(normais)
 


# Gráficos: Duas variáveis


## Gráficos de dispersão

plot(pardais$Tarsus, pardais$Wingcrd)
 

## Scatterplot condicional
 
xyplot(Wt~Head|as.factor(Observer), data=pardais)
 

## Scatterplot condicional

#criando vetor com cores
cores <- ifelse(pardais$Sex=="Male", "red", "black")

plot(Wt~Head, data=pardais, col=cores, pch=16)
 

## Pairs - correlações

  
# função para colocar histogramas de frequência na diagonal do plot
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Função para colocar os valores das correlações com tamanho proporcional à correlação
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
 


 
pairs(pardais[ ,3:5], upper.panel = panel.cor, diag.panel = panel.hist)
 

# Edição de gráficos

## Parâmetros gráficos  

riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
local <- rep(c("lago1", "lago2"), each=6)
 

plot(riqueza~area)
 
  
# Aumentando os tamanhos de eixos e pontos  

  
par(cex=1.5)
plot(riqueza~area)
par(cex=1) # para voltar ao tamanho padrão
 

plot(riqueza~area, cex=1.5)
 
# Para "limpar" os parâmetros de `par`, ou volta com o valor inicial de par ou fecha o dispositivo gráfico (`dev,off`)
dev.off()

## Mudando a cor e o formato dos pontos do gráfico 
par(cex=1.5)

plot(riqueza~area, pch=4, col="orangered2")


plot(riqueza~area, pch=16, col="deeppink")
 
## Mudando nome de eixos e títulos dos gráficos
   
   
plot(riqueza~area, 
     xlab = "Área", 
     ylab="Número de Espécies", 
     main= "relação espécies-área")
 

# área em metros quadrados
plot(riqueza~area, 
     xlab = expression("Área (m"^2*")"), 
     ylab="Número de Espécies", 
     main= "Borboletas do Parque")
 
 
## Dois gráficos num mesmo plot
   
par(mfrow=c(1,2))
plot(riqueza~area, col=cores, pch=16)
boxplot(riqueza~local)
 
## Inserindo mais informações em gráficos
  
## `lines()`
 
plot(riqueza~area)
lines(area, riqueza)
lines(lowess(area,riqueza), col="red")
 

## `abline()`
   
model <-lm(riqueza~area)
plot(riqueza~area)
abline(model) # reta da regressão
 

abline(v=mean(area), lty=2, col="darkgreen") # linha vertical
abline(h=mean(riqueza), lty=4, col="purple") # linha
 
 
## `text()` e `mtext()`   
   
   
plot(riqueza~area)
text(x=10,y=43, "texto aqui")
 
 
## trocando a legenda do texto
   
   
plot(riqueza~area, xlab="")
mtext(side=1, "área do lago", line=3, cex=2)
 
 
## `axis()`
  

plot(riqueza~area, xaxt="n")
axis(1, at=seq(0,80,20), labels=c("zero", "vinte", "quarenta", "sessenta", "oitenta"))
 
 
## sobrepondo outro gráfico
  
umidade <- c(82,91,78,57,48,45,35,38,21,15,24,33)

#dando mais espaço para a margem direita
par(mar=c(5,4,4,4))

plot(riqueza~area)

par(new=T) # para permitir sobrescrever o gráfico

#plotando a nova variável Y2, mas sem desenhar o eixo
plot(umidade~area, yaxt="n", ylab="",type="l") 

# desenhando o eixo de Y2
axis(4)
mtext("Umidade (%)", 4, line = 2)
 
 
## sobrepondo outros gráficos
  
    
plot(riqueza~area)

par(new=T) # para permitir sobrescrever o gráfico

#plotando a nova variável Y2, mas sem desenhar o eixo
plot(umidade~area, yaxt="n", ylab="", type="l") 

# desenhando o eixo de Y2
axis(4)
mtext("Umidade (%)", 4, line = 2)
 

## `points`
 
riq.lago2 <- riqueza[local=="lago2"]
area.lago2 <- area[local=="lago2"]

plot(riqueza~area, pch=16, cex=2, col="orange")
points(riq.lago2~area.lago2, pch=16, col="purple", cex=2)
 
 
## Incluindo legendas
  
plot(riqueza~area, pch=16, cex=2, col="orange")
points(riq.lago2~area.lago2, pch=16, col="purple", cex=2)
legend("topleft", legend= c("lago 1", "lago 2"), pch=16, col=c("orange", "purple"), bty="n")
 

## Salvando gráficos em arquivos de imagem
  
## Exemplo

  
jpeg(filename = "figurinha.jpg", width = 480, height = 480)
plot(riqueza~area)
dev.off() # sempre precisa fechar com isso para salvar o arquivo!
 


## Abrir janela de dispositivo no Rstudio

# Geralmente usado em gráficos muito grandes

# abrindo uma janela para fazer o gráfico, à parte do Rstudio
x11()

# fazendo o plot
plot(riqueza~area)

# o plot será enviado diretamente para a janela aberta.

dev.off() #fecha o dispositivo gráfico

