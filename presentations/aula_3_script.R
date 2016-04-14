#####################################
# Material de acompanhamento Aula 2 #
####################################
# Por: Melina Leite

## Um problema

arv <-read.table("arvores.csv", sep=";", header=T)
hist(arv$alt, main="Altura das árvores, média = 22,7; sd = 5,6")
mean(arv$alt); var(arv$alt); sd(arv$alt)


## Função de probabilidade 

# exemplo de gráfico da função de densidade (prob) de uma distribuição normal
mean=22.7; sd=5.6
lb=10; 
ub=40

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="X", ylab="Densidade",
     main="Distribuição Normal, Média = 25, sd= 4", axes=FALSE)
axis(1)
axis(2)
i <- x >= lb & x <= ub
lines(x, hx)
abline(v=22.7, lty=2)

  
# Algumas Distribuições de Probabilidade
  
## Bernouli: X ~ Bernouli(p)
plot(dbinom(0:1, size=1, prob=0.3), 
     xlim=c(0,3), ylim=c(0,1), 
     xlab="resultados previstos",
     ylab="Probabilidade", 
     main="Distribuição Bernoulli p=0.3",
     type="h", xaxt="n")
axis(1, at=c(1,2), labels = c(0,1))

## Binomial: X ~ Bin(n, p)
plot(y=dbinom(0:6, size=6, prob=0.3), 
     x=c(0:6),
     xlab="Número de girinos predados",
     ylab="Probabilidade", 
     main="Distribuição binomial:  n=6, p=0.3",
     type="h")

## Poisson:  X ~ Pois(λ)
plot(dpois(seq(1,20, by =1), lambda = 10), type ="h",
     xlab = "Número de visitas", 
     ylab = "Probabilidade", 
     main = "Função massa de probabilidade")

## Normal: X ~ N(μ, σ) {.columns-2}
prob <- pnorm(q = 20.15, mean = 17.1, sd = 1.21, lower.tail = F)
prob

x<-seq(13, 22, 0.1)
y=dnorm(x, mean = 17.1, sd=1.21)
plot(x, y, type="l", lwd=2, col="red", 
     ylab = "Probabilidade",
     xlab= "Comprimento dos peixes",
     main ="Função densidade de probabilidade")
abline(v = 17.1, lty=2)
abline(v = 20.15, lty=3)
text(x=21,y=0.3, labels="P = 0.006")


## Voltando ao nosso problema
  
levels(arv$sp)
library(lattice)
histogram(~alt|sp, 
          data = arv,
          layout = c(1,2),
          strip=F,
          strip.left=T)

## Diferença entre as médias das alturas

mean(arv$alt[arv$sp=="sp2"]) - mean(arv$alt[arv$sp=="sp1"])

## Teste t

t.test(arv$alt~arv$sp, var.equal=T)

curve(expr=dt(x, 98),
      main="Distribuição t (df=98)", 
      xlab="Valor t",
      ylab="Densidade Probabilística (dt)",
      xlim=c(-6,6))
abline(v=-5.0125, col="red")

text(x= -4, y=0.35, paste("p =", round(pt(-5.0125,98),6)), 
     cex=0.8, col="red")


# Modelos lineares
  
## ANOVA one-way
# dados novos para sp 3
alt.sp3 <- c(36.90076761,33.23131727,32.82767311,24.93410577,31.87626329,
             28.76824248,26.6436144,31.41238398,27.65383929,31.70425719,
             33.60752445,28.30263354,29.32210674,23.4790054,22.20793046,
             18.28797293,23.39044736,28.75820917,29.94344703,34.47430235,
             32.09820862,26.99845592,27.64858951,31.90557306,33.63236368,
             25.35966179,35.00885172,32.89125598,18.98802229,30.8516906,
             22.04191398,35.37232889,35.67150408,34.30257037,24.43038475,
             20.6274663,31.15717916,23.27023231,36.20568545,34.33068448,
             28.40769054,31.00578874,29.50989158,31.69915991,37.25975797,
             30.1279997,38.623371,35.36472829,29.88304259,31.00145491)

sp3 <- data.frame(alt=alt.sp3, sp=rep("sp3",50))
arv2 <- rbind(arv,sp3)

#pegando uma amostra dos dados para facilitar os cálculos subsequentes
amostras <- seq(1,150,by=5)
arv3 <- arv2[ amostras, ]
arv3 <- arv3[order(arv3$sp),]

# gráfico dos dados usados nas análises
boxplot(alt~sp, data=arv3, ylab="Altura árvores")
points(arv3$alt~arv3$sp)

  
## ANOVA na unha
  
## Soma dos quadrados total
# re-arranjando os dados para fazer os cálculos mais facilmente
arv4 <- data.frame(sp1=arv3$alt[arv3$sp=="sp1"],
                   sp2=arv3$alt[arv3$sp=="sp2"],
                   sp3=arv3$alt[arv3$sp=="sp3"])

media.geral <- mean(arv3$alt)
media.geral
medias.sps <- apply(arv4, 2, mean)
medias.sps
dif.geral <- arv4 - media.geral
ss.especies <- dif.geral^2
ss.especies
ss.total <- sum(ss.especies)
ss.total

vetor.cor <- rep(1:3, each=10) #vetor de cores

# figura da soma dos quadrados total
plot(x = c(1:30), y = arv3$alt, ylim=c(8,40),
     pch=(rep(c(15,16,17), each=10)),
     col=arv3$sp,
     ylab="Altura árvores", xlab="Observações",
     main="Variação total")
for(i in 1:30)
{
  lines(c(i,i),c(arv3$alt[i],mean(arv3$alt)),col=vetor.cor[i])
}
abline(h=media.geral)


## Soma dos quadrados dentro de cada grupo

vetor.medias<-rep(medias.sps, each=10)

#grafico da soma dos quadrados em cada grupo
plot(c(1:30), arv3$alt, ylim=c(8,40),
     pch=(rep(c(15,16,17),each=10)),
     col=vetor.cor,
     main="Variação Intra Grupos",
     ylab="Variável Resposta", xlab="Observações")
for(i in 1:30)
{
  lines(c(i,i),c(vetor.medias[i],arv3$alt[i]),col=vetor.cor[i])
}
lines(c(1,10),c(medias.sps[1],medias.sps[1]),col=1)
lines(c(11,20),c(medias.sps[2],medias.sps[2]),col=2)
lines(c(21,30),c(medias.sps[3],medias.sps[3]),col=3)


## Soma dos quadrados dentro de cada grupo
(ss.sp1=sum((arv4$sp1-medias.sps["sp1"])^2))
(ss.sp2=sum((arv4$sp2-medias.sps["sp2"])^2))
(ss.sp3=sum((arv4$sp3-medias.sps["sp3"])^2))
(ss.intra=ss.sp1+ss.sp2+ss.sp3)

## Soma dos quadrados entre grupos
# grafico da soma dos quadrados entre grupos
plot(c(1:30), vetor.medias, ylim=c(10,40), 
     pch=(rep(c(15,16,17),each=10)),
     col=vetor.cor, 
     main="Variação Entre Grupos", 
     ylab="Variável Resposta", xlab="Observações")
for(i in 1:30)
{
  lines(c(i,i),c(vetor.medias[i],mean(vetor.medias)),col=vetor.cor[i])
}
abline(h=media.geral)
points(c(1:30),arv3$alt, ylim=c(10,50), 
       pch=(rep(c(0,1,2),each=10)), col=vetor.cor, cex=0.5)


## Soma dos quadrados entre grupos 
medias.sps
media.geral
ss.entre=10*sum((medias.sps-media.geral)^2)
ss.entre

#conferindo os cálculos
ss.intra+ss.entre
ss.total

## Desvios médios
ms.entre=ss.entre/2
ms.intra=ss.intra/27
ms.entre
ms.intra

## Cálculo de F
#F - razão das variâncias
F.sps=ms.entre/ms.intra
F.sps

## Distribuição F 

p.sps=pf(F.sps, 2, 27, lower.tail=FALSE)
p.sps

# gráfico da distribuição F indicando o resultado
curve(expr=df(x, 2,27),
      main="Distribuição F de Fisher (df=2,27)", 
      xlab="Valor F",
      ylab="Densidade Probabilística (df)",
      xlim=c(2,12))
abline(v=F.sps, col="red")
abline(h=0, lty=2)

xf=seq(F.sps, 12, 0.01)
ydf=df(xf, 2, 27)
polygon(c(F.sps,xf),c(0,ydf),col="red")

text(x= 7,y=0.08,paste("pf(x) =",
                       round(pf(F.sps,2,27,lower.tail=F),4)), 
     cex=0.8, col="red")

  
## ANOVA no R
anova.sps <- aov(alt~sp, data=arv3)
summary(anova.sps)


## Testes à posteriori - onde estão as diferenças entre os grupos?
boxplot(arv4) # 

## Teste Tukey
TukeyHSD(anova.sps)

#revendo gráfico dos dados
stripchart( arv3$alt~arv3$sp,
      vertical = TRUE, pch = 16, 
      col = c("black", "red", "green"))


## Outra maneira de fazer a ANOVA no R 
lm.anova.sp <- lm(alt~sp, data=arv3)
summary.aov(lm.anova.sp) #mesmo que o summary do aov

## Resultado ANOVA como LM 
summary(lm.anova.sp) 


## Regressão linear simples

# criando a variável nutrientes
nutri <- c(1.09, 1.73, 1.49, 2.84, 3.08, 8.24, 8.67, 6.10, 8.37, 8.03,
           10.62, 14.26, 13.77, 14.81, 14.40, 17.27, 15.46, 18.66, 16.03,
           17.37, 26.80, 27.1, 29.09, 20.46, 24.51, 28.56, 24.87, 27.44,
           26.02, 22.94)

# ordenando arv3 por altura (para criar uma relação com nutrientes)
arv3 <-arv3[order(arv3$alt), ]

#colocando no data.frame de dados
arv3$nutri <- nutri

plot(arv3$alt ~ nutri)

  
## Modelo da Regressão 
alt.nutr <- lm(alt ~ nutri, data = arv3);     summary(alt.nutr)

## Plot da Regressão
plot(arv3$alt ~ nutri)
abline(alt.nutr, col = "red") # reta da regressão
abline(h = mean(arv3$alt), col = "blue", lty = 2) #reta hip nula  


## Análise de Covariância
plot(alt~nutri, data=arv3, col=arv3$sp)

## Modelo da ANCOVA: aditivo 
alt.sp.nutr <- lm(alt ~ nutri + sp, data = arv3);     summary(alt.sp.nutr)


## Gráfico ANCOVA aditivo
# tirando os valores de intercepto do modelo para cada especie
coef(alt.sp.nutr)
int.sp1 <- coef(alt.sp.nutr)[1]
int.sp2 <- coef(alt.sp.nutr)[1]+coef(alt.sp.nutr)[3]
int.sp3 <- coef(alt.sp.nutr)[1]+coef(alt.sp.nutr)[4]

plot(alt~nutri, data=arv3, col=arv3$sp)

# reta de regressão para espécie 1
abline(a=int.sp1, b=coef(alt.sp.nutr)[2], col="black")
# reta de regressão para sp 2
abline(a=int.sp2, b=coef(alt.sp.nutr)[2], col="red")
# reta de regressão para ps 3
abline(a=int.sp3, b=coef(alt.sp.nutr)[2], col="green")


## Modelo ANCOVA com interação 
# duas notações para o mesmo modelo:
alt.sp.nutr2 <- lm(alt ~ nutri + sp + nutri:sp, data=arv3)
alt.sp.nutr3 <- lm(alt ~ nutri*sp, data=arv3) #
summary(alt.sp.nutr3)

## Gráfico ANCOVA com interação

# interceptos
int.sp1 <- coef(alt.sp.nutr2)[1]
int.sp2 <- coef(alt.sp.nutr2)[1] + coef(alt.sp.nutr2)[3]
int.sp3 <- coef(alt.sp.nutr2)[1] + coef(alt.sp.nutr2)[4]

#coeficientes
coef.sp1 <- coef(alt.sp.nutr2)[2]
coef.sp2 <- coef(alt.sp.nutr2)[2] + coef(alt.sp.nutr2)[5]
coef.sp3 <- coef(alt.sp.nutr2)[2] + coef(alt.sp.nutr2)[6]

plot(alt~nutri, data=arv3, col=arv3$sp)

abline(a=int.sp1, b=coef.sp1, col="black", xlim=c(0.5))
abline(a=int.sp2, b=coef.sp2, col="red")
abline(a=int.sp3, b=coef.sp3, col="green")

## Tabela de ANOVA do modelo

summary.aov(alt.sp.nutr3)


## Modelos lineares generalizados

## GLM Poisson 
atrop <- read.table("atropela.csv", header = T, sep = ";")

plot( atrop$TOT.N ~ atrop$D.PARK,
      xlab = " Distância ao parque",
      ylab = "Número de atropelamentos")

## Modelo GLM Poisson 
mod <- glm(TOT.N ~ D.PARK, data = atrop, family = poisson)
summary(mod)


## Testando contra hipótese nula 
anova(mod, test = "Chisq")

## Gráfico Modelo GLM Poisson
coefi <- coef(mod)
coefi

plot(atrop$TOT.N~atrop$D.PARK,
     xlim = c(0,25000), ylim = c(0, 110),
     xlab = "Distância ao parque", ylab = "Número de atropelamentos")

#essa função plota a curva do modelo, colocamos exp para voltar à escala original dos dados
curve(exp(coefi[1] + coefi[2]*x), 
      xlim = c(0,25000), ylim = c(0, 110),
      xlab = "", ylab = "", 
      add=T, #adicionando a curva no plot já desenhado
      col="red")

  
## GLM Binomial
veados <- read.table("veados_parasitas.csv", header=T, sep=";")

str(veados)
summary(veados)

prop.veados <- veados$infectado/veados$amostrado
prop.veados

plot(prop.veados ~ veados$openland,
ylim=c(0,1),
xlab = "Porcentagem de vegetação aberta",
ylab="Proporção de veados infectados")


## Modelo GLM binomial 
y <- cbind(veados$infectado, veados$amostrado - veados$infectado)
mod.veado <- glm(y ~  openland, data=veados, family="binomial")
summary(mod.veado)

## Teste GLM binomial
anova(mod.veado, test = "Chisq")

## Gráfico GLM binomial
# salvando os coeficientes
coefs <- coef(mod.veado)
coefs
#lembre que estes coefs foram linearizados com a função de ligação
# para a binomial a link function é a logit
# então temos que transformar os y no inverso do logit, a função é essa:
invlogit = function(x) return(exp(x)/(1+exp(x)))

plot(prop.veados ~ veados$openland, ylim=c(0,1))
#usando o invlogit para desenhar a curva:
curve(invlogit(coefs[1] + coefs[2]*x), add=T, col="red")


# Checando as premissas dos modelos

## Teste de normalidade 
shapiro.test(arv3$alt)

shapiro.test(arv$alt[arv$sp == "sp1"])
shapiro.test(arv$alt[arv$sp == "sp2"])

## Teste de homogeneidade de variâncias
bartlett.test(arv3$alt~arv3$sp)

## Teste homogeneidade de variâncias
# carregando o pacote car que tem a função do teste
library(car)
leveneTest(arv3$alt~arv3$sp)


## Checagem do ajuste do modelo - Resíduos

resid(anova.sps)
#residuals(anova.sps) # o mesmo que a função de cima

## Resíduos ANOVA: alt ~ sp
par(mfrow=c(2,2)) # separando a área gráfica em 4 espaços 
plot(anova.sps)

## Resíduos regressão: alt ~ nutri
plot(alt.nutr)

## Resíduos ANCOVA aditivo: alt ~ sp + nutri
plot(alt.sp.nutr)

## Resíduos ANCOVA interativo: alt ~ sp * nutri
plot(alt.sp.nutr2)

## Resíduos GLM poisson: atrop ~ dist.park
plot(mod)

## Resíduos LM com a poisson: atrop ~ dist.park
plot(lm(TOT.N ~ D.PARK, data = atrop))

## Resíduos GLM binomial: prop.infec ~ openland
plot( mod.veado)

## Resíduos LM com a binomial: prop.infec ~ openland
plot( lm(prop.veados ~ veados$openland))


par(mfrow=c(1,1)) # para "desligar" o par inicial
