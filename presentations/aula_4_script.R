#####################################
# Material de acompanhamento Aula 4 #
####################################
# Por: Melina Leite
# maio 2016

#install.packages(c("vegan", "BiodiversityR", 'plot3D' ))
# pacotes usados
library(vegan)
library(BiodiversityR)
library(plot3D)


# Análises da riqueza de espécies

## Vegetação em dunas - espécies
data("dune")
dim(dune)
head(dune)

## Vegetação em dunas - ambiente 
data("dune.env")
head(dune.env)
str(dune.env)

## Curva de acumulação de espécies - curva do coletor
(Accum <- accumresult(dune, method = 'collector'))

## Curva baseada em amostra
accumplot(Accum)

## Curva de rarefação - exact method
(Accum.1 <- accumresult(dune, method='exact'))
accumplot(Accum.1)

## Curva de rarefação - random method
(Accum.2 <- accumresult(dune, method='random', permutations=1000))
accumplot(Accum.2)

Accum.21 <- accumresult(dune, method='rarefaction')
Accum.21
accumplot(Accum.21)


## Curva baseada em indivíduo
# sabendo o total de indvíduos por local
dune.env$site.totals <- apply(dune,1,sum)

(Accum.5 <- accumresult(dune, y=dune.env, scale='site.totals', method='rarefaction'))
accumplot(Accum.5, xlab='pooled individuals')


## Curva de acumulação para comparar riquezas 
Accum.6 <- accumcomp(dune, y = dune.env, factor = 'Management', method = 'exact', legend = F)


## Estimando o número total de espécies

## Jackniffe 1 e 2
(Diversity.5 <- diversityresult(dune, index='jack1'))
(Diversity.6 <- diversityresult(dune, index='jack2'))


## Chao
Diversity.7 <- diversityresult(dune, index='chao')
Diversity.7

## Bootstrap
Diversity.8 <- diversityresult(dune, index='boot')
Diversity.8

## Estimando o número total de espécies
accumplot(Accum.5, xlab='pooled individuals', ylim=c(0,35))
points(x=685, y=Diversity.5, col='red')
points(x=685, y=Diversity.6, col='blue')
points(x=685, y=Diversity.7, col='green')
points(x=685, y=Diversity.8, col='black')

  
# Análises de diversidade em comunidades
  
## Índice de Simpson
ni=c(91,1,1,1,1,1,1,1,1,1)
sim <- data.frame(ni=ni,pi=ni/100, pi2 = (ni/100)^2)
sim 

(D = sum(sim$pi2))
(D1 = 1-D)
(D2 = 1/D)

## Equabilidade de Simpson
(D2max = 10)
(E = D2/D2max)


## Índice de Simpson
(diversity.simp <- diversity(dune, index='simpson'))

# todos locais agrupados
(divers.all <- diversityresult(dune, index = "Simpson", method='all'))

## inverso de simpson
(diversity.invsimp <- diversity(dune, index='invsimpson'))


## Índice de Shannon
sim
(shan <- -(sum(sim$pi*log(sim$pi))))

(Hmax = log(10))
(Hshan <- shan/Hmax)


## Índice de Shannon
diversity.sha2 <- diversityresult(dune, index='Shannon', method='s')
diversity.sha2

# Equabilidade de Shannon:
diversity.shaE <- diversityresult(dune, index='Jevenness', method='s')
diversity.shaE


## Índice de Dominância de Berger-Parker 
sim
(berger <- 91/100)

(diversity.ber <- diversityresult(dune, index='Berger', method='s'))



## Perfil de diversidade de Rényi

#dados criados
abcd <- matrix(c(1,1,4,2,1,1,2,2,1,1,1,2,0,1,0,0,0,1,0,0), nrow=4, 
               dimnames=list(c("A", "B","C","D"), c("sp1","sp2","sp3","sp4","sp5")))
abcd

# perfil de rényi - diversidade
reu <- renyiresult(abcd, method='s')
renyiplot(reu, legend=F)

## Pefil de Rényi - equabilidade
renyiplot(reu, legend=F, evenness = T)

## Perfil de Rényi - Diversidade
#dados dunas
Renyi.1 <- renyiresult(dune)
renyiplot(Renyi.1,legend=F)

## Perfi de Rényi - equabilidade
renyiplot(Renyi.1, evenness=TRUE, legend=F)

## Perfil de Rényi por local
Renyi.2 <- renyiresult(dune, method='s')
renyiplot(Renyi.2, legend=F)
renyiplot(Renyi.2, evenness=TRUE, legend=F)

## Rényi: comparando diversidade entre locais
Renyi.3 <- renyicomp(dune, y = dune.env, factor = 'Management',
                     permutations = 100, legend = F)

  

  
# Análises das diferenças na composição de espécies

## Distância Ecológica
## Distância Euclidiana
mat <- matrix(c(1,5,0,1,5,0,0,0,1), nrow=3,
              dimnames = list(c("A","B","C"), c("sp1","sp2","sp3")))
mat

# plot distância euclidiana
scatter3D(x = mat[,1], y = mat[,2], z = mat[,3], 
          xlim = c(0, 6), ylim = c(0, 6), zlim = c(0, 4),
          xlab = "sp1", ylab = "sp2", zlab = "sp3", 
          colkey = F, ticktype = "detailed", phi = 10, 
          pch = 16, cex=2)

text3D(x = mat[ , 1] + 0.2, y = mat[ , 2] + 0.2, 
       z = mat[ , 3] + 0.3, labels = c("A", "B", "C"), add = T)

lines3D(mat [ , 1], mat[ , 2], mat[ , 3], add = T, colkey = F)
lines3D(mat[-2,1], mat[-2, 2], mat[-2, 3], add = T, colkey = F)


## Distância Euclidiana 
vegdist(mat, method="euclidean")

## Distância Euclidiana dados transformados
#abundância total por local
abund.t <- apply(mat, 1, sum)

#matriz transformada para abundancias relativas
mat2 <- mat /abund.t
mat2

dist(mat2)

scatter3D(x = mat2[,1], y = mat2[,2], z = mat2[,3], 
          xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1),
          xlab = "sp1", ylab = "sp2", zlab = "sp3", 
          colkey = F, ticktype = "detailed", phi = 10, 
          pch = 16, cex=2)
text3D(x = mat2[1, 1] + 0.1, y = mat2[1, 2] + 0.1, 
       z = mat2[1, 3] + 0.1, labels = c("A"), add=T)
text3D(x = mat2[2, 1] + 0.1, y = mat2[2, 2] - 0.1, 
       z = mat2[2, 3] + 0.2, labels = c("B"), add=T)
text3D(x = mat2[3, 1] + 0.1, y = mat2[3, 2] + 0.1, 
       z = mat2[3, 3] + 0.1, labels = c("C"), add=T)

lines3D(mat2[ , 1], mat2[ , 2], mat2[ , 3], add = T, colkey = F)


## Distância de Bray-Curtis
vegdist(mat, method = 'bray')




## Análise da distância ecológica por Ordenação

## Escalonamento multidimensional não paramétrico (NMDS)

## Dados de espécies em um gradiente de precipitação
tab <- matrix(c(9,0,0,0,4,0,8,6,2,0,0,10,
                1,1,0,2,0,1,0,0,0,2,0,0,
                0,0,0,1,0,0,0,0,2,1,4,0,
                0,0,0,1,0,0,0,0,2,1,4,0,
                0,0,4,0,6,0,7,3,3,0,1,1,
                0,1,0,2,0,0,0,0,1,1,0,0),
              nrow=12)
colnames(tab) <- c("spA", "spB", "spC", "spD", "spE","spF")
tab

# dados de precipitação
(prec <- c(270, 315, 200, 255, 190, 290, 150, 125, 230, 290, 240, 100))

  
## matriz de distância para NMDS
(distmatrix <- vegdist(tab, method = 'bray'))

## NMDS 

initNMS <- NMSrandom(distmatrix, perm=100, k=2)
model1 <- postMDS(initNMS, distmatrix)

# adicionando os escores das espécies no modelo 
model1 <- add.spec.scores(model1, tab, method='wa.scores')
# permite colocar a ordenação das espécies no mesmo gráfico

model1$stress #esses valor de stress está em % porcentagem



#plotando o NMDS
plot5 <- ordiplot(model1, type='text', cex=1.5)


## Comparando resultado do NMDS com gradiente de precipitação
  
# categorizando os dados de precipitação 
prec.cat <- c("A","A","B","A","B","A",
              "B","B","B","A","A","B")

plot6 <- ordiplot(model1, type='none')
text(x=model1$points[,1], y=model1$points[,2],
     labels=prec.cat)
text(x=model1$cproj[,1], y=model1$cproj[,2],
     labels=colnames(tab), col='red' )

## Comparando resultado do NMDS com gradiente de precipitação
m<-lm(prec~model1$points[,1])
summary(m)

plot(x=model1$points[,1], y=prec, main=paste("R²=",round(RsquareAdj(m)[[1]],2)))
abline(m, col='red')


## Shepard Diagram

# NMDS usando as funções do pacote vegan
spec <- metaMDS(tab, distance = 'bray',k=2)
spec

spec$stress # esse valor de stress está entre 0 e 1

plot6 <- ordiplot(spec, type='text')

# Shepard diagram
stressplot(spec)


## Análise de componentes principais (PCA)

## Dados abundância de peixes em rios
spe <- read.csv("DoubsSpe.csv", row.names=1)
head(spe)
env <- read.csv("DoubsEnv.csv", row.names=1)
head(env)
str(env)


## PCA variáveis ambientais (padronizadas) 

pca1 <- rda(env, scale=TRUE) # scale = T padroniza dados
summary(pca1)

## Autovalores PCA
screeplot(pca1)
points(bstick(pca1), col="red", type='o')

## Plotando os resultados da PCA
pcplot1 <- biplot(pca1, scaling=1, type="text")
pcplot2 <- biplot(pca1, scaling=2, type="text")

pcplot1 <- ordiplot(pca1, scaling=1, type="text")
ordiequilibriumcircle(pca1, pcplot1 , col='red')


# outra funca gráfica 
source("cleanplot.pca.r") #carregando a função do arquivo no workspace

cleanplot.pca(pca1)

  
## PCA em matriz de espécies

# Transformando os dados
spe.h <- decostand(spe, "hellinger")
spe.h.pca <- rda(spe.h)
summary(spe.h.pca)

screeplot(spe.h.pca)
points(bstick(spe.h.pca), col="red", type='o')

cleanplot.pca(spe.h.pca)

## FIM
