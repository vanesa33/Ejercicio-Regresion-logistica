## Regresion Logistica
setwd("C:/Users/Layla Scheli/Desktop/Docencia/BA Emprende/Programa en Ciencia de Datos/6. Clase/Ejercicio_Regresion_Logistica")
h <- read.csv("RegresionLogistica1.csv",sep=";")

#Convierto a Urgente en un vector
h$Urgente <- as.vector(h$Urgente)

#Genero una variable dummy y la agrego al dataframe
u = ifelse(h$Urgente == "SI",1,0)
h <- cbind(h,u)
h$Urgente<-NULL
head(h)

#Separamos en train y test
x <- runif(nrow(h))
train <- h[which(x<.7),]
test <- h[which(x>=.7),]

#Miramos como quedo todo
nrow(h)
nrow(train)
nrow(train)

#Aplicamos formula y el predict
g <- glm(formula = u ~ Temperatura + Tensi.n.Art..Alta + Tensi.n.Art..Baja + Saturaci.n.O2 + Pulso.Card.aco, data = train)
p <- predict(g, newdata = test)
hist(p)

q <- floor(p+.5) #redondeamos y normalizamos para tener solo 0 y/o 1
table(q)

#Generamos la matriz de confusion
m <- matrix(nrow=2, ncol=2, 0)
for(i in 0:1) {
  for(j in 0:1) {
    m[i+1,j+1] <- length(which(q==i & test$u==j))
  }
}
m