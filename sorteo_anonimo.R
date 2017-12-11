rm(list=ls())

install.packages("mailR")
#lee el archivo conf
library(mailR)

Enviar <- function(conf){    
  for (i in conf$persona){
    amigo1<-conf[conf$persona==i,"amigo1"]
    amigo2<-conf[conf$persona==i,"amigo2"]
    complementario1<-conf[conf$amigo2==amigo1,"persona"]
    complementario2<-conf[conf$amigo1==amigo2,"persona"]
    text<-sprintf("Hola %s soy el amigo invisible, tus amigos son %s (lo compartes con %s) y %s (lo compartes con %s)",i,amigo1,complementario1,amigo2,complementario2)
    #print(text)
    send.mail(from = "direccion_envio@dominio.com",to = c(conf[conf$persona==i,"correo"]),subject = "Resultados DEFINITIVOS del Sorteo con información complementaria",body = text,smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "direccion_envio@dominio.com", passwd ="password", ssl = TRUE),authenticate = TRUE,send = TRUE)
    #sendmail(conf[conf$persona==i,"correo"], subject="****PRUEBA***** Resultados del Sorteo",message=text, password="rmail")
  }
}

Sortear <- function(){
conf<-read.csv("conf.txt",stringsAsFactors=FALSE)

lista1<-conf$persona
lista2<-conf$persona

for (i in lista1){
  restriccion<-conf[conf$persona==i,]
  for (n in 1:100){
    amigo1<-sample(lista1,1)
    if (! amigo1 %in% restriccion){ break}
  }
  conf[conf$persona==i,"amigo1"]<-amigo1
  lista1<-lista1[- which(lista1==amigo1) ]
  restriccion<-conf[conf$persona==i,]
  for (n in 1:100){
    amigo2<-sample(lista2,1)
    if (! amigo2 %in% restriccion){ break}
  }
  conf[conf$persona==i,"amigo2"]<-amigo2
  lista2<-lista2[- which(lista2==amigo2) ]
}
return(conf)
}

#ahora comprobaciones
#1 nadie tiene un amigo que este entre sus restricciones y el mismo y no tiene el mismo amigo repetido

Comprobaciones <- function(conf){
  for (i in conf$persona){
    k <- 1
    amigos<-as.vector(conf[conf$persona==i,grepl( "amigo" , names( conf ) )])
    if (amigos[1]==amigos[2]){
      print("Revisar hay amigos repetidos")
      k <- 0
    } 
    restricciones<-unlist(c(i,conf[conf$persona==i,grepl( "restriccion" , names( conf ) )]))
    if (sum(amigos %in% restricciones)!=0){
      print("Revisar hay algún amigo en restriccion")
      k <- 0
    } 
    #todo el mundo sale dos veces
    regalos=sum(conf$amigo1==i)+sum(conf$amigo2==i)
    if (regalos!=2){
      print("Revisar hay alguien con numero de regalos distinto de 2")
      k <- 0
    } 
  }
  return(k)
}

conf <- Sortear()
k <- Comprobaciones(conf)
if (k==1){
  Enviar(conf)
}