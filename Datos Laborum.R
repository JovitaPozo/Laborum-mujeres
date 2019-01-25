#=== Laborum ===#

library("rvest")


#Leer las paginas

#hacer una lista
#un for que recorra el total de paginas de resultados que arroja
#leyendo las paginas
#buscando la info en la clase
#se pasa a texto
#se guarda en la lista creada

paginas <-list()
for(i in 1:431){
  print(paste("https://www.laborum.cl/region-metropolitana/empleos-publicacion-menor-a-1-mes-pagina-",i,".html", sep= ""))
  DescargaLaborum<-read_html(paste("https://www.laborum.cl/region-metropolitana/empleos-publicacion-menor-a-1-mes-pagina-",i,".html",sep= ""))
  ContLaborum<-html_nodes(DescargaLaborum, '.titulo-aviso')
  textoLaborum<-html_text(ContLaborum)
  paginas<-c(paginas, textoLaborum)
}


print(paginas)

#Unificando los titulos
todocont <- ""
for(i in 1:length(paginas)){
  todocont<-paste(todocont," ",paginas[[i]])
}

print(todocont)

#Limpiar los datos
todocont<-gsub("/","",todocont)
todocont<-gsub(",","",todocont)
todocont<-gsub("[(]","",todocont)
todocont<-gsub("[)]","",todocont)
todocont<-gsub("-","",todocont)
todocont<-gsub("[.]","",todocont)
todocont<-gsub(":","",todocont)
todocont<-gsub("¡","",todocont)
todocont<-gsub("!","",todocont)


#separando palabras por espacio y pasando a minuscula
todocont<-strsplit(todocont," ")[[1]]
todocont<-tolower(todocont)


#contar las palabras
Unlistcont<-unlist(todocont)
tablaLab<-table(Unlistcont)


#pasar a data frame
dfPalabrasLab<-as.data.frame(tablaLab)



####tablas
dfgenero<-dfPalabrasLab[dfPalabrasLab$Unlistcont=="mujeres"|dfPalabrasLab$Unlistcont=="hombres",]
dfcomuna<-dfPalabrasLab[dfPalabrasLab$Unlistcont=="providencia"|dfPalabrasLab$Unlistcont=="peñalolen"|dfPalabrasLab$Unlistcont=="vitacura"|dfPalabrasLab$Unlistcont=="estacion"|dfPalabrasLab$Unlistcont=="maipu"|dfPalabrasLab$Unlistcont=="pudahuel"|dfPalabrasLab$Unlistcont=="quilicura"|dfPalabrasLab$Unlistcont=="huechuraba"|dfPalabrasLab$Unlistcont=="bernardo"|dfPalabrasLab$Unlistcont=="pintana"|dfPalabrasLab$Unlistcont=="condes"|dfPalabrasLab$Unlistcont=="barnechea"|dfPalabrasLab$Unlistcont=="navia"|dfPalabrasLab$Unlistcont=="renca"|dfPalabrasLab$Unlistcont=="cerrillos"|dfPalabrasLab$Unlistcont=="florida"|dfPalabrasLab$Unlistcont=="cisterna",]



##Graficos
library('ggplot2')


dfgenero %>%
  ggplot()+
  aes(x=Unlistcont, y=Freq) +
  geom_bar(stat="identity")


dfcomuna %>%
  ggplot()+
  aes(x=Unlistcont, y=Freq) +
  geom_bar(stat="identity")


#almacenar la informacion en csv
write.csv(dfgenero,file="dfgenero.csv")
write.csv(dfcomuna,file= "dfcomuna.csv")
