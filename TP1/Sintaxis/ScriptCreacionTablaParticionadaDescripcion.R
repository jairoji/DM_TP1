DataSetCategorias <- read.csv(paste("~/Data Mining Maestria/Proyectos/DM/Query Subcategorias con Attributos",".csv", sep=""), sep=",", stringsAsFactors=FALSE)
names(DataSetCategorias)
dfMarcaAtributos <- data.frame(marca_ID=integer(),
                 marca_Nombre=character(), 
                 marca_AtributoNombre=character(),
                 marca_AtributoValor=character(),
                 subCat_id = integer(),
                 stringsAsFactors=FALSE)
rows = nrow(DataSetCategorias)
cols = ncol(DataSetCategorias)
print (paste("total of rows:", rows, sep=" "))
print (paste("total of cols:", cols, sep=" "))
#looping to the dataset
id= 1
for(i in 1:rows){
 
    #print(paste("rowSelected: ", i, "colSelected:", j, sep=" "))
   stringAtributoGeneral = DataSetCategorias[i,4]   
   print(stringAtributoGeneral)
   #x <- "BAÑO QUÍMICO    "
   #x <- "MIRA TELESCÓPICA HO Aumentos: 4 X Diámetro: 20 mm.  Poste: MULTICROSS"
   #patron de expresion regular para extraer las categoria antes del primer atributo
   patronCategoria <- "^(.*?)[a-zA-Z0-9,ú,é,á,ó,/]*:"
   #patron de expresion regular para extraer los atributos con los dos puntos
   patronAtributos <- "[a-z,A-Z,0-9,ú,é,á,ó,/]*:"
   m <- regexpr(patronCategoria, stringAtributoGeneral)
   previoCategoria <-regmatches(stringAtributoGeneral, m, invert = FALSE)
   mAtributos <- regexpr(patronAtributos, previoCategoria)
   z1<- regmatches(previoCategoria, mAtributos, invert = FALSE)
   z2 <- regmatches(previoCategoria, mAtributos, invert = TRUE)
   #mAtributos <- gregexpr(patronAtributos, categoria)
   if(length(z1) == 0){ # Sin attributos
     categoria <- stringAtributoGeneral
     listaNombreAtributos <- ""
     listaValorAtributos <- ""
   } else{
     #reemplazo con el primer Atributo
     strAtributos <- gsub(patronCategoria, z1, stringAtributoGeneral , perl=TRUE)
     ListaCategoria <- unlist(z2)
     categoria <- ListaCategoria[ListaCategoria != ""]
     #separar atributos
     mAtributos <- gregexpr(patronAtributos, strAtributos)
     nombreAtributos <- regmatches(strAtributos, mAtributos, invert = FALSE)
     valorAtributos <- regmatches(strAtributos, mAtributos, invert = TRUE)
     listaNombreAtributos <- unlist(nombreAtributos)
     #eliminar los dos puntos con reemplazo
     listaNombreAtributos <- gsub(":", "", listaNombreAtributos)
     listaValorAtributos <- unlist(valorAtributos)
     listaValorAtributos <- listaValorAtributos[listaValorAtributos != ""]
   }
   print(id)
   print(categoria) #no necesario
   print(listaNombreAtributos)
   print(listaValorAtributos)
   if(length(listaNombreAtributos) == 0){
     dfMarcaAtributos[id,1]<-id
     dfMarcaAtributos[id,2]<-DataSetCategorias[i,3]
     dfMarcaAtributos[id,3]<-DataSetCategorias[i,""]
     dfMarcaAtributos[id,4]<-DataSetCategorias[i,""]
     dfMarcaAtributos[id,5]<-DataSetCategorias[i,1]
     id=id+1
   }else{
     j <-0
     for(j in 1:length(listaNombreAtributos)){
       dfMarcaAtributos[id,1]<-id
       dfMarcaAtributos[id,2]<-DataSetCategorias[i,3]
       dfMarcaAtributos[id,3]<-listaNombreAtributos[j]
       dfMarcaAtributos[id,4]<-listaValorAtributos[j]
       dfMarcaAtributos[id,5]<-DataSetCategorias[i,1]
       id=id+1
     }
   }
   
}
 
#DataSetCategorias$DescAdic
View(dfMarcaAtributos)
fileName <- paste("~/Data Mining Maestria/Proyectos/DM/marcaAtributos",".csv", sep="")
write.csv(dfMarcaAtributos, fileName, row.names=FALSE)