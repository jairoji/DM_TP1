Important.measures = function(X, X.Transactions){
  Soporte.compartido = interestMeasure(X, method = c("support"),
                                       transactions = X.Transactions)
  Soporte.izquierdo = Soporte.compartido/interestMeasure(X, method = c("confidence"),
                                                         transactions = X.Transactions)
  Confidence = interestMeasure(X, method = c("confidence"),
                  transactions = X.Transactions)
  lift = interestMeasure(X, method = c("lift"),
                         transactions = X.Transactions)
  soporte.derecho = Soporte.compartido /(Soporte.izquierdo*lift)
  Kulczinsky = Soporte.compartido*((1/Soporte.izquierdo) + (1/soporte.derecho))/2
  IR = abs(Soporte.izquierdo -soporte.derecho)/(Soporte.izquierdo + soporte.derecho - Soporte.compartido)
  Other.Measures = interestMeasure(X, method = c("chiSquared", "cosine"),
                                   transactions = X.Transactions)
  Measures = cbind(Other.Measures, Kulczinsky, IR)
  return(Measures)
}

prunning.fun = function(X){
  X <- sort(X, by="lift")
  subset.X <- is.subset(X, X)
  subset.X[lower.tri(subset.X, diag=T)] <- NA
  redundant <- colSums(subset.X, na.rm=T) >= 1
  X.pruned <- X[!redundant]
  return(X.pruned)
}
  
data.frame.rules = function(X){
  X = prunning.fun(X)
  X = sort(X, by="Kulczinsky")
  X = as(X, "data.frame")
  return(X)
}
  



setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/DM/TP1/Resultados/")

library(xlsx)
library(reshape)
library(arules)
library(DiscriMiner)
library(arulesViz)

#datos = read.xlsx("TP1_DM - Base Consolidada.xlsx", sheetIndex = 1)
datos = read.csv("TP1_DM - Base Consolidada_SINNEGATIVOS.csv", sep = ",", encoding = 'UTF-8')
datos[["Fecha"]] = as.Date(datos[["Fecha"]], "%d/%m/%Y")
datos = na.omit(datos)

#datos = subset(datos, Fecha > "2015-01-01")
datos = subset(datos, Fecha < "2014-05-08" & Fecha >= "2014-03-01")
Info.clientes = unique(datos[,c("Venta_ID", "CLI_Gasto", "CLI_Compras", "CLI_CAT_DESC")])
Info.clientes.bin = as.data.frame(binarize(Info.clientes[,c("CLI_Gasto", "CLI_Compras", "CLI_CAT_DESC")]))

####################
Info.Demograficas = unique(datos[,c("Venta_ID", "cli_Loc", "CLI_Prv")])
Info.Demograficas = as.data.frame(binarize(Info.Demograficas[,c("cli_Loc", "CLI_Prv")]))

Producto = datos[,c("Venta_ID", "Prod_ID", "SubCat_Desc", "Cat_Desc", "DescGen")]
Producto[,"Venta_ID"] = as.factor(Producto[,"Venta_ID"])
for(i in 1:ncol(Producto))
  Producto[,i] = droplevels(Producto[,i])
rm(datos)
Producto = unique(Producto)

##Tablas            
Producto.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")]))), "transactions")
SubCat.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "SubCat_Desc")]))), "transactions")
Cat.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Cat_Desc")]))), "transactions")
DescGen.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "DescGen")]))), "transactions")


##No bajar el soporte a menos de 0.0005
reglas.producto = apriori(Producto.table, parameter = list(supp = 0.005, conf = 0.6))
reglas.SubCat = apriori(SubCat.table, parameter = list(supp = 0.005, conf = 0.6))
reglas.Cat = apriori(Cat.table, parameter = list(supp = 0.005, conf = 0.6))
reglas.DescGen = apriori(DescGen.table, parameter = list(supp = 0.005, conf = 0.6))


medidas.cat = Important.measures(reglas.Cat, Cat.table)
medidas.SubCat = Important.measures(reglas.SubCat, SubCat.table)
medidas.Prod = Important.measures(reglas.producto, Producto.table)
medidas.DescGen = Important.measures(reglas.DescGen, DescGen.table)

quality(reglas.Cat) = cbind(quality(reglas.Cat), medidas.cat)
quality(reglas.SubCat) = cbind(quality(reglas.SubCat), medidas.SubCat)
quality(reglas.producto) = cbind(quality(reglas.producto), medidas.Prod)
quality(reglas.DescGen) = cbind(quality(reglas.DescGen), medidas.DescGen)

#reglas.producto.subset = subset(reglas.producto, Kulczinsky > 0.6)

reglas.Cat.prunned = data.frame.rules(reglas.Cat)
reglas.SubCat.prunned = data.frame.rules(reglas.SubCat)
reglas.producto.prunned = data.frame.rules(reglas.producto)
reglas.DescGen.prunned = data.frame.rules(reglas.DescGen)

rm(reglas.producto, medidas.cat, medidas.SubCat, medidas.DescGen, 
   medidas.Prod, reglas.DescGen, reglas.Cat, reglas.SubCat, 
   Cat.table, DescGen.table, Producto.table, Info.clientes)

write.xlsx(reglas.Cat.prunned, file = "1_ReglasSup0_005.xlsx", sheetName = "Categoría", row.names = F)
write.xlsx(reglas.SubCat.prunned, file = "1_ReglasSup0_005.xlsx", sheetName = "SubCategoría", row.names = F, append = T)
write.xlsx(reglas.DescGen.prunned, file = "1_ReglasSup0_005.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(reglas.producto.prunned, file = "1_ReglasSup0_005.xlsx", sheetName = "Producto", row.names = F, append = T)


###################### Clientes

Prod.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")])), Info.clientes.bin))
Prod.table = as(Prod.table, "transactions")
DescGen.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "DescGen")])), Info.clientes.bin))
DescGen.table = as(DescGen.table, "transactions")
SubCat.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "SubCat_Desc")])), Info.clientes.bin))
SubCat.table = as(SubCat.table, "transactions")
Cat.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Cat_Desc")])), Info.clientes.bin))
Cat.table = as(Cat.table, "transactions")

###Reglas para todos los grupos

Prod.rules.cliente = apriori(Prod.table, parameter = list(supp = 0.005, conf = 0.6))
DescGen.rules.cliente = apriori(DescGen.table, parameter = list(supp = 0.005, conf = 0.6))
SubCat.rules.cliente = apriori(SubCat.table, parameter = list(supp = 0.005, conf = 0.6))
Cat.rules.cliente = apriori(Cat.table, parameter = list(supp = 0.005, conf = 0.6))

###Agregando medidas para las reglas
quality(Prod.rules.cliente) = cbind(quality(Prod.rules.cliente), Important.measures(Prod.rules.cliente, Prod.table))
quality(DescGen.rules.cliente) = cbind(quality(DescGen.rules.cliente), Important.measures(DescGen.rules.cliente, DescGen.table))
quality(SubCat.rules.cliente) = cbind(quality(SubCat.rules.cliente), Important.measures(SubCat.rules.cliente, SubCat.table))
quality(Cat.rules.cliente) = cbind(quality(Cat.rules.cliente), Important.measures(Cat.rules.cliente, Cat.table))

###Podando reglas
Prod.rules.cliente = data.frame.rules(Prod.rules.cliente)
DescGen.rules.cliente = data.frame.rules(DescGen.rules.cliente)
SubCat.rules.cliente = data.frame.rules(SubCat.rules.cliente)
Cat.rules.cliente = data.frame.rules(Cat.rules.cliente)

###Escribiendolas en un archivo xlsx
write.xlsx(Prod.rules.cliente, "2_Reglas_cliente.xlsx", sheetName = "Producto", row.names = F)
write.xlsx(DescGen.rules.cliente, "2_Reglas_cliente.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(SubCat.rules.cliente, "2_Reglas_cliente.xlsx", sheetName = "Subcategoría", row.names = F, append = T)
write.xlsx(Cat.rules.cliente, "2_Reglas_cliente.xlsx", sheetName = "Categoría", row.names = F, append = T)

###2014
write.xlsx(Prod.rules.cliente, "3_1_Reglas_cliente_2014.xlsx", sheetName = "Producto", row.names = F)
write.xlsx(DescGen.rules.cliente, "3_1_Reglas_cliente_2014.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(SubCat.rules.cliente, "3_1_Reglas_cliente_2014.xlsx", sheetName = "Subcategoría", row.names = F, append = T)
write.xlsx(Cat.rules.cliente, "3_1_Reglas_cliente_2014.xlsx", sheetName = "Categoría", row.names = F, append = T)

write.xlsx(Prod.rules.cliente, "3_1_Reglas_cliente_2014_mar-may.xlsx", sheetName = "Producto", row.names = F)
write.xlsx(DescGen.rules.cliente, "3_1_Reglas_cliente_2014_mar-may.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(SubCat.rules.cliente, "3_1_Reglas_cliente_2014_mar-may.xlsx", sheetName = "Subcategoría", row.names = F, append = T)
write.xlsx(Cat.rules.cliente, "3_1_Reglas_cliente_2014_mar-may.xlsx", sheetName = "Categoría", row.names = F, append = T)


###2015
write.xlsx(Prod.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Producto", row.names = F)
write.xlsx(DescGen.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(SubCat.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Subcategoría", row.names = F, append = T)
write.xlsx(Cat.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Categoría", row.names = F, append = T)



###################### Demográficas


Prod.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")])), Info.Demograficas))
Prod.table = as(Prod.table, "transactions")
DescGen.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "DescGen")])), Info.Demograficas))
DescGen.table = as(DescGen.table, "transactions")
SubCat.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "SubCat_Desc")])), Info.Demograficas))
SubCat.table = as(SubCat.table, "transactions")
Cat.table = as.matrix(cbind(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Cat_Desc")])), Info.Demograficas))
Cat.table = as(Cat.table, "transactions")


###Reglas para todos los grupos

Prod.rules.Demograficas = apriori(Prod.table, parameter = list(supp = 0.005, conf = 0.6))
DescGen.rules.Demograficas = apriori(DescGen.table, parameter = list(supp = 0.005, conf = 0.6))
SubCat.rules.Demograficas = apriori(SubCat.table, parameter = list(supp = 0.005, conf = 0.6))
Cat.rules.Demograficas = apriori(Cat.table, parameter = list(supp = 0.005, conf = 0.6))

###Agregando medidas para las reglas
quality(Prod.rules.Demograficas) = cbind(quality(Prod.rules.Demograficas), Important.measures(Prod.rules.Demograficas, Prod.table))
quality(DescGen.rules.Demograficas) = cbind(quality(DescGen.rules.Demograficas), Important.measures(DescGen.rules.Demograficas, DescGen.table))
quality(SubCat.rules.Demograficas) = cbind(quality(SubCat.rules.Demograficas), Important.measures(SubCat.rules.Demograficas, SubCat.table))
quality(Cat.rules.Demograficas) = cbind(quality(Cat.rules.Demograficas), Important.measures(Cat.rules.Demograficas, Cat.table))

###Podando reglas
Prod.rules.Demograficas = data.frame.rules(Prod.rules.Demograficas)
DescGen.rules.Demograficas = data.frame.rules(DescGen.rules.Demograficas)
SubCat.rules.Demograficas = data.frame.rules(SubCat.rules.Demograficas)
Cat.rules.Demograficas = data.frame.rules(Cat.rules.Demograficas)

###Escribiendolas en un archivo xlsx
write.xlsx(Prod.rules.Demograficas, "4_Reglas_Demograficas.xlsx", sheetName = "Producto", row.names = F)
write.xlsx(DescGen.rules.Demograficas, "4_Reglas_Demograficas.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(SubCat.rules.Demograficas, "4_Reglas_Demograficas.xlsx", sheetName = "Subcategoría", row.names = F, append = T)
write.xlsx(Cat.rules.Demograficas, "4_Reglas_Demograficas.xlsx", sheetName = "Categoría", row.names = F, append = T)

reglas.existentes.prod = is.element(Prod.rules.Demograficas[,"rules"], reglas.producto.prunned[,"rules"])
Nuevas.reglas.producto = Prod.rules.Demograficas[!reglas.existentes.prod,]
reglas.existentes.DescGen = is.element(DescGen.rules.Demograficas[,"rules"], reglas.DescGen.prunned[,"rules"])
Nuevas.reglas.DescGen = DescGen.rules.Demograficas[!reglas.existentes.DescGen,]
reglas.existentes.SubCat = is.element(SubCat.rules.Demograficas[["rules"]], reglas.SubCat.prunned[["rules"]])
Nuevas.reglas.SubCat = SubCat.rules.Demograficas[!reglas.existentes.SubCat,]
reglas.existentes.Cat = is.element(Cat.rules.Demograficas[["rules"]], reglas.Cat.prunned[["rules"]])
Nuevas.reglas.Cat = Cat.rules.Demograficas[!reglas.existentes.Cat,]

library(stringr)
#####Para Producto
Prod.rules.Demograficas.rhs = t(as.data.frame(strsplit(as.character(Nuevas.reglas.producto[["rules"]]), "=>")))[,2]
Prod.rules.Demograficas.rhs = str_replace_all(Prod.rules.Demograficas.rhs, "[{}]", "")
Prod.rules.Demograficas.rhs = str_replace(Prod.rules.Demograficas.rhs, "[ ]", "")
to.remove = is.element(Prod.rules.Demograficas.rhs, unique(c(as.character(Info.Demograficas[["cli_Loc"]]), as.character(Info.Demograficas[["CLI_Prv"]]))))
Prod.rules.Demograficas.No.Loc = Nuevas.reglas.producto[!to.remove,]
Prod.rules.Demograficas.Final = subset(Prod.rules.Demograficas.No.Loc, Kulczinsky > 0.6 | Kulczinsky <0.4)
Reglas.Prod.Final = is.element(as(Prod.rules.Demograficas, "data.frame")[,1], Prod.rules.Demograficas.Final[["rules"]])
plot(Prod.rules.Demograficas[Reglas.Prod.Final], method="paracoord", , control = list(main = "Reglas demográficas producto"))

#####Para DescGen
DescGen.rules.Demograficas.rhs = t(as.data.frame(strsplit(as.character(Nuevas.reglas.DescGen[["rules"]]), "=>")))[,2]
DescGen.rules.Demograficas.rhs = str_replace_all(DescGen.rules.Demograficas.rhs, "[{}]", "")
DescGen.rules.Demograficas.rhs = str_replace(DescGen.rules.Demograficas.rhs, "[ ]", "")
to.remove = is.element(DescGen.rules.Demograficas.rhs, unique(c(as.character(Info.Demograficas[["cli_Loc"]]), as.character(Info.Demograficas[["CLI_Prv"]]))))
DescGen.rules.Demograficas.No.Loc = Nuevas.reglas.DescGen[!to.remove,]
DescGen.rules.Demograficas.Final = subset(DescGen.rules.Demograficas.No.Loc, Kulczinsky > 0.6 | Kulczinsky <0.4)
Reglas.DescGen.Final = is.element(as(DescGen.rules.Demograficas, "data.frame")[,1], DescGen.rules.Demograficas.Final[["rules"]])
plot(DescGen.rules.Demograficas[Reglas.DescGen.Final], method="paracoord", , control = list(main = "Reglas demográficas descripción general"))

#####Para SubCategoría
SubCat.rules.Demograficas.rhs = t(as.data.frame(strsplit(as.character(Nuevas.reglas.SubCat[["rules"]]), "=>")))[,2]
SubCat.rules.Demograficas.rhs = str_replace_all(SubCat.rules.Demograficas.rhs, "[{}]", "")
SubCat.rules.Demograficas.rhs = str_replace(SubCat.rules.Demograficas.rhs, "[ ]", "")
to.remove = is.element(SubCat.rules.Demograficas.rhs, unique(c(as.character(Info.Demograficas[["cli_Loc"]]), as.character(Info.Demograficas[["CLI_Prv"]]))))
SubCat.rules.Demograficas.No.Loc = Nuevas.reglas.SubCat[!to.remove,]
SubCat.rules.Demograficas.Final = subset(SubCat.rules.Demograficas.No.Loc, Kulczinsky > 0.6 | Kulczinsky <0.4)
Reglas.SubCat.Final = is.element(as(SubCat.rules.Demograficas, "data.frame")[,1], SubCat.rules.Demograficas.Final[["rules"]])
plot(SubCat.rules.Demograficas[Reglas.SubCat.Final], method="paracoord", control = list(main = "Reglas demográficas subcategoría"))

#####Para Categoría
Cat.rules.Demograficas.rhs = t(as.data.frame(strsplit(as.character(Nuevas.reglas.Cat[["rules"]]), "=>")))[,2]
Cat.rules.Demograficas.rhs = str_replace_all(Cat.rules.Demograficas.rhs, "[{}]", "")
Cat.rules.Demograficas.rhs = str_replace(Cat.rules.Demograficas.rhs, "[ ]", "")
to.remove = is.element(Cat.rules.Demograficas.rhs, unique(c(as.character(Info.Demograficas[["cli_Loc"]]), as.character(Info.Demograficas[["CLI_Prv"]]))))
Cat.rules.Demograficas.No.Loc = Nuevas.reglas.Cat[!to.remove,]
Cat.rules.Demograficas.Final = subset(Cat.rules.Demograficas.No.Loc, Kulczinsky > 0.6 | Kulczinsky <0.4)
Reglas.Cat.Final = is.element(as(Cat.rules.Demograficas, "data.frame")[,1], Cat.rules.Demograficas.Final[["rules"]])
plot(Cat.rules.Demograficas[Reglas.Cat.Final], method="paracoord", control = list(main = "Reglas demográficas categoría"))

png("4_1_Reglas.demograficas_prod.png", width = 800, height = 600)
plot(Prod.rules.Demograficas[Reglas.Prod.Final], method="paracoord", , control = list(main = "Reglas demográficas producto"))
dev.off()

png("4_2_Reglas.demograficas_DescGen.png", width = 800, height = 600)
plot(DescGen.rules.Demograficas[Reglas.DescGen.Final], method="paracoord", , control = list(main = "Reglas demográficas descripción general"))
dev.off()

png("4_3_Reglas.demograficas_SubCat.png", width = 800, height = 600)
plot(SubCat.rules.Demograficas[Reglas.SubCat.Final], method="paracoord", control = list(main = "Reglas demográficas subcategoría"))
dev.off()

png("4_4_Reglas.demograficas_Cat.png", width = 800, height = 600)
plot(Cat.rules.Demograficas[Reglas.Cat.Final], method="paracoord", control = list(main = "Reglas demográficas categoría"))
dev.off()

Reglas.Prod.Final.Demog = as(Prod.rules.Demograficas[Reglas.Prod.Final], "data.frame")
Reglas.DescGen.Final.Demog = as(DescGen.rules.Demograficas[Reglas.DescGen.Final], "data.frame")
Reglas.SubCat.Final.Demog = as(SubCat.rules.Demograficas[Reglas.SubCat.Final], "data.frame")
Reglas.Cat.Final.Demog = as(Cat.rules.Demograficas[Reglas.Cat.Final], "data.frame")
Reglas.Prod.Final.Demog = cbind(Reglas.Prod.Final.Demog, rep("Producto"))
Reglas.DescGen.Final.Demog = cbind(Reglas.DescGen.Final.Demog, rep("Descripción General"))
Reglas.SubCat.Final.Demog = cbind(Reglas.SubCat.Final.Demog, rep("Subcategoría"))
Reglas.Cat.Final.Demog = cbind(Reglas.Cat.Final.Demog, rep("Categoría"))
colnames(Reglas.Prod.Final.Demog)[ncol(Reglas.Prod.Final.Demog)] = "Grupo"
colnames(Reglas.DescGen.Final.Demog)[ncol(Reglas.DescGen.Final.Demog)] = "Grupo"
colnames(Reglas.SubCat.Final.Demog)[ncol(Reglas.SubCat.Final.Demog)] = "Grupo"
colnames(Reglas.Cat.Final.Demog)[ncol(Reglas.Cat.Final.Demog)] = "Grupo"

Reglas.Demo = rbind(Reglas.Prod.Final.Demog, Reglas.DescGen.Final.Demog, Reglas.SubCat.Final.Demog, 
        Reglas.Cat.Final.Demog)

Prod.rules.Demograficas.Final = cbind(Prod.rules.Demograficas.Final, rep("Producto"))
DescGen.rules.Demograficas.Final = cbind(DescGen.rules.Demograficas.Final, rep("Descripción General"))
SubCat.rules.Demograficas.Final = cbind(SubCat.rules.Demograficas.Final, rep("Subcategoría"))
Cat.rules.Demograficas.Final = cbind(Cat.rules.Demograficas.Final, rep("Categoría"))

colnames(Prod.rules.Demograficas.Final)[ncol(Prod.rules.Demograficas.Final)] = "Grupo"
colnames(DescGen.rules.Demograficas.Final)[ncol(DescGen.rules.Demograficas.Final)] = "Grupo"
colnames(SubCat.rules.Demograficas.Final)[ncol(SubCat.rules.Demograficas.Final)] = "Grupo"
colnames(Cat.rules.Demograficas.Final)[ncol(Cat.rules.Demograficas.Final)] = "Grupo"


Reglas.Demo = rbind(Prod.rules.Demograficas.Final, DescGen.rules.Demograficas.Final, 
                    SubCat.rules.Demograficas.Final, Cat.rules.Demograficas.Final)

Orden = order(Reglas.Demo[["Kulczinsky"]], decreasing = T)
Reglas.Demo = Reglas.Demo[Orden, ]
Reglas.Demo

write.xlsx(Reglas.Demo, "4_Reglas.Definitivas.Demograficas.xlsx", row.names = F)

View(Reglas.Demo[,c("rules", "Grupo")])
View(as(Cat.rules.Demograficas[Reglas.Cat.Final], "data.frame"))




rownames(Nuevas.reglas.producto) = 1:nrow(Nuevas.reglas.producto)
#Prod.rules.Demograficas = sort(Prod.rules.Demograficas, by = support)
Prod.rules.Demograficas.rhs = t(as.data.frame(strsplit(as.character(Nuevas.reglas.producto[["rules"]]), "=>")))[,2]
Prod.rules.Demograficas.rhs = str_replace_all(Prod.rules.Demograficas.rhs, "[{}]", "")
Prod.rules.Demograficas.rhs = str_replace(Prod.rules.Demograficas.rhs, "[ ]", "")
to.remove = is.element(Prod.rules.Demograficas.rhs, unique(c(as.character(Info.Demograficas[["cli_Loc"]]), as.character(Info.Demograficas[["CLI_Prv"]]))))
Prod.rules.Demograficas.No.Loc = Nuevas.reglas.producto[!to.remove,]
asda = subset(Prod.rules.Demograficas.No.Loc, Kulczinsky > 0.6 | Kulczinsky <0.4)
asda = as(asda, "rules")



View(asda)

Prod.rules.Demograficas.rhs = t(as.data.frame(strsplit(as.character(Prod.rules.Demograficas.No.Loc[["rules"]]), "=>")))[,2]
Prod.rules.Demograficas.rhs = str_replace_all(Prod.rules.Demograficas.rhs, "[{}]", "")
Prod.rules.Demograficas.rhs = str_replace_all(Prod.rules.Demograficas.rhs, "[ ]", "")
to.remove = is.element(Prod.rules.Demograficas.rhs, as.character(Info.Demograficas[["CLI_Prv"]]))
Prod.rules.Demograficas.No.Loc.No.Prv = Prod.rules.Demograficas.No.Loc[!to.remove,]

cbind(Prod.rules.Demograficas.rhs, to.remove)
names(Info.Demograficas)

nrow(Prod.rules.Demograficas.No.Loc)

is.element(fds, as.character(Info.Demograficas[["cli_Loc"]]))

write.xlsx(Nuevas.reglas.producto, "4_Reglas_Demograficas_Nuevas.xlsx", sheetName = "Producto", row.names = F)
write.xlsx(Nuevas.reglas.DescGen, "4_Reglas_Demograficas_Nuevas.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(Nuevas.reglas.SubCat, "4_Reglas_Demograficas_Nuevas.xlsx", sheetName = "Subcategoría", row.names = F, append = T)
write.xlsx(Nuevas.reglas.Cat, "4_Reglas_Demograficas_Nuevas.xlsx", sheetName = "Categoría", row.names = F, append = T)



rules.SubCat.table.cliente.prunned = prunning.fun(rules.SubCat.table.cliente)
inspect(head(sort(rules.SubCat.table.cliente.prunned, by = "Kulczinsky")))


Cat.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Cat_Desc")]))), "transactions")


Producto.table.Cliente = as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")]))
prueba = which(colSums(Producto.table.Cliente)/nrow(Producto.table.Cliente)<0.005)
which(Producto.table.Cliente==1)
prueba[1:218]

setdiff(names(prueba), names(Producto.table.Cliente))
str(prueba)
names(prueba)
Producto.table.Cliente = as(Producto.table.Cliente, "transactions")
asdf = apriori(Producto.table.Cliente)

which(Producto.table.Cliente[,219]==1)

dput(names(Producto.table.Cliente))
Producto.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")]))), "transactions")
SubCat.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "SubCat_Desc")]))), "transactions")
Cat.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Cat_Desc")]))), "transactions")
DescGen.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "DescGen")]))), "transactions")



?write.xlsx

write.PMML(reglas.Cat.prunned, file = "1_Reglas_categoria.xml")




inspect(head(reglas.Cat.prunned))


reglas.Cat <- sort(reglas.Cat, by="Kulczinsky")
inspect(head(reglas.Cat, 10))

quality(reglas.SubCat) = cbind(quality(reglas.SubCat), medidas.SubCat)

reglas.SubCat <- sort(reglas.SubCat, by="Kulczinsky")
inspect(head(reglas.SubCat, 10))




quality(head(reglas.producto))
head(medidas.Prod)
inspect(reglas.Cat[as.integer(rownames(subset(medidas.cat, Kulczinsky > 0.45 & Kulczinsky < 0.55)))])



inspect(reglas.SubCat[which(medidas.SubCat$Kulczinsky > 0.55)])
inspect(reglas.SubCat[])

head(medidas.SubCat)


colnames(medidas.Prod)
inspect(reglas.Cat[asd])
?inspect

tail(medidas.cat)

which(IR < .9)
which(Kulczinsky < 0.4)
nrow(medidas.cat[which(Kulczinsky > 0.45 & Kulczinsky < 0.55),])
asd = medidas.cat[which(Kulczinsky > 0.55),]
asd = which(Kulczinsky > 0.55)


inspect(tail(reglas.Cat, 3))
items.loquesea = as(reglas.Cat@lhs, "rules")
  class(reglas.Cat)
inspect(head(items.loquesea), 3)

inspect(head(as(reglas.Cat@data, itemMatrix), 10))
inspect(tail(items.loquesea, 3))

Medidas = c("allConfidence", "chiSquared", "cosine")

quality(rules) <- cbind(quality(rules),
                        hyperConfidence = interestMeasure(rules, method = c("allConfidence"),
                                                          transactions = Income))