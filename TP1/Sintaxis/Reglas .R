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

#datos = read.xlsx("TP1_DM - Base Consolidada.xlsx", sheetIndex = 1)
datos = read.csv("TP1_DM - Base Consolidada_SINNEGATIVOS.csv", sep = ",", encoding = 'UTF-8')
datos[["Fecha"]] = as.Date(datos[["Fecha"]], "%d/%m/%Y")
#datos = na.omit(datos)

datos = subset(datos, Fecha > "2015-01-01")
#datos = subset(datos, Fecha < "2015-01-01")
Info.clientes = unique(datos[,c("Venta_ID", "CLI_Gasto", "CLI_Compras", "CLI_CAT_DESC")])
Info.clientes.bin = as.data.frame(binarize(Info.clientes[,c("CLI_Gasto", "CLI_Compras", "CLI_CAT_DESC")]))


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
   Cat.table, DescGen.table, Producto.table, reglas.producto.subset.prunned, Info.clientes)

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

###2015
write.xlsx(Prod.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Producto", row.names = F)
write.xlsx(DescGen.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Descripción General", row.names = F, append = T)
write.xlsx(SubCat.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Subcategoría", row.names = F, append = T)
write.xlsx(Cat.rules.cliente, "3_2_Reglas_cliente_2015.xlsx", sheetName = "Categoría", row.names = F, append = T)
















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