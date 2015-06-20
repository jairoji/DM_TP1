setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/DM/TP1/Resultados/")

library(xlsx)
library(reshape)
library(arules)
library(RWeka)
#datos = read.xlsx("TP1_DM - Base Consolidada.xlsx", sheetIndex = 1)
datos = read.csv("TP1_DM - Base Consolidada.csv", sep = ";", dec = ",")
datos[["Fecha"]] = as.Date(datos[["Fecha"]], "%d/%m/%Y")
datos = na.omit(datos)

#datos.2014 = subset(datos, Fecha < "2015-01-01")
Producto = datos[,c("Venta_ID", "Prod_ID", "SubCat_Desc", "Cat_Desc")]
Producto[,"Venta_ID"] = as.factor(Producto[,"Venta_ID"])
for(i in 1:ncol(Producto))
  Producto[,i] = droplevels(Producto[,i])
rm(datos)
Producto = unique(Producto)
Producto.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")]))), "transactions")
SubCat.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "SubCat_Desc")]))), "transactions")
Cat.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Cat_Desc")]))), "transactions")


##No bajar el soporte a menos de 0.0005
reglas.producto = apriori(Producto.table, parameter = list(supp = 0.001, conf = 0.6))
reglas.SubCat = aggregate(reglas.producto, unique(Producto[,c("SubCat_Desc", "Prod_ID")])[["SubCat_Desc"]])
reglas.Cat = aggregate(reglas.SubCat, unique(Producto[,c("SubCat_Desc", "Cat_Desc")])[["Cat_Desc"]])


X = reglas.Cat
X.Transactions = Cat.table
Important.measures = function(X, X.Transactions){
  Soporte.compartido = interestMeasure(X, method = c("support"),
                                     transactions = X.Transactions)
  Soporte.izquierdo = Soporte.compartido/interestMeasure(X, method = c("confidence"),
                                                       transactions = X.Transactions)
  lift = interestMeasure(X, method = c("lift"),
                       transactions = X.Transactions)
  soporte.derecho = Soporte.compartido /(Soporte.izquierdo*lift)
  Kulczinsky = Soporte.compartido*((1/Soporte.izquierdo) + (1/soporte.derecho))/2
  IR = abs(Soporte.izquierdo -soporte.derecho)/(Soporte.izquierdo + soporte.derecho - Soporte.compartido)
  Other.Measures = interestMeasure(X, method = c("chiSquared", "cosine"),
                                     transactions = X.Transactions)
  Measures = cbind(Soporte.compartido, lift, Other.Measures, Kulczinsky, IR)
  return(Measures)
}

medidas.cat = Important.measures(reglas.Cat, Cat.table)
medidas.Prod = Important.measures(reglas.producto, Producto.table)
quality(head(reglas.producto))
head(medidas.Prod)
inspect(reglas.producto[as.integer(rownames(subset(medidas.Prod, Kulczinsky > 0.8 & Soporte.compartido > 0.01)))])


medidas.SubCat = Important.measures(reglas.SubCat, SubCat.table)
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