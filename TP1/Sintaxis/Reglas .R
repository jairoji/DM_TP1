setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/DM/TP1/Resultados/")

library(xlsx)
library(reshape)
library(arules)

#datos = read.xlsx("TP1_DM - Base Consolidada.xlsx", sheetIndex = 1)
datos = read.csv("TP1_DM - Base Consolidada.csv", sep = ";", dec = ",")
datos = na.omit(datos)
datos[["Fecha"]] = as.Date(datos[["Fecha"]], "%d/%m/%Y")
#datos.2014 = subset(datos, Fecha < "2015-01-01")
Producto = datos[,c("Venta_ID", "Prod_ID", "SubCat_Desc", "Cat_Desc")]
Producto[,"Venta_ID"] = as.factor(Producto[,"Venta_ID"])
for(i in 1:ncol(Producto))
  Producto[,i] = droplevels(Producto[,i])
rm(datos)
Producto = unique(Producto)
Producto.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")]))), "transactions")
Producto.table = as(as.matrix(as.data.frame.matrix(table(Producto[,c("Venta_ID", "Prod_ID")]))), "transactions")

##No bajar el soporte a menos de 0.0005
reglas.producto = apriori(Producto.table, parameter = list(supp = 0.001, conf = 0.6))
reglas.SubCat = aggregate(reglas.producto, unique(Producto[,c("SubCat_Desc", "Prod_ID")])[["SubCat_Desc"]])
reglas.Cat = aggregate(reglas.SubCat, unique(Producto[,c("SubCat_Desc", "Cat_Desc")])[["Cat_Desc"]])
