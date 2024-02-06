#Pruebas nuevas de analisis para hacer con INEXT
library(iNEXT)
library(ggplot2)
library(cowplot) 
library(here)
library(tidyverse)


HillART<-read.table("~/Dropbox/TRABAJO/R/PAPIIT2019/Datos_Faride/BasesMorfo/Hill_ART.txt", header= TRUE)
attach(HillART)

names(HillART)
names(HillART) <- c("2019-HPD", "2019-LPD", "2021-HPD", "2021-LPD")

#levels_order <- c("2019-LPD", "2019-HPD", "2021-LPD", "2021-HPD")

iNEXT(HillART, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95,
      nboot=50)
out <- iNEXT(HillART, q=c(0,1,2), datatype="abundance", size=NULL)

prep <- carto_pal(7, "Sunset")
pal <- prep[c(6, 2, 7, 3)]


# Plot iNEXT graph 

ggiNEXT(out, type=1, se=TRUE, facet.var="Order.q", color.var="Assemblage", grey=FALSE)
b <- ggplot_build(ggiNEXT(out, type=1, se=TRUE, facet.var="Order.q", color.var="Assemblage", grey=FALSE))
unique(b$data[[1]]$colour)

# "#D55E00" naranja
# "#0072B2" azul 
# "#CC79A7" rosa
# "#330066" morado


b$data[[1]]$colour[b$data[[1]]$colour == "#D55E00"] <- pal[1]
b$data[[1]]$colour[b$data[[1]]$colour == "#0072B2"] <- pal[2]
b$data[[1]]$colour[b$data[[1]]$colour == "#CC79A7"] <- pal[3]
b$data[[1]]$colour[b$data[[1]]$colour == "#330066"] <- pal[4]

b$data[[2]]$colour[b$data[[2]]$colour == "#D55E00"] <- pal[1]
b$data[[2]]$colour[b$data[[2]]$colour == "#0072B2"] <- pal[2]
b$data[[2]]$colour[b$data[[2]]$colour == "#CC79A7"] <- pal[3]
b$data[[2]]$colour[b$data[[2]]$colour == "#330066"] <- pal[4]

b$data[[3]]$fill[b$data[[3]]$fill == "#D55E00"] <- pal[1]
b$data[[3]]$fill[b$data[[3]]$fill == "#0072B2"] <- pal[2]
b$data[[3]]$fill[b$data[[3]]$fill == "#CC79A7"] <- pal[3]
b$data[[3]]$fill[b$data[[3]]$fill == "#330066"] <- pal[4]

b$plot$scales$scales[[1]]$palette.cache <- pal
b$plot$scales$scales[[2]]$palette.cache <- pal

bb <- ggplot_gtable(b)

plot_grid(bb)

bb
print(bb)

ggsave(bb, 
       file="~/Dropbox/TRABAJO/R/PAPIIT2019/graficas/Fig_S4Detritivores.png", 
       width = 32, 
       height = 16, 
       dpi = 300, 
       units = "cm", 
       device='png')


# Script para datos de incidencia


Art19<-read.table("~/Dropbox/TRABAJO/R/PAPIIT2019/Datos_Faride/BasesMolecular/Artropodos19.txt", header= TRUE)
attach(Art19)
Art21<-read.table("~/Dropbox/TRABAJO/R/PAPIIT2019/Datos_Faride/BasesMolecular/Artropodos21.txt", header= TRUE)
attach(Art21)

matrix.please<-function(x) {
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}

HPD19Art<-Art19[,c(1,3,4,5,8,10,12)]%>% matrix.please()
LPD19Art<-Art19[,c(1,2,6,7,9,11,13)]%>% matrix.please()
HPD21Art<-Art21[,c(1,3,4,5,8,10,12)]%>% matrix.please()
LPD21Art<-Art21[,c(1,2,6,7,9,11,13)]%>% matrix.please()
ct.df3<- list(HPD19Art,LPD19Art,HPD21Art,LPD21Art)
ct.df3

names(ct.df3) <- c("HPD19","LPD19", "HPD21","LPD21") # AGREGAR LOS NOMBRES DE LA LISTA
str(ct.df3)

#levels_order <- c("LPD19", "LPD21", "HPD19", "HPD21 )
out<-iNEXT(ct.df3,q=c(0,1,2), datatype="incidence_raw")
out

prep <- carto_pal(7, "Sunset")
pal <- prep[c(6, 2, 7, 3)]

# Plot iNEXT graph 
ggiNEXT(out, type=1, se=TRUE, facet.var="Order.q", color.var="Assemblage", grey=FALSE)
b <- ggplot_build(ggiNEXT(out, type=1, se=TRUE, facet.var="Order.q", color.var="Assemblage", grey=FALSE))
unique(b$data[[1]]$colour)

# "#D55E00" naranja
# "#0072B2" azul 
# "#CC79A7" rosa
# "#330066" morado

b$data[[1]]$colour[b$data[[1]]$colour == "#D55E00"] <- pal[1]
b$data[[1]]$colour[b$data[[1]]$colour == "#0072B2"] <- pal[2]
b$data[[1]]$colour[b$data[[1]]$colour == "#CC79A7"] <- pal[3]
b$data[[1]]$colour[b$data[[1]]$colour == "#330066"] <- pal[4]

b$data[[2]]$colour[b$data[[2]]$colour == "#D55E00"] <- pal[1]
b$data[[2]]$colour[b$data[[2]]$colour == "#0072B2"] <- pal[2]
b$data[[2]]$colour[b$data[[2]]$colour == "#CC79A7"] <- pal[3]
b$data[[2]]$colour[b$data[[2]]$colour == "#330066"] <- pal[4]

b$data[[3]]$fill[b$data[[3]]$fill == "#D55E00"] <- pal[1]
b$data[[3]]$fill[b$data[[3]]$fill == "#0072B2"] <- pal[2]
b$data[[3]]$fill[b$data[[3]]$fill == "#CC79A7"] <- pal[3]
b$data[[3]]$fill[b$data[[3]]$fill == "#330066"] <- pal[4]

b$plot$scales$scales[[1]]$palette.cache <- pal
b$plot$scales$scales[[2]]$palette.cache <- pal

bb <- ggplot_gtable(b)

plot_grid(bb)

bb
print(bb)

ggsave(bb, 
       file="~/Dropbox/TRABAJO/R/PAPIIT2019/graficas/Fig_S3Mol.png", 
       width = 32, 
       height = 16, 
       dpi = 300, 
       units = "cm", 
       device='png')
















I2es<-estimateD(ct.df3, datatype = "incidence_raw", base="coverage", level=NULL, conf = 0.95)
I2es
write.csv(I2es,"ResultadosMol_")
gDEP<- ggiNEXT(I2, type=1, facet.var = "Order.q")
gDEP


names(HillART)
names(HillART) <- c("2019-HPD", "2019-LPD", "2021-HPD", "2021-LPD")

#levels_order <- c("2019-LPD", "2019-HPD", "2021-LPD", "2021-HPD")

2019 <- as.matrix(apply(Hill19[,-1],2,as.integer))


iNEXT(HillART, q=0, datatype="incidence_raw", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95,
      nboot=50)


out <- iNEXT(HillART, q=c(0,1,2), datatype="abundance", size=NULL)
# Run iNEXT analysis
out <- iNEXT(HillART, q = 0, datatype = "incidence_raw", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 50)


prep <- carto_pal(7, "Sunset")
pal <- prep[c(6, 2, 7, 3)]


# Plot iNEXT graph 
ggiNEXT(out, type=1, se=TRUE, facet.var="Order.q", color.var="Assemblage", grey=FALSE)
b <- ggplot_build(ggiNEXT(out, type=1, se=TRUE, facet.var="Order.q", color.var="Assemblage", grey=FALSE))
unique(b$data[[1]]$colour)

# "#D55E00" naranja
# "#0072B2" azul 
# "#CC79A7" rosa
# "#330066" morado


b$data[[1]]$colour[b$data[[1]]$colour == "#D55E00"] <- pal[1]
b$data[[1]]$colour[b$data[[1]]$colour == "#0072B2"] <- pal[2]
b$data[[1]]$colour[b$data[[1]]$colour == "#CC79A7"] <- pal[3]
b$data[[1]]$colour[b$data[[1]]$colour == "#330066"] <- pal[4]

b$data[[2]]$colour[b$data[[2]]$colour == "#D55E00"] <- pal[1]
b$data[[2]]$colour[b$data[[2]]$colour == "#0072B2"] <- pal[2]
b$data[[2]]$colour[b$data[[2]]$colour == "#CC79A7"] <- pal[3]
b$data[[2]]$colour[b$data[[2]]$colour == "#330066"] <- pal[4]

b$data[[3]]$fill[b$data[[3]]$fill == "#D55E00"] <- pal[1]
b$data[[3]]$fill[b$data[[3]]$fill == "#0072B2"] <- pal[2]
b$data[[3]]$fill[b$data[[3]]$fill == "#CC79A7"] <- pal[3]
b$data[[3]]$fill[b$data[[3]]$fill == "#330066"] <- pal[4]

b$plot$scales$scales[[1]]$palette.cache <- pal
b$plot$scales$scales[[2]]$palette.cache <- pal

bb <- ggplot_gtable(b)

plot_grid(bb)

bb
print(bb)

ggsave(bb, 
       file="~/Dropbox/TRABAJO/R/PAPIIT2019/graficas/Fig_S4Detritivores.png", 
       width = 32, 
       height = 16, 
       dpi = 300, 
       units = "cm", 
       device='png')



