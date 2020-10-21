library(rgdal)
library(rgeos)
library(mapview)
library(leaflet)
library(RColorBrewer)
library(dplyr)

# Leitura das bases georreferenciadas
## myshp = linhas. myshp3 = estações
myshp <- readOGR(dsn=path.expand("C:\\2020\\ANTT\\Mapas_FRA\\Linhas_Final"),
                 layer="Linhas_Final", stringsAsFactors = FALSE, GDAL1_integer64_policy=TRUE)

shpESTA <- readOGR(dsn=path.expand("C:\\Declaração de Rede 2020\\Estacoes"),
                   layer="Estacoes", stringsAsFactors = FALSE)

ufshp <- readOGR(dsn=path.expand("C:\\2020\\ANTT\\Mapas_FRA\\UFs"),
                 layer="UFs", stringsAsFactors = FALSE, GDAL1_integer64_policy=TRUE, use_iconv=TRUE, encoding="UTF-8")

munshp <- readOGR(dsn=path.expand("C:\\2020\\ANTT\\Mapas_FRA\\municip"),
                 layer="municipios", stringsAsFactors = FALSE, GDAL1_integer64_policy=TRUE, use_iconv=TRUE, encoding="UTF-8")
munshp <- munshp[!is.na(munshp@data$n),]

# parte da representação gráfica atualizada está no arquivo Graficos_Mapview.R
n <- 14
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
white = colorRampPalette(c('white', 'gray'))

red = colorRampPalette(c('yellow','red4'))


m1 <- mapview(myshp, zcol='Ferrovia',
              legend = FALSE,
              layer.name = 'Ferrovia',
              color = 'black')
m2 <- mapview(shpESTA, zcol='CodigoTres', cex = 2,
              alpha = 0, alpha.regions = 0.5, legend = FALSE,
              col.regions = 'black', layer.name = 'Estações')
mF = m1 + mmun # Gráfico do SFF com duas camadas; linhas e estações.

mmun <- mapview(munshp, zcol='n',
               legend=TRUE,
               layer.name = 'Municipios',
               cex = "n",
               col.regions = red)

muf <- mapview(ufshp, zcol='n',
               legend=TRUE,
               layer.name = 'Estados',
               cex = "n",
               col.regions = red)
mapa <- muf+m1

mapshot(mapa, 'index.html', selfcontained=TRUE)
