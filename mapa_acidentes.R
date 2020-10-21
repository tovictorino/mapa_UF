library(leaflet)
library(mapview)
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(rmarkdown)
library(lubridate)
library(plotly)
library(jpeg)
library(htmlTable)
library(htmlwidgets)
library(R3port)
library(ggplot2)
library(htmltools)

####################### MAPA DOS ACIDENTES FERROVIÁRIOS #######################

# Leitura da base de dados de acidentes
acidentes <- readxl::read_excel("Dados/base_acidentes.xlsx")
str(acidentes)
acidentes$`Data da Ocorrência` <- dmy(acidentes$`Data da Ocorrência`)

acidentes <- rename(acidentes, "EstaçãoAnterior" = Estação)
  
acidentes <- rename(acidentes, "EstaçãoPosterior" = `...10`)

acidentes <- acidentes[2:7830,]

acidentes$N_acidentes <- 1

acidentes <- acidentes %>% 
  mutate(mes = format(`Data da Ocorrência`, "%m"),
         ano = format(`Data da Ocorrência`, "%Y"))

acidentes2019 <- acidentes %>% 
  filter(ano == 2019)

base_nomes <- readxl::read_excel("Dados/nome_est.xlsx")
base_nomes <- base_nomes[,c(2,3)]

acidentes2019 <- rename(acidentes2019, "Nome" = EstaçãoAnterior)

acidentes2019 <- plyr::join(acidentes2019, base_nomes, by='Nome', match="first")

acidentes2019$NomeEsta <- paste(acidentes2019$Nome, " (", acidentes2019$Código, ")", sep="")

acidentes2019 <- acidentes2019 %>% 
  mutate(Descarrilamento = ifelse(Natureza == 'Descarrilamento', 1, 0)) %>% 
  mutate(Abalroamento = ifelse(Natureza == 'Abalroamento', 1, 0)) %>% 
  mutate(Atropelamento = ifelse(Natureza == 'Atropelamento', 1, 0)) %>% 
  mutate(Colisão = ifelse(Natureza == 'Colisão', 1, 0)) %>% 
  mutate(Incêndio = ifelse(Natureza == 'Incêndio', 1, 0))

acidentes2019 <- acidentes2019 %>%
  group_by(NomeEsta, .add=TRUE) %>% 
  summarise(
    Acidentes = sum(N_acidentes),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`),
    Descarrilamentos = sum(Descarrilamento),
    Abalroamentos = sum(Abalroamento),
    Atropelamentos = sum(Atropelamento),
    Colisões = sum(Colisão),
    Incêndios = sum(Incêndio)) # fim aqui...

acidentes2019e <- acidentes %>% 
  filter(ano == 2019)

acidentes2019e <- rename(acidentes2019e, "Nome Estação A" = EstaçãoAnterior)
acidentes2019e <- rename(acidentes2019e, "Nome Estação B" = EstaçãoPosterior)
base_nomes <- rename(base_nomes, "Nome Estação A" = Nome)

acidentes2019e <- plyr::join(acidentes2019e, base_nomes, by='Nome Estação A', match="first")
acidentes2019e <- rename(acidentes2019e, "CódigoA" = Código)


base_nomes <- rename(base_nomes, "Nome Estação B" = `Nome Estação A`)

acidentes2019e <- plyr::join(acidentes2019e, base_nomes, by='Nome Estação B', match="first")
acidentes2019e <- rename(acidentes2019e, "CódigoB" = Código)

acidentes2019e$`Nome Estação A` <- paste(acidentes2019e$`Nome Estação A`, " (", acidentes2019e$CódigoA, ")", sep="")
acidentes2019e$`Nome Estação B` <- paste(acidentes2019e$`Nome Estação B`, " (", acidentes2019e$CódigoB, ")", sep="")

acidentes2019e <- acidentes2019e %>% 
  mutate(Descarrilamento = ifelse(Natureza == 'Descarrilamento', 1, 0)) %>% 
  mutate(Abalroamento = ifelse(Natureza == 'Abalroamento', 1, 0)) %>% 
  mutate(Atropelamento = ifelse(Natureza == 'Atropelamento', 1, 0)) %>% 
  mutate(Colisão = ifelse(Natureza == 'Colisão', 1, 0)) %>% 
  mutate(Incêndio = ifelse(Natureza == 'Incêndio', 1, 0))

acidentes2019e <- acidentes2019e %>%
  group_by(`Nome Estação A`, `Nome Estação B`) %>% 
  summarise(
    Acidentes = sum(N_acidentes),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`),
    Descarrilamentos = sum(Descarrilamento),
    Abalroamentos = sum(Abalroamento),
    Atropelamentos = sum(Atropelamento),
    Colisões = sum(Colisão),
    Incêndios = sum(Incêndio))

####################### MAPA DO SUBSISTEMA FERROVIÁRIO FEDERAL #######################

# Leitura das bases georreferenciadas
## myshp é um shapefile do tipo linestring das linhas do SFF. 
## myshp2 é um shapefile do tipo point das estações cadastradas na Declaração de Rede
myshp <- readOGR(dsn=path.expand("shp_atualizado"),
                 layer="dbo_tblLinhaEstacao_spatial_linestring", stringsAsFactors = FALSE)
myshp2 <- readOGR(dsn=path.expand("shp_atualizado"),
                  layer="dbo_tblEstacao_spatial_point", stringsAsFactors = FALSE, encoding = "UTF-8")

# Substituição do CodigoFerr, antes numérico, agora string, com o nome das estações cadastradas na DR
tblFerrovia <- readxl::read_excel("Dados/tblFerrovia.xlsx")
tblFerrovia$CodigoFerr <- tblFerrovia$CodigoFerrovia
myshp@data <- plyr::join(myshp@data,
                         tblFerrovia,
                         by='CodigoFerr')
myshp2@data <- plyr::join(myshp2@data,
                          tblFerrovia,
                          by='CodigoFerr')

# Substituição do CodigoLi00, antes numérico, agora string, com o nome das linhas cadastradas na DR
tblLinha <- readxl::read_excel("Dados/tblLinha.xlsx")
tblLinha$CodigoLi00 <- tblLinha$CodigoLinha
myshp@data <- plyr::join(myshp@data,
                         tblLinha,
                         by='CodigoLi00')

# Substituição do CodigoEsta, antes numérico, agora string, com o código de tres letras
tblEstacao <- readxl::read_excel("Dados/tblEstacao.xlsx")
tblEstacao$CodigoEsta <- tblEstacao$CodigoEstacao
myshp@data <- plyr::join(myshp@data,
                         tblEstacao,
                         by='CodigoEsta')

# Merge da tabela de atributos do shapefile com a Declaração de Rede 2020
DR_2020 <- readxl::read_excel("Dados/dr2020_original.xlsx")
DR_2020$linesta <- paste(DR_2020$Linha, DR_2020$B, sep='!')
myshp@data$linesta <- paste(myshp@data$NomeLinha, myshp@data$CodigoTresLetrasEstacao, sep='!')
myshp@data <- plyr::join(myshp@data,
                         DR_2020,
                         by='linesta')

# Ajustes à tabela de atributos do shapefile: drop de colunas desnecessárias e 
# alteração dos nomes das variáveis
myshp2@data <- select(myshp2@data, -c(CodigoEsta, CodigoFerr, CodigoMuni, DataExclus,
                                      IndicadorP, CodigoPort, CodigoEsca, CodigoEsca,
                                      CodigoArqu, IndicadorT, IndicadorF,LogotipoFerrovia,
                                      DataExclusao, IndicadorObrigatorioDesempenhoProducao))

myshp2@data <- rename(myshp2@data, "Nome Estação" = NomeEstaca)
myshp2@data <- rename(myshp2@data, "Código Estação" = CodigoTres)
myshp2@data <- rename(myshp2@data, "Ferrovia" = NomeFerrovia)

## Composição do nome completo das estações: Nome da Estação (Código de Três Letras)
df_estac <- tibble(myshp2@data$`Código Estação`, myshp2@data$`Nome Estação`)
colnames(df_estac) <- c('A','NomeA')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='A')
colnames(df_estac) <- c('B','NomeB')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='B')

myshp@data$`Nome Estação A` <- paste(myshp@data$NomeA, " (", myshp@data$A,")", sep="")
myshp@data$`Nome Estação B` <- paste(myshp@data$NomeB, " (", myshp@data$B,")", sep="")
myshp2@data$`Código Estação` <- paste(myshp2@data$`Nome Estação`, " (", myshp2@data$`Código Estação`,")", sep="")

myshp@data <- select(myshp@data, -c(B, A, linesta, CodigoEsta, CodigoLi00, CodigoFerr,
                                    CodigoLinh, NumeroSequ, IndicadorC, IndicadorE,
                                    NomeReduzidoFerrovia, LogotipoFerrovia, DataExclusao,
                                    IndicadorObrigatorioDesempenhoProducao, CodigoTresLetrasEstacao,
                                    Ferrovia, CodigoBito, NomeLinha, NomeA, NomeB,
                                    CodigoFerrovia, CodigoLinha, CodigoEstacao))

myshp@data <- rename(myshp@data, "Marco Quilométrico" = NumeroQuil)
myshp@data <- rename(myshp@data, "Extensão do Entre Pátio (km)" = NumeroExte)
myshp@data <- rename(myshp@data, "Ferrovia" = NomeFerrovia)
myshp@data <- rename(myshp@data, "Sigla - Ferrovia" = SiglaFerrovia)

myshp@data <- myshp@data %>%
  select("Ferrovia", "Sigla - Ferrovia", "Nome Estação A", "Nome Estação B", everything())

acidentes2019 <- rename(acidentes2019, "Código Estação" = NomeEsta)

myshp2@data <- plyr::join(myshp2@data, acidentes2019, by='Código Estação', match="first")

shp_acid <- myshp2

shp_acid@data$Acidentes[is.na(shp_acid@data$Acidentes)] <- 0

shp_acid.sub <- shp_acid[shp_acid$Acidentes > 0,]

shp_acid@data <- shp_acid@data %>% 
  filter(Acidentes != 0)

myshp@data <- plyr::join(myshp@data, acidentes2019e, by=c('Nome Estação A', 'Nome Estação B'), match="first")

# Logo ANTT
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Logo_ANTT.svg/1200px-Logo_ANTT.svg.png"

table_html <- acidentes %>% 
  filter(ano == 2019) %>% 
  group_by(Ferrovia) %>% 
  summarise(
    Acidentes = sum(N_acidentes),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`)
  )
  
table_html <- htmlTable(table_html)

table_html2 <- acidentes %>% 
  filter(ano==2019) %>% 
  group_by(Natureza) %>% 
  summarise(
    Acidentes = sum(N_acidentes),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`)
  )

table_html2 <- htmlTable(table_html2)

black = colorRampPalette(c('black'))







# Criação do mapa

mapa <- mapview(myshp, zcol="Acidentes",
                legend = FALSE,
                layer.name = '',
                color = black,
                lwd = 4)
  
  
mapa <- mapa@map %>%
  
  leaflet::addControl("Pesquisa de Estações", position = "topleft") %>%
  
  
  addCircleMarkers(data = myshp2, lng = myshp2@coords[,1],
                   lat=myshp2@coords[,2],
                   popup = ~`Código Estação`,
                   label=~`Código Estação`,
                   group='Código Estação') %>%
  
  addSearchFeatures(targetGroups = 'Código Estação',
                    options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = FALSE, hideMarkerOnCollapse = FALSE)) %>%
  
  groupOptions('Código Estação', zoomLevels = 10:30) 


# Mapa UFs
# Shapefile IBGE UFs
ufshp <- readOGR(dsn=path.expand("br_unidades_da_federacao"),
                 layer="BRUFE250GC_SIR", stringsAsFactors = FALSE, GDAL1_integer64_policy=TRUE, use_iconv=TRUE, encoding="UTF-8")

acidentes <- readxl::read_excel("Dados/base_acidentes.xlsx")
str(acidentes)
acidentes$`Data da Ocorrência` <- dmy(acidentes$`Data da Ocorrência`)

acidentes <- rename(acidentes, "EstaçãoAnterior" = Estação)

acidentes <- rename(acidentes, "EstaçãoPosterior" = `...10`)

acidentes <- acidentes[2:7830,]

acidentes$N_acidentes <- 1

acidentes <- acidentes %>% 
  mutate(mes = format(`Data da Ocorrência`, "%m"),
         ano = format(`Data da Ocorrência`, "%Y"))

acidentes2019UF <- acidentes %>% 
  filter(ano == 2019)

acidentes2019UF <- acidentes2019UF %>% 
  mutate(Descarrilamento = ifelse(Natureza == 'Descarrilamento', 1, 0)) %>% 
  mutate(Abalroamento = ifelse(Natureza == 'Abalroamento', 1, 0)) %>% 
  mutate(Atropelamento = ifelse(Natureza == 'Atropelamento', 1, 0)) %>% 
  mutate(Colisão = ifelse(Natureza == 'Colisão', 1, 0)) %>% 
  mutate(Incêndio = ifelse(Natureza == 'Incêndio', 1, 0))

acidentes2019UF <- acidentes2019UF %>%
  group_by(UF) %>% 
  summarise(
    Acidentes = sum(N_acidentes),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`),
    Descarrilamentos = sum(Descarrilamento),
    Abalroamentos = sum(Abalroamento),
    Atropelamentos = sum(Atropelamento),
    Colisões = sum(Colisão),
    Incêndios = sum(Incêndio))

ufs_base <- readxl::read_excel("Dados\\ufs_base.xlsx")

acidentes2019UF <- plyr::join(acidentes2019UF, ufs_base, by='UF')

acidentes2019UF$NM_ESTADO <- acidentes2019UF$est

ufshp@data <- plyr::join(ufshp@data,
                         acidentes2019UF,
                         by='NM_ESTADO')

ufshp@data <- select(ufshp@data, -c(CD_GEOCUF, NM_REGIAO, est))

red = colorRampPalette(c('yellow', 'red4'))

muf <- mapview(ufshp, zcol='Acidentes',
               legend=TRUE,
               layer.name = 'Acidentes',
               # cex = "Acidentes",
               col.regions = red,
               alpha = 0.6) +
  mapview(myshp, zcol="Acidentes",
          legend = FALSE,
          layer.name = 'Linhas',
          color = black,
          lwd = 4,
          alpha=1)

muf <- muf@map %>%
  
  leafem::addLogo(img, width = 120, height = 60, url = "http://www.antt.gov.br/", position="topleft") %>%
  
  leaflet::addControl("Pesquisa de Estações", position = "topleft") %>%
  
  
  addCircleMarkers(data = myshp2, lng = myshp2@coords[,1],
                   lat=myshp2@coords[,2],
                   popup = ~`Código Estação`,
                   label=~`Código Estação`,
                   group='Código Estação') %>%
  
  addSearchFeatures(targetGroups = 'Código Estação',
                    options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = FALSE, hideMarkerOnCollapse = FALSE)) %>%
  
  groupOptions('Código Estação', zoomLevels = 10:30) 

mapshot(muf, 'index.html', selfcontained=TRUE)

























red2 = colorRampPalette(c('yellow','red4'))
mapaUFs <- mapview(ufshp, zcol='n',
                   legend=TRUE,
                   layer.name = 'Estados',
                   cex = "n",
                   col.regions = red2)



               
mapshot(mapa, url = "index.html", selfcontained=TRUE)




addCircleMarkers(data = shp_acid.sub, lng = shp_acid.sub@coords[,1],
                 lat=shp_acid.sub@coords[,2],
                 #radius=5,
                 popup = paste("Estação: ", shp_acid.sub$`Código Estação`, "<br>",
                               "Acidentes: ", shp_acid.sub$Acidentes, "<br>",
                               "Feridos: ", shp_acid.sub$Feridos, "<br>",
                               "Óbitos: ", shp_acid.sub$Obitos),
                 label=~Acidentes,
                 color=~pal(Acidentes),
                 stroke = FALSE,
                 fillOpacity = 0.5,
                 group='Acidentes')