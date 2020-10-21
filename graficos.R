library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

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


############# Graficos ############# 
# paleta de cores
col_vector = c(1:13)
col_vector[[1]] = 'chartreuse4'
col_vector[[2]] = 'cyan3'
col_vector[[3]] = 'orange'
col_vector[[4]] = 'steelblue'
col_vector[[5]] = 'darkblue'
col_vector[[6]] = 'darkgoldenrod3'
col_vector[[7]] = 'mediumorchid3'
col_vector[[8]] = 'green4'
col_vector[[9]] = 'brown4'
col_vector[[10]] = 'deeppink3'
col_vector[[11]] = 'yellow3'
col_vector[[12]] = 'red4'
col_vector[[13]] = 'orange'

# ggplots
plot1 <- ggplot(acidentes %>% 
                  filter(ano != 2020) %>% 
                  group_by(ano, Natureza) %>% 
                  summarise(
                    Acidentes = sum(N_acidentes)),
                aes(x=ano, y=Acidentes, fill=Natureza)) +
  geom_col() +
  scale_fill_manual(values=c('green4', 'red4', 'navyblue', 'mediumorchid3', 'steelblue', 'gold4'))

ggsave("p_natureza.png", plot=plot1, width=20, height=12, units='cm', dpi=1000)


ggsave('plot1.jpeg',
       width = 12,
       height = 10,
       units = 'cm')

plot2 <- ggplot(acidentes %>% 
                  filter(ano != 2020) %>% 
                  group_by(ano, Grave) %>% 
                  summarise(
                    AcidentesGraves = sum(N_acidentes)),
                aes(x=ano, y=AcidentesGraves, fill = Grave)) +
  geom_col() +
  scale_fill_manual(values=c("steelblue", "red3"))+
  ggtitle("Acidentes Ferroviários por Gravidade")

plot3 <- ggplot(acidentes %>% 
                  filter(ano != 2020) %>% 
                  group_by(ano) %>% 
                  summarise(
                    Feridos = sum(`Nº Feridos`)),
                aes(x=ano, y=Feridos)) +
  geom_col(fill = 'steelblue')+
  ggtitle("Feridos em Acidentes Ferroviários")

plot4 <- ggplot(acidentes %>% 
                  filter(ano != 2020) %>% 
                  group_by(ano) %>% 
                  summarise(
                    Obitos = sum(`Nº Óbitos`)),
                aes(x=ano, y=Obitos)) +
  geom_col(fill = 'steelblue') +
  ggtitle("Óbitos em Acidentes Ferroviários")

plot1 <- ggplotly(plot1)
plot2 <- ggplotly(plot2)
plot3 <- ggplotly(plot3)
plot4 <- ggplotly(plot4)