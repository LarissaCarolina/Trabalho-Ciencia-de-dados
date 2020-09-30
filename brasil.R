#######################################################
#             Trabalho de Ciencia de Dados            #
#     Ana Clara, Amanda, Larisa, Leticia, Natalia     #
#######################################################

#setwd("~/Nat?lia/Ciencia de dados/Brasil")
rm(list=ls(all=TRUE))

#########################
#  Carregando pacotes   #
#########################
require(tidyr)
require(tidyverse)
require(data.table)
require(lubridate)
require(gganimate)
#########################

#### Brasil ####

###################################
#   Manipulando o banco de dados  #
###################################
url <- "https://github.com/sjlva/Covid19BR/blob/master/rds/covid_ms.rds?raw=true"
dados <- readr::read_rds(url(url))

#View(dados)

#### Calculo do numero do dia ####
data_inicial_pandemia_br <- as.character(ymd(dados[1,'data']))
data_atual_pandemia_br <- ymd(today())
periodo <- data_inicial_pandemia_br%--%data_atual_pandemia_br

qnt_dias <- as.numeric(periodo, "days") + 1
qnt_meses <- as.numeric(str_sub(as.period(periodo, "month"), start = 1, end=1))

# Vemos que as primeiras linhas s?o referentes a apenas os valores para o Brasil:
# Depois s?o os dados de cada estado
# Apos isso, temos os dados de cada municipio

# Iremos utilizar no trabalho os dados do Brasil como um todo, de cada estado e para 
# fazer uma analise mais detalhada par ao Brasil. Ent?o vamos separar dados em:

# - dados_brasil: dados contendos os totais do brasil
# - dados_estados: dados contendo detalhes dos estados
# - dados_mg: dados contendo detalhes de minas e municipios


#### Dados Brasil: ####

#E possivel perceber que os dados para o Brasil s?o todos com coduf = 76
dados_brasil <- filter(dados, coduf==76)

#### Dados Estados ###

#numero da linha inicial do banco estados:
inicio_est <- qnt_dias + 1
final_est <- (inicio_est + (qnt_dias*27))-1 #sao 27 estados

#Tem um problema que RO repete ap?s a linha 6021 a parit da linha 6021 na data atual.
# Ent?o, dados do estado vai at? final_est
dados_estado <- dados[inicio_est:(final_est-1),]
#dados_estado[nrow(dados_estado)]
#View(dados_estado)

#### Dados Minas Gerais - nivel municipio ####

dados_mg_mun <- filter(dados, municipio != "" & estado == "MG")
#View(dados_mg_mun)
#dim(dados_mg_mun)

# Em MG, os dados a nivel de municipio come?am em 27/03/2020
# Modificando o periodo do dia:
data_inicial_pandemia_mg <- as.character(ymd(dados_mg_mun[1,'data']))
data_atual_pandemia_mg <- ymd(today()-1)
periodo_mg <- data_inicial_pandemia_mg%--%data_atual_pandemia_mg

qnt_dias_mg <- as.numeric(periodo_mg, "days") + 1


#checando a quantidade de dados por municipio:
check_mg <- dados_mg_mun %>%
              group_by(municipio) %>%
              summarise(contagem = length(municipio))
ifelse(nrow(dados_mg_mun) == 853*qnt_dias_mg, "Dados ok", "Dados Errados")
ifelse(nrow(filter(check_mg, contagem != 184)) == 0, "Dados ok", "Dados Errados")

#### Excluir a base inteira ####
rm("dados", "check_mg")


###################################
#           Descritivas           #
###################################

#### Brasil ####

View(dados_brasil)

###  cartoes: ###

#acumulado de mortes: ultima linha
dados_brasil$casosAcumulado[nrow(dados_brasil)]

#acumulado de mortes: ultima linha
dados_brasil$obitosAcumulado[nrow(dados_brasil)]

#dia em que teve meior numero de mortes:
filter(dados_brasil, obitosNovos==max(dados_brasil$obitosNovos, na.rm = TRUE))$data

#dia em que teve maior numero de casos:
filter(dados_brasil, casosNovos==max(dados_brasil$casosNovos, na.rm = TRUE))$data

#media de casos nos ultimos 14 dias:
mean(dados_brasil$casosNovos[(nrow(dados_brasil)-14):nrow(dados_brasil)], na.rm = TRUE)

#Media de mortes nos ultimos 14 dias:
mean(dados_brasil$obitosNovos[(nrow(dados_brasil)-14):nrow(dados_brasil)], na.rm = TRUE)



########################  Tentar fazer um mapinha do Brasil no leaflet

library(rgdal)   # para carregar o shape
library(leaflet)

## Lembrar de baixar os dados site do ibge, eles est?o dentro de uma pasta chamada Mapa
shp <- readOGR("Mapa\\.", "BR_UF_2019", stringsAsFactors=FALSE, encoding="UTF-8")

Brasillastadate<- dados_estado %>% filter(data == max(data))
brasileiropg <- merge(shp,Brasillastadate, by.x = "CD_UF", by.y = "coduf")
proj4string(brasileiropg) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

Encoding(brasileiropg$NM_ESTADO) <- "UTF-8"

brasileiropg$Score[is.na(brasileiropg$Score)] <- 0


pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa

state_popup <- paste0("<strong>Estado: </strong>", 
                      brasileiropg$estado, 
                      "<br><strong>Casos: </strong>", 
                      brasileiropg$casosAcumulado)
leaflet(data = brasileiropg) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(brasileiropg$casosAcumulado), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~brasileiropg$casosAcumulado,
            title = "Total de casos",
            opacity = 1)



############ Agora um mapinha animado



###### Twitter #######
########################  Tentar buscar dados do Twitter
library(twitteR)
library(wordcloud2)
library(stringr)

#https://apps.twitter.com/app/new
akey<-'SNjqQpunmVROj78bpSrbXaY42'
asecret<-'JPA5NePjI1wchJ5tTA7S94LEskmuMWb8bCIJBQa4ozECcze8x6'
atoken<-'588512062-cnhAyXUANA71u9isu5Smr490l4e8ebOb5eePpDBQ'
atokenSecret<-'HKEsqOh6Kix50iYcunURpdLJBFo8MSRU2QMKmhQDINjJ6'
setup_twitter_oauth(akey,asecret,atoken,atokenSecret)

#pesquisar assunto ou hashtag
p<-searchTwitter('covid19',n=10,lang="pt")
dfp<-twListToDF(p)
dfp$text<-str_to_lower(dfp$text)

lista_palavras <- strsplit(dfp$text, "\\W+")
vetor_palavras <- unlist(lista_palavras)

frequencia_palavras <- table(vetor_palavras)
frequencia_ordenada_palavras <- sort(frequencia_palavras, decreasing=TRUE)

wordcloud2(data = frequencia_ordenada_palavras, figPath = "Brasil.png")
