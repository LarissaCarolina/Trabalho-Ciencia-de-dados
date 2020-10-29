#######################################################
#             Trabalho de Ciencia de Dados            #
#     Ana Clara, Amanda, Larisa, Leticia, Natalia     #
#######################################################
#
#setwd("~/Nat?lia/Ciencia de dados/Brasil")
rm(list=ls(all=TRUE))

#########################
#  Carregando pacotes   #
#########################
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(gganimate)
library(rio)
library(httr)
library(jsonlite)
library(rgdal)  
library(brmap)  
#########################

#### Brasil ####

###################################
#   Manipulando o banco de dados  #
###################################
# url para baixar os dados:
url <-  "https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral"
covid <- httr::GET(url, 
             add_headers("x-parse-application-id" = "unAFkcaNDeXajurGB7LChj8SgQYS2ptm"))

# Resultado:
results <- covid %>% 
  httr::content()

#results

# url para baixar os dados:
url_data <- results$results[[1]]$arquivo$url
url_data


if(str_detect(url_data, ".rar")){
  file_name <- paste0(getwd(),"/data/covid.rar")
  download.file(url_data, destfile = file_name)
  out <- archive(file_name)
  archive_extract(out, "data")
  dados <- rio::import(paste0("data/", out$path)) %>% 
    as_tibble()
} else if (str_detect(url_data, "xlsx")){
  dados <- rio::import(url_data, readxl =F, detectDates = T) %>% 
    as_tibble()
} else{
  dados <- rio::import(url_data) %>% 
    as_tibble()
}

ultima_atualizacao <- results$results[[1]]$dt_atualizacao

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

dados_estado$data <- ymd(dados_estado$data)


###################################
#           Descritivas           #
###################################

#### Brasil ####

#View(dados_brasil)

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
if(!require(brmap)){install.packages("brmap");library(brmap)} 


Brasillastadate<- dados_estado %>% 
  filter(data == max(data)) %>%
  left_join(brmap_estado, by = c("coduf" = "estado_cod"))%>%
  as_tibble()


d<- ggplot() + 
  geom_sf(data =Brasillastadate ,aes(geometry=geometry,fill= casosAcumulado, label=estado))+
  theme(
    panel.background = element_blank(), 
    panel.grid.major = element_line(color = "transparent"), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5) ## hjus = 0.5 centraliza o título
  )

plotly::ggplotly(d, tooltip = c('estado',"casosAcumulado"))

class(dados_estado)


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
1
#pesquisar assunto ou hashtag
p<-searchTwitter('covid19',n=1000,lang="pt", resultType = 'recent') ## locale ou geocode
dfp<-twListToDF(p)
dfp$text<-str_to_lower(dfp$text)


lista_palavras <- strsplit(dfp$text, "\\W+")
vetor_palavras <- unlist(lista_palavras)
library(tm)
library(SnowballC)
word.corpus <- Corpus(VectorSource(vetor_palavras)) 

##

word.corpus<-word.corpus%>%
  tm_map(removePunctuation)%>% ##eliminar pontuacao
  tm_map(removeNumbers)%>% #sem numeros
  tm_map(stripWhitespace)# sem espacos

word.corpus<-word.corpus%>%
  tm_map(tolower)%>% ##make all words lowercase
  tm_map(removeWords, stopwords("por"))

word.corpus <- tm_map(word.corpus, stemDocument)
word.corpus <- tm_map(word.corpus, removeWords, c("https"))
word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
word.freq <- sort(rowSums(word.counts), decreasing = TRUE)
word.freq<- rownames_to_column(as.data.frame(word.freq))
head(word.freq)
#frequencia_palavras <- table(vetor_palavras)
#frequencia_ordenada_palavras <- sort(frequencia_palavras, decreasing=TRUE)

wordcloud2(word.freq, size=0.5,color = "random-light", backgroundColor = "black")


#---------------------- Animações---------------------

##Animação: Corrida de barras ordenado S2, tô emocionada!

#Aqui iremos pegar o top 6 de casos atuais.
brasil_atual <-
  dados_estado %>% 
  filter(data==max(data))

top_estados<-brasil_atual %>%
  top_n(6,casosAcumulado) %>%
  select(estado)

#Filtrando a base atual pelos países selecionados anteriormente.
top<-dados_estado %>%
  filter(estado %in% c(top_estados$estado))

# Para que o gráfico fique ordenado em cada data, precisamos criar do número de casos por data:
rank<-top %>% 
  select(regiao, data, estado, casosAcumulado) %>% 
  group_by(data) %>%
  arrange(-casosAcumulado) %>%
  mutate(rank=row_number())


g_corrida <- rank %>%
  ggplot(aes(x = -rank,y = casosAcumulado, group = estado)) +
  geom_tile(aes(y = casosAcumulado / 2, height = casosAcumulado, fill = estado), width = 0.9) +
  geom_text(aes(label = estado), hjust = "right", colour = "black", fontface = "bold", nudge_y = -100000) +
  geom_text(aes(label = casosAcumulado), hjust = "left", nudge_y = 100000, colour = "grey30") +
  coord_flip(clip="off") +
  # gganimate
  transition_time(data) +
  theme_minimal()+
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())+
  ease_aes('cubic-in-out') +
  labs(title='Estados com os seis maiores números de casos confirmados atualmente',
       subtitle='Total de casos confirmados em {round(frame_time,0)}', x=" ", y=" ",fill = "Estado (UF)")
corridabrasil<- animate(g_corrida, nframes = 200, fps = 25, end_pause = 50)
anim_save("corridabrasil.gif", corridabrasil)

## Animação: Novos Casos
#Acho que o gráfico com mais de duas localidades fica muito poluído, vou fixar um top 2.
brasil_atual <-
  dados_estado %>% 
  filter(data==max(data))

top_estados<-brasil_atual %>%
  top_n(2,casosAcumulado) %>%
  select(estado)

#Filtrando a base atual pelos países selecionados anteriormente.
top<-dados_estado %>%
  filter(estado %in% c(top_estados$estado))

g_linha_novos_casos<-ggplot(top,aes(y=casosNovos, col= estado,x=data))+
  geom_line()+
  geom_point() +
  transition_reveal(data)+
  theme_minimal()+
  view_follow(fixed_x = T)+
  labs(title='Número de novos casos',
        x=" ", y=" ",col = "Estado (UF)")
linhabrasil<- animate(g_linha_novos_casos, nframes = 300, fps = 25, end_pause = 50)
anim_save("linhabrasil.gif", linhabrasil)
