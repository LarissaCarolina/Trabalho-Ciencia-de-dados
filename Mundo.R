#----------------------------------------------------------------------------------------------
#---------------------------------- Dados Covid-19 - Mundo ------------------------------------
#----------------------------------------------------------------------------------------------

#Carregando os pacotes necessários:
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(data.table)
library(tidyverse)
library(rgeos)
library(sf)
library(fuzzyjoin)


# URLs com os dados da Johns Hopkins:
url_confirmed <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"
url_deaths <-    "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"
url_recovered <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv&filename=time_series_covid19_recoverd_global.csv"


# Lendo os dados:
confirmed<-read_csv(url(url_confirmed))
#View(confirmed)
deaths <- read_csv(url(url_deaths))
#View(deaths)
recovered <- read_csv(url(url_recovered))
#View(recovered)

#----------------------------- Processo de manipulação dos dados -----------------------#

# Alterando o formato dos dados (queremos que os dados estejam em um formato "tidy"):
confirmados<- confirmed %>% pivot_longer( cols=-c("Province/State" ,"Country/Region" ,"Lat", "Long") , 
             names_to = "Data", 
             values_to =  "CasosConfirmados")
mortes <- deaths %>% pivot_longer(cols = -c("Province/State", "Country/Region", "Lat", "Long"),
                                        names_to="Data", values_to="Mortes")
recuperados <- recovered %>% pivot_longer(cols = -c("Province/State", "Country/Region", "Lat", "Long"),
                                     names_to="Data", values_to="Recuperados")

#View(confirmados)
#View(mortes)
#View(recuperados)


# Juntando as tabelas (Confirmados, recuperados e mortes) em uma única tabela:
mundo <- confirmados %>% 
  inner_join(recuperados, by=c("Country/Region", "Lat", "Long","Data")) %>% 
  inner_join(mortes, by=c("Country/Region", "Lat", "Long","Data")) %>% 
  select("Province/State", "Country/Region", "Lat", "Long", "Data",
                         "CasosConfirmados", "Mortes", "Recuperados")
#View(mundo)

#Renomeando as colunas Province e Country:
mundo<-mundo %>% rename("Province"="Province/State")
mundo<-mundo %>% rename("Country"="Country/Region")
names(mundo)

# Conferindo a classe do banco e de cada uma das colunas:
class(mundo)
sapply(mundo,class)

# É necessário converter a classe da coluna de data para date:
require(lubridate)
head(mundo$Data)
mundo$Data<-mdy(mundo$Data)
summary(mundo$Data)

# Banco de dados com os totais mais atuais (será utilizado no mapa).
# No aplicativo, pretendemos colocar um filtro de data. De maneira que utilizar o filtro, o mapa seja de acordo a essa data. 
mundo_atual<-
  mundo %>% 
  filter(Data==max(Data))
#View(mundo_atual)

# A seguir criaremos uma base agrupada por país:
mundo_pais<-mundo %>%
  group_by(Country,Data) %>%
  summarise(
    TotalCasos=sum(CasosConfirmados),
    TotalMortes=sum(Mortes),
    TotalRecuperados=sum(Recuperados)
  )


# A seguir criaremos as colunas novos Casos, novos Recuperados e novos Mortos por dia:
# Falta estruturar melhor essa parte, mas a ideia é essa. 
# Ordenar por pais e data, aplicar o diff por país.
# a tapply retorna uma lista, temos q voltar pro banco
mundo_pais<-mundo_pais[order(mundo_pais$Country,mundo_pais$Data),]
novos<-tapply(mundo_pais$TotalCasos,mundo_pais$Country,diff)

# É necessário colocar o número de casos no dia 1. Porque a função diff exclui esse valor.
# Por enquento faremos assim:
for( i in names(novos)){
  casos_pais<-filter(mundo_pais,Country==i) %>% select(TotalCasos) #Pegando o data frame com o n?mero de casos do pa?s em quest?o.
  novos[[i]]<-c(as.numeric(casos_pais[1,2]),novos[[i]]) #Conacatena o n?mero de caso no pa?s no dia 1 com o vetor de novos casos.
}

# Voltando para o formato de df:
dados3<-cbind(plyr::ldply(novos, data.frame))

# Acrescentando a coluna de novos casos no banco:
mundo_pais$NovosCasos<-dados3$X..i..

# Verificando o resultado:
#View(mundo_pais) # Funcionou!

# Iremos repetir o mesmo processo para os novos casos recuperados por dia:
novos<-tapply(mundo_pais$TotalRecuperados,mundo_pais$Country,diff)

for( i in names(novos)){
  casos_pais<-filter(mundo_pais,Country==i) %>% select(TotalRecuperados)
  novos[[i]]<-c(as.numeric(casos_pais[1,2]),novos[[i]])
}

# Voltando para o formato df:
dados3<-cbind(plyr::ldply(novos, data.frame))

#Acrescentando a nova coluna no banco:
mundo_pais$NovosRec<-dados3$X..i..

# Iremos repetir o mesmo processo para os novos mortos por dia:
novos<-tapply(mundo_pais$TotalMortes,mundo_pais$Country,diff)


for(i in names(novos)){
  casos_pais<-filter(mundo_pais,Country==i) %>% select(TotalMortes)
  novos[[i]]<-c(as.numeric(casos_pais[1,2]),novos[[i]])
}

# Voltando para o formato df
dados3<-cbind(plyr::ldply(novos, data.frame))

# Acrescentado a nova coluna no banco:
mundo_pais$NovosMortos<-dados3$X..i..

#Conferindo o resultado:
View(mundo_pais)
summary(mundo_pais$NovosCasos)

# ------------------ A partir daqui iremos criar algumas vizualizações: ----------------#

## Tentaremos produzir algumas animações:
require(gganimate)

#Pegando o top 6 do número de casos confirmados atualmente e selecionando o nome dos países.
topPaises<-mundo_atual %>% top_n(6,CasosConfirmados) %>% select(Country)
#Filtrando a base atual pelos países selecionados anteriormente.
top<-mundo_pais %>% filter(Country %in% c(topPaises$Country,'China'))

#Animação: Número de casos confirmados (Corrida de barras):
g_corridas_barras<-ggplot(top,aes(x=TotalCasos,y=Country,fill=Country))+
    geom_col()+
    transition_time(Data)+
    theme_minimal()+
    labs(title = "Data: {frame_time}", x="País", y="Total de casos")
animate(g_corridas_barras, renderer = gifski_renderer(loop = FALSE), nframes=300) #Esse argumento faz a animação pare na última data.

#Animação: Número de novos casos.
g_linha_novos_casos<-ggplot(top,aes(y=NovosCasos, col= Country,x=Data))+
  geom_line()+
  geom_text(aes(x = today()-1, label = Country), hjust = 0) +
  transition_reveal(Data)+
  theme_minimal()+
  view_follow(fixed_x = T)+
  labs(x="Data",y="Número de novos casos")
animate(g_linha_novos_casos, renderer = gifski_renderer(loop = FALSE), nframes=300)

#Animação: Número de casos confirmados.
topPaises<-mundo_atual %>% top_n(6,CasosConfirmados) %>% select(Country)
top<-mundo_pais %>% filter(Country %in% c(topPaises$Country,'China',"Italy"))

g_linha_CasosConf<- ggplot(top,aes(y=TotalCasos,col=Country,x=Data))+
  geom_line()+
  #geom_segment(aes(xend = today()-1, yend = NovosCasos), linetype =2, colour = Country) +
  geom_text(aes(x = today()-1, label = Country), hjust = 0) + 
  transition_reveal(Data)+theme_minimal()+view_follow(fixed_x = T)+
  labs(x="Data",y="Total de casos")
animate(g_linha_CasosConf, renderer = gifski_renderer(loop = FALSE), nframes=300)



# --------------------------- Base para o mapa -------------------------------------#

#Para o mapa não agregaremos os territórios independentes ao seu país 'dono' sempre que possível
# Ex. os dados das Ilhas Malvinas ficarão no território da ilha, não no Reino unido.
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
#View(world)

#Precisamos unir a geometria do banco world com os nossos papises:
# Transformar as coordenadas em  geometria no pacote:
mapa<-mundo_atual %>% 
  st_as_sf(coords = c("Long", "Lat"))%>% 
  st_set_crs("+proj=longlat +datum=WGS84")
head(mapa)

# A ideia é pegar o poligono mais próximo do ponto, ou seja, o polígono em que o ponto está.
mapa$geom<-world$name[st_nearest_feature(mapa$geometry, world$geometry)]
mapa$geometry<-world$geometry[st_nearest_feature(mapa$geometry,world$geometry)] 
head(mapa)


#Para conferir se a junção foi feita de forma correta:
#Primeiro tirar os acentos do nome da variavel geom:
mapa$geom<-abjutils::rm_accent(mapa$geom)

# Substiruir o '-' por espaço:
require(stringr)
mapa$geom<-str_replace(mapa$geom,'-',' ')

# Substiruir o 'Is.' por Islands e St por Saint:
mapa$geom<-str_replace(mapa$geom,'Is.','Islands')
mapa$geom<-str_replace(mapa$geom,'St','Saint')
mapa$Province<-str_replace(mapa$Province,'St','Saint')
summary(mapa$Province==mapa$geom)
summary(mapa$Country==mapa$geom)

# Visualizar os paises/provincias que estão diferentes:
v<-mapa %>% filter(Country!=geom & Province!=geom) %>% select(Province,Country,geom,CasosConfirmados) 
View(v)

# Visualizar os que começam com a mesma letra:
v %>% filter(str_sub(v$Province,end=2)==str_sub(v$geom,end=2)) %>% View 
# Todos esses correspondem ao território certo.

# Visualizar os que não começam com a mesma letra:
v %>% filter(str_sub(v$Province,end=2)!=str_sub(v$geom,end=2)) %>% View 
# Entre esses:
# Jersey é a maior Ilha de Chanell islands, então faz sentido deixar neste território
# Polinésia Francesa é uma ilha próxima ? Autrália, mas que pertence a França
# Incorporar a França? Faz sentido? São territórios muito distantes entre si. Talvez seja melhor desconsiderar para o mapa
# Talvez faça sentido incorporar Gilbraltar na Espanha já que o território pertence ao Reino Unido mas está dentro da espanha


# Agora precisamos criar uma base agrupada pela geometria que usaremos:
# mapa_g<-
#   mapa %>% group_by(geom) %>%
#   summarise(
#     Casos=sum(CasosConfirmados),
#     Mortes=sum(Mortes),
#     )

# Construindo o mapa para ilustrar o número de casos confirmados:
conf1<-ggplot()+
  geom_sf(data=world,fill='white')+
  geom_sf(data=mapa,aes(geometry=geometry,fill=CasosConfirmados))+
  theme_classic()

plotly::ggplotly(conf1)

# Construindo o mapa para ilustrar o número de mortes:
conf2<-ggplot()+
  geom_sf(data=world,fill='white')+
  geom_sf(data=mapa,aes(geometry=geometry,fill=Mortes))+
  theme_classic()

plotly::ggplotly(conf2)


