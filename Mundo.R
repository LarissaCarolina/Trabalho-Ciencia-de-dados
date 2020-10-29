#----------------------------------------------------------------------------------------------
#---------------------------------- Dados Covid-19 - Mundo ------------------------------------
#----------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#Carregando os pacotes necess?rios:
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

#----------------------------- Processo de manipula??o dos dados -----------------------#

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


# Juntando as tabelas (Confirmados, recuperados e mortes) em uma ?nica tabela:
mundo <- confirmados %>% 
  left_join(recuperados, by=c("Country/Region", "Lat", "Long","Data")) %>% 
  left_join(mortes, by=c("Country/Region", "Lat", "Long","Data")) %>% 
  select("Province/State", "Country/Region", "Lat", "Long", "Data",
         "CasosConfirmados", "Mortes", "Recuperados")
#View(mundo)

# Problema: Alguns pa?ses com informa??es diferentes nas  3 bases

# Ver quais:
teste<-mundo %>% 
  filter(is.na(Recuperados)|is.na(Mortes)) %>% 
  select("Province/State","Country/Region",Data,CasosConfirmados) %>%
  inner_join(mortes,by=c("Province/State",'Country/Region','Data')) %>%
  inner_join(recuperados,by=c("Province/State",'Country/Region','Data'))

#View(teste)


auxiliar<-mundo %>% 
  filter(!paste(mundo$`Province/State`,mundo$`Country/Region`,mundo$Data)%in% 
           paste(teste$`Province/State`,teste$`Country/Region`,teste$Data))



teste<-teste %>% select("Province/State",'Country/Region',Lat=Lat.x,Long=Long.y,Data,
                        CasosConfirmados,Mortes,Recuperados)

mundo <-rbind(auxiliar,teste)  



#Renomeando as colunas Province e Country:
mundo<-mundo %>% rename("Province"="Province/State")
mundo<-mundo %>% rename("Country"="Country/Region")
names(mundo)

# Canada est? com os mortos e confirmados por prov?ncia, mas os recuperados n?o est?o

canada<-filter(mundo,Country=='Canada') %>% 
  group_by(Country,Data) %>% 
  summarise(CasosConfirmados=sum(CasosConfirmados),
            Mortes=sum(Mortes))

recuperados<-rename(recuperados,"Country"="Country/Region")
recuperados<-rename(recuperados,"Province"="Province/State")
cnd_mundo<- canada %>% 
  left_join(recuperados, by=c("Country","Data"))

# Reordenando as colunas

cnd_mundo<- 
  cnd_mundo %>%
  select(Province,Country,Lat,Long,Data,CasosConfirmados,Mortes,Recuperados)
  
# Tirando Canada do banco 'mundo'

mundo <- mundo %>% filter(Country!='Canada')

# Juntando mundo com cnd_mundo

mundo<- add_row(mundo,cnd_mundo)

# Conferindo a classe do banco e de cada uma das colunas:
class(mundo)
sapply(mundo,class)

# ? necess?rio converter a classe da coluna de data para date:
require(lubridate)
head(mundo$Data)
mundo$Data<-mdy(mundo$Data)
summary(mundo$Data)

# Banco de dados com os totais mais atuais (ser? utilizado no mapa).
# No aplicativo, pretendemos colocar um filtro de data. De maneira que utilizar o filtro, o mapa seja de acordo a essa data. 
mundo_atual<-
  mundo %>% 
  filter(Data==max(Data))
#View(mundo_atual)

# A seguir criaremos uma base agrupada por pa?s:
mundo_pais<-mundo %>%
  group_by(Country,Data) %>%
  summarise(
    TotalCasos=sum(CasosConfirmados),
    TotalMortes=sum(Mortes),
    TotalRecuperados=sum(Recuperados)
  )


# A seguir criaremos as colunas novos Casos, novos Recuperados e novos Mortos por dia:
# Falta estruturar melhor essa parte, mas a ideia ? essa. 
# Ordenar por pais e data, aplicar o diff por pa?s.
# a tapply retorna uma lista, temos q voltar pro banco
mundo_pais<-mundo_pais[order(mundo_pais$Country,mundo_pais$Data),]
novos<-tapply(mundo_pais$TotalCasos,mundo_pais$Country,diff)

# ? necess?rio colocar o n?mero de casos no dia 1. Porque a fun??o diff exclui esse valor.
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
dados3<-plyr::ldply(novos, data.frame)

#Acrescentando a nova coluna no banco:
mundo_pais$NovosRec<-dados3$X..i..

# Iremos repetir o mesmo processo para os novos mortos por dia:
novos<-tapply(mundo_pais$TotalMortes,mundo_pais$Country,diff)


for(i in names(novos)){
  casos_pais<-filter(mundo_pais,Country==i) %>% select(TotalMortes)
  novos[[i]]<-c(as.numeric(casos_pais[1,2]),novos[[i]])
}

# Voltando para o formato df
dados3<-plyr::ldply(novos, data.frame)

# Acrescentado a nova coluna no banco:
mundo_pais$NovosMortos<-dados3$X..i..

#Conferindo o resultado:
#View(mundo_pais)
summary(mundo_pais$NovosCasos)

# ------------------ Anima??es: ----------------#

require(gganimate)
##Anima??o: Corrida de barras ordenado S2, t? emocionada!

#Aqui iremos pegar o top 6 de casos atuais.
mundo_atual<-
  mundo %>% 
  filter(Data==max(Data))

top_mundo<-mundo_atual %>%
  top_n(6,CasosConfirmados) %>%
  select(Country)

#Filtrando a base atual pelos pa?ses selecionados anteriormente.
top<-mundo_pais %>%
  filter(Country %in% c(top_mundo$Country,'China',"Italy"))

# Para que o gr?fico fique ordenado em cada data, precisamos criar do n?mero de casos por data:
rank<-top %>% 
  select(Country, Data, TotalCasos) %>% 
  group_by(Data) %>%
  arrange(-TotalCasos) %>%
  mutate(rank=row_number())


g_corrida <- rank %>%
  ggplot(aes(x = -rank,y = TotalCasos, group = Country)) +
  geom_tile(aes(y = TotalCasos / 2, height = TotalCasos, fill = Country), width = 0.9) +
  geom_text(aes(label = Country), hjust = "right", colour = "black", fontface = "bold", nudge_y = -100000) +
  geom_text(aes(label = TotalCasos), hjust = "left", nudge_y = 100000, colour = "grey30") +
  coord_flip(clip="off") +
  # gganimate
  transition_time(Data) +
  theme_minimal()+
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())+
  ease_aes('cubic-in-out') +
  labs(title='Rank dos paises com os seis maiores numeros de casos confirmados',
       subtitle='Total de casos confirmados em {round(frame_time,0)}', x=" ", y=" ",fill = "País")


corrida <- animate(g_corrida, nframes = 300, fps = 25, end_pause = 50) 
anim_save("corrida.gif", corrida)


## Anima??o: Novos Casos
#Acho que o gr?fico com mais de duas localidades fica muito polu?do, vou fixar um top 2.
#Aqui iremos pegar o top 2 de casos atuais.
mundo_atual<-
  mundo %>% 
  filter(Data==max(Data))

top_mundo<-mundo_atual %>%
  top_n(2,CasosConfirmados) %>%
  select(Country)

#Filtrando a base atual pelos pa?ses selecionados anteriormente.
top<-mundo_pais %>%
  filter(Country %in% c(top_mundo$Country))

g_linha_novos_casos<-ggplot(top,aes(y=NovosCasos, col= Country,x=Data))+
  geom_line()+
  geom_point() +
  transition_reveal(Data)+
  theme_minimal()+
  view_follow(fixed_x = T)+
  labs(title='Novos casos',
       x="Data ", y=" ",col = "País")
linhanovos<- animate(g_linha_novos_casos, nframes = 300, fps = 25, end_pause = 50)
anim_save("linhanovos.gif", linhanovos)
# --------------------------- Base para o mapa -------------------------------------#

#Para o mapa n?o agregaremos os territ?rios independentes ao seu pa?s 'dono' sempre que poss?vel
# Ex. os dados das Ilhas Malvinas ficar?o no territ?rio da ilha, n?o no Reino unido.
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
#View(world)

#Precisamos unir a geometria do banco world com os nossos papises:
# Transformar as coordenadas em  geometria no pacote:
mapa<-mundo_atual %>% 
  st_as_sf(coords = c("Long", "Lat"))%>% 
  st_set_crs("+proj=longlat +datum=WGS84")
head(mapa)
class(mapa)

# A ideia ? pegar o poligono mais pr?ximo do ponto, ou seja, o pol?gono em que o ponto est?.
mapa$geom<-world$name[st_nearest_feature(mapa$geometry, world$geometry)]
mapa$geometry<-world$geometry[st_nearest_feature(mapa$geometry,world$geometry)] 
head(mapa)


#Para conferir se a jun??o foi feita de forma correta:
#Primeiro tirar os acentos do nome da variavel geom:
mapa$geom<-abjutils::rm_accent(mapa$geom)

# Substiruir o '-' por espa?o:
require(stringr)
mapa$geom<-str_replace(mapa$geom,'-',' ')

# Substiruir o 'Is.' por Islands e St por Saint:
mapa$geom<-str_replace(mapa$geom,'Is.','Islands')
mapa$geom<-str_replace(mapa$geom,'St\n','Saint')
mapa$Province<-str_replace(mapa$Province,'St','Saint')
summary(mapa$Province==mapa$geom)
summary(mapa$Country==mapa$geom)

# Visualizar os paises/provincias que est?o diferentes:
v<-mapa %>% filter(Country!=geom & Province!=geom) %>% select(Province,Country,geom,CasosConfirmados) 
View(v)

# Visualizar os que come?am com a mesma letra:
v %>% filter(str_sub(v$Province,end=2)==str_sub(v$geom,end=2)) %>% View 
# Todos esses correspondem ao territ?rio certo.

# Visualizar os que n?o come?am com a mesma letra:
v %>% filter(str_sub(v$Province,end=2)!=str_sub(v$geom,end=2)) %>% View 
# Entre esses:
# Jersey ? a maior Ilha de Chanell islands, ent?o faz sentido deixar neste territ?rio
# Polin?sia Francesa ? uma ilha pr?xima ? Autr?lia, mas que pertence a Fran?a
# Incorporar a Fran?a? Faz sentido? S?o territ?rios muito distantes entre si. Talvez seja melhor desconsiderar para o mapa
# Talvez fa?a sentido incorporar Gilbraltar na Espanha j? que o territ?rio pertence ao Reino Unido mas est? dentro da espanha



# Agora precisamos criar uma base agrupada pela geometria que usaremos:
mapa_g<-
  mapa %>% group_by(geom) %>%
  summarise(
    Casos=sum(CasosConfirmados),
    Mortes=sum(Mortes),
    Recuperados= sum(Recuperados)
    
  )
mapa_g<- rename(mapa_g,Territorio=geom)
# Construindo o mapa para ilustrar o n?mero de casos confirmados:
conf1<-ggplot()+
  geom_sf(data=world,fill='white')+
  geom_sf(data=mapa_g,aes(label=Territorio,geometry=geometry,fill=Casos))+
  theme_classic()

plotly::ggplotly(conf1, tooltip = c("Casos",'Territorio'))


# Construindo o mapa para ilustrar o n?mero de mortes:
conf2<-ggplot()+
  geom_sf(data=world,fill='white')+
  geom_sf(data=mapa_g,aes(label=Territorio,geometry=geometry,fill=Mortes))+
  theme_classic()

plotly::ggplotly(conf2,tooltip = c("Casos",'Territorio'))



# ----------------------------- Algumas m?tricas: -----------------
#N?mero de casos atual:
numeros_mundo_atual <- mundo_atual %>%
  group_by(Country) %>% 
  summarise(Casos_atual = sum(CasosConfirmados),
            Mortes_atual = sum(Mortes),
            Recuperados_atual = sum(Recuperados))

#Identificando a data do primeiro caso nos pa?ses:
primeiro_pais <- mundo_pais %>% 
  filter(TotalCasos > 0)  %>%
  group_by(Country) %>%
  mutate(PrimeiroCaso = min(Data)) 
primeiro_pais

#Contagem de dias desde o primeiro caso:
primeiro_pais$Dias_desde_primeiro <- difftime(primeiro_pais$Data,primeiro_pais$PrimeiroCaso,units = "days")

