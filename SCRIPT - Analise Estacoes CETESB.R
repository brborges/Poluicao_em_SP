########################################### PACOTES ###########################################

library(readr)
library(tmap)
library(rgdal)
library(sf)
library(tidyverse)
library(rgeos)

install.packages("esquisse")

########################################### Estacoes de tratamento e monitoria da CETESB ###########################################

df_estacoes_cetesb <- read_csv("estacoes-cetesb.csv/estacoes-cetesb.csv")

View(df_estacoes_cetesb)
head(df_estacoes_cetesb)
df_estacoes_cetesb$DATA
dim(df_estacoes_cetesb)
class(df_estacoes_cetesb)
summary(df_estacoes_cetesb)

df_estacoes_cetesb %>%
  group_by(Municipio) %>%
  count()


###### Data wrangling


df_estacoes_cetesb_v1 <- df_estacoes_cetesb

temp <- str_split_fixed(df_estacoes_cetesb_v1$Qualidade, " - ", 2)

df_temp <- data.frame(temp)

df_temp <- bind_cols(df_estacoes_cetesb_v1, df_temp)

df_temp <- select(df_temp,  everything(), -Qualidade) # todas menos uma

df_temp <- rename(df_temp, Cod_qualidade = X1,
                Qualidade = X2)
rm(temp)


head(df_temp,n=30)


df_temp %>%
  group_by(Qualidade) %>%
  count()

filtro_1 <- df_temp[df_temp$Qualidade != "",] # Observações sem qualidade do AR

df_temp <- filter(df_temp, Qualidade != "") # Usando a funcao filter

df_temp_sp <- filter(df_temp, Municipio == "SAO PAULO") # filtrando somente municipio de SP


########################################### Criando objeto simple feature (sf) ###########################################

sf_cetesb <- st_as_sf(x = df_temp_sp, 
                         coords = c("x", "y"), 
                         crs = 4326)


class(df_temp_sp)

sf_cetesb$geometry

# Plotando o objeto sf_cetesb de forma espacial:
tm_shape(shp = sf_cetesb) + 
  tm_dots(size = 1)

# Adicionando uma camada de um mapa do Leafleet que considere a bounding box do 
# objeto sf_shoppings:
tmap_mode("view")

tm_shape(shp = sf_cetesb) + 
  tm_dots(col = "Qualidade", 
          border.col = "black", 
          size = 0.2, 
          alpha = 0.8)


#tmap_mode("plot")



#Carregando um shapefile do município de São Paulo
shp_saopaulo <- readOGR("shapefile_municipio", "municipio_sp")

tm_shape(shp = shp_saopaulo) + 
  tm_borders()

# Combinando o objeto shp_saopaulo com o objeto sf_cetesb:
tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_cetesb) + 
  tm_dots(col = "Qualidade", 
          size = 0.02)


# Depois, utilizaremos a função SpatialPoints() para criar um objeto do tipo sp:
coordenadas_estacoes <- cbind(df_temp_sp$x,
                               df_temp_sp$y)

coordenadas_estacoes


sp_cetesb <- SpatialPoints(coords = coordenadas_estacoes,
                              proj4string = CRS("+proj=longlat"))

# Criamos nosso primeiro objeto de classe sp! Vamos explorá-lo:
sp_cetesb@coords
sp_cetesb@bbox
sp_cetesb@proj4string@projargs

# Uma plotagem básica:
plot(sp_cetesb)

# Visualizando o resultado:
tmap_mode("plot")

tm_shape(shp = sp_cetesb) + 
  tm_dots(size = 1)

# Nosso atual objeto se orienta de forma geodésica.
estacoes_UTM <- spTransform(x = sp_cetesb,
                             CRSobj = CRS("+init=epsg:22523"))

# Visualizando o resultado:
tm_shape(shp = estacoes_UTM) + 
  tm_dots(size = 1)

# Agora sim, poderemos aplicar a função gBuffer():
buffer_estacoes <- gBuffer(spgeom = estacoes_UTM, 
                            width = 1500, 
                            byid = TRUE)

# Plotagem do objeto buffer_shoppings:
tm_shape(shp = buffer_estacoes) + 
  tm_borders()

tmap_mode("view")

tm_shape(shp = buffer_estacoes) + 
  tm_borders()

# Combinando os objetos shp_saopaulo, sf_shoppings e buffer_shoppings:
tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_cetesb) + 
  tm_dots(col = "Qualidade", 
          size = 0.02) +
  tm_shape(buffer_estacoes) + 
  tm_borders(col = "black") 


# A técnica de buffer union combina aqueles outputs da técnica de buffering que,
# por ventura, se encontrem.
buffer_union <- gUnaryUnion(spgeom = buffer_estacoes)


tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_cetesb) + 
  tm_dots(col = "Qualidade", 
          size = 0.02) +
  tm_shape(shp = buffer_union) + 
  tm_borders(col = "black") + 
  tm_fill(col = "gray",
          alpha = 0.5)


########################################### Dados de poluicao entre os anos de 2013-2019 ###########################################
df_cetesb <- read_csv("cetesb.csv/cetesb.csv")
View(df_cetesb)

names(df_cetesb)


#####
library(dplyr)
library(ggplot2)

df_estacoes_cetesb %>%
  filter(POLUENTE %in% "O3" | is.na(POLUENTE)) %>%
  filter(!is.na(Qualidade)) %>%
  ggplot() +
  aes(x = Qualidade) +
  geom_bar(fill = "#465BB4") +
  labs(x = "Qualidade do AR", 
       y = "Indice de O3",
       title = "Estatisticas do O3 No Estado de Sao Paulo",
       subtitle = "Qualidade do AR (Boa/Moderada/Ruim") +
  ggthemes::theme_calc() +
  theme(plot.title = element_text(size = 15L, face = "bold"))

