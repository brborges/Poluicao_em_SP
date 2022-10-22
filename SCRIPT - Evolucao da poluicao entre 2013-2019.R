########################################### PACOTES ###########################################

library(tidyverse)

########################################### Dados de poluicao entre os anos de 2013-2019 ###########################################

df_cetesb <- read_csv("cetesb.csv/cetesb.csv")
#View(df_cetesb)

names(df_cetesb)


#### Fazendo exercicios com uma base menor - filtro



teste <- as.Date(df_cetesb$time)
ano <- str_split_fixed(teste,"-", 2)


df_ano <- data_frame(ano)

df_cetesb <- bind_cols(df_cetesb, df_ano)

names(df_cetesb)


df_cetesb_2019 <- filter(df_cetesb, ano=="2019")


head(df_cetesb_2019)
tail(df_cetesb_2019)

ggplot(df_cetesb_2019, aes(x = time, y = BENZENO)) +
  geom_col()


#df_cetesb %>%
#  group_by(time) %>%
#  count(O3)


