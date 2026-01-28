
# Pruebas del paquete

setwd('~/')
library(redalycR)

query <- "\"capital humano\" AND \"desarrollo economico\""


df.muestra <- redalyc_get_articles(query, page = 1, page_size = 100)
df <- redalyc_get_all_articles(query, page_size = 100)

#write.csv(x = df, file = 'data_redalyc.csv', row.names = F)
#dplyr::glimpse(df)

df <- read.csv('data_redalyc.csv')

df.bi <- redalyc_as_bibliometrics(df)
