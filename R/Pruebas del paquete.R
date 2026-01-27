
# Pruebas del paquete 
library(redalycR)

query <- "\"capital humano\" AND \"desarrollo economico\""


df.muestra <- redalyc_get_articles(query, page = 1, page_size = 100)

setwd('~/')


df <- redalyc_get_all_articles(query, page_size = 100)

write.csv(x = df, file = 'data_redalyc.csv', row.names = F)
dplyr::glimpse(df)

