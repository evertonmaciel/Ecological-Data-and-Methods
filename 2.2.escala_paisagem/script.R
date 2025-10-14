#################################################################
## Serviço Público Federal
## Ministério da Educação
## Universidade Federal de São Paulo – UNIFESP
## Curso: Ciências Ambientais
## Disciplina: Modelagem de Paisagem
## Aula: Importância da escala em estudos de paisagem
## Prof. Dr. Everton Alves Maciel (everttonmaciel@gmail.com)
################################################################
## Analisar o efeito da extensão e resolução espacial nos 
## padrões de temperatura de São Paulo e Brasil
################################################################
## caso nao tenham sido instalados
#install.packages("raster")
#install.packages("sf")


## carregar pacotes
library(raster)
library(sf)


## ETAPA 1 EXTENSAO BRASIL VS SP
#carregar variavel temperatura do worldclim de janeiro
temp_1<- raster("wc2.1_10m_tavg_01.tif")

## carregar variavel altitude de Sao Paulo
altitude_sp <- st_read("altitude_1km_municipios_sao_paulo.shp")

## carregar variavel altitude do Brasil
altitude_br<- st_read("altitude_1km_municipios_brasileiros.shp")


## extrair dados de temperatura para Sao Paulo
resultados <- extract(temp_1,altitude_sp)
altitude_sp$temp_10m_sp <- resultados

## extrair dados de precipitacao para o Brasil
resultados_2 <- extract(temp_1,altitude_br)
altitude_br$temp_10m_br <- resultados_2


## grafico de barras com a temperatura de SAO PAULO
# Criando categorias (faixas de valores)
faixas <- cut(altitude_sp$temp_10m_sp, breaks = 20)
# Contando as frequências em cada faixa
frequencias <- table(faixas)


## grafico de barras com a temperatura de SAO PAULO
# Criando categorias (faixas de valores)
faixas_2 <- cut(altitude_br$temp_10m_br, breaks = 20)
# Contando as frequências em cada faixa
frequencias_2 <- table(faixas_2)


# Criando o gráfico de barras
png("graficos_extensao.png", width = 1400, height = 1000)
par(mfrow = c(2, 2))## se quiser imprimir todos os graficos, habilite essa funcao

# Criando o gráfico de barras Sao Paulo
barplot(frequencias,
        main = "Temperatura São Paulo",
        xlab = "Valores Temperatura",
        ylab = "Frequência",
        col = "steelblue")

# Criar gráfico de barras para Brasil
barplot(frequencias_2,
        main = "Tempetura Brasil",
        xlab = "Valores Temperatura",
        ylab = "Frequência",
        col = "steelblue")


## grafico de dispersao  altitude SAO PAULO
plot(altitude_sp$temp_10m_sp~ altitude_sp$BR_elev, 
     xlab = "Altitude (m)",
     ylab = "Temperatura",
     pch = 16)

## grafico de dispersao altitude BRASIL
plot(altitude_br$temp_10m_br~altitude_br$BR_elev, 
     xlab = "Altitude (m)",
     ylab = "Temperatura",
     pch = 16)

dev.off()

cor.test(altitude_sp$BR_elev,altitude_sp$temp_10m, method = "spearman")
cor.test(altitude_br$BR_elev,altitude_br$temp_10m, method = "spearman")



### ETAPA 2 RESOLUCAO ESPACIAL PARA QUATRO TAMANHOS DE PIXELS
## carregar variavel altitude de Sao Paulo
altitude_sp <- st_read("altitude_1km_municipios_sao_paulo.shp")


#carregar variavel temperatura do worldclim mes de janeiro
temp_1<- raster("camada_1.tif")
temp_2<- raster("camada_2.tif")
temp_3<- raster("camada_3.tif")
temp_4<- raster("camada_4.tif")


## extrair dados de temperatura para Sao Paulo
resultados_1 <- extract(temp_1,altitude_sp)
resultados_2 <- extract(temp_2,altitude_sp)
resultados_3 <- extract(temp_3,altitude_sp)
resultados_4 <- extract(temp_4,altitude_sp)



## adicionar os valores das camadas a tabela de atributps
altitude_sp$temp1 <- resultados_1
altitude_sp$temp2 <- resultados_2
altitude_sp$temp3 <- resultados_3
altitude_sp$temp4 <- resultados_4

## exportar tabelas de atributos
write.csv(altitude_sp,"tabela_atributos.csv")


## grafico de barras com a temperatura de SAO PAULO
# Criando categorias (faixas de valores)
faixas_1 <- cut(altitude_sp$temp1, breaks = 20)
faixas_2 <- cut(altitude_sp$temp2, breaks = 20)
faixas_3 <- cut(altitude_sp$temp3, breaks = 20)
faixas_4 <- cut(altitude_sp$temp4, breaks = 20)


# Contando as frequências em cada faixa
frequencias_1 <- table(faixas_1)
frequencias_2 <- table(faixas_2)
frequencias_3 <- table(faixas_3)
frequencias_4 <- table(faixas_4)




# Criando o gráfico de barras
png("barras_resolucao_espacial.png", width = 1400, height = 1000)
par(mfrow = c(2, 2))## se quiser imprimir todos os graficos, habilite essa funcao
barplot(frequencias_1,
        main = "Temperatura janeiro, Municípios de São Paulo (A)",
        xlab = "Faixas de Valores",
        ylab = "Frequência",
        col = "steelblue")

barplot(frequencias_2,
        main = "Temperatura janeiro, Municípios de São Paulo (B)",
        xlab = "Faixas de Valores",
        ylab = "Frequência",
        col = "steelblue")

barplot(frequencias_3,
        main = "Temperatura janeiro, Municípios de São Paulo (C)",
        xlab = "Faixas de Valores",
        ylab = "Frequência",
        col = "steelblue")

barplot(frequencias_4,
        main = "Temperatura janeiro, Municípios de São Paulo (D)",
        xlab = "Faixas de Valores",
        ylab = "Frequência",
        col = "steelblue")
dev.off()

## plotar os resultados com a altitude SAO PAULO
png("dispersao_resolucao_espacial.png", width = 1400, height = 1000)
par(mfrow = c(2, 2))## se quiser imprimir todos os graficos, habilite essa funcao
plot(altitude_sp$temp1~ altitude_sp$BR_elev, 
     xlab = "Altitude (m)",
     ylab = "Temperatura",
     main = "Temperatura janeiro, Municípios de São Paulo (A)",
     pch = 16)

plot(altitude_sp$temp2~ altitude_sp$BR_elev, 
     xlab = "Altitude (m)",
     ylab = "Temperatura",
     main = "Temperatura janeiro, Municípios de São Paulo (B)",
     pch = 16)

plot(altitude_sp$temp3~ altitude_sp$BR_elev, 
     xlab = "Altitude (m)",
     ylab = "Temperatura",
     main = "Temperatura janeiro, Municípios de São Paulo (C)",
     pch = 16)

plot(altitude_sp$temp4~ altitude_sp$BR_elev, 
     xlab = "Altitude (m)",
     ylab = "Temperatura",
     main = "Temperatura janeiro, Municípios de São Paulo (D)",
     pch = 16)
dev.off()

## Testar a correlacao com o modelo teste de Spearman
cor.test(altitude_sp$BR_elev,altitude_sp$temp1, method = "spearman")
cor.test(altitude_sp$BR_elev,altitude_sp$temp2, method = "spearman")
cor.test(altitude_sp$BR_elev,altitude_sp$temp3, method = "spearman")
cor.test(altitude_sp$BR_elev,altitude_sp$temp4, method = "spearman")









