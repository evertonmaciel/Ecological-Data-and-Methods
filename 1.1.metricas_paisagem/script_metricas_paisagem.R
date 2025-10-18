#################################################################
## Serviço Público Federal
## Ministério da Educação
## Universidade Federal de São Paulo – UNIFESP
## Curso: Ciências Ambientais
## Disciplina: Modelagem de Paisagem
## Aula Métricas de Paisagem
## Prof. Dr. Everton Alves Maciel (everttonmaciel@gmail.com)
## São Paulo, 2025.
################################################################
## Objetivos
## Criar métricas de paisagem usando o pacote landscapemetrics
## Testar redundância entre as métricas
###############################################################

#Primeira etapa
#Criar métricas de paisagem

# pacotes para métricas de paisagem
install.packages("landscapemetrics")
install.packages("terra")


library(landscapemetrics)
library(terra)


list_lsm()


# camadas de dados com diferentes clases de uso da terra
data <- rast("classes_uso_terra.tif")
plot(data)
check_landscape(data)

# métricas no nível de manchas
resultados_manchas <-calculate_lsm(data, level = "patch")
write.csv(resultados_manchas,"class_result.csv")

# métricas no nível de classe
resultados_classes <-calculate_lsm(data, level = "class")
write.csv(resultados_classes,"class_result.csv")

# métricas no nível de paisagem
resultados_paisagem <-calculate_lsm(data, level = "landscape")
write.csv(resultados_paisagem,"landscape_result.csv")





########################################
#Segunda etapa
#Analisar a redundância das métricas de paisagem


## Pacotes necessário 
library(dplyr)
library(tidyr)
library(ggplot2)

# excluir as colunas que não fazem sentido
resultados_classes$layer <- NULL
resultados_classes$id <- NULL

#excluir os NAs da coluna value
resultados_classes <- resultados_classes[!is.na(resultados_classes$value), ]

# remover os desvios padroes e os coeficietes de variacoes
result <- resultados_classes%>% 
  filter(!grepl("(_cv|_sd)", metric))

## TESTE DE CORRELACAO COM TODAS AS METRICAS
## inverter as colunas para as linhas
df_wide <- result %>% 
  pivot_wider(id_cols = c(level, class), names_from = metric, values_from = value)

metric_cols <- setdiff(names(df_wide), c( "level", "class"))

## Calcular a matriz de correlação de Spearman, usando observações completas em pares
cor_mat <- cor(df_wide %>% select(all_of(metric_cols)), method = "spearman", use = "pairwise.complete.obs")

## Preparar a matriz de correlação 
cor_df <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Metric1", "Metric2", "Correlation")

png("grafico_correlacao.png", width = 1500, height = 1200)
## Create a heatmap using ggplot2
ggplot(cor_df, aes(x = Metric1, y = Metric2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limit = c(-1,1), name="Spearman\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Spearman Correlation between Metric Parameters", x = "", y = "")
dev.off() 





