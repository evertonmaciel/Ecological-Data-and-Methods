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



# pacotes necessários
install.packages("landscapemetrics")
install.packages("terra")


library(landscapemetrics)
library(terra)


list_lsm()


data <- rast("classes_uso_terra.tif")
plot(data)
check_landscape(data)

resultado_machas <-calculate_lsm(data, level = "patch")


resultados_classe <-calculate_lsm(data, level = "class")

write.csv(resultados_classe,"class_result.csv")

resultados_paisagem <-calculate_lsm(data, level = "landscape")


write.csv(resultados_paisagem,"landscape_result.csv")





########################################
#Manipular os resultados

# excluir as colunas que nao fazem sentido
resultados_classe$layer <- NULL
resultados_classe$id <- NULL

#excluir os NAs da coluna value
resultados_classe <- resultados_classe[!is.na(resultados_classe$value), ]


## reorganizar a planilha de dados
library(dplyr)
library(tidyr)
library(ggplot2)

# remover os desvios padroes e os coeficietes de variacoes
result <- resultados_classe%>% 
  filter(!grepl("(_cv|_sd)", metric))

#teste de correlacao
# inverter as colunas para as linhas
df_wide <- result %>% 
  pivot_wider(id_cols = c(level, class), names_from = metric, values_from = value)

# Identify the metric columns (all columns that were created from 'metric')
metric_cols <- setdiff(names(df_wide), c( "level", "class"))

# Compute Spearman correlation matrix, using pairwise complete observations
cor_mat <- cor(df_wide %>% select(all_of(metric_cols)), method = "spearman", use = "pairwise.complete.obs")

# Prepare the correlation matrix for plotting
cor_df <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Metric1", "Metric2", "Correlation")


png("grafico_correlacao.png", width = 1500, height = 1200)
# Create a heatmap using ggplot2
ggplot(cor_df, aes(x = Metric1, y = Metric2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limit = c(-1,1), name="Spearman\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Spearman Correlation between Metric Parameters", x = "", y = "")
dev.off() 



 