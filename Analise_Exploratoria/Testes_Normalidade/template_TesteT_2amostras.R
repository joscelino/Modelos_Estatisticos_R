library(readr)
library(tibble)
library(dplyr)

# TESTE-T para duas variaveis independentes
# Pressupostos
# 1. Normalidade dos dados
# 2. Homogeneidade de Variancias

# importacao dos daados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosDuasAmostrasIndependentes.csv")
dados <- as_tibble(dados)
dados$Sujeito <- as.factor(dados$Sujeito)
dados$Genero <- as.factor(dados$Genero)
dados$Escola <- as.factor(dados$Escola)
dados$Posicao_Sala <- as.factor(dados$Posicao_Sala)
dados
glimpse(dados)

# Teste de Normalidade por grupos
# H0: Normalidade dos dados | H1: Dados nao normais
library(RVAideMemoire)
byf.shapiro(Nota_Biol ~ Posicao_Sala, dados)
byf.shapiro(Nota_Fis ~ Posicao_Sala, dados)
byf.shapiro(Nota_Hist ~ Posicao_Sala, dados)

# Verificacao da homogeneidade de Variancias
# H0: Variancas homogeneas | H1: Variancas nao homogeneas
library(rstatix)
levene_test(dados, Nota_Biol ~ Posicao_Sala, center = mean)
levene_test(dados, Nota_Fis ~ Posicao_Sala, center = mean)
levene_test(dados, Nota_Hist ~ Posicao_Sala, center = mean)

# Realizacao do teste-t para amostras independentes
# H0: Medias iguais | H1: Medias diferentes
t_test(dados, Nota_Biol ~ Posicao_Sala, var.equal = TRUE)
t_test(dados, Nota_Fis  ~ Posicao_Sala, var.equal = FALSE)
t_test(dados, Nota_Hist ~ Posicao_Sala, var.equal = FALSE)

# Visualizacao da distribuicao dos dados
library(ggplot2)
library(plotly)

# GGplot
graficoBoxPlot <- ggplot(dados, aes(x=Posicao_Sala, y=Nota_Biol)) + 
  geom_boxplot(fill="green", colour = "#3366FF", alpha=0.2) + 
  theme_classic() + xlab("Posicao na sala") + 
  ylab("Altura (cm)") + ggtitle("Notas de Biologia")
ggplotly(graficoBoxPlot)

graficoBoxPlot2 <- ggplot(dados, aes(x=Posicao_Sala, y=Nota_Fis)) + 
  geom_boxplot(fill="gray", colour = "#3366FF", alpha=0.2) + 
  theme_classic() + xlab("Posicao na sala") +
  ylab("Altura (cm)") + ggtitle("Notas de Fisica")
ggplotly(graficoBoxPlot2)

graficoBoxPlot3 <- ggplot(dados, aes(x=Posicao_Sala, y=Nota_Hist)) + 
  geom_boxplot(fill="yellow", colour = "#3366FF", alpha=0.2) + 
  theme_classic() + xlab("Posicao na sala") +
  ylab("Altura (cm)") + ggtitle("Notas de Historia")
ggplotly(graficoBoxPlot3)

# Identificando os Outliers
library(rstatix)
dados %>% group_by(Posicao_Sala) %>% identify_outliers(Nota_Biol)
dados %>% group_by(Posicao_Sala) %>% identify_outliers(Nota_Fis)
dados %>% group_by(Posicao_Sala) %>% identify_outliers(Nota_Hist)

