library(readr)
library(tibble)
library(dplyr)

# TESTE T ARA UMA AMOSTRA
# Pressupostos
# 1. Normalidade dos dados

# Funcao para testes de Normalidade

testesNormalidade <- function(x) {
  
  " Verificando a suposicao de normalidade atraves de testes estatisticos "
  " se p-valor < 0.05, rejeita-se a hipotese nula (no caso, normalidade) "
  " Na comparacao de 2 grupos independentes, a suposicao de normalidade somente
  sera aceita quando houver normalidade em ambos os grupos (dados serao parametricos)"
  
  library(nortest)
  
  # Kolmogorov-Smirnov
  t1 <- ks.test(x, "pnorm")
  # Teste de Shapiro-Wilk
  t2 <- shapiro.test(x)
  # Teste de Shapiro-Francia
  t3 <- sf.test(x)
  # Teste Anderson-Darling
  t4 <- ad.test(x)
  # Teste Lilliefors
  t5 <- lillie.test(x)
  # Teste Cramer-Von Mises
  t6 <- cvm.test(x)
  # Teste Qui-quadrado Pearson
  t7 <- pearson.test(x)
  
  # Teste JarqueBera 
  library(tsoutliers)
  t8 <- JarqueBera.test(x)
  
  testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method, t6$method, 
              t7$method, t8[[1]]$method)
  estatisticas <- c(t1$statistic, t2$statistic, t3$statistic, t4$statistic, 
                    t5$statistic, t6$statistic, t7$statistic, t8[[1]]$statistic)
  estatisticas <- as.numeric(estatisticas)
  pvalues <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value, 
               t6$p.value, t7$p.value, t8[[1]]$p.value)
  pvalues <- as.numeric(pvalues)
  
  # Matriz de resultados
  estatisticas <- round(estatisticas,4)
  pvalues <- round(pvalues,4)
  
  resultados <- data.frame(testes, estatisticas, pvalues)
  
  # Densidades
  library(ggpubr)
  
  a <- ggdensity(x)
  b <- ggqqplot(x)
  print(a)
  print(b)
  
  return(resultados)
}


# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosTesteT_NormalidadeUmaAmostra.csv")
dados <- as_tibble(dados)
dados$Sujeito <- as.factor(dados$Sujeito)
dados$Genero <- as.factor(dados$Genero)
dados$Grau_de_Instruçao <- as.factor(dados$Grau_de_Instruçao)
glimpse(dados)

# Testes de Normalidade
testesNormalidade(dados$Altura)

# Teste-t para uma amostra
# H0: Medias iguais | H1: Medias diferentes
valorDeReferencia <- 167
testeT <- t.test(dados$Altura, mu = valorDeReferencia)
testeT
testeT$p.value

# Visualizacao da distribuicao dos dados
library(ggplot2)
library(plotly)

graficoBoxPlot <- ggplot(dados, aes(x=Genero, y=Altura)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  theme_classic() +
  ylab("Altura (cm)") + ggtitle("Graficos BoxPlot Teste-t")

ggplotly(graficoBoxPlot)

# Identificando os Outliers
library(rstatix)
dados %>% group_by(Genero) %>% identify_outliers(Altura)
