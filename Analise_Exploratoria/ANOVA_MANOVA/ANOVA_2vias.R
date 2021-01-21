library(readr)
library(tibble)

### ANOVA de variaveis independentes intra-sujeitos 
### PRESSUPOSTOS (DADOS E NOS RESIDUOS)
# 1. Normalidade dos dados
# 2. Homogeneidade de variancias
# 3. Ausencia de Outliers


# Funcao de Teste de Normalidade

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
dados <- read_csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosAnova2vias.csv")
dados$Genero <- as.factor(dados$Genero)
summary(dados$Alcool)
dados$Alcool <- factor(dados$Alcool, 
                          levels = c("Nenhum", "2 Canecas", "4 Canecas"))

dados
glimpse(dados)

# Verificando os pressupostos dos dados brutos
# Verificacao da Normalidade dos dados
library(dplyr)
library(rstatix)

# Teste de Shapiro-Wilk
# H0: dados normais - H1: dados nao normais
dados %>% group_by(Genero, Alcool) %>% shapiro_test(Memoria)

# Verificacao de outliers por grupo
# Verificacao grafica
boxplot(dados$Memoria ~ dados$Genero:dados$Alcool)

# Verificacao estatistica
dados %>% group_by(Genero, Alcool) %>% identify_outliers(Memoria)

# Verificacao da homogeneidade de variancias
# H0: homogeneidade - H1: nao homogeneidade
levene_test(dados, Memoria ~ Genero * Alcool, center = mean)

# Verificacao dos pressupostos nos residuos
# Construcao do modelo
modelo <- aov(Memoria ~ Genero * Alcool, dados)
modelo

# Teste de Normalidade
testesNormalidade(modelo$residuals)

# Verificacao de outliers
# Verificacap grafica
boxplot(modelo$residuals)

# Verificacao estatistica
dados$Residuals <- modelo$residuals #Criando uma coluna no Banco de dados

dados %>% group_by(Genero, Alcool) %>% identify_outliers(Residuals)
dados %>% identify_outliers(Residuals)

# Verificacao da homogeneidade de variancias
# H0: homogeneidade - H1: nao homogeneidade
levene_test(dados, Residuals ~ Genero * Alcool, center = mean)

# Realizacao da ANOVA
# Mudanca de contraste para equivale ao SPSS
options(contrasts = c("contr.sum", "contr.poly"))

# Criacao do Modelo
modeloANOVA <- aov(Memoria ~ Genero * Alcool, dados)
summary(modeloANOVA)
# H0: nao ha efeito - H1: Ha efeito
Anova(modeloANOVA, type = "III")

# Graficos de interacao
library(ggplot2)
library(plotly)

# Grafico com generos de cores diferentes
graficoA <- ggplot(dados, aes(x = Alcool, y = Memoria, group = Genero, 
                              color = Genero)) +
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.6) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)
ggplotly(graficoA)

# Grafico com generos de linhas diferentes
graficoB <- ggplot(dados, aes(x = Alcool, y = Memoria, group = Genero)) +
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.6,
            aes(linetype = Genero)) +
  geom_point(stat = "summary", fun.y = "mean", size = 2, aes(shape = Genero)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)
ggplotly(graficoB)

# Estimated Marginal Means 
library(emmeans)

# Metodo Bonferroni (* opcao de ser por Sidak)
dados %>% group_by(Genero) %>%
  emmeans_test(Memoria ~ Alcool, p.adjust.method = "bonferroni")

dados %>% group_by(Alcool) %>%
  emmeans_test(Memoria ~ Genero, p.adjust.method = "bonferroni")

# Analises de Post-Hoc
library(DescTools)

# Uso do Duncan
PostHocTest(modeloANOVA, method = "duncan", conf.level = 0.95)

# Uso do TukeyHSD
PostHocTest(modeloANOVA, method = "hsd", conf.level = 0.95)

# Uso do Bonferroni
PostHocTest(modeloANOVA, method = "bonferroni", conf.level = 0.95)

# Analise descritiva dos dados (Pacotes psych e rstatix)
library(psych)

dados %>% group_by(Genero, Alcool) %>%
  get_summary_stats(Memoria, type = "mean_sd")
  