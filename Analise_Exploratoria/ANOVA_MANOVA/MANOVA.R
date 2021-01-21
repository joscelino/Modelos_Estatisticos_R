library(readr)
library(tibble)

### MANOVA (ANOVA Multivariada)
### PRESSUPOSTOS (DADOS E NOS RESIDUOS)
# 1. Normalidade dos dados (univariada e multivariada)
# 2. Homogeneidade das matrizes de covariancias-variancias entre grupos
# 3. Ausencia de Outliers multivariados
# 4. Multicolinearidade 
# 5. Lineariedade entre os pares


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

# Verificacao de Normalidade Multivariada
library(dplyr)
library(rstatix)

# Teste de Shapiro-Wilk
dados %>% select(2:4) %>% group_by(Alcool) %>%
  doo(~mshapiro_test(.))

# Teste de Henze-Zirkler
# Testa normalidades UNIVARIADA e MULTIVARIADA por grupos
library(MVN)
mvn(data = dados[,2:4], subset = "Alcool", mvnTest = "hz")

# Teste de normalidade UNIVARIADA
dados %>% select(2:4) %>% group_by(Alcool) %>%
  shapiro_test(Latencia, Memoria)

# Verificacao dos Outliers Multivariados
# Pela distancia de Mahalanobis (outlier: p<0.001)
outliersM <- dados %>% select(2:4) %>% group_by(Alcool) %>%
  doo(~mahalanobis_distance(.)) 
summary(outliersM$is.outlier)
  
# Verificacao grafica de Outliers Univariados por grupo
boxplot(dados$Memoria ~ dados$Alcool)
boxplot(dados$Latencia ~ dados$Alcool)

# Verificando a presenca de OutLiers por grupo
dados %>% group_by(Alcool) %>% identify_outliers(Memoria)
dados %>% group_by(Alcool) %>% identify_outliers(Latencia)

# Verificando a homogeneidade das Matrizes de covariancia e variancia
# Se rompido e n iguais por grupo: Pillai e Hotelling sao confiaveis
# caso os n sejam diferentes, uma opcao eh usar a MANOVA robusta
# Alfa: p>0,001

box_m(dados[,c("Memoria", "Latencia")], dados$Alcool)

# Verificacao da Homogeneidade de Variancias 
levene_test(dados, Memoria ~ Alcool, center = mean)
levene_test(dados, Latencia ~ Alcool, center = mean)

# Verificacao da presenca de multicolinearidade (r > 0.9)
# Matriz de correlacao
cor(dados[,3:4])

# Verificacao da relacao linear entre as variaveis dependentes por grupo
pairs(dados[,3:4], pch = 19, col = dados$Alcool)

library(GGally)

grafico <- dados %>%
  select(2:4) %>%
  group_by(Alcool) %>%
  doo(~ggpairs(.) + theme_grey(), result = "plots")

grafico$plots[wich=1]
grafico$plots[wich=2]
grafico$plots[wich=3]

# Construcao do modelo
modelo <- manova(cbind(Latencia, Memoria) ~ Alcool, data = dados)
summary(modelo, test = "Wilks")
summary(modelo, test = "Pillai")

# ANOVA univariada
summary.aov(modelo)
# Analises de Post-Hoc
# Estimated Marginal Means
library(emmeans)

dados %>% emmeans_test(Memoria ~ Alcool, p.adjust.method = "bonferroni")
dados %>% emmeans_test(Latencia ~ Alcool, p.adjust.method = "bonferroni")

# Outras opcoes de Post Hoc
library(DescTools)
TukeyHSD(x = aov(Memoria ~ Alcool, data = dados), "Alcool", conf.level = 0.95)
TukeyHSD(x = aov(Latencia ~ Alcool, data = dados), "Alcool", conf.level = 0.95)

pairwise.t.test(dados$Memoria, dados$Alcool, paired = TRUE,
                p.adjust.method = "bonferroni")

pairwise.t.test(dados$Latencia, dados$Alcool, paired = TRUE,
                p.adjust.method = "bonferroni")

PostHocTest(aov(Memoria ~ Alcool, data = dados), method = "duncan", conf.level = 0.95)
PostHocTest(aov(Memoria ~ Alcool, data = dados), method = "bonferroni", conf.level = 0.95)
PostHocTest(aov(Memoria ~ Alcool, data = dados), method = "hsd", conf.level = 0.95)

PostHocTest(aov(Latencia ~ Alcool, data = dados), method = "duncan", conf.level = 0.95)
PostHocTest(aov(Latencia ~ Alcool, data = dados), method = "bonferroni", conf.level = 0.95)
PostHocTest(aov(Latencia ~ Alcool, data = dados), method = "hsd", conf.level = 0.95)


# Grafico Final
library(ggplot2)
library(plotly)
graficoFinal <- ggplot(dados, aes(x =  Latencia, y = Memoria, group = Alcool,
                                  color = Alcool)) +
  geom_point(size = 1.5) + theme_classic()
ggplotly(graficoFinal)

# Analise descritiva
dados %>% group_by(Alcool) %>%
  get_summary_stats(Memoria, Latencia, type = "mean_sd")

