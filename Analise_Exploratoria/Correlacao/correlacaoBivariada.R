library(readr)
library(tibble)

# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/correlacaoBivariada.csv")
dados <- as_tibble(dados)
dados
glimpse(dados)

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

# Verificacap dos Pressupostos para correlacao de Pearson 
# Pressupostos: normalidade dos dados, ausencia de outliers
# Teste de normalidade
testesNormalidade(dados$Ansiedade)
testesNormalidade(dados$Nota)

# Identificacao de outliers
library(rstatix)

# Usando modelo estatistico
dados %>% group_by(Nota) %>% identify_outliers(Ansiedade)

# Boxplot
boxplot(dados$Ansiedade)
boxplot(dados$Nota)

# Relacao linear entre variaveis
plot(dados$Ansiedade, dados$Nota)

# Verificacao dos pressupostos nos residuos
# Construcao do Modelo

modeloRegressao <- lm(Nota ~ Ansiedade, dados)
modeloRegressao

# Analise Grafica 1 - Homocedasticidade
par(mfrow = c(1,2))
plot(modeloRegressao, wich = c(1,3))

par(mfrow = c(1,1))

## Correlacao Linear de Pearson (coeficiente = r)
# H0: coeficiente de correcao eh zero, H1: coeficiente de correlacao diferente de zero
# Nao indicada se nao forem atendidos os pressupostos
r <- cor.test(dados$Nota, dados$Ansiedade, method = "pearson")
r$p.value
r$estimate

## Correlacao de Postos de Spearman (coeficiente = ro)
ro <- cor.test(dados$Nota, dados$Ansiedade, method = "spearman")
ro$p.value
ro$estimate

## Correlacao de Tau de Kendall (coeficiente = tau)
tau <- cor.test(dados$Nota, dados$Ansiedade, method = "kendall")
tau$p.value
tau$estimate

# Grafico de dispersao
library(plotly)
library(ggplot2)
a <- ggplot(dados, aes(x = Ansiedade, y = Nota)) + 
  labs(x = "Ansiedade pre-prova", y =  "Desempenho na Prova") +
  geom_point(size = 1.8) + theme_classic2()
ggplotly(a)

# Matrizes de correlacao

# Criando a matriz de Pearson
matrizPearson <- round(cor(dados[2:4], method = "pearson"),2)
view(matrizPearson)

# Criando uma matriz visual 
cor_plot(matrizPearson, method = "color", type = "upper")

