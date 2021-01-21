library(pacman)
pacman::p_load(readr,tibble,dplyr)

# TESTE-T PAREADO
# Pressupostos
# 1. Normalidade dos dados
# 2. Homogeneidade de Variancias

# Funcao para testes de Normalidade

testesNormalidade <- function(x) {
  
  " Verificando a suposicao de normalidade atraves de testes estatisticos "
  " se p-valor < 0.05, rejeita-se a hipotese nula (no caso, normalidade) "
  " Na comparacao de 2 grupos independentes, a suposicao de normalidade somente
  sera aceita quando houver normalidade em ambos os grupos (dados serao parametricos)"
  library(pacman)
  pacman::p_load(nortest)

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
  pacman::p_load(tsoutliers)

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
  estatisticas <- round(estatisticas,7)
  pvalues <- round(pvalues,7)
  
  resultados <- data.frame(testes, estatisticas, pvalues)
  
  # Densidades
  pacman::p_load(ggpubr)

  a <- ggdensity(x)
  b <- ggqqplot(x)
  print(a)
  print(b)
  
  return(resultados)
}

# importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosDuasAmostrasDependentes.csv") %>%
      rename(Convulsoes_PT = Convulsões_PT, Convulsoes_S1 = Convulsões_S1,
             Convulsoes_S6 = Convulsões_S6, ID_Medico = ID_Médico,
             Genero = Gênero)
dados <- as_tibble(dados)
dados$ID_Paciente <- as.factor(dados$ID_Paciente)
dados$ID_Medico <- as.factor(dados$ID_Medico)
dados$Genero <- as.factor(dados$Genero)
dados$Data_Nasc <- as.character.Date(dados$Data_Nasc)
dados
glimpse(dados)

# Teste de normalidade dos dados
###  Suposicao de normalidade para o vetor de diferencas
dados$DiferencasPTS1 <- dados$Convulsoes_S1 - dados$Convulsoes_PT
testesNormalidade(dados$DiferencasPTS1)

# Verificacao grafica
hist(dados$DiferencasPTS1, main = "Histograma das diferencas")

# Teste-t pareado
# H0: Medias iguais | H1: Medias diferentes
testeTPareado <- t.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE)

# Estimando o tamanho do efeito(r)
" r < 0.30: efeito pequeno
  0.30 < r < 0.50: efeito medio
  r > 0.50: efeito grande "

t <- testeTPareado$statistic
df <- testeTPareado$parameter

efeitoRpareado <- round(sqrt((t^2)/((t^2) + df)),4)
efeitoRpareado

# BoxPlot
par(mfrow=c(1, 2))
boxplot(dados$Convulsoes_PT, ylab = "Quantidade de convulsoes",
        xlab = "Pre=tratamento")
boxplot(dados$Convulsoes_S1, ylab = "Quantidade de convulsoes",
        xlab = "1a Semana de Tratamento")

# Graficos estao em escalas diferentes o que impede comparacao
# Alterar escalas eh uma das formas de MENTIR COM ESTATISTICA

# Identificando os Outliers
pacman::p_load(rstatix)
dados %>% group_by(Genero) %>% identify_outliers(Convulsoes_PT)
dados %>% group_by(Genero) %>% identify_outliers(Convulsoes_S1)

# Analise descritiva dos dados
pacman::p_load(psych)

summary(dados$Convulsoes_PT)
summary(dados$Convulsoes_S1)

# Funcao describe
describe(dados$Convulsoes_PT)
describe(dados$Convulsoes_S1)

# Abordagem nao parametrica (Diferencas nao-normal ou poucos dados)
wilcoxTeste <- wilcox.test(dados$Convulsoes_PT,dados$Convulsoes_S1,
                           paired = TRUE)

# Estimando o tamanho do efeito(r)
" r < 0.30: efeito pequeno
  0.30 < r < 0.50: efeito medio
  r > 0.50: efeito grande "

z <- qnorm(wilcoxTeste$p.value/2)
n <- nrow(dados)*2
r <- z / sqrt(n)
r
