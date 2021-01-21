#### Dois grupos pareados

# Dados: peso de ratos ANTES e DEPOIS do tratamento

# Como colunas separadas

peso_antes  <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
peso_depois <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
length(peso_antes)

dados_col_sep <- data.frame(Rato = seq(1,10),peso_antes,peso_depois)

# Como fator de agrupamento
rato <- rep(1:10,2)
grupo <- rep(c("Antes","Depois"),each = 10)
peso <- c(peso_antes,peso_depois)
dados_por_grupos <- data.frame(rato,grupo,peso)

library(magrittr)
library(dplyr)

coeficiente_variacao <- function(x) {
  " CV de 0 - 15% : Muito baixa
  CV de 15% a 30% : Baixa
  CV de 30% a 60% : Moderada
  CV de 60% a 80% : Alta
  CV maior de 80% : Muito Alta"
  
  cv <- (sd(x)/mean(x))*100
  return(cv)
}

dados_por_grupos %>% group_by(grupo) %>% 
  summarise(n = n(), Media = mean(peso), Mediana = median(peso), 
            Desvio = sd(peso), CV = coeficiente_variacao(peso))

library(psych)
summary(dados_col_sep)
describe(dados_col_sep)

library(pastecs)
by(dados_por_grupos$peso,dados_por_grupos$grupo,stat.desc)

library(ggplot2)
boxPlot <- dados_por_grupos %>% ggplot(aes(reorder(grupo, peso), peso)) + 
  geom_boxplot() + xlab("Grupo") + ylab("Peso") 
boxPlot

###  Suposicao de normalidade para o vetor de diferencas
dif <- dados_col_sep$peso_depois - dados_col_sep$peso_antes

# TESTES DE NORMALIDADE

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
  
  return(resultados)
}

testesNormalidade(dif)

# Verificacao grafica
hist(dif)

# Teste T pareado
# H0: medias iguais, H1: medias nao iguais
testeTpareado <- t.test(dados_col_sep$peso_depois, dados_col_sep$peso_antes, 
                        paired = TRUE)

# Estimando o tamanho do efeito(r)
" r < 0.30: efeito pesqueno
  0.30 < r < 0.50: efeito medio
  r > 0.50: efeito grande "

t <- testeTpareado$statistic
df <- testeTpareado$parameter

efeitoRpareado <- round(sqrt((t^2)/((t^2) + df)),4)

# Abordagem nao parametrica (Diferencas nao-normal ou poucos dados)
wilcoxTeste <- wilcox.test(dados_col_sep$peso_antes,dados_col_sep$peso_depois,
                           paired = TRUE)

# Estimando o tamanho do efeito(r)
" r < 0.30: efeito pesqueno
  0.30 < r < 0.50: efeito medio
  r > 0.50: efeito grande "

z <- qnorm(wilcoxTeste$p.value/2)
n <- nrow(dados_col_sep)*2
r <- z / sqrt(n)


