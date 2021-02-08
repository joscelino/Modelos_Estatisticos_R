# Analise exploratoria
library(pacman)
pacman::p_load(tidyverse, pastecs, tibble, ggpubr, lawstat, HH, stringr, ggplot2,
               patchwork, plotly, dplyr)

# Banco de dados
dados <- ToothGrowth
dados <- tibble::as.tibble(dados)
glimpse(dados)

# Acessando as variaveis
dados$len
dados$supp
dados$dose <- factor(dados$dose) # Tranformando em variaveis categoricas
# Acessando as variaveis
names(dados) <- c("comp","metodo","dose")

pastecs::stat.desc(dados$comp) # Analise geral 

by(dados[,"comp"],dados$metodo,stat.desc) # Analise por grupos
by(dados[,"comp"],dados$metodo,summary) # Analise por grupos COM QUARTIS

boxPlot <- plotly::plot_ly(data = dados, y = ~comp, x =  ~metodo,
                           type = "box", quartilemethod = "linear",
                           jitter = 0.3, color = ~metodo)
boxPlot

" Verificando a suposicao de normalidade atraves de graficos "
# Separando os dados em grupos
grupo_oj <- dados %>% dplyr::filter(metodo == "OJ")
grupo_vc <- dados %>% filter(metodo == "VC")


# Graficos
# Histograma
hist(grupo_oj$comp,main = "Histograma comprimento - Grupo OJ") 
hist(grupo_vc$comp,main = "Histograma comprimento - Grupo VC")

# Densidade
ggpubr::ggdensity(grupo_oj$comp, xlab = "Comprimento", main = "Comprimento - grupo OJ",
          fill = "lightgray")
ggpubr::ggdensity(grupo_vc$comp, xlab = "Comprimento", main = "Comprimento - grupo VC",
          fill = "lightgray")

# QQPlot (Grafico de probabilidade)
ggpubr::ggqqplot(grupo_oj$comp, main = "Normalidade do grupo OJ" )
ggpubr::ggqqplot(grupo_vc$comp, main = "Normalidade do grupo VC" )


# TESTES DE NORMALIDADE

testesNormalidade <- function(x) {
  
  " Verificando a suposicao de normalidade atraves de testes estatisticos "
  " se p-valor < 0.05, rejeita-se a hipotese nula (no caso, normalidade) "
  " Na comparacao de 2 grupos independentes, a suposicao de normalidade somente
  sera aceita quando houver normalidade em ambos os grupos (dados serao parametricos)"
  
  library(nortest)
  
  # Teste de Shapiro-Wilk
  t1 <- shapiro.test(x)
  # Teste de Shapiro-Francia
  t2 <- sf.test(x)
  # Teste Anderson-Darling
  t3 <- ad.test(x)
  # Teste Lilliefors
  t4 <- lillie.test(x)
  # Teste Cramer-Von Mises
  t5 <- cvm.test(x)
  # Teste Qui-quadrado Pearson
  t6 <- pearson.test(x)
  
  # Teste JarqueBera 
  library(tsoutliers)
  t7 <- JarqueBera.test(x)
  
  testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method, t6$method, 
              t7[[1]]$method)
  estatisticas <- c(t1$statistic, t2$statistic, t3$statistic, t4$statistic, 
                    t5$statistic, t6$statistic, t7[[1]]$statistic)
  estatisticas <- as.numeric(estatisticas)
  pvalues <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value, 
               t6$p.value, t7[[1]]$p.value)
  pvalues <- as.numeric(pvalues)
  
  # Matriz de resultados
  estatisticas <- round(estatisticas,4)
  pvalues <- round(pvalues,4)
  
  resultados <- data.frame(testes, estatisticas, pvalues)

  return(resultados)
}

df_Grupo_oj <- testesNormalidade(grupo_oj$comp)
df_Grupo_vc <- testesNormalidade(grupo_vc$comp)

# Suposicao de Homogeneidade de variancias
# Medidas descritivas 

coeficiente_variacao <- function(x) {
  " CV de 0 - 15% : Muito baixa
  CV de 15% a 30% : Baixa
  CV de 30% a 60% : Moderada
  CV de 60% a 80% : Alta
  CV maior de 80% : Muito Alta"
  
  cv <- (sd(x)/mean(x))*100
  return(cv)
}

cv_oj <- coeficiente_variacao(grupo_oj$comp)
cv_vc <- coeficiente_variacao(grupo_vc$comp)

# Testes de Homogeineidade de Variancias
# H0 : variancias iguais e H1: variancias deiferentes
library(lawstat)
levene.test(dados$comp, group = dados$metodo) # Nao precisa de normalidade dos dados

# Bartlett Teste
bartlett.test(comp ~ metodo, data = dados) # Mai indicado para dados normais

# Fligner Test (robusto)
fligner.test(comp ~ metodo, data = dados) # Robusto, nao precisa de normalidade dos dados

# Teste de Brown
library(HH)
a <- hov(comp ~ metodo, data = dados)
hovPlot(comp ~ metodo, data = dados, main = a$p.value)


# Teste t para amostras independentes
# Suposicoes : Normalidade dos dados e homogeneidade das variancas
# H0: medias iguais e H1: medias nao iguais

# Teste t
testeT <- t.test(comp ~ metodo, data = dados, paired = FALSE)

# Estimando o tamanho do efeito(r)
" r < 0.30: efeito pesqueno
  0.30 < r < 0.50: efeito medio
  r > 0.50: efeito grande "

t <- testeT$statistic
df <- testeT$parameter

efeitoR <- sqrt((t^2)/((t^2) + df))

# Estimando o tamanho do efeito de um fator pela estatistica de Wilcoxon Rank-Sum test
# PS: Consultar Tabela normal padrao 
mannWhitney <- wilcox.test(comp ~ metodo, data = dados, paired = FALSE)
z <- qnorm(mannWhitney$p.value/2)
N <- nrow(dados)
efeito <- z/sqrt(N)

# Plotando graficos
grafico <- ggplot(dados, 
                  aes(x = metodo, y = comp)) + 
  geom_boxplot(fill = c("#00CED1", "#BC8F8F")) + 
  stat_summary(fun = mean, geom = "point")
grafico <- grafico + geom_dotplot(binaxis = "y", stackdir = "center", 
                                  dotsize = 0.5)

library(stringr)
titulo <- str_c("Comparacao usando: ",testeT$method," | p-valor: ",
                round(testeT$p.value,4))
grafico <- grafico + ggtitle(titulo) + 
  geom_point(mapping = aes(x = metodo, y = comp))

ggplotly(grafico)

# Visualizacao dos dados
boxPlotMetodo <- ggplot2::ggplot(data = dados, aes(x = metodo, y = comp)) +
  ggplot2::geom_boxplot(mapping = aes(colour = dose), fill = "#F5FFFA", 
                        notch = FALSE, show.legend = TRUE, position = "dodge2") +
  ggplot2::scale_alpha_continuous() +
  ggplot2::labs(x = "Metodo", y =  "Comp") +
  ggplot2::theme_classic() 

grafico / boxPlotMetodo
