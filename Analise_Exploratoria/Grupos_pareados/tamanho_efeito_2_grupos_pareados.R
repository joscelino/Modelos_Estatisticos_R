################ Teste T Pareado (dois grupos) ################################
########################## Tamanho de efeito ##################################

library(dplyr)
library(rstatix)
library(effectsize)

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

# 1. Carregamento do banco de dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/efeito_2_grupos_pareados.csv",
                   stringsAsFactors = TRUE) %>%
  dplyr::rename(ID_Medico = ID_Médico, Convulsoes_PT = Convulsões_PT,
                Convulsoes_S1 = Convulsões_S1, Convulsoes_S6 = Convulsões_S6,
                Genero = Gênero)
base::is.null(dados)
dados$Data_Nasc <- base::as.Date(dados$Data_Nasc, format='%m/%d/%Y')
dados <- tibble::as_tibble(dados)

# 2. Verificacao da normalidade dos dados
dados$DiferencaPTS1 <- dados$Convulsoes_PT - dados$Convulsoes_S1
testesNormalidade(dados$DiferencaPTS1)

# Verificacao grafica
hist(dados$DiferencaPTS1, main = "Histograma das diferencas")

# 3. Teste t paredo
stats::t.test(dados$Convulsoes_S1, dados$Convulsoes_PT, paired = TRUE)

################# Calculo do tamno de efeito ###################################
# d de Cohen

dados %>% ggpubr::get_summary_stats(c(Convulsoes_PT, Convulsoes_S1, 
                                      DiferencaPTS1),
                                    type = "mean_sd")

# Lakens, 2013
# dz: calculado com base no desvio-padrao da diferenca
# delta de Glass: calculado com base no desvio-padrao do momento pre

# d de Cohen 

efeito_d <- effectsize::cohens_d(dados$Convulsoes_S1, dados$Convulsoes_PT, 
                               paired = TRUE)

d <- efeito_d[[1]] 

effectsize::interpret_d(d, rules = "cohen1988")
effectsize::interpret_d(d, rules = "sawilowsky2009")

effectsize::d_to_common_language(d) # 43.77%


# g de Hedges (correcao para amostras pequenas - n < 20)

efeito_g <- effectsize::hedges_g(dados$Convulsoes_S1, dados$Convulsoes_PT, 
                     paired = TRUE)
g <- efeito_g[[1]]

effectsize::interpret_g(g, rules = "cohen1988")
effectsize::interpret_d(g, rules = "sawilowsky2009")

# Delta de Glass (recomendado para pre x pos, usa do DP do pre)
# deve ser montado com (pos, pre)
efeito_delta <- effectsize::glass_delta(dados$Convulsoes_S1, dados$Convulsoes_PT)

delta <- efeito_delta[[1]]

effectsize::interpret_g(delta, rules = "cohen1988")
effectsize::interpret_d(delta, rules = "sawilowsky2009")

effectsize::d_to_common_language(delta) # 45.50%
