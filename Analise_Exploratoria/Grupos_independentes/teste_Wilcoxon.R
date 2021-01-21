library(pacman)
pacman::p_load(dplyr, readr, rstatix, tibble)

# TESTE NAO-PARAMETRICO DE WILCOXON

# PRESSUPOSTOS
# 1. Variavel depedente deve ser numerica ou categorica ordinal
# Variavel independente composta por dois grupos dependentes (pareados)

# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/BancodeDados4.csv") 
dados <- dados %>% rename(Convulsoes_PT = Convulsões_PT, 
         Convulsoes_S1 = Convulsões_S1,
         Convulsoes_S6 = Convulsões_S6,
         ID_Medico = ID_Médico,
         Genero = Gênero)
dados <- as_tibble(dados)
dados$ID_Paciente <- as.factor(dados$ID_Paciente)
dados$ID_Medico <- as.factor(dados$ID_Medico)
dados$Genero <- as.factor(dados$Genero)
dados
glimpse(dados)

# Realizacao do Teste de Wilcoxon
# H0: Mediana das diferencas igual a ZERO | H1: Mediana das diferencas diferente de ZERO
# Neste caso verifica se a mediana das Convulsoes_PT eh maior que a mediana das
# Convulsoes_S1
wilcox.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE)

# Analise descritiva dos dados
dados$dif <- dados$Convulsoes_PT - dados$Convulsoes_S1
dados

dados %>% get_summary_stats(Convulsoes_PT, Convulsoes_S1, dif, type = "median_iqr")
