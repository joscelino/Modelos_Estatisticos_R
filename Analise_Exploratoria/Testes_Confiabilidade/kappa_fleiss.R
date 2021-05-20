# 1. Pacotes necessarios
library(dplyr)
library(DescTools)
library(irr)
library(tibble)


# 2.  Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dados_fleiss.csv",
                   stringsAsFactors = TRUE) 
dados <- tibble::as_tibble(dados)
dplyr::glimpse(dados)

# 3. Calculo do Kappa
# H0: kappa = 0 | H1: kappa != 0
# Interpretacao do kappa (McHugh, 2012)
# 0.0 - 0.2 - Nula
# 0.21 - 0.39 - Minima
# 0.40 - 0.59 - Fraca
# 0.6 - 0.79 - Moderada
# 0.8 - 0.9 - Forte
# acima de 0.9 - Perfeita
irr::kappam.fleiss(dados[2: 4], detail = TRUE)

# 4. Calculo do IC 95%
DescTools::KappaM(dados[2: 4], method = "Fleiss", conf.level = 0.95)

# 4. Calculo da concordancia
irr::agree(dados[2: 4])
