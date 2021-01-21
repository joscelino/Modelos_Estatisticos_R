# Teste Qui-quadrado de Aderencia
library(readr)
library(tibble)

# Importacao dos dados
dados <- read_csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/quiQuadradoAderencia.csv")
dados
glimpse(dados)

# Criacao da Tabela 
tabela <- table(dados$Tipo_Ervilha)
tabela

# Realizacao do Modelo
# H0: Frequencias observadas sao iguais as frequencias esperadas
# H1: Frequencias observadas sao diferentes das frequencias esperadas
quiqua <- chisq.test(tabela, p = c(0.5625, 0.1875, 0.1875, 0.0625))
quiqua

### Analise de residuos (SOMENTE SE A HIPOTESE NULA FOR REJEITADA)
# Analise dos Residuos Padronizados 
quiqua$residuals

# Analise dos Residuos Padronizados  Ajustados > 1,96 e < - 1.96
quiqua$stdres

# Calculo do ponte de corte para os residuos padronizados
### Sendo "l" o numero de linhas e "c" o numero de colunas
### Dividiremos o 0.05 pelo produto c*l (numero de celulas)

novoAlfa <- 0.05/length(tabela)
novoAlfa

# Calculo do ponto de corte, com base no bovo Alfa
## Divisao por 2 por ser bicaudal

qnorm(novoAlfa/2) # Novos limites para os residuos padronizados ajustados (positivo e negativo)

# Calculo de p para os residuos (Serao estatisticamente significativos se menores que novoAlfa)
round(2 * (1 - pnorm(abs(quiqua$stdres))), 6)

# Tamanho do Efeito - V de Cramer
# A interpretacao depende do grau de liberdade
# gl = (linhas - 1) * (colunas - 1)
library(rstatix)
cramer_v(tabela, p = c(0.5625, 0.1875, 0.1875, 0.0625))

