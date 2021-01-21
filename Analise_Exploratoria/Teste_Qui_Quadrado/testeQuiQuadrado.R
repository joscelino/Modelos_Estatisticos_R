# Teste Qui-quadrado de independencia
library(readr)
library(tibble)

# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/diabetes.csv")
glimpse(dados)

# Criacao da Tabela de Contingencia
dados$Faixa_Etaria <- factor(dados$Faixa_Etaria,
                             levels = c("Menos de 30 anos",
                                        "30 a 50 anos",
                                        "Mais de 50 anos"))

tabela <- table(dados$Diabetes, dados$Faixa_Etaria)
tabela 

# Realizacao do Modelo
# H0: O desenvolvimento da diabetesnao DEPENDE da faixa etaria 
# H1: O desenvolvimento da diabetes DEPENDE da faixa etaria
quiquad <- chisq.test(tabela)
quiquad

# Analise das Frequencias esperadas

# Pressuposto: frequencias esperadas (se nao existisse associacao entre variaveis) > 5
quiquad$expected

# Analise dos residuos ajustados

# Residuo padronizado (SPSS) - residuos de Pearson
quiquad$residuals

# Residuo padronizado ajustado > 1,96 e < - 1.96
quiquad$stdres

# Calculo do ponte de corte para os residuos padronizados
### Sendo "l" o numero de linhas e "c" o numero de colunas
### Dividiremos o 0.05 pelo produto c*l (numero de celulas)

novoAlfa <- 0.05/(nrow(tabela) * ncol(tabela))
novoAlfa

# Calculo do ponto de corte, com base no bovo Alfa
## Divisao por 2 por ser bicaudal

qnorm(novoAlfa/2)

# Calculo de p para os residuos
round(2 * (1 - pnorm(abs(quiquad$stdres))), 6)

# Tamanho do Efeito - V de Cramer
# A interpretacao depende do grau de liberdade
# gl = (linhas - 1) * (colunas - 1)
library(rstatix)
cramer_v(tabela)

# Representacao grafica dos residuos
library(corrplot)

corrplot(quiquad$stdres, is.corr = FALSE,
         method = "color", tl.col = "black", tl.srt = 0)
