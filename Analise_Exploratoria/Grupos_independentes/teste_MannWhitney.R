library(pacman)
pacman::p_load(dplyr, readr, tibble, ggplot2, patchwork)

# TESTE NAO-PARAMETRICO MANN-WHITNEY
# Utilizado quando os dados sao nao normais
# Reporte: Mediana e intervalo inter-quartil

# PRESSUPOSTOS
# 1. Variavel dependente numerica ou variavel categorica ordinal
# 2. Variavel independente composta por dois grupos independentes

# * Sensivel a distribuicao entre as variaveis

# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/testeNaoParametrico1.csv")
dados <- as_tibble(dados)
dados$Sujeito <- as.factor(dados$Sujeito)
dados$Genero <- as.factor(dados$Genero)
dados$Escola <- as.factor(dados$Escola)
dados$Posicao_Sala <- as.factor(dados$Posicao_Sala)
glimpse(dados)

# Estimando o tamanho do efeito de um fator pela estatistica de Wilcoxon Rank-Sum test
# PS: Consultar Tabela normal padrao 
# O teste bicaudal eh o default. Para teste unicaudal necessario incluir:
# alternative = "greater" ou alternative = "less"
# H0: medianas dos grupos sao iguais | H1: medianas dos grupos nao sao iguais
wilcox.test(Nota_Biol ~ Posicao_Sala, data = dados, paired = FALSE)
wilcox.test(Nota_Fis ~ Posicao_Sala, data = dados, paired = FALSE)
wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados, paired = FALSE)

# Analise descritiva dos dados
library(rstatix)
dados %>% group_by(Posicao_Sala) %>% 
  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "median_iqr")

# Visualizacao da distribuicao
par(mfrow = c(1,2))
hist(dados$Nota_Biol[dados$Posicao_Sala == "Frente"],
     ylab = "Frequencia", xlab = "Nota", main = "Grupo Frente")
hist(dados$Nota_Biol[dados$Posicao_Sala == "Fundos"],
     ylab = "Frequencia", xlab = "Nota", main = "Grupo Fundos")

# Visualizacao de boxplots dos dados
boxPlotBio <- ggplot2::ggplot(data = dados, aes(x = Posicao_Sala, y = Nota_Biol)) +
  ggplot2::geom_boxplot(mapping = aes(colour = Escola), fill = "#F5FFFA", 
                        notch = FALSE, show.legend = TRUE, position = "dodge2") +
  ggplot2::scale_alpha_continuous() +
  ggplot2::labs(x = "Posicao na sala", y =  "Nota de Biologia") +
  ggplot2::theme_classic() 

boxPlotFis <- ggplot2::ggplot(data = dados, aes(x = Posicao_Sala, y = Nota_Fis)) +
  ggplot2::geom_boxplot(mapping = aes(colour = Escola), fill = "#F5FFFA", 
                        notch = FALSE, show.legend = TRUE, position = "dodge2") +
  ggplot2::scale_alpha_continuous() +
  ggplot2::labs(x = "Posicao na sala", y =  "Nota de Fisica") +
  ggplot2::theme_classic() 

boxPlotHis <- ggplot2::ggplot(data = dados, aes(x = Posicao_Sala, y = Nota_Hist)) +
  ggplot2::geom_boxplot(mapping = aes(colour = Escola), fill = "#F5FFFA", 
                        notch = FALSE, show.legend = TRUE, position = "dodge2") +
  ggplot2::scale_alpha_continuous() +
  ggplot2::labs(x = "Posicao na sala", y =  "Nota de Historia") +
  ggplot2::theme_classic() 

boxPlotBio + boxPlotFis + boxPlotHis
