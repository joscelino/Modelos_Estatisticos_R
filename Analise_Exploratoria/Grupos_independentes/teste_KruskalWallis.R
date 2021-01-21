library(pacman)
pacman::p_load(dplyr, readr, tibble, rstatix, ggplot2, car, plotly)

# TESTE DE KRUSKAL-WALLIS PARA MAIS DE DUAS AMOSTRAS INDEPENDENTES

# PRESSUPOSTOS
# 1. Variavel dependente numerica ou variavel categorica ordinal
# 2. Variavel independente composta grupos independentes

# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/BancodeDados5.csv") 
dados$Sujeito <- as.factor(dados$Sujeito)
dados$Grupo <- as.factor(dados$Grupo)
dados <- as.tibble(dados)
view(dados)
dados

# Realizacao do teste de Kruskal-Wallis
# H0: Medianas iguais | H1: Medianas diferentes
kruskal_test(dados, BC ~ Grupo)
kruskal_test(dados, Pressao ~ Grupo)

# testes de post-hoc
# Teste de Dunn com ajuste do valor p
dunn_test(BC ~ Grupo, data = dados, p.adjust.method = "bonferroni")
dunn_test(Pressao ~ Grupo, data = dados, p.adjust.method = "bonferroni")

# Analise descritiva dos dados
dados %>% group_by(Grupo) %>% 
  get_summary_stats(BC, Pressao, type = "median_iqr")

# Visualizacao dos dados
par(mfrow=c(1,2))
boxplot(BC ~ Grupo, data = dados)
boxplot(Pressao ~ Grupo, data = dados)

# Identificando outliers
dados %>% group_by(Grupo) %>% identify_outliers(BC)
dados %>% group_by(Grupo) %>% identify_outliers(Pressao)

# Analise de distribuicao
par(mfrow=c(1, 3))
hist(dados$BC[dados$Grupo == "Placebo"],
     ylab = "Frequencia", xlab = "bps", main = "Placebo")
hist(dados$BC[dados$Grupo == "Placebo"],
     ylab = "Frequencia", xlab = "AH Novo", main = "AH Novo")
hist(dados$BC[dados$Grupo == "AH Padrão"],
     ylab = "Frequencia", xlab = "bps", main = "AH Padrão")

# 2a opcao de histograma
histograma_BC <- ggplot(dados, aes(x = BC)) +
  geom_histogram(aes(color = Grupo, fill = Grupo), alpha = 0.3,
                 position = "identity", binwidth = 9)

histograma_Pressao <- ggplot2::ggplot(dados, aes(x = Pressao)) +
  ggplot2::geom_histogram(aes(color = Grupo, fill = Grupo), alpha = 0.3,
                 position = "identity", binwidth = 9) +
  ggplot2::theme_classic()

# Graficos interativos
ggplotly(histograma_BC)
ggplotly(histograma_Pressao)
