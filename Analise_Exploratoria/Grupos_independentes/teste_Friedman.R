library(pacman)
pacman::p_load(dplyr, readr, tibble, rstatix, ggplot2, plotly, reshape,
               PMCMRplus, patchwork)

# TESTE DE FRIEDMAN
# PRESSUPOSTOS
# 1. Variavel dependente numerica ou ordinaria
# 2. Variavel independente formada por grupos dependentes (variaveis intrasujeito)

# Importacao dos dados
dados <- readr::read_csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/BancodeDados7.csv") 
dados$ID <- as.factor(dados$ID)
dados

# Alterando o formado do banco de dados de "wide" para "long" 
# Reestruturando o banco de dados
dadosLong <- reshape2::melt(dados,
                            id = "ID",
                            measure.vars = c("Professor1","Professor2",
                                             "Professor3","Professor4"))
dadosLong <- tibble::as_tibble(dadosLong)


# Renomeando as colunas do data frame no formato long
colnames(dadosLong) = c("ID", "Professor", "Nota")
dadosLong

# Ordenando as colunas pelo sujeito experimental
dadosLong <- reshape::sort_df(dadosLong, vars = "ID")
dadosLong

# Realizacao do teste de Friedman
# H0: Medianas/Distribuicao sao iguais | H1: Medianas/Distribuicao nao sao iguais
rstatix::friedman_test(dadosLong,Nota ~ Professor | ID)

# Testes de Post hoc
# Opcao 1: Wilcoxon com correcao de Bonferroni
dadosLong %>% rstatix::wilcox_test(Nota ~ Professor, paired = TRUE,
                                   p.adjust.method = "bonf")

# Opcao 2: Dunn-Bonferroni (SPSS)
PMCMRplus::frdAllPairsSiegelTest(dadosLong$Nota, dadosLong$Professor,
                                 dadosLong$ID, p.adjust = "bonferroni")

# Outros
PMCMRplus::frdAllPairsNemenyiTest(dadosLong$Nota, dadosLong$Professor,
                                  dadosLong$ID, p.adjust = "bonferroni")

PMCMRplus::frdAllPairsConoverTest(dadosLong$Nota, dadosLong$Professor,
                                  dadosLong$ID, p.adjust = "bonferroni")

# Analise descritiva dos dados
dadosLong %>% group_by(Professor) %>%
  get_summary_stats(Nota, type = "median_iqr")

# Visualizacao dos dados
boxPlot <- ggplot2::ggplot(data = dadosLong, aes(x = Professor, y = Nota)) +
  ggplot2::geom_boxplot(mapping = aes(colour = Professor), fill = "#F5FFFA", 
                        notch = TRUE, show.legend = TRUE, position = "dodge2") +
  ggplot2::scale_alpha_continuous() +
  ggplot2::labs(x = "Professor", y =  "Nota") +
  ggplot2::theme_classic() 


# Identificando outliers
dadosLong %>% group_by(Professor) %>% identify_outliers(Nota)

# Analise das distribuicoes
par(mfrow=c(2, 2))
hist(dadosLong$Nota[dadosLong$Professor == "Professor1"],
     ylab = "Frequencia", xlab = "Notas", main = "Professor 1")
hist(dadosLong$Nota[dadosLong$Professor == "Professor2"],
     ylab = "Frequencia", xlab = "Notas", main = "Professor 2")
hist(dadosLong$Nota[dadosLong$Professor == "Professor3"],
     ylab = "Frequencia", xlab = "Notas", main = "Professor 3")
hist(dadosLong$Nota[dadosLong$Professor == "Professor4"],
     ylab = "Frequencia", xlab = "Notas", main = "Professor 4")
par(mfrow=c(1, 1))
# Histograma com todos os grupos, separados por cor
histograma <- ggplot2::ggplot(dadosLong, aes(x = Nota)) + 
  ggplot2::geom_histogram(aes(color = Professor, fill = Professor),
                          alpha = 0.3, position = "identity", binwidth = 1) +
  ggplot2::labs(x = "Professor", y = "Totais", 
                main = "Distribuicao de Notas") +
  ggplot2::theme_classic()
  
boxPlot + histograma
