library(readr)
library(tibble)

## Usando ANOVA para avaliar variaveis intra-sujeitos (a variavel mais comum eh o tempo)

# Importacao dos dados
dados <- read_csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosAnovaMedidasRepetidas.csv")
as.factor(dados$ID)
dados

# Alterando o formato do banco de dados de "wide" para "long" 
library(reshape2)
dadosLong <- melt(dados, id.vars =  "ID", 
                  measure = c("Professor1", "Professor2", "Professor3", "Professor4"))
dadosLong <- as.tibble(dadosLong)
dadosLong

# Renomeando as variaveis
colnames(dadosLong) <- c("ID", "Professor", "Nota")

# Ordenando as colunas pelo sujeito experimental
dadosLong <- sort_df(dadosLong, vars = "ID")

# Transformando a variavel ID para factor
dadosLong$ID <- as.factor(dadosLong$ID)
glimpse(dadosLong)

# Checando os pressupostos de normalidade
library(rstatix)
library(dplyr)

# Verificando a presenca de OutLiers por grupo
dadosLong %>% group_by(Professor) %>% identify_outliers(Nota)

# Verificando a normalidade por grupo
dadosLong %>% group_by(Professor) %>% shapiro_test(Nota)

# Modelo de ANOVA para medidas repetidas
library(ez)

# Construindo o modelo
# dv = Variavel dependente
# wid = identificador
# within = variavel intrasujeito
# type = tipo de soma dos quadrados
modeloANOVA <- ezANOVA(data = dadosLong, dv = Nota, wid = ID, 
                        within = Professor, detailed = TRUE, type = 3)

# Analisando os resultados
# Libera o teste de esferecidade de Mauchly e as correcoes de greenhouse-Geisser 
# e Huynh-Feldt
modeloANOVA

# Testes Post-Hoc
pairwise.t.test(dadosLong$Nota, dadosLong$Professor, paired = TRUE,
                p.adjust.method = "bonferroni")

# Analise descritiva dos dados (pacote rstatix)
dadosLong %>% group_by(Professor) %>% get_summary_stats(Nota, type = "mean_sd")

# Boxplot

library(ggplot2)
# Opacao 1
boxPlot <- dadosLong %>% ggplot(aes(reorder(Professor, -Nota), Nota)) + 
  geom_boxplot() + xlab("Professor") + ylab("Notas") 
boxPlot
# Opcao 2
boxplot(Nota ~ Professor, data = dadosLong, ylab = "Notas", xlab = "Professor",
        main = "Analise de Notas de Professores")
