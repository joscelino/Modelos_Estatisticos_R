library(readr)
library(tibble)

### ANOVA MISTA
### PRESSUPOSTOS 
# 1. Ao menos 1 VI entre sujeitos e 1 VI intra-sujeitos
# 2. Normalidade dos dados
# 2. Homogeneidade de variancias
# 3. Ausencia de Outliers
# 4. Esfericidade

# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosAnovaMista.csv")
dados$ID <- as.factor(dados$ID)
dados$Gênero <- as.factor(dados$Gênero)
dados
summary(dados)
glimpse(dados)

# Reestruturando o banco de dados de formato "wide" para formato "long"
dadosLong <- reshape(dados, direction = "long", 
                  idvar = "ID",
                  varying = list(c("TG.1", "TG.2", "TG.3", "TG.4", "TG.5"),
                                 c("Peso.1","Peso.2","Peso.3","Peso.4","Peso.5")),
                  v.names = c("TG", "Peso"), timevar = "Tempo")
dadosLong <- as_tibble(dadosLong)
dadosLong

# Ordenando as colunas pelo sujeito experimental
library(reshape)
dadosLong <- sort_df(dadosLong, vars = "ID")
glimpse(dadosLong)

# Transformando variaveis em factor
dadosLong$Tempo <- as.factor(dadosLong$Tempo)
glimpse(dadosLong)

# Teste dos pressupostos
# Verificacao grafica de outliers por grupo
boxplot(TG ~ Gênero:Tempo, dadosLong)

# Verificacao estatistica de outliers
library(rstatix)
library(dplyr)
dadosLong %>% group_by(Gênero, Tempo) %>% identify_outliers(TG)

# Verificando a normalidade dos dados por grupo
dadosLong %>% group_by(Gênero, Tempo) %>% shapiro_test(TG)

# Verificando a homogeneidade de variancias
levene_test(dadosLong, TG ~ Gênero)

# Construcao do modelo ANOVA com medidas repetidas
library(ez)
# dv: variavel dependente
# wid: variavel de identificacao do seujeito
# within : variavel independente de medidas repetidas
# between = variavel independente entre sujeitos
# type: tipo da soma dos quadrados (default tipo II, padrao SPSS: tipo III)
modeloANOVA <- ezANOVA(data = dadosLong, 
                       dv =  .(TG),
                       wid = .(ID),
                       within = .(Tempo),
                       between = .(Gênero),
                       detailed = TRUE,
                       type = 3)

# Analisando os resultados do modelo
# options(scipen = 999)
modeloANOVA

# Visualizacao das diferencas
# Graficos de interacao
library(ggplot2)
library(ggiraph)

# Grafico com generos de cores diferentes
graficoA <- ggplot(dadosLong, aes(x = Tempo, y = TG, group = Gênero, 
                              color = Gênero)) +
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.8) +
  geom_point_interactive(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.4)
ggiraph(code = print(graficoA))

# Testes de post hoc
library(emmeans)
comparacaoTempo <- dadosLong %>% group_by(Gênero) %>%
  emmeans_test(TG ~ Tempo, p.adjust.method = "bonferroni")
summary(comparacaoTempo)
view(comparacaoTempo)

ComparacaoGenero <- dadosLong %>% group_by(Tempo) %>%
  emmeans_test(TG ~ Gênero, p.adjust.method = "bonferroni")
summary(ComparacaoGenero)
view(comparacaoTempo)

# Analise descritiva dos dados (Pacotes psych e rstatix)
library(psych)

dadosLong %>% group_by(Tempo, Gênero) %>%
  get_summary_stats(TG, type = "mean_sd")
