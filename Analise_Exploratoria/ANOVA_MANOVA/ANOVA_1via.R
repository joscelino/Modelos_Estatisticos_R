library(readr)
library(psych)

# Importacao dos dados
dados <- read_csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosAnova.csv")
dados

# Verificando a Normalidade dos dados
## Shapiro_Wilk por grupo
library(RVAideMemoire)
byf.shapiro(BC ~ Grupo, dados)
byf.shapiro(Pressao ~ Grupo, dados)

# Verificando a Homogeneidade das Variancas
# Por padrao o pacote car usa o center como ma mediana
library(car)
leveneTest(BC ~ Grupo, dados, center = mean)
leveneTest(Pressao ~ Grupo, dados, center = mean)

# Verificando a presenca de Outliers (por grupo)
library(rstatix)
library(dplyr)
dados %>% group_by(Grupo) %>% identify_outliers(BC)
dados %>% group_by(Grupo) %>% identify_outliers(Pressao)

# Realizacao da ANOVA
# H0: Todos os grupos apresentam medias iguais, H1: Ha ao menos uma diferenca 
# ANOVA Batimentos Cardiacos
anova_BC <- aov(BC ~ Grupo, dados)
summary(anova_BC)

# ANOVA Batimentos Pressao
anova_Pressao <- aov(Pressao ~ Grupo, dados)
summary(anova_Pressao)

# Analise Post Hoc
# Post-hocs permitidos: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"

analisePostHoc <- function(x) {
  
  library(DescTools)
  
  # Uso do Duncan
  a1 <- PostHocTest(x, method = "duncan", conf.level = 0.95)
  
  # Uso do TukeyHSD
  a2 <- PostHocTest(x, method = "hsd", conf.level = 0.95)

  # Uso do Bonferroni
  a3 <- PostHocTest(x, method = "bonf", conf.level = 0.95)

  # Matriz de Resultados
  resultados <- cbind(duncan = a1$Grupo[,"pval"],
                      hsd = a2$Grupo[,"pval"],
                      bonf = a3$Grupo[,"pval"])

  print("Resultados de p-values consolidados")

  return(resultados)
}

analisePostHoc(anova_BC)
analisePostHoc(anova_Pressao)

# Analise Descritiva dos dados
describeBy(dados$BC, group = dados$Grupo)
describeBy(dados$Pressao, group = dados$Grupo)

