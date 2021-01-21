library(readr)
library(tibble)

# Funcao de Teste de Normalidade

testesNormalidade <- function(x) {
  
  " Verificando a suposicao de normalidade atraves de testes estatisticos "
  " se p-valor < 0.05, rejeita-se a hipotese nula (no caso, normalidade) "
  " Na comparacao de 2 grupos independentes, a suposicao de normalidade somente
  sera aceita quando houver normalidade em ambos os grupos (dados serao parametricos)"
  
  library(nortest)
  
  # Kolmogorov-Smirnov
  t1 <- ks.test(x, "pnorm")
  # Teste de Shapiro-Wilk
  t2 <- shapiro.test(x)
  # Teste de Shapiro-Francia
  t3 <- sf.test(x)
  # Teste Anderson-Darling
  t4 <- ad.test(x)
  # Teste Lilliefors
  t5 <- lillie.test(x)
  # Teste Cramer-Von Mises
  t6 <- cvm.test(x)
  # Teste Qui-quadrado Pearson
  t7 <- pearson.test(x)
  
  # Teste JarqueBera 
  library(tsoutliers)
  t8 <- JarqueBera.test(x)
  
  testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method, t6$method, 
              t7$method, t8[[1]]$method)
  estatisticas <- c(t1$statistic, t2$statistic, t3$statistic, t4$statistic, 
                    t5$statistic, t6$statistic, t7$statistic, t8[[1]]$statistic)
  estatisticas <- as.numeric(estatisticas)
  pvalues <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value, 
               t6$p.value, t7$p.value, t8[[1]]$p.value)
  pvalues <- as.numeric(pvalues)
  
  # Matriz de resultados
  estatisticas <- round(estatisticas,4)
  pvalues <- round(pvalues,4)
  
  resultados <- data.frame(testes, estatisticas, pvalues)
  
  # Densidades
  library(ggpubr)
  
  a <- ggdensity(x)
  b <- ggqqplot(x)
  print(a)
  print(b)
  
  return(resultados)
}

## Pressupostos da ANCOVA
# 1. Independencia entre a covariavel e a VI
# 2. Relacao linear entre a VD e a covariavel
# 3. Homogeneidade dos parametros de regressao
# 4. Normalidade dos residuos
# 5. Homocedasticidade e ausencia de outliers

# Importacao dos dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosAncova.csv")
dados <- as_tibble(dados)
dados$Sujeito <- as.factor(dados$Sujeito)
dados$Gênero <- as.factor(dados$Gênero)
dados$Grau_Instrução <- as.factor(dados$Grau_Instrução)
dados
glimpse(dados)

# VD: Salario
# VI: Grau de Instrucao
# Covariavel: Idade

# Verificando se ha efeito da VI sobre a covariavel (cov ~ VI)
# Se rompido: nao ha outro modelo - problema de delineamento
# H0: nao ha efeito - H1: ha efeito
mod_cov <- aov(Idade ~ Grau_Instrução, data = dados)
summary(mod_cov)

# Verificando se a relacao entre a covariavel e a VD eh linear(VD ~ cov)
library(ggplot2)
library(plotly)

a <- ggplot(data = dados, aes(x = Idade, y = Salário, group = Grau_Instrução,
                         color = Grau_Instrução)) + 
  geom_point(size = 2) + xlab("Idade") + ylab("Salario") +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) + theme_grey()
ggplotly(a)

# Verificando se o efeito da covariavel eh o mesmo para todos os niveis da VI (VD ~ VI*cov)
# Pressuposto: "homogeneidade dos parametros de regressao"
# Compara as inclinacoes das retas para cada grupo de VI
library(rstatix)
modelo_int <- aov(Salário ~ Grau_Instrução*Idade, data = dados)

# H0: existe homogeneidade dos parametros de regressao - H1: nao existe homogeneidade
Anova(modelo_int, type = "III")

# Verificando a homogeneidade de variaveis (VD ~ VI)
# Se rompido: versao robusta da ANCOVA
# H0: Existe homogeneidade - H1: nao existe homogeneidade
levene_test(Salário ~ Grau_Instrução, center = mean, data = dados)

# Ajuste do modelo de ANCOVA (VD ~ cov + VI)
# Se os resultados forem avaliados pelo tipo I da soma dos quadrados
# eh obrigatorio que a covariavel seja inserida no modelo

options(contrasts = c("contr.sum", "contr.poly"))

modelo_ANCOVA <- aov(Salário ~ Grau_Instrução + Idade, data = dados)

# H0: nao ha efeito - H1: Ha efeito
Anova(modelo_ANCOVA, type = "III")

# Verificando a normalidade dos residuos
testesNormalidade(modelo_ANCOVA$residuals)

# Verificando se homocedasticidade e outliers
boxplot(modelo_ANCOVA$residuals, main = "Boxplot dos residuos do modelo ANOVA")
par(mfrow =  c(1,2))
plot(modelo_ANCOVA, wich = c(1,3))

par(mfrow =  c(1,1))

# H0: Existe homogeneidade - H1: nao existe homogeneidade
levene_test(modelo_ANCOVA$residuals ~ dados$Grau_Instrução, 
            data = dados, center = mean)

# Realizacao das comparacoes
# Pelo pacote multcomp
library(multcomp)

posthoc <- glht(modelo_ANCOVA, linfct = mcp(Grau_Instrução = "Tukey"))
summary(posthoc)

# Pelo pacote rstatix
comparacoes <- dados %>% emmeans_test(Salário ~ Grau_Instrução, 
                                      covariate = Idade, 
                                      p.adjust.method = "bonferroni")

# Obtencao das medias ajustadas
# Opcao 1
mediasAjustadas <- (emmeans(modelo_ANCOVA, ~ Idade:Grau_Instrução))
mediasAjustadas

# Opcao 2
get_emmeans(comparacoes)

# Medias reais
dados %>% group_by(Grau_Instrução) %>% get_summary_stats(Salário, type = "mean_sd")
