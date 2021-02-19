library(pacman)
pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2,
               plotly, readr, tibble)

## PRESSUPOSTOS
# 1. Ausencia de multicolinearidade
# 2. Ausencia de outliers
# 3. Relacao linear entre logito e cada variavel independente continua (teste de Box-Tidwell)

# Carregamento dos dados
# Diretorio raiz
ROOT_DIR <- getwd()

# Diretorio do arquivo
ARQ_DIR <- 
  'Dados'

# Nome do arquivo (com extensao)
ARQ_NOME <- 'dados_fumante_cancer.csv'


# Path do arquivo a ser carregado
arquivo_path <- 
  file.path(ROOT_DIR,
            ARQ_DIR,
            ARQ_NOME
  )

# Dados
dados <- read.csv2(arquivo_path, stringsAsFactors = TRUE)
dados <- tibble::as_tibble(dados)

# Analise das frequencias das categorias da variavel dependente
# Nao usar o modelo em bancos de dados muito desbalanceados
base::table(dados$Cancer)
base::summary(dados)

# Checagem das categorias de referencia ('Não' = categoria de referencia)
base::levels(dados$Cancer)
base::levels(dados$Hab_Fumar)

# Checagem dos pressupostos
# 1. Variavel dependente dicotomica (categorias mutuamente exclusivas)
# 2. Independencia das observacoes (sem medidas repetidas)

# Construcao do modelo
modelo <- glm(Cancer ~ Estresse + Hab_Fumar,
              family = binomial(link = 'logit'),
              data = dados)

# Ausencia de outliers / pontos de alavancagem
graphics::plot(modelo, which = 5)
base::summary(MASS::stdres(modelo))

# Ausencia de multicolinearidade : r > 0.9 (ou 0.8)
psych::pairs.panels(dados) 

# Multicolinearidade: VIF > 10
car::vif(modelo)

# Relacao linear entre cada variavel independente e logito da variavel dependente
# Interacao entre variavel independente continua e seu log nao significativa
# Box-Tidwell
# H0: interacao nao significativa | H1: interacao significativa
intlog <- dados$Estresse * base::log(dados$Estresse)

dados$intlog <- intlog

modint <- stats::glm(Cancer ~ Hab_Fumar + Estresse + intlog,
                     family = binomial(link = 'logit'),
                     data = dados)
base::summary(modint)

# Outra opcao
logito <- modelo$linear.predictors
dados$logito <- logito

# Analise da relacao linear
graficoRL <- ggplot2::ggplot(dados, aes(logito, Estresse)) +
    ggplot2::geom_point(size = 0.5, alpha = 0.5) +
    ggplot2::geom_smooth(method = "loess") +
    ggplot2::theme_classic()

plotly::ggplotly(graficoRL)

# Analise do modelo
# Overall effects
# H0: nao eh previsor estaticamente signficativo | H1: previsor estativamente significativo
car::Anova(modelo, type = "II", test = "Wald")

# Efeitos especificos
base::summary(modelo)

# Obtencao das razoes de chance com IC 95% (usando erro-padrao = SPSS)
razao_chances <- exp(base::cbind(OR = stats::coef(modelo), 
                                 stats::confint.default(modelo)))
# Chance de Estresse ter cancer 'sim'
razao_chances[[2]]
# # Chance de Fumar 'sim' ter cancer 'sim'
razao_chances[[3]]

# Criacao e analise de 2o modelo para efeito de comparacao
modelo2 <- glm(Cancer ~ Hab_Fumar,
               family = binomial(link = 'logit'),
               data = dados)
# Analise do modelo
# Overall effects
# H0: nao eh previsor estaticamente signficativo | H1: previsor estativamente significativo
car::Anova(modelo2, type = "II", test = "Wald")

# Efeitos especificos
base::summary(modelo2)

# Obtencao das razoes de chance com IC 95% (usando erro-padrao = SPSS)
razao_chances2 <- exp(base::cbind(OR = stats::coef(modelo2), 
                                 stats::confint.default(modelo2)))

# # Chance de Fumar 'sim' ter cancer 'sim'
razao_chances2[[2]]

# Avaliacao da qualidade e comparacao entre modelos
DescTools::PseudoR2(modelo, which = "Nagelkerke")
DescTools::PseudoR2(modelo2, which = "Nagelkerke")

# Comparacao de Modelos (AIC, BIC) # Quanto menor, melhor
AIC(modelo, modelo2)
BIC(modelo, modelo2)

# Qui-quadrado
stats::anova(modelo2, modelo, test = "Chisq")

# Tabela de Classificacao (Matriz de confusao)
QuantPsyc::ClassLog(modelo, dados$Cancer)
QuantPsyc::ClassLog(modelo2, dados$Cancer)


############## Modificando as categorias de referencia #########################
base::levels(dados$Hab_Fumar)
dados$Hab_Fumar <- stats::relevel(dados$Hab_Fumar, ref = "Sim")

### ATENCAO RODAR O MODELO NOVAMENTE
