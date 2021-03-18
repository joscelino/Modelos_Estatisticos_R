################ Teste T para amostras independentes ###########################
################## Calculo do tamanho do efeito ################################
# Carregando os pacotes
library(dplyr)
library(car)
library(rstatix)
library(effectsize)

# Carregando os dados
dados <- read.csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/dadosDuasAmostrasIndependentes.csv",
                   stringsAsFactors = TRUE)
dados <- as_tibble(dados)
dados$Sujeito <- as.factor(dados$Sujeito)
glimpse(dados)

summary(dados)

# Verificacao da normalidade dos dados
# Shapiro por grupo (pacotes dplyr e rstatix)

dados %>% dplyr::group_by(Posicao_Sala) %>% 
  rstatix::shapiro_test(Nota_Biol)

dados %>% dplyr::group_by(Posicao_Sala) %>% 
  rstatix::shapiro_test(Nota_Fis)

dados %>% dplyr::group_by(Posicao_Sala) %>% 
  rstatix::shapiro_test(Nota_Hist)

# Verificacao da Homogeneidade de Variancias
car::leveneTest(Nota_Biol ~ Posicao_Sala, dados, center = mean)
car::leveneTest(Nota_Fis ~ Posicao_Sala, dados, center = mean)
car::leveneTest(Nota_Hist ~ Posicao_Sala, dados, center = mean)

# Realizacao do test t para amostras independentes
rstatix::t_test(dados, Nota_Biol ~ Posicao_Sala, var.equal = TRUE)

rstatix::t_test(dados, Nota_Fis ~ Posicao_Sala, var.equal = FALSE)

rstatix::t_test(dados, Nota_Hist ~ Posicao_Sala, var.equal =FALSE)


############### CALCULO DO TAMANHO DO EFEITO ###################################
# d de Cohen (variancias homogenias, grupos grandes e/ou com tamanhos muito semelhantes)
summary(dados$Posicao_Sala) # Tamanho dos grupos

d <- effectsize::cohens_d(Nota_Biol ~ Posicao_Sala, data = dados)

effectsize::interpret_d(d[[1]], rules = "cohen1988")
effectsize::interpret_d(d[[1]], rules = "sawilowsky2009")

effectsize::d_to_common_language(d[[1]])
# Interpretacao: corresponde a probabilidade de uma pessoa retirada ao acaso de um grupo
# apresentar um valor superior dao de uma pessoa retirada ao acaso de outro grupo
# chamado tambem de probabilidade de superioridade

# g de Hedges (grupos pequenos - amostras < 20 - e de tamanhos diferentes)
g <- effectsize::hedges_g(Nota_Biol ~ Posicao_Sala, data = dados)


effectsize::interpret_g(g[[1]], rules = "cohen1988")
effectsize::interpret_g(g[[1]], rules = "sawilowsky2009")

# delta de Glass (variancias heterogenias)
deltaFis <- effectsize::glass_delta(Nota_Fis ~ Posicao_Sala, data = dados)

effectsize::interpret_delta(deltaFis[[1]], rules = "cohen1988")
effectsize::interpret_delta(deltaFis[[1]], rules = "sawilowsky2009")

deltaHist <- effectsize::glass_delta(Nota_Hist ~ Posicao_Sala, data = dados)

effectsize::interpret_delta(deltaHist[[1]], rules = "cohen1988")
effectsize::interpret_delta(deltaHist[[1]], rules = "sawilowsky2009")

base::levels(dados$Posicao_Sala)
dados$Posicao_Sala <- stats::relevel(dados$Posicao_Sala, ref = "Fundos")
