library(benford.analysis)
library(BenfordTests)

# Carrega Banco de Dados
data("corporate.payment")
dim(corporate.payment)

head(corporate.payment)
tail(corporate.payment)

summary(corporate.payment)

# Cria objeto da Lei de Benford
teste = benford(corporate.payment$Amount, 2, sign = "positive", discrete = F, round = 2)

# Estatisticas dos dados analisados
edit(teste$bfd)

# Desvio absoluto
teste$MAD

# Valores suspeitos
suspectsTable(teste)

# Valores duplicados
duplicatesTable(teste)

# Graficos
plot(teste, multiple = F)

# Teste Kolmogorov-Smirnoff (KN) - BenfordTests
ks.benftest(corporate.payment$Amount, digits = 2)

# Analise Visual
signifd.analysis(corporate.payment$Amount, digits = 1, freq = T, ci_lines = c(0.05))

# Gerando dados comparativos 
benf = rbenf(10000)
signifd.analysis(benf, digits = 1, freq = T, ci_lines = c(0.05))


