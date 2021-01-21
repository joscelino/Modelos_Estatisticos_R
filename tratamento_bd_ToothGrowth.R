#### Comparacao de dois grupos independentes

# Banco de dados
dados <- ToothGrowth
dados

# Acessando as variaveis
dados$len
dados$supp
dados$dose <- factor(dados$dose) # Tranformando em variaveis categoricas

names(dados) <- c("comp","metodo","dose")
dados
