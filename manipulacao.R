# Pacotes -------------------------------------------------- 

# Manipulação de dados
#install.packages("tidyverse")
library(tidyverse)

#install.packages("tidymodels")
#pacote completo para modelagem
library(tidymodels)

#install.packages("multiclassPairs")
library(multiclassPairs)
# precedimento de dummies
library(fastDummies)

# Gráficos
library(ggplot2)

library(plotly)


###### Algoritmos ###########


# Manipulação de machine learning
library(themis)

###### Modelos ###########



# Métodos baseado em arvore de decisão

#install.packages("rpart", dependencies=T)
library(rpart)
#install.packages("randomForest")
library(randomForest)

###########################################################
# Wrangling -----------------------------------------------
###########################################################

dados <- read.csv("INFLUD_21-18-07-2022.csv", sep = ";", na.strings = "", stringsAsFactors = T)
summary(dados)

# ANÁLISE DOS DADOS 

# Selecionando as variáveis para o estudo
dataset <- select(dados,
                  CS_SEXO, NU_IDADE_N,FEBRE, 
                  TOSSE, GARGANTA, DISPNEIA,
                  DESC_RESP, SATURACAO, DIARREIA,
                  VOMITO, DOR_ABD, FADIGA,
                  PERD_OLFT, PERD_PALA,CARDIOPATI,
                  HEMATOLOGI, HEPATICA,VACINA_COV,
                  VACINA,ANTIVIRAL,HOSPITAL,
                  CLASSI_FIN, EVOLUCAO) # especificando

names(dataset)


# Verificando descritivas dos dados
summary(dataset)
# Obs.: Muitos NA's que acredito ser inviável corrigilos, sendo o ideal apagar.
# Obs2.: Existem valores "I" que representa sexo Ignorado.
# Obs3.: Variável "NU_IDADE_N" tem valores negativos, acredito que é melhor apagar como Outliers.

# Verificando as variáveis que têm valores faltantes
colSums(is.na(dataset))

# Apagando NA's
dataset <- na.omit(dataset)

# DADOS CATEGÓRICOS

# CS_SEXO
summary(dataset$CS_SEXO)
counts_sexo <- table(dataset$CS_SEXO)
barplot(counts_sexo,
        main = "Gêneros",
        xlab = "Gêneros",
        ylab = "Fequência",
        col = "purple" )
# Obs.: Existem valores "I" que representa sexo Ignorado.
# Obs2.: Colocar os valores "I"(ignorado) para a maior quantidade (MODA)

# FEBRE
summary(dataset$FEBRE)
counts_febre <- table(dataset$FEBRE)
barplot(counts_febre) 
counts_febre # 9 trata-se de valores Ignorados

# TOSSE
summary(dataset$TOSSE)
counts_tosse <- table(dataset$TOSSE)
barplot(counts_tosse)
counts_tosse # 9 Ignorado

# CARGANTA
summary(dataset$GARGANTA)
counts_garganta <- table(dataset$GARGANTA)
barplot(counts_garganta)
counts_garganta # 9 Ignorado 

# DISPNEIA
summary(dataset$DISPNEIA)
counts_disp <- table(dataset$DISPNEIA)
barplot(counts_disp)
counts_disp  # 9 Ignorado 

# DESC_RESP
summary(dataset$DESC_RESP)
counts_resp <- table(dataset$DESC_RESP)
barplot(counts_resp)
counts_resp # 9 Ignorado

# SATURAÇÃO
summary(dataset$SATURACAO)
counts_satur <- table(dataset$SATURACAO)
barplot(counts_satur)
counts_satur # 9 Ignorado

# DIARREIA
summary(dataset$DIARREIA)
counts_diarreia <- table(dataset$DIARREIA)
barplot(counts_diarreia)
counts_diarreia # 9 Ignorado

# VOMITO
summary(dataset$VOMITO)
counts_vomito <- table(dataset$VOMITO)
barplot(counts_vomito)
counts_vomito # 9 Ignorado 

# DOR_ABD
summary(dataset$DOR_ABD)
counts_abd <- table(dataset$DOR_ABD)
barplot(counts_abd)
counts_abd # 9 Ignorado

# FADIGA
summary(dataset$FADIGA)
counts_fadiga <- table(dataset$FADIGA)
barplot(counts_fadiga)
counts_fadiga # 9 Ignorado

# PERD_OLFT
summary(dataset$PERD_OLFT)
counts_olft <- table(dataset$PERD_OLFT)
barplot(counts_olft)
counts_olft # Ignorado

# PERD_PALA
summary(dataset$PERD_PALA)
counts_pala <- table(dataset$PERD_PALA)
barplot(counts_pala)
counts_pala # 9 Ignorado

# CARDIOPATI
summary(dataset$CARDIOPATI)
counts_cardi <- table(dataset$CARDIOPATI)
barplot(counts_cardi)
counts_cardi # 9 Ignorado

# HEMATOLOGI
summary(dataset$HEMATOLOGI)
counts_hemato <- table(dataset$HEMATOLOGI)
barplot(counts_hemato)
counts_hemato # 9 Ignorado

# HEPATICA
summary(dataset$HEPATICA)
counts_hepati <- table(dataset$HEPATICA)
barplot(counts_hepati)
counts_hepati # Ignorado

# VACINA_COV
summary(dataset$VACINA_COV)
counts_vcov <- table(dataset$VACINA_COV)
barplot(counts_vcov)
counts_vcov # Igorado

# VACINA
summary(dataset$VACINA)
counts_vacina <- table(dataset$VACINA)
barplot(counts_vacina)
counts_vacina # Ignorado
# Obs.: Se recebeu vacina da gripe na última campanha 

# ANTIVIRAL
summary(dataset$ANTIVIRAL)
counts_antiviral <- table(dataset$ANTIVIRAL)
barplot(counts_antiviral)
counts_antiviral # 9 Ignorado 

# HOSPITAL
summary(dataset$HOSPITAL)
counts_hosp <- table(dataset$HOSPITAL)
barplot(counts_hosp)
counts_hosp # Ignorado 
# Obs.: O paciente foi internado?

# CLASSI_FIN
summary(dataset$CLASSI_FIN)
counts_classfinal <- table(dataset$CLASSI_FIN)
barplot(counts_classfinal)
counts_classfinal 
# Obs.: Diagnóstico final do caso 
#1-SRAG por influenza
#2-SRAG por outro vírus respiratório
#3-SRAG por outro agente etiológico, qual: X
#4-SRAG não especificado
#5-SRAG por covid-19
# Obs.2: Deixar todos os SRAGs

# EVOLUCAO
summary(dataset$EVOLUCAO)
counts_evolucao <- table(dataset$EVOLUCAO)
barplot(counts_evolucao)
counts_evolucao
#Obs.:
#1-Cura
#2-Óbito
#3-Óbito por outras causas
#9-Ignorado
#Obs.2: Apagar o 3-Óbito por outras causas e Colocar o 9 na MODA estatística

# DADOS NUMÉRICOS

# Variável NU_IDADE_N
summary(dataset$NU_IDADE_N) # anomalias de idades negativas e idades acima de 120 anos de idade
hist(dataset$NU_IDADE_N,
     main = "Idades",
     xlab = "Idades",
     ylab = "Frequência",
     col = "purple")
# Gerando boxplot para ver outliers
bp <- boxplot(dataset$NU_IDADE_N,
              col = "purple")
# Analisando os dados do boxplot
bp
# Obs.: Tirar valores a baixo de 0 que são considerados anormalidade e acima de 111 são considerados outliers
# Obs.2: Passar as idades para categorias de criança, jovem, adulto e idoso.

################################################
# PRÉ-PROCESSAMENTO DOS DADOS-------------------
################################################

# CS_SEXO
unique(dataset$CS_SEXO)
summary(dataset$CS_SEXO)
# Colocando sexo Ignorado para a maior quantidade de pessoas(MODA)
dataset <- mutate(dataset,
                  CS_SEXO = replace(CS_SEXO, CS_SEXO == "I", "M"))
                  
# Tirando o valor "I" da variável 
dataset$CS_SEXO <- factor(dataset$CS_SEXO)

                
str(dataset)

# Revendo a variável
unique(dataset$CS_SEXO)
# Fazendo table para novo barplot 
counts_sexo_2 <- table(dataset$CS_SEXO)
barplot(counts_sexo_2,
        main = "Gênero",
        xlab = "Gênero",
        ylab = "Frequência" )

# NU_IDADE_N
summary(dataset$NU_IDADE_N)
# substituindo valores com anomalias e Outliers por mediana 
dataset[dataset$NU_IDADE_N < 0 | dataset$NU_IDADE_N > 111,]$NU_IDADE_N <- median(dataset$NU_IDADE_N)
# buscando dados com essas características 
dataset[dataset$NU_IDADE_N < 0 | dataset$NU_IDADE_N > 111,]
# Revendo
summary(dataset$NU_IDADE_N)
table(dataset$NU_IDADE_N)

# Transformando a variável 'idade' em categorias
dataset$NU_IDADE_N <- cut(dataset$NU_IDADE_N,
                          breaks = c(0, 12, 29, 59, Inf),
                          labels = c("INFANTIL", "JOVEM", "ADULTO", "IDOSO"),
                          include.lowest = TRUE)

str(dataset)

# FEBRE
counts_febre
barplot(counts_febre)
# Preenchendo com MODA
dataset[dataset$FEBRE %in% 9,]$FEBRE = 1
# fazendo table para novo barplot 
counts_febre_2 <- table(dataset$FEBRE)
# Novo barplot
barplot(counts_febre_2)
counts_febre_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não para febre e 1 = Febre
dataset <- mutate(dataset,
                  FEBRE = replace(FEBRE, 
                                  FEBRE == 2, 0 ))

# fazendo table para novo barplot final
counts_febre_3 <- table(dataset$FEBRE)
barplot(counts_febre_3)
counts_febre_3

# TOSSE
counts_tosse
barplot(counts_tosse)
# Preenchendo com MODA
dataset[dataset$TOSSE %in% 9,]$TOSSE = 1
# fazendo table para novo barplot
counts_tosse_2 <- table(dataset$TOSSE)
# Novo barplot
barplot(counts_tosse_2)
counts_tosse_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não para tosse e 1 = Tosse
dataset <- mutate(dataset,
                  TOSSE = replace(TOSSE, 
                                  TOSSE == 2, 0 ))

# fazendo table para novo barplot final
counts_tosse_3 <- table(dataset$TOSSE)
barplot(counts_tosse_3)
counts_tosse_3

# GARGANTA
counts_garganta
barplot(counts_garganta)
# Preenchendo com MODA
dataset[dataset$GARGANTA %in% 9,]$GARGANTA = 2
# fazendo table para novo barplot
counts_garganta_2 <- table(dataset$GARGANTA)
# Novo barplot
barplot(counts_garganta_2)
counts_garganta_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não para dor de garganta e 1 = Dor de garganta
dataset <- mutate(dataset,
                  GARGANTA = replace(GARGANTA,
                                     GARGANTA == 2, 0 ))

# fazendo table para novo barplot final
counts_garganta_3 <- table(dataset$GARGANTA)
barplot(counts_garganta_3)
counts_garganta_3

# DISPNEIA
counts_disp
barplot(counts_disp)
# Preenchendo com MODA
dataset[dataset$DISPNEIA %in% 9,]$DISPNEIA = 1
# fazendo table para novo barplot
counts_disp_2 <- table(dataset$DISPNEIA)
# Novo barplot
barplot(counts_disp_2)
counts_disp_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não para dispneia e 1 = Dispneia
dataset <- mutate(dataset,
                  DISPNEIA = replace(DISPNEIA, 
                                     DISPNEIA == 2, 0 ))

# fazendo table para novo barplot final
counts_disp_3 <- table(dataset$DISPNEIA)
barplot(counts_disp_3)
counts_disp_3

# DESC_RESP
counts_resp
barplot(counts_resp)
# Preenchendo com MODA
dataset[dataset$DESC_RESP %in% 9,]$DESC_RESP = 1
# fazendo table para novo barplot
counts_resp_2 <- table(dataset$DESC_RESP)
# Novo barplot
barplot(counts_resp_2)
counts_resp_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não ao desconforto respiratório e 1 = Desconforto respiratório
dataset <- mutate(dataset,
                  DESC_RESP = replace(DESC_RESP, 
                                      DESC_RESP == 2, 0 ))

# fazendo table para novo barplot final
counts_resp_3 <- table(dataset$DESC_RESP)
barplot(counts_resp_3)
counts_resp_3

# SATURAÇÃO
counts_satur
barplot(counts_satur)
# Preenchendo com MODA
dataset[dataset$SATURACAO %in% 9,]$SATURACAO = 1
# fazendo table para novo barplot
counts_satur_2 <- table(dataset$SATURACAO)
# Novo barplot
barplot(counts_satur_2)
counts_satur_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Paciente NÃO apresentou saturação O2 < 95%? e 1 = Paciente apresentou saturação O2 < 95%?
dataset <- mutate(dataset,
                  SATURACAO = replace(SATURACAO, 
                                      SATURACAO == 2, 0 ))

# fazendo table para novo barplot final
counts_satur_3 <- table(dataset$SATURACAO)
barplot(counts_satur_3)
counts_satur_3

# DIARREIA
counts_diarreia
barplot(counts_diarreia)
# Preenchendo com MODA
dataset[dataset$DIARREIA %in% 9,]$DIARREIA = 2
# fazendo table para novo barplot
counts_diarreia_2 <- table(dataset$DIARREIA)
barplot(counts_diarreia_2)
counts_diarreia_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não tem diarreia e 1 = Diarreia
dataset <- mutate(dataset,
                  DIARREIA = replace(DIARREIA, 
                                     DIARREIA == 2, 0 ))

# fazendo table para novo barplot final
counts_diarreia_3 <- table(dataset$DIARREIA)
barplot(counts_diarreia_3)
counts_diarreia_3

# VOMITO
counts_vomito
barplot(counts_vomito)
# Preenchendo com MODA
dataset[dataset$VOMITO %in% 9,]$VOMITO = 2
# fazendo table para novo barplot
counts_vomito_2 <- table(dataset$VOMITO)
barplot(counts_vomito_2)
counts_vomito_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não apresentou Vomito e 1 = Vomito
dataset <- mutate(dataset,
                  VOMITO = replace(VOMITO, 
                                   VOMITO == 2, 0 ))

# fazendo table para novo barplot final
counts_vomito_3 <- table(dataset$VOMITO)
barplot(counts_vomito_3)
counts_vomito_3

# DOR_ABD
counts_abd
barplot(counts_abd)
# Preenchendo com MODA
dataset[dataset$DOR_ABD %in% 9,]$DOR_ABD = 2
# fazendo table para novo barplot
counts_abd_2 <- table(dataset$DOR_ABD)
barplot(counts_abd_2)
counts_abd_2 

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não apresentou Dor Abdominal e 1 = Dor Abdominal
dataset <- mutate(dataset,
                  DOR_ABD = replace(DOR_ABD, 
                                    DOR_ABD == 2, 0 ))

# fazendo table para novo barplot final
counts_abd_3 <- table(dataset$DOR_ABD)
barplot(counts_abd_3)
counts_abd_3

# FADIGA
counts_fadiga
barplot(counts_fadiga)
# Preenchendo com MODA
dataset[dataset$FADIGA %in% 9,]$FADIGA = 2
# fazendo table para novo barplot
counts_fadiga_2 <- table(dataset$FADIGA)
barplot(counts_fadiga_2)
counts_fadiga_2 

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não apresentou Fadiga e 1 = Fadiga
dataset <- mutate(dataset,
                  FADIGA = replace(FADIGA, 
                                   FADIGA == 2, 0 ))

# fazendo table para novo barplot final
counts_fadiga_3 <- table(dataset$FADIGA)
barplot(counts_fadiga_3)
counts_fadiga_3

# PERD_OLFT
counts_olft
barplot(counts_olft)
# Preenchendo com MODA
dataset[dataset$PERD_OLFT %in% 9,]$PERD_OLFT = 2
# fazendo table para novo barplot
counts_olft_2 <- table(dataset$PERD_OLFT)
barplot(counts_olft_2)
counts_olft_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não teve Perda de Olfato e 1 = Perda de olfato
dataset <- mutate(dataset,
                  PERD_OLFT = replace(PERD_OLFT, 
                                      PERD_OLFT == 2, 0 ))

# fazendo table para novo barplot final
counts_olft_3 <- table(dataset$PERD_OLFT)
barplot(counts_olft_3)
counts_olft_3

# PERD_PALA
counts_pala
barplot(counts_pala)
# Preenchendo com MODA
dataset[dataset$PERD_PALA %in% 9,]$PERD_PALA = 2
# fazendo table para novo barplot
counts_pala_2 <- table(dataset$PERD_PALA)
barplot(counts_pala_2)
counts_pala_2 

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não para perda de paladar e 1 = Perda de paladar
dataset <- mutate(dataset,
                  PERD_PALA = replace(PERD_PALA, 
                                      PERD_PALA == 2, 0 ))

# fazendo table para novo barplot final
counts_pala_3 <- table(dataset$PERD_PALA)
barplot(counts_pala_3)
counts_pala_3

# CARDIOPATI
counts_cardi
barplot(counts_cardi)
# Preenchendo com MODA
dataset[dataset$CARDIOPATI %in% 9,]$CARDIOPATI = 1
# fazendo table para novo barplot
counts_cardi_2 <- table(dataset$CARDIOPATI)
barplot(counts_cardi_2)
counts_cardi_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não possui doença Cardiovascular Crônica e 1 = Cardiovascular Crônica
dataset <- mutate(dataset,
                  CARDIOPATI = replace(CARDIOPATI, 
                                       CARDIOPATI == 2, 0 ))

# fazendo table para novo barplot final
counts_cardi_3 <- table(dataset$CARDIOPATI)
barplot(counts_cardi_3)
counts_cardi_3

# HEMATOLOGI
counts_hemato
barplot(counts_hemato)
# Preenchendo com MODA
dataset[dataset$HEMATOLOGI %in% 9,]$HEMATOLOGI = 2
# fazendo table para novo barplot
counts_hemato_2 <- table(dataset$HEMATOLOGI)
barplot(counts_hemato_2)
counts_hemato_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não possui doença Hematológica Crônica e 1 = Hematológica Crônica
dataset <- mutate(dataset,
                  HEMATOLOGI = replace(HEMATOLOGI, 
                                       HEMATOLOGI == 2, 0 ))

# fazendo table para novo barplot final
counts_hemato_3 <- table(dataset$HEMATOLOGI)
barplot(counts_hemato_3)
counts_hemato_3

# HEPATICA
counts_hepati
barplot(counts_hepati)
# Preenchendo com MODA
dataset[dataset$HEPATICA %in% 9,]$HEPATICA = 2
# fazendo table para novo barplot
counts_hepati_2 <- table(dataset$HEPATICA)
barplot(counts_hepati_2)
counts_hepati_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não possui doença Hepática Crônica e 1 = Hepática Crônica
dataset <- mutate(dataset,
                  HEPATICA = replace(HEPATICA, 
                                     HEPATICA == 2, 0 ))

# fazendo table para novo barplot final
counts_hepati_3 <- table(dataset$HEPATICA)
barplot(counts_hepati_3)
counts_hepati_3

# VACINA_COV
counts_vcov
barplot(counts_vcov)
# Preenchendo com MODA
dataset[dataset$VACINA_COV %in% 9,]$VACINA_COV = 2
# fazendo table para novo barplot
counts_vcov_2 <- table(dataset$VACINA_COV)
barplot(counts_vcov_2)
counts_vcov_2

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não recebeu vacina convid-19 e 1 = Recebeu vacina covid-19
dataset <- mutate(dataset,
                  VACINA_COV = replace(VACINA_COV, 
                                       VACINA_COV == 2, 0 ))

# fazendo table para novo barplot final
counts_vcov_3 <- table(dataset$VACINA_COV)
barplot(counts_vcov_3)
counts_vcov_3

# VACINA
counts_vacina
barplot(counts_vacina)
# Preenchendo com MODA
dataset[dataset$VACINA %in% 9,]$VACINA = 2
# fazendo table para novo barplot
counts_vacina_2 <- table(dataset$VACINA)
barplot(counts_vacina_2)
counts_vacina_2
# Obs.: Se recebeu vacina da gripe na última campanha 

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não recebeu Vacina da Gripe e 1 = Recebeu Vacina da Gripe
dataset <- mutate(dataset,
                  VACINA = replace(VACINA, 
                                   VACINA == 2, 0 ))

# fazendo table para novo barplot final
counts_vacina_3 <- table(dataset$VACINA)
barplot(counts_vacina_3)
counts_vacina_3

# ANTIVIRAL
counts_antiviral
barplot(counts_antiviral)
# Preenchendo com MODA
dataset[dataset$ANTIVIRAL %in% 9,]$ANTIVIRAL = 2
# fazendo table para novo barplot
counts_antiviral_2 <- table(dataset$ANTIVIRAL)
barplot(counts_antiviral_2)
counts_antiviral_2
# Obs.: Fez uso de antiviral para tratamento da doença?

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não Recebeu antiviral e 1 = Recebeu antiviral
dataset <- mutate(dataset,
                  ANTIVIRAL = replace(ANTIVIRAL, 
                                      ANTIVIRAL == 2, 0 ))

# fazendo table para novo barplot final
counts_antiviral_3 <- table(dataset$ANTIVIRAL)
barplot(counts_antiviral_3)
counts_antiviral_3

# HOSPITAL
counts_hosp
barplot(counts_hosp)
# Preenchendo com MODA
dataset[dataset$HOSPITAL %in% 9,]$HOSPITAL = 1
# fazendo table para novo barplot
counts_hosp_2 <- table(dataset$HOSPITAL)
barplot(counts_hosp_2)
counts_hosp_2
# Obs.: O paciente foi internado?

# Para não influenciar no peso do algorítmo, substituir o elemento 2 para 0 e 1 para 1
# Onde: 0 = Não foi Internado e 1 = Internado
dataset <- mutate(dataset,
                  HOSPITAL = replace(HOSPITAL, 
                                     HOSPITAL == 2, 0 ))

# fazendo table para novo barplot final
counts_hosp_3 <- table(dataset$HOSPITAL)
barplot(counts_hosp_3)
counts_hosp_3

# CLASSI_FIN
counts_classfinal
barplot(counts_classfinal)
str(dataset)


# Renomenado e passando para factor
dataset <- dataset %>% 
           mutate(CLASSI_FIN = 
                  recode(CLASSI_FIN,
                       "1" = "SRAG_influenza",
                       "2" = "SRAG_outros_virus",
                       "3" = "SRAG_outros_agente_etiologico",
                       "4" = "SRAG_nao_especificado",
                       "5" = "SRAG_covid_19"))
view(dataset)
# Passando para Factor
dataset$CLASSI_FIN <- as.factor(dataset$CLASSI_FIN)
str(dataset)
# fazendo table para novo barplot
counts_classfinal_2 <- table(dataset$CLASSI_FIN)
barplot(counts_classfinal_2)
counts_classfinal_2
# Obs.: Diagnóstico final do caso 
#1-SRAG por influenza
#2-SRAG por outro vírus respiratório
#3-SRAG por outro agente etiológico, qual: X
#4-SRAG não especificado
#5-SRAG por covid-19

# EVOLUCAO
counts_evolucao
barplot(counts_evolucao)
# Preenchendo com MODA
dataset[dataset$EVOLUCAO %in% 9,]$EVOLUCAO = 1
# Renomenado e passando para factor
dataset <- dataset %>% 
           mutate(EVOLUCAO = 
                  recode(EVOLUCAO,
                        "1" = "cura",
                        "2" = "obito",
                        "3" = "obito_outras_causas"))

# Passando para Factor
dataset$EVOLUCAO <- as.factor(dataset$EVOLUCAO)
str(dataset)
# fazendo table para novo barplot final
counts_evolucao_2 <- table(dataset$EVOLUCAO)
barplot(counts_evolucao_2)
counts_evolucao_2
#Obs.:
#1-Cura
#2-Óbito
#3-Óbito por outras causas
#9-Ignorado

view(dataset)

##############################################
# Descritiva gráfica -------------------------
##############################################

# Criando base temporária para manter a base original intacta.

tmp <- dataset
tmp$EVOLUCAO <- as.integer(dataset$EVOLUCAO=="cura")  # 1 = SIM (cura)

str(dataset)

# Função para fazer a análise descritiva #
# Vamos avaliar a distribuição de curados da covid-19 por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de curados por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="EVOLUCAO", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/366229, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro     # Média dos sobreviventes menos o desvio padrão e mais o desvio padrão
    geom_errorbar(aes(x=tgc[,var], y=EVOLUCAO, ymin=EVOLUCAO-se, ymax=EVOLUCAO+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y = EVOLUCAO, colour = '1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=EVOLUCAO, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction=-1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction=-1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de cura") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*366229, name = "Frequência"), labels = scales::percent)
}

descritiva("CS_SEXO")
descritiva("NU_IDADE_N")
descritiva("FEBRE")
descritiva("TOSSE")
descritiva("GARGANTA")
descritiva("DISPNEIA") # experiência subjetiva de desconforto respiratório 
descritiva("DESC_RESP")
descritiva("SATURACAO") # Paciente apresentou saturação O² < 95%?
descritiva("DIARREIA")
descritiva("VOMITO")
descritiva("DOR_ABD")
descritiva("FADIGA")
descritiva("PERD_OLFT")
descritiva("PERD_PALA")
descritiva("CARDIOPATI")
descritiva("HEMATOLOGI") # doença que comprometem a produção dos componetes do sangue 
descritiva("HEPATICA") # doença no fígado
descritiva("VACINA_COV")
descritiva("VACINA") #VACINAS CONTRA GRIPE
descritiva("ANTIVIRAL")
descritiva("HOSPITAL")  #INTERNAÇÃO
descritiva("CLASSI_FIN")

str(dataset)
##############################################
###### Categóricos para numéricos ------------
##############################################

#SEXO

# Transformando um texto em valores, onde:
# Feminino = 0 e Masculino = 1
dataset <- mutate(dataset,
                  CS_SEXO = recode(CS_SEXO,
                                   "F" = 0,
                                   "M" = 1))
str(dataset)

#NU_IDADE_N 

# Criando One Hot Encoder 
dataset <- mutate(dataset, INFANTIL = ifelse(NU_IDADE_N == "INFANTIL",1,0))
dataset <- mutate(dataset, JOVEM = ifelse(NU_IDADE_N == "JOVEM",1,0))
dataset <- mutate(dataset, ADULTO = ifelse(NU_IDADE_N == "ADULTO",1,0))
dataset <- mutate(dataset, IDOSO = ifelse(NU_IDADE_N == "IDOSO", 1,0))

# Apagando variável anterior
dataset <- select(dataset, everything(), -NU_IDADE_N)  

# Realocando as variáveis
dataset <- dataset %>% 
  relocate(INFANTIL, JOVEM, ADULTO, IDOSO, .after = CS_SEXO)

str(dataset)

#CLASSI_FIN

# Criando One Hot Encoder 
dataset <- mutate(dataset, INFLUENZA = ifelse(CLASSI_FIN == "SRAG_influenza", 1,0))
dataset <- mutate(dataset, OUTROS_VIRUS = ifelse(CLASSI_FIN == "SRAG_outros_virus", 1,0))
dataset <- mutate(dataset, OUTROS_AGENTES = ifelse(CLASSI_FIN == "SRAG_outros_agente_etiologico",1,0))
dataset <- mutate(dataset, NAO_ESPECIFICADO = ifelse(CLASSI_FIN == "SRAG_nao_especificado" , 1,0))
dataset <- mutate(dataset, COVID_19 = ifelse(CLASSI_FIN == "SRAG_covid_19" , 1,0))

# Apagando variável anterior
dataset <- select(dataset, everything(), -CLASSI_FIN)  

# Realocando as variáveis
dataset <- dataset %>% 
  relocate(INFLUENZA, OUTROS_VIRUS, OUTROS_AGENTES, NAO_ESPECIFICADO, COVID_19, .before = EVOLUCAO)

view(dataset)

###### Salvando o dataset no formato .csv ---------------------
write.csv(dataset, file = "dataset_tcc.csv", row.names = FALSE)

# chamando dataset salvo
dataset_2 <- read.csv("dataset_tcc.csv", sep = ",", na.strings = "", stringsAsFactors = T)

view(dataset_2)



###### Construindo modelos ---------------------------


inTraining <- createDataPartition(dataset_2$EVOLUCAO, p = .70, list = FALSE)
training <- dataset_2[ inTraining,]
testing  <- dataset_2[-inTraining,]

table(dataset_2$EVOLUCAO)
plot(dataset_2$EVOLUCAO)


# Balanceando os dados preditivos usando step_downsample 
# Referência: https://themis.tidymodels.org/ 
# Registros cortados para ajustar com o de tamanho menor
training_balance <- recipe(EVOLUCAO ~., data = training) %>% 
  themis::step_upsample(EVOLUCAO) %>% 
  prep() %>% 
  juice()

# Os dados diminuiram
dim(training_balance)
plot(training_balance$EVOLUCAO)


###### Logistic #####################################



#Modelo
logistic <-
  multinom_reg() %>%
  set_mode("classification") %>% 
  set_engine("nnet")
??multinom_reg 

logistic

#Treinamento

mdl_fit_logistic <-
  logistic %>% 
  fit(EVOLUCAO ~ ., data = training_balance)

# iniciar o cronômetro
tempo_inicial <- Sys.time()
mdl_fit_logistic
# Tempo total
Sys.time() - tempo_inicial


# Validação ----------------------------

# Reamostragem 
resample_logistic <- bootstraps(training_balance, strata = EVOLUCAO)
resample_logistic

# Treinando com reamostragem incluindo métricas

# iniciar o cronômetro
tempo_inicial <- Sys.time()
mdl_fit_resample_logistic <-
  logistic %>%
  fit_resamples(EVOLUCAO ~.,
                resample_logistic,
                metrics = metric_set(roc_auc, accuracy, sens, precision, recal),
                control = control_resamples(save_pred = TRUE))
# Tempo total
Sys.time() - tempo_inicial

mdl_fit_resample_logistic

# Analisando os resultados da reamostragem
                
metricas_resample <- mdl_fit_resample_logistic %>% 
  collect_metrics()

metricas_resample

matriz_confusao_resample <- mdl_fit_resample_logistic %>% 
  # chamando unnest para desaninhar a pasta .predictions
  unnest(.predictions) %>% 
  conf_mat(EVOLUCAO, .pred_class) %>% 
  autoplot(mdl_fit_logistic, type = "heatmap")

matriz_confusao_resample


# Prevendo com o teste --------------------------------------- 

# bootstraps tem reposição
resample_logistic_teste <- bootstraps(testing, strata = EVOLUCAO)
resample_logistic_teste

# Validação do teste

# iniciar o cronômetro
tempo_inicial <- Sys.time()
mdl_fit_resample_logistic_teste <-
  logistic %>%
  fit_resamples(EVOLUCAO ~.,
                resample_logistic_teste,
                metrics = metric_set(roc_auc, accuracy, sens, precision, recall),
                control = control_resamples(save_pred = TRUE))
# Tempo total
Sys.time() - tempo_inicial

#Modelo do treino com a amostra de teste
predicao_logistic <- predict(mdl_fit_logistic,new_data = testing)
predicao_logistic

predicao_teste_res <- bind_cols(predicao_logistic, testing %>% select(EVOLUCAO))
predicao_teste_res <- bind_cols(predicao_teste_res, testing %>% select(-EVOLUCAO))
resample_logistic_teste_2 <- bootstraps(predicao_teste_res, strata = EVOLUCAO)
resample_logistic_teste_2
predicao_teste_res <- bind_cols(predicao_teste_res, resample_logistic_teste %>% select(id))
accuracy(testing, predicao_teste_res$EVOLUCAO, predicao_teste_res$.pred_class)
######################### Matriz de confusão ######################
# Análise de resultado reamostragem de teste
mdl_fit_logistic
metrica_teste <- mdl_fit_logistic %>% 
  conf_mat(EVOLUCAO, )
  
metricas_teste <- mdl_fit_resample_logistic_teste %>% 
  collect_metrics()
metricas_teste

matriz_confusao_teste <- mdl_fit_resample_logistic_teste %>% 
  unnest(.predictions) %>%
  conf_mat(EVOLUCAO, .pred_class) %>% 
  #com os modelos treinados dentro da reamostragem
  autoplot(mdl_fit_resample_logistic_teste, type = "heatmap")
matriz_confusao_teste

########################## ROC #########################

X <- subset(testing, select = -c(EVOLUCAO))

logistic_teste <-
  multinom_reg() %>%
  set_mode("classification") %>% 
  set_engine("nnet") %>%
  fit(EVOLUCAO ~ ., data = testing)


y_scores <- logistic_teste %>%
  predict(X, type = 'prob')


y_onehot <- dummy_cols(testing$EVOLUCAO)
colnames(y_onehot) <- c('drop','cura', 'obito', 'obito_outras_causas')
y_onehot <- subset(y_onehot, select = -c(drop))

str(y_onehot)
z = cbind(y_scores, y_onehot)
str(z)

z$cura <- as.factor(z$cura)
roc_cura <- roc_curve(data = z, cura, .pred_cura)
roc_cura$specificity <- 1 - roc_cura$specificity
colnames(roc_cura) <- c('threshold', 'tpr', 'fpr')
auc_cura <- roc_auc(data = z, cura, .pred_cura)
auc_cura <- auc_cura$.estimate
cura <- paste('cura (AUC=',toString(round(1-auc_cura,2)),')',sep = '')


z$obito <- as.factor(z$obito)
roc_obito <- roc_curve(data = z, obito, .pred_obito)
roc_obito$specificity <- 1 - roc_obito$specificity
colnames(roc_obito) <- c('threshold', 'tpr', 'fpr')
auc_obito <- roc_auc(data = z, obito, .pred_obito)
auc_obito <- auc_obito$.estimate
obito <- paste('obito (AUC=',toString(round(1-auc_obito,2)),')', sep = '')

z$obito_outras_causas <- as.factor(z$obito_outras_causas)
roc_obito_outras_causas <- roc_curve(data = z, obito_outras_causas, .pred_obito_outras_causas)
roc_obito_outras_causas$specificity <- 1 - roc_obito_outras_causas$specificity
colnames(roc_obito_outras_causas) <- c('threshold', 'tpr', 'fpr')
auc_obito_outras_causas <- roc_auc(data = z, obito_outras_causas, .pred_obito_outras_causas)
auc_obito_outras_causas <- auc_obito_outras_causas$.estimate
obito_outras_causas <- paste('obito_outras_causas (AUC=',toString(round(1-auc_obito_outras_causas,2)),')',sep = '')

# Create an empty figure, and iteratively add a line for each class
fig <- plot_ly()%>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'), showlegend = FALSE) %>%
  add_trace(data = roc_cura,x = ~fpr, y = ~tpr, mode = 'lines', name = cura, type = 'scatter')%>%
  add_trace(data = roc_obito,x = ~fpr, y = ~tpr, mode = 'lines', name = obito, type = 'scatter')%>%
  add_trace(data = roc_obito_outras_causas,x = ~fpr, y = ~tpr, mode = 'lines', name = obito_outras_causas, type = 'scatter')%>%
  layout(xaxis = list(
    title = "False Positive Rate"
  ), yaxis = list(
    title = "True Positive Rate"
  ),legend = list(x = 100, y = 0.5))
fig



