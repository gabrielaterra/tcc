library(readxl)
install.packages("tidyverse")
library(tidyverse)

#### ----- Pegando os dados do censo para dados gerais via atlas ####
readxl::read_excel("C:/Users/golte/OneDrive/Documentos/monografia/atlas.xlsx") %>%
    select (ANO, Codmun7, Município, UF, ESPVIDA, FECTOT, MORT1, E_ANOSESTUDO, GINI, RDPC, EMP, IDHM, tratado, Município) %>%
    filter (ANO == 2000) %>% mutate (id_municipio = as.character(Codmun7)) -> censo

#### ---- Pegando da BD Para achar o total de municípios ####
library(basedosdados)
set_billing_id('mono-355120')
"SELECT id_municipio, id_mesorregiao, nome_mesorregiao, sigla_uf
FROM `basedosdados.br_bd_diretorios_brasil.municipio` 
WHERE id_mesorregiao IN ('1501',
                          '1503',
                          '1504',
                          '1505',
                          '1506',
                          '1702',
                          '1701',
                          '2201',
                          '2203',
                          '2302',
                          '2307',
                          '2306',
                          '2501',
                          '2503',
                          '2502',
                          '2602',
                          '2601',
                          '2906',
                          '2902',
                          '2901',
                          '2905',
                          '2903',
                          '2907',
                          '3107',
                          '3110',
                          '3515',
                          '4106',
                          '4108',
                          '4107',
                          '4201',
                          '4306',
                          '4307',
                          '4301',
                          '5004',
                          '5205')" %>% 
  read_sql() -> totalmun

#### ---- Tentando pegar os dados da RAIS via BD ####
library(basedosdados)
set_billing_id('mono-355120')
"SELECT ano, id_municipio, vinculo_ativo_3112, valor_remuneracao_media, cnae_1 
FROM `basedosdados.br_me_rais.microdados_vinculos` 
WHERE ano = 2000" %>%
  read_sql() -> rais

rais %>%
  group_by (id_municipio) %>%
  summarise(totalemp =sum(vinculo_ativo_3112), totalpess = n()) %>%
  ungroup() %>%
  mutate(propemp = totalemp / totalpess * 100)-> raiscalc

  
#### ---- Pegando os dados de PIB ####
readxl::read_excel("C:/Users/golte/OneDrive/Documentos/monografia/pib2002.xlsx") %>%
  select (Ano, cod_munic, ind, serv, va, agro, pib, pib_pcap) %>%
  filter (Ano == 2000) %>%
  mutate(pibind = ind / pib * 100, pibagro = agro / pib *100, pibserv = serv / pib *100) -> pib

#### ---- Pegando os dados BD de população ####
set_billing_id('mono-355120')
"SELECT id_municipio, populacao, ano 
FROM `basedosdados.br_ibge_populacao.municipio` 
WHERE ano = 2000" %>%
read_sql() -> populacao

# Tirando de id64
populacao %>%
  mutate_if(bit64::is.integer64, as.integer)
# Mudando o nome da variável de populacao pra poptotal pra nao dar sobreposição de funções
populacao %>%
  mutate_if(bit64::is.integer64, as.integer) %>%
  summarise(id_municipio, ano, poptotal = populacao) -> populacao1
glimpse(populacao1)
#### ---- Join para o total dos c + t ####

totalmun %>% 
  left_join(populacao1, by = "id_municipio") -> junto

junto %>%
  left_join(pib, junto, by = c("id_municipio" = "cod_munic")) -> junto1

junto1 %>% 
  left_join(censo, junto1, by = "id_municipio") -> integrado
            
integrado %>%
  left_join(raiscalc, integrado, by = "id_municipio") -> match2000
#### ---- Criando o controle ####
install.packages("MatchIt")
library(MatchIt)
library(dtplyr)
library(ggplot2)

# Variável dependente de interesse é a porcentagem de empregados e a variável 
#independente de interesse é os alunos

match2000 %>%
  filter(id_mesorregiao == "1501") %>%
  group_by(tratado) %>%
  summarise(n_municipio = n(),
            emprego_tratados = mean(propemp, na.rm=T),
            std_error = sd(propemp, na.rm=T) / sqrt(n_municipio))

#A diferença da média é estatisticamente significativa a 95% de confidencialidade
with(match2000, t.test(propemp ~ tratado))

#Temos um pvalor menos que 0,05 logo estatisticamente se rejeita a hip nula de que as medias sao iguais ha dif estatística

#### ---- Estatísticas básicas para as variáveis ####
match2000cov <- c('poptotal', 'pib_pcap', 'pibind', 'pibagro', 'pibserv', 'ESPVIDA', 'MORT1', 'E_ANOSESTUDO', 'GINI', 'RDPC', 'IDHM')

basicstatcov <- match2000 %>%
  group_by(tratado, id_mesorregiao) %>%
  select(one_of(match2000cov)) %>%
  summarise_all(~ mean(., na.rm = T))

#Fazendo testes t para checar se há diferenças estatísticas entre os grupos

with(match2000, t.test(poptotal ~ tratado))
with(match2000, t.test(pib_pcap ~ tratado))
with(match2000, t.test(pibind ~ tratado))
with(match2000, t.test(RDPC ~ tratado))
with(match2000, t.test(pibagro ~ tratado))
with(match2000, t.test(pibserv ~ tratado))
with(match2000, t.test(ESPVIDA ~ tratado))
with(match2000, t.test(MORT1 ~ tratado))
with(match2000, t.test(E_ANOSESTUDO ~ tratado))
with(match2000, t.test(GINI ~ tratado))
with(match2000, t.test(IDHM ~ tratado))

#### ---- Propensity Score Estimation ####
m_ps <- glm(tratado ~ poptotal + pib_pcap + pibind + pibagro + pibserv + ESPVIDA 
            + MORT1 + E_ANOSESTUDO + GINI + RDPC + IDHM, family = binomial(), data = match2000)
  summary (m_ps)

prs_trat <- data.frame(pr_score = predict(m_ps, type = "response"), tratado = m_ps$model$tratado)
head(prs_trat)

# Examinando a região de suporte comum, onde depois de estimar os pp, plotar
#histogramas das estimativas por status de tratamento

prs_trat %>%
  mutate(tratado = ifelse(tratado == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) + geom_histogram(color = "black") + facet_wrap(~tratado) + xlab("Probabilidade de ser criada uma UF") + theme_bw()

match2000_nomiss <- match2000 %>%
  select(propemp, tratado, one_of(match2000cov)) %>%
  na.omit()

dim(match2000)
dim(match2000_nomiss)


install.packages("optmatch")
library(optmatch)
mod_match <- matchit(tratado ~ poptotal + pib_pcap + pibind + pibagro + pibserv + ESPVIDA 
                     + MORT1 + E_ANOSESTUDO + GINI + RDPC + IDHM, method = "optimal" , data = match2000_nomiss)
summary(mod_match)
plot(mod_match)

dta_m <- match.data(mod_match)
head(dta_m)
dim(dta_m)
