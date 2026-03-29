#2.10 Exercícios

#2.10.1 Importação de dados

install.packages("tidyverse")

library(tidyverse)

# fonte: https://tidyverse.org/packages/
# fonte: https://app.datacamp.com/learn/tutorials/r-data-import-tutorial

install.packages("readr")
library (readr)
library(datasets)

#1______________________________________________________________________

data_municipios <- read.csv('municipios_virgula.csv')
head(data_municipios, 5)
str(data_municipios)
# No banco de dados há 15 municípios. As colunas são 'nome_municipio', 'uf', 'populacao', 'idh' e 'pib_per_capita'

#2_______________________________________________________________________

data_municipios_ponto_virgula <- read.csv('municipios_pontovirgula.csv')
#mais colunas do que nomes de colunas
data_municipios_ponto_virgula <- vroom('municipios_pontovirgula.csv')


data_municipios_ponto_virgula <- read.csv2('municipios_pontovirgula.csv', fileEncoding = 'latin1')

#Error in read_csv2("municipios_pontovirgula.csv", fileEncoding = "latin1") : 
# argumento não utilizado (fileEncoding = "latin1")
install.packages("data.table")

data_municipios_ponto_virgula <- read.csv2('municipios_pontovirgula.csv', fileEncoding = 'latin1')


library(dplyr)
glimpse (data_municipios_ponto_virgula)
glimpse (data_municipios)

#csv lê arquivos delimitados por vírgula com ponto-final de marcadores de decimal
#csv2 lê arquivos delimitados por ponto-e-vírgula com vírgula de marcadores de decimal

#3______________________________________________________________________ 
dados_eleicoes_2022 <- read.csv('eleicoes_2022.csv')
head(dados_eleicoes_2022)
dados_eleicoes_2022 %>%
  select(municipio)

vetor_municipios <- dados_eleicoes_2022 %>%
  select(municipio)

View(vetor_municipios)
#Sim, o nome aparece completo (linhas 15 e 16)

#4________________________________________________________________________ 
idh_municipios <- read.csv('idh_municipios.csv')
idh_municipios_pv <- read.csv2('idh_municipios.csv', fileEncoding = "latin1")
#verificar se as colunas são numeric
str(idh_municipios_pv)
#checar a coluna idh_2010
class(idh_municipios_pv$idh_2010)
#Resultado deu "NULL". ALgum problema na importação
ncol(idh_municipios_pv)
#Deu 13 colunas. Então o problema não é o separador
glimpse(idh_municipios_pv)
#Em vez de idh_2010, deve ser idhm.2010
class(idh_municipios_pv$idhm_2010)
#NULL também
#Vou verificar as colunas
names(idh_municipios_pv)
#Verificar a classe das colunas numéricas
class(idh_municipios_pv$IDHM.2010)
#Resultado "character".

#5 Tres formas diferentes______________________________________________
# a) 
idh_municipios_csv <- read.csv('idh_municipios.csv')
#importação sem erro
View(idh_municipios_csv)
#os dados não estão separados, há apenas 1 coluna, mas os nomes aparecem corretamente

# b) 
idh_municipios_csv2 <- read.csv2('idh_municipios.csv')
#importação sem erro
View(idh_municipios_csv2)
#os dados estão separados corretamente, há 13 colunas, e os nomes aparecem corretamente
class(idh_municipios_csv2$IDHM.2010)
#Mas os dados estão sendo lidos como character

# c)
idh_municipios_csv2_latin1 <- read.csv2('idh_municipios.csv', fileEncoding = "latin1")
View(idh_municipios_csv2_latin1)
#os dados estão separados corretamente, há 13 colunas, mas os nomes não aparecem corretamente
class(idh_municipios_csv2_latin1$IDHM.2010)
#Os dados estão sendo lidos como character


#6 Municipios com maior e menor IDHs________________________________
municipios_virgula <- read.csv('municipios_virgula.csv')
#Fonte de consulta: https://r4ds.hadley.nz/data-transform
View(municipios_virgula)
#coluna "idh"
municipios_virgula |> #dessa forma, não preciso repetir o nome do data.frame toda hora
  arrange(idh)
#com base no resultado, o município com maior IDH é Florianópolis e o com menor é Pau D'Arco

#7 Soma e médias das população_______________________________________

total_idh <- sum(municipios_virgula$populacao)
media_idh <- median(municipios_virgula$populacao)

total_idh
#36.601.670
media_idh
#2.440.111

#8_________________________________________________
#total de votos de cada candidato
View(dados_eleicoes_2022)
#Agrupar por candidato
dados_eleicoes_2022 |>
  group_by(candidato)
#Não deu certo
dados_eleicoes_2022 |>
  arrange(by_group = candidato)

df_bolsonaro <-  filter(dados_eleicoes_2022,candidato =='Bolsonaro')
soma_bolsonaro <- sum(df_bolsonaro$votos)
soma_bolsonaro
#6.861.801
df_lula <-  filter(dados_eleicoes_2022,candidato =='Lula')
soma_lula <- sum(df_lula$votos)
soma_lula
# 8.249.244
df_tebet <-  filter(dados_eleicoes_2022,candidato =='Simone Tebet')
soma_tebet <- sum(df_tebet$votos)
soma_tebet
# 758.133
df_ciro <-  filter(dados_eleicoes_2022,candidato =='Ciro Gomes')
soma_ciro <- sum(df_ciro$votos)
soma_ciro
#355.023
arrange(dados_eleicoes_2022, group_by(TRUE))
#https://r4ds.hadley.nz/workflow-scripts.html => Explica como fazer mais rápido
# usando group_by e summarize
ranking_votos_decresc <- dados_eleicoes_2022 |> 
  group_by(candidato) |> 
  summarize(total_votos = sum(votos, na.rm = TRUE)) |>
  arrange(desc(total_votos))
print(ranking_votos_decresc)


#9___________________________________________________

#Consulta: https://stackoverflow.com/questions/13706188/importing-csv-file-into-r-numeric-values-read-as-characters
#Consulta: https://r4ds.hadley.nz/strings.html#extracting-data-from-strings



idh_municipios_csv2_final <- read.csv2('idh_municipios.csv', sep = ";", 
                                       dec = ".")
class(idh_municipios_csv2_final$IDHM.2010)
#NUMERIC
View(idh_municipios_csv2_final)
#extrair os dados entre parênteses

library(tidyr)
library(tidyverse)
library(stringr)



idh_municipios_regiao <- NULL

idh_municipios_regiao <- idh_municipios_csv2_final %>%
  separate_wider_delim (cols = Territorialidades, 
                        delim = " (", 
                        names = c("municipio", "sigla"), 
                        too_few = "align_start"
  ) %>%
  mutate(sigla = str_remove_all(sigla, "\\)"),
         sigla = str_trim(sigla) 
  )%>%
  filter(!is.na(sigla)) #para evitar erro porque tem Brasil na lista
#resultado da separate_wider_delim deixou um
#parenteses 
#str_trim() para tirar espaços
View(idh_municipios_regiao)  
#Para agrupar os dados por região, descobri que tem um pacote do IPEA
#Tentei por API no IBGE mas o link não existia mais.
install.packages("geobr")
library(geobr)

tb_estados_regiao <- read_state (year = 2020, showProgress = FALSE) %>%
  #https://www.rdocumentation.org/packages/geobr/versions/1.9.1/topics/read_state
  as.data.frame() %>% #transformar em tabela simples, sem mapas etc.
  select(abbrev_state, name_region) %>%
  rename(sigla = abbrev_state, regiao = name_region) %>%
  distinct() #cada estado aparece uma vez

#conferir os dados
View(tb_estados_regiao)
#leftjoin() junta dadoos. Neste caso, vai usar "siglas" como referência para
#juntat a região correspondente
View(idh_municipios_regiao)
# "sigla" em vez de "siglaS" 
idh_estados_regiaos <- idh_municipios_regiao %>%
  left_join (tb_estados_regiao,by = "sigla")

#conferindo os dados

View(idh_estados_regiaos)

#Agora, finalmente tem os dados para agrupar e calcular as métricas pedidas
ranqueamento <- idh_estados_regiaos %>%
  group_by(regiao) %>%
  summarise(media_idhs = median(IDHM.2010, na.rm = TRUE)) %>%
  arrange(desc(media_idhs))
#na.rm serve para lidar com dados em branco
View(ranqueamento)


#RESP: A região Sul tem o maior IDH médio e a região Nordeste, o menor IDH médio

#10_________________________________________________________________________
#Dica: agrupe por município, filtre a linha com o maior percentual (slice_max()), 
#e selecione as colunas relevantes.

#tidyverse
View(dados_eleicoes_2022)
#criar im df novo
dados_eleicoes_exerc10 <- read.csv("eleicoes_2022.csv")

View(dados_eleicoes_exerc10)
#Criar a tabela vencedores
eleicoes_2022_vencedores <- dados_eleicoes_exerc10 %>%
  #agrupar por município
  group_by(municipio) %>% 
  #slice_max() filtra linha com o maior valor na coluna percentual
  slice_max(order_by = percentual, n = 1) %>%
  # n = 1 => pegar somente o primeiro
  #mutate para mudar o nome
  mutate(vencedor = candidato)
#conferir os dados
View(eleicoes_2022_vencedores)

#Novos resultados com median em vez de mean

print("Novos resultados Mediana")

# Alteração feita via Worktree para teste.
print("Alteração feita via Worktree para teste")















