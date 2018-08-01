library(abjutils)
library(caret)
library(data.table)
library(dplyr)
library(janitor)
library(lubridate)
library(magrittr)
library(MASS)
library(readr)
library(utils)
library(odbc)
library(readxl)
library(readr)


## functions
qtd_semestres <- function(date1){
  
  diff_in_days <- difftime(floor_date(Sys.Date(), "halfyear"), floor_date(date1, "halfyear"), units = "days") # days
  diff_in_years <- as.double(diff_in_days)/365 # absolute years
  (floor(diff_in_years * 2) + 1)
  
}

regencia_fies <- read_delim("D:/Users/B7215078/Desktop/FNDE/regencia_fies.csv", ";", 
                            escape_double = FALSE, col_types = cols(fim = col_date(format = "%d/%m/%Y"), 
                                                                    inicio = col_date(format = "%d/%m/%Y")), trim_ws = TRUE)

df <- read_csv("D:/Users/B7215078/Desktop/FNDE/base_alunos_atualizada.csv")
df %<>% dplyr::select(-X1)


## transform Variables

# transforming na em 0
df[is.na(df)] <- 0

# to_date
df$dt_assinatura_contrato <- as.Date(df$dt_assinatura_contrato)

# to_num
cols_to_num <- c("tx_juros","qtd_sem_contratado","qtd_sem_dilatado", "qtd_sem_cursado")
df[cols_to_num] <- sapply(df[cols_to_num], as.numeric)

# create variables
df %<>% mutate(tipo_regencia = ifelse((dt_assinatura_contrato >= regencia_fies$inicio[1] & dt_assinatura_contrato <= regencia_fies$fim[1]) ,regencia_fies$tipo_regencia[1],
                               ifelse((dt_assinatura_contrato >= regencia_fies$inicio[2] & dt_assinatura_contrato <= regencia_fies$fim[2]) ,regencia_fies$tipo_regencia[2],
                                       regencia_fies$tipo_regencia[3])))

df %<>% left_join(dplyr::select(regencia_fies,tipo_regencia,carencia,amortizacao,pgto_juros,percent_fgeduc), by = "tipo_regencia")


### calcular duracao utilizacao com ifelse: fase utilizacao, usar qtd_contratado, outras fases usar qtd_sem_cursado

df %<>% mutate(duracao_utilizacao = ifelse(fase=="utilizacao", qtd_sem_contratado, qtd_sem_cursado),
               duracao_amortizacao = ifelse(tipo_regencia %in% c("I","II"), (3*duracao_utilizacao)+2, 3*duracao_utilizacao),
               in_utilizacao = floor_date(df$dt_assinatura_contrato,"halfyear"),
               fim_utilizacao = in_utilizacao + ((duracao_utilizacao-1) * months(6)),
               in_carencia = fim_utilizacao + months(6),
               fim_carencia = fim_utilizacao + months(6)*3,
               in_amortizacao = fim_carencia + months(6),
               fim_amortizacao = fim_carencia + months(6)*duracao_amortizacao,
               vlr_semestre = ifelse(fase %in% c("utilizacao","carencia"), round(valor_divida/qtd_sem_cursado), round(valor_liberado/qtd_sem_cursado)),
               divida_total = round(vlr_semestre*qtd_sem_contratado),
               vlr_abatimento = round(divida_total/duracao_amortizacao))


# create database "df_ref"
df_ref <- cbind.data.frame(rep(df$cpf, each = length(seq.Date(from = min(df$in_utilizacao),to = max(df$fim_amortizacao), by = '6 month'))),
                           rep(seq.Date(from = min(df$in_utilizacao),to = max(df$fim_amortizacao), by = '6 month'), nrow(df)))

names(df_ref) <- c("cpf","cmpt")

# filter date "2018-07-01"
df_ref %<>% filter(cmpt >= "2018-01-01" & cmpt <= "2030-07-01")

# join with date variables
df_ref %<>% left_join(dplyr::select(df, cpf, tx_juros, in_utilizacao, fim_utilizacao, in_carencia, fim_carencia, in_amortizacao, fim_amortizacao, percent_fgeduc), by = "cpf")

# convert to data.table
df_ref <- as.data.table(df_ref)

# create prediction variable "fase"
df_ref[data.table::between(cmpt, in_utilizacao, fim_utilizacao), fase := '1-utilizacao']
df_ref[data.table::between(cmpt, in_carencia, fim_carencia), fase := '2-carencia']
df_ref[data.table::between(cmpt, in_amortizacao, fim_amortizacao), fase := '3-amortizacao']
df_ref[data.table::between(cmpt, fim_amortizacao, "2030-07-01"), fase := '4-sem fase']

df_ref %<>% dplyr::select(cpf, cmpt, tx_juros, fase, percent_fgeduc)
df_ref %<>% replace(., is.na(.), "")

# create database "tab_divida"
tab_divida <- df %>% dplyr::select(cpf, valor_divida, valor_liberado, vlr_semestre, vlr_abatimento) %>% mutate(cmpt=as.Date("2018-01-01"))

# join with "tab_divida"
df_ref %<>% left_join(dplyr::select(tab_divida,cpf, cmpt, valor_divida), by = c("cpf","cmpt")) %>% left_join(dplyr::select(tab_divida,cpf,vlr_semestre, vlr_abatimento), by = "cpf") 

# create tab_score

tab_score <- readRDS("D:/Users/B7215078/Desktop/FNDE/tab_score.rds")
                       

# join tab_score

df_ref %<>% left_join(tab_score, by = "cpf")
df_ref %<>% mutate(score = ifelse(is.na(score), 0.5, score))

df_ref %<>% mutate(valor_divida_II = valor_divida, valor_divida_III = valor_divida, valor_divida_IV = valor_divida)


df_ref <- df_ref %>%
  group_by(cpf) %>%
  mutate(id_cpf = row_number()) %>%
  ungroup()

df_ref <- df_ref %>%
  group_by(cpf, fase) %>%
  mutate(id_fase = row_number()) %>%
  ungroup()


### Evolucao da Divida --------


while(sum(is.na(df_ref$valor_divida)>0)){
  df_ref <- df_ref %>%
    
    mutate(valor_divida = ifelse(id_cpf == 1, valor_divida, 
                             ifelse((id_cpf > 1 & fase == "1-utilizacao"), lag(valor_divida) * (1+(tx_juros/200)) + vlr_semestre,
                             ifelse((id_cpf > 1 & fase == "2-carencia")  , lag(valor_divida) * (1+(tx_juros/200)),
                             ifelse((id_cpf > 1 & score > 0.3), lag(valor_divida) * (1+(tx_juros/200)),
                              lag(valor_divida) * (1+(tx_juros/200)) - vlr_abatimento * (1+(tx_juros/200)))))),
    
           valor_divida_II = ifelse(id_cpf == 1, valor_divida_II, 
                             ifelse((id_cpf > 1 & fase == "1-utilizacao"), lag(valor_divida_II) * (1+(tx_juros/200)) + vlr_semestre,
                             ifelse((id_cpf > 1 & fase == "2-carencia")  , lag(valor_divida_II) * (1+(tx_juros/200)),
                             ifelse((id_cpf > 1 & score > 0.3), lag(valor_divida_II) * (1+(tx_juros/200)),
                                     lag(valor_divida_II) * (1+(tx_juros/200)) - vlr_abatimento * (1+(tx_juros/200)) * (1-score))))),
           
           valor_divida_III = ifelse(id_cpf == 1, valor_divida_III, 
                              ifelse((id_cpf > 1 & fase == "1-utilizacao"), lag(valor_divida_III) * (1+(tx_juros/200)) + vlr_semestre,
                              ifelse((id_cpf > 1 & fase == "2-carencia")  , lag(valor_divida_III) * (1+(tx_juros/200)),
                              ifelse((id_cpf > 1 & score > 0.3), lag(valor_divida_III) * (1+(tx_juros/200)),
                                     lag(valor_divida_III) * (1+(tx_juros/200)) - vlr_abatimento * (1+(tx_juros/200)) * (1-score))))),
           
           valor_divida_IV =  ifelse(id_cpf == 1, valor_divida_IV, 
                              ifelse((id_cpf > 1 & fase == "1-utilizacao"), lag(valor_divida) * (1+(tx_juros/200)) + vlr_semestre,
                              ifelse((id_cpf > 1 & fase == "2-carencia")  , lag(valor_divida) * (1+(tx_juros/200)),
                                     lag(valor_divida_IV) * (1+(tx_juros/200)) - vlr_abatimento * (1+(tx_juros/200)) * (1-score))))
           )
} 
  



# Replace negative values to zero
df_ref[df_ref$valor_divida<0,"valor_divida"] <- 0
df_ref[df_ref$valor_divida_II<0,"valor_divida_II"] <- 0
df_ref[df_ref$valor_divida_III<0,"valor_divida_III"] <- 0
df_ref[df_ref$valor_divida_IV<0,"valor_divida_IV"] <- 0

# Arredondamento dos valores das d?vidas
df_ref$valor_divida <- round(df_ref$valor_divida)
df_ref$valor_divida_II <- round(df_ref$valor_divida_II)
df_ref$valor_divida_III <- round(df_ref$valor_divida_III)
df_ref$valor_divida_IV <- round(df_ref$valor_divida_IV)

df_ref$percent_fgeduc <- df_ref$percent_fgeduc/100

## Provisionamento: Cenario 1 -------

#---------------------------------------------------------------------- #
# Provisiona a divida completa apenas para os alunos classificados como #
# maus pagadores(score > 0.3);                                          #
# O provisionamento é feito apenas para alunos em fase de amortizacao;  #
#---------------------------------------------------------------------- #

df_ref %>% filter(score > 0.3) %>% mutate(prov_cen1 = ifelse(fase == "3-amortizacao", valor_divida * percent_fgeduc,0)) %>%
          group_by(cmpt) %>% summarise(qtd_contratos = n(), tot_prov_cen1 = sum(prov_cen1))


## Provisionamento: Cenario 2 -------

#----------------------------------------------------------------------- #
# Provisiona a divida completa  para os alunos classificados como maus   #
# pagadores(score > 0.3) e proporcional ao score para os bons pagadores; #
# O provisionamento é feito apenas para alunos em fase de amortizacao;   #
#----------------------------------------------------------------------- #

df_ref %>% mutate(prov_cen2 = ifelse(score  > 0.3 & fase == "3-amortizacao", valor_divida_II * percent_fgeduc,
                              ifelse(score <= 0.3 & fase == "3-amortizacao", valor_divida_II * percent_fgeduc * score,0))) %>%
           group_by(cmpt) %>% summarise(qtd_contratos = n(), tot_prov_cen2 = sum(prov_cen2))
                  
     

## Provisionamento: Cenario 3 -------

#----------------------------------------------------------------------- #
# Provisiona a divida completa  para os alunos classificados como maus   #
# pagadores(score > 0.3) e proporcional ao score para os bons pagadores  #
# baseado na maxima divida;                                              #
# O provisionamento é feito apenas para alunos em fase de amortizacao;   #
#----------------------------------------------------------------------- #

df_ref %>% mutate(prov_cen3 = ifelse(score  > 0.3 & fase == "3-amortizacao", valor_divida_III * percent_fgeduc,
                              ifelse(score <= 0.3 & fase == "3-amortizacao" & id_fase == 1, valor_divida_III * percent_fgeduc * score,
                              ifelse(score <= 0.3 & fase == "3-amortizacao" & id_fase > 1, lag(valor_divida_III) * tx_juros,0)))) %>%
  group_by(cmpt) %>% summarise(qtd_contratos = n(), tot_prov_cen3 = sum(prov_cen3))


## Provisionamento: Cenario 4 -------

#----------------------------------------------------------------------- #
# Bons e maus pagam e o valor provisionado é proporcional ao score;      #
# O provisionamento é feito apenas para alunos em fase de amortizacao;   #
#----------------------------------------------------------------------- #

df_ref %>% mutate(prov_cen4 = ifelse(fase == "3-amortizacao", valor_divida_II * percent_fgeduc * score, 0)) %>%
           group_by(cmpt) %>% summarise(qtd_contratos = n(), tot_prov_cen4 = sum(prov_cen4))


