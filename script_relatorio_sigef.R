
# Pacotes
library(readr)
library(flexdashboard)
library(highcharter)
library(dplyr)
library(viridisLite)
library(forecast)
library(treemap)

thm <- 
  hc_theme(
    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = 'Tangerine')
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )

extracao_cgaux <- read_delim("~/extracao_cgaux.csv", 
                             ";", escape_double = FALSE, col_types = cols(NU_PROCESSO = col_character()), 
                             locale = locale(encoding = "latin1"), 
                             trim_ws = TRUE)

extracao_cgaux$ME_REFERENCIA <- as.Date(extracao_cgaux$ME_REFERENCIA, "%d/%m/%Y")

uf <- extracao_cgaux %>% distinct(SG_UF)

saldo_estados <- extracao_cgaux %>% select(SG_UF,
                                           ME_REFERENCIA,
                                           VL_SALDO_CONTA, 
                                           VL_SALDO_FUNDOS,
                                           VL_SALDO_POUPANCA, 
                                           VL_SALDO_RDB) %>%
group_by(SG_UF, ME_REFERENCIA) %>% 
                 summarise(saldo_1 = sum(VL_SALDO_CONTA),
                           saldo_2 = sum(VL_SALDO_FUNDOS),
                           saldo_3 = sum(VL_SALDO_POUPANCA),
                           saldo_4 = sum(VL_SALDO_RDB)) %>%
  filter(ME_REFERENCIA == '2018-09-01')


names(saldo_estados) <- c("code", "value")

mapdata <- get_data_from_map(download_map_data("countries/br/br-all"))
glimpse(mapdata)

######################################
## Gráfico Brasil
######################################

n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()

hcmap(mapdata, data = saldo_estados, value = "value",
      joinBy = c("hc-a2", "code"), 
      dataLabels = list(enabled = TRUE, format = '{point.code}'),
      tooltip = list(valuePrefix = "R$")) %>%
  hc_colorAxis(stops = colstops) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(thm)


######################################
## Gráfico Maps
######################################

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")


######################################
######################################

data("USArrests", package = "datasets")
data("usgeojson")

USArrests <- USArrests %>%
  mutate(state = rownames(.))

n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()

highchart() %>%
  hc_add_series_map(usgeojson, USArrests, name = "Sales",
                    value = "Murder", joinBy = c("woename", "state"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.postalcode}')) %>%
  hc_colorAxis(stops = colstops) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(thm)


extracao_cgaux %>% filter(SG_UF=="PE" & ME_REFERENCIA >= '2013-01-01') %>% 
  group_by(ME_REFERENCIA) %>% 
  summarise(saldo_fundos=sum(VL_SALDO_FUNDOS)) %>% select(saldo_fundos) %>%
  ts(start = 2013, end = c(2018, 9), frequency = 12) %>%
  hchart() %>% 
  hc_add_theme(thm)


filtered_df <- extracao_cgaux[extracao_cgaux$SG_UF == "AC" & extracao_cgaux$ME_REFERENCIA >= '2013-01-01', ]

filtered_df <- data.table(filtered_df)
filtered_df[, Media_Saldo:=mean(VL_SALDO_FUNDOS), by=ME_REFERENCIA]
filtered_df %<>% group_by(ME_REFERENCIA) %>% summarise(saldo_fundos = max(Media_Saldo))

ggplot(filtered_df)

hchart(filtered_df, "line", x = filtered_df$ME_REFERENCIA, y = filtered_df$saldo_fundos)



extracao_cgaux$ME_REFERENCIA <- as.Date(extracao_cgaux$ME_REFERENCIA, "%d/%m/%Y")

df_saldo <- extracao_cgaux %>% 
  filter(SG_UF== "PE" & DS_PROGRAMA_FNDE=="EDUCAÇÃO INFANTIL - APOIO SUPLEMENTAR") %>% 
  group_by(ME_REFERENCIA) %>% 
  summarise(total_saldo=sum(VL_SALDO_FUNDOS))

hchart(df_saldo)

x <- extracao_cgaux %>% 
  filter(SG_UF == input$UF & ME_REFERENCIA >= '2013-01-01') %>% 
  group_by(ME_REFERENCIA) %>% 
  summarise(total_saldo=sum(VL_SALDO_FUNDOS)) %>% 
  select(total_saldo) %>% ts(start = 2013, end = c(2018, 9), frequency = 12)


extracao_cgaux %>% 
  select(SG_UF, ME_REFERENCIA, VL_SALDO_CONTA, VL_SALDO_FUNDOS, VL_SALDO_POUPANCA, VL_SALDO_RDB) %>%
  filter(lubridate::year(ME_REFERENCIA) >= 2018) %>% 
  group_by(SG_UF, ME_REFERENCIA) %>% 
  summarise(tot_saldo_conta=sum(VL_SALDO_CONTA),
            tot_saldo_fundos=sum(VL_SALDO_FUNDOS),
            tot_saldo_poup=sum(VL_SALDO_POUPANCA),
            tot_saldo_rdb=sum(VL_SALDO_RDB)) %>% View()


extracao_cgaux %>% filter(ME_REFERENCIA >= "2018-01-01") %>%
  group_by(SG_UF, ME_REFERENCIA, DS_PROGRAMA_FNDE) %>% 
  summarise(vl_saldo_conta=sum(VL_SALDO_CONTA), vl_saldo_fundos=sum(VL_SALDO_FUNDOS), vl_saldo_poupanca=sum(VL_SALDO_POUPANCA), vl_saldo_rdb=sum(VL_SALDO_RDB))


install.packages('rsconnect')
rsconnect::setAccountInfo(name='rodrigoarruda14',
                          token='F5E3D2187938A7C4F356F537BAD7EEED',
                          secret='XADXQrcDcKBzU24g2JHMYyJrr32RN98IY3d0OffU')

library(rsconnect)
rsconnect::deployApp()


#############################################
#Usando pacote ROracle
library(ROracle)
#############################################

drv <- dbDriver("Oracle")
# Create the connection string
host <- "exafnde1-scan2.fnde.gov.br"
port <- 1521
#Digite o usuario
nome_usuario <- "MN_CGAUX_FNDE"
#Digite a senha aqui
senha<-"fnde_mn_cgaux"

#Apenas execute as linhas abaixo para estabelecer a conexao

svc <- "berilo.fnde.gov.br"
connect.string <- paste("(DESCRIPTION=","(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))","(CONNECT_DATA=(SERVICE_NAME=", svc, ")))", sep = "")
con <- dbConnect(drv, username = nome_usuario, password = senha, dbname = connect.string)

consulta<-"SELECT sc.*, 
cc.co_programa_fnde, 
pf.ds_programa_fnde, 
e.no_razao_social,
e.sg_uf, 
scc.no_st_conta,
SUBSTR(corp_fnde.mascara_processo_fc (sc.nu_processo), 1, 30) nu_processo_mask
FROM sigef_fnde.a_saldo_conta_corrente@DBL_MNCGAUX_DG sc,
sigef_fnde.s_conta_corrente@DBL_MNCGAUX_DG cc,
sigef_fnde.s_programa_fnde_vinculado@DBL_MNCGAUX_DG pv,
sigef_fnde.s_situacao_conta@DBL_MNCGAUX_DG scc,
corp_fnde.s_programa_fnde pf,
corp_fnde.s_entidade e
WHERE sc.nu_cnpj = cc.nu_identificador
AND sc.nu_agencia = cc.nu_agencia
AND sc.nu_banco = cc.nu_banco
AND sc.nu_conta_corrente = cc.nu_conta_corrente
AND cc.co_programa_fnde = pv.co_programa_fnde_vinculado
AND cc.co_programa_fnde = pf.co_programa_fnde
AND cc.nu_seq_entidade = e.nu_seq_entidade
AND cc.co_situacao_conta = scc.co_st_conta
AND sc.nu_banco = '001'
--AND sc.me_referencia = '09/2018'
AND pv.co_programa_fnde IN ('TI')"

resultados<-dbGetQuery(con,consulta)