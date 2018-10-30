
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

uf <- extracao_cgaux %>% distinct(SG_UF)

saldo_estados <- extracao_cgaux %>% group_by(SG_UF) %>% 
                 summarise(saldo_conta = sum(VL_SALDO_FUNDOS))

names(saldo_estados) <- c("code", "value")

######################################
## Gráfico Brasil
######################################

n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()

hcmap("countries/br/br-all", data = saldo_estados, value = "value",
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
