# Pacotes
library(brazilmaps)
library(readxl)
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)

##########################
# Mapa - Região Nordeste #
##########################

# Importa o arquivo
Cenarios_Fundeb <- read_excel("C:/Users/08259760495/Desktop/Cenarios_Fundeb.xlsx", 
                              sheet = "Planilha4")

# Importa tabela com c?digos do IBGE
codigo_lat_long <- read_csv("~/codigo_lat_long.csv", 
                            locale = locale(encoding = "latin1"))

codigo_lat_long %<>% select(Nome_Municipio, Codigo_IBGE)

# Join entre as bases
Cenarios_Fundeb %<>% left_join(codigo_lat_long , by = c("municipio"="Nome_Municipio"))

# Carrega o mapa

map_ne <- get_brmap(geo = "City", geo.filter = list(Region = 2))

mapa_atual <- plot_brmap(map_ne,
                         data_to_join = Cenarios_Fundeb,
                         join_by = c("City" = "Codigo_IBGE"),
                         var = "valor_atual") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Average age",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))

mapa_atual  + labs(title = "Valor Por Aluno - Cenário Atual")


mapa_proposto <- plot_brmap(map_ne,
                            data_to_join = Cenarios_Fundeb,
                            join_by = c("City" = "Codigo_IBGE"),
                            var = "valor_proposto")

mapa_proposto + labs(title = "Valor Por Aluno - Cenário Proposto")



#######################
# Mapa - Regi?o Norte #
#######################

# Importa o arquivo
Cenarios_Fundeb <- read_excel("C:/Users/08259760495/Desktop/Cenarios_Fundeb.xlsx", 
                              sheet = "Planilha4")

# Importa tabela com códigos do IBGE
codigo_lat_long <- read_csv("~/codigo_lat_long.csv", 
                            locale = locale(encoding = "latin1"))

codigo_lat_long %<>% select(Nome_Municipio, Codigo_IBGE)
# Join 

Cenarios_Fundeb %<>% left_join(codigo_lat_long , by = c("municipio"="Nome_Municipio"))

# Carrega o mapa

map_no <- get_brmap(geo = "City", geo.filter = list(Region = 1))

mapa_atual <- plot_brmap(map_no,
                         data_to_join = Cenarios_Fundeb,
                         join_by = c("City" = "Codigo_IBGE"),
                         var = "valor_atual")

mapa_atual  + labs(title = "Valor Por Aluno - Cen?rio Atual")


mapa_proposto <- plot_brmap(map_no,
                            data_to_join = Cenarios_Fundeb,
                            join_by = c("City" = "Codigo_IBGE"),
                            var = "valor_proposto")

mapa_proposto + labs(title = "Valor Por Aluno - Cen?rio Proposto")



##########################
# Mapa - Regi?es NO e NE #
##########################

# Importa o arquivo
Cenarios_Fundeb <- read_excel("C:/Users/08259760495/Desktop/Cenarios_Fundeb.xlsx", 
                              sheet = "Planilha4")

# Importa tabela com códigos do IBGE
codigo_lat_long <- read_csv("~/codigo_lat_long.csv", 
                            locale = locale(encoding = "latin1"))

codigo_lat_long %<>% select(Nome_Municipio, Codigo_IBGE)
# Join 

Cenarios_Fundeb %<>% left_join(codigo_lat_long , by = c("municipio"="Nome_Municipio"))

# Carrega o mapa

map_no <- get_brmap(geo = "City", geo.filter = list(Region = c(1,2)))

mapa_atual <- plot_brmap(map_no,
                         data_to_join = Cenarios_Fundeb,
                         join_by = c("City" = "Codigo_IBGE"),
                         var = "valor_atual")

mapa_atual  + labs(title = "Valor Por Aluno - Cen?rio Atual")


mapa_proposto <- plot_brmap(map_no,
                            data_to_join = Cenarios_Fundeb,
                            join_by = c("City" = "Codigo_IBGE"),
                            var = "valor_proposto")

mapa_proposto + labs(title = "Valor Por Aluno - Cen?rio Proposto")
