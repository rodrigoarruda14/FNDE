# Pacotes
library(brazilmaps)
library(readxl)
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)

##########################

plot_map <- function (map, data_to_join = data.frame(), join_by = NULL, var = "values")
{
  if (nrow(data_to_join) != 0) {
    map <- join_data(map = map, data = data_to_join, by = join_by)
    if (any(class(map) == "sf")) {
      map_ggplot <- as(map, "Spatial")
      map_ggplot@data$id = row.names(map_ggplot@data)
      map_ggplot_df <- suppressMessages(ggplot2::fortify(map_ggplot))
      map_ggplot_df <- dplyr::left_join(map_ggplot_df, 
                                        map_ggplot@data, by = "id")
    }
    else if (class(map) == "SpatialPolygonsDataFrame") {
      map_ggplot <- map
      map_ggplot@data$id = row.names(map_ggplot@data)
      map_ggplot_df <- suppressMessages(ggplot2::fortify(map_ggplot))
      map_ggplot_df <- dplyr::left_join(map_ggplot_df, 
                                        map_ggplot@data, by = "id")
    }
    ggplot2::ggplot() + ggplot2::geom_polygon(ggplot2::aes(fill = map_ggplot_df[[var]], 
                                                           x = map_ggplot_df$long, y = map_ggplot_df$lat, group = map_ggplot_df$group), 
                                              data = map_ggplot_df, color = "black", size = 0.2) + 
      ggplot2::coord_fixed() + ggplot2::labs(fill = var) + 
      scale_fill_gradient( low = "#56B4E9", high = "#D55E00", na.value = "transparent",
                           breaks=c(3300,3450,3600,3750), 
                           labels=c('3300','3450','3600','3750'), 
                           limits=c(3300,3750)) + theme_map()
  }
  else {
    if (any(class(map) == "sf")) 
      map <- as(map, "Spatial") %>% ggplot2::fortify()
    if (class(map) == "SpatialPolygonsDataFrame") 
      map <- map %>% ggplot2::fortify()
    ggplot2::ggplot() + ggplot2::geom_polygon(ggplot2::aes(x = map$long, 
                                                           y = map$lat, group = map$group), data = map, color = "black", 
                                              size = 0.2, fill = "white") + ggplot2::coord_fixed() + 
      scale_fill_gradient( low = "#56B4E9", high = "#D55E00", na.value = "transparent",
                           breaks=c(3300,3400,3500,3600,3700,3800), 
                           labels=c('3300','3400','3500','3600','3700','3800'), 
                           limits=c(3300,3800)) + theme_map()
  }
}

##########################

##########################
# Mapa - Região Nordeste #
##########################

# Importa o arquivo
Cenarios_Fundeb <- read_excel("C:/Users/08259760495/Desktop/Cenarios_Fundeb.xlsx", 
                              sheet = "Planilha4")

# Importa tabela com codigos do IBGE
codigo_lat_long <- read_csv("~/codigo_lat_long.csv", 
                            locale = locale(encoding = "latin1"))

codigo_lat_long %<>% select(Nome_Municipio, Codigo_IBGE)

# Join entre as bases
Cenarios_Fundeb %<>% left_join(codigo_lat_long , by = c("municipio"="Nome_Municipio"))

# Carrega o mapa
map_ne <- get_brmap(geo = "City", geo.filter = list(Region = 2))

mapa_atual <- plot_map(map_ne,
                         data_to_join = Cenarios_Fundeb,
                         join_by = c("City" = "Codigo_IBGE"),
                         var = "valor_atual") 

mapa_atual  + labs(title = "Valor Por Aluno - Cenário Atual")


mapa_proposto <- plot_map(map_ne,
                            data_to_join = Cenarios_Fundeb,
                            join_by = c("City" = "Codigo_IBGE"),
                            var = "valor_proposto")

mapa_proposto + labs(title = "Valor Por Aluno - Cenário Proposto")



#######################
# Mapa - Região Norte #
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

mapa_atual <- plot_map(map_no,
                         data_to_join = Cenarios_Fundeb,
                         join_by = c("City" = "Codigo_IBGE"),
                         var = "valor_atual")

mapa_atual  + labs(title = "Valor Por Aluno - Cenário Atual")


mapa_proposto <- plot_map(map_no,
                            data_to_join = Cenarios_Fundeb,
                            join_by = c("City" = "Codigo_IBGE"),
                            var = "valor_proposto")

mapa_proposto + labs(title = "Valor Por Aluno - Cenário Proposto")



##########################
# Mapa - Regiões NO e NE #
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
