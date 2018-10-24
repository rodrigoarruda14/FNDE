
## Web Scraping Diario Oficial da União ##

library(RSelenium)
library(testit)
library(RDCOMClient)
library(magrittr)
library(taskscheduleR)

nome_programa <- c("Programa de Fomento às Escolas de Ensino Médio em Tempo Integral",
                   "Programa Brasil Alfabetizado",
                   "Programa de Apoio aos Sistemas de Ensino para Atendimento à Educação de Jovens e Adultos",
                   "Programa Nacional de Inclusão de Jovens",
                   "Programa Nacional de Inclusão de Jovens",
                   "Programa Nacional de Acesso ao Ensino Técnico e Emprego")
                   
sigla_programa <- c("EMTI", "PBA", "PEJA", "Projovem Urbano", 
                    "Projovem Campo - Saberes da Terra", "Pronatec")

publico_programa <- c("ensino medio", rep("jovens e adultos", 4), "educação profissional")

df_programas <- data.frame(nome_programa, sigla_programa, publico_programa)

# Titulo do Email
subject <- paste0("Diário Oficial da União - ", format(Sys.Date(), "%d %B %Y"))
body_email <- data.frame()
# Envio do Email
OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "rodrigo.arruda@fnde.gov.br"
outMail[["subject"]] = subject


driver<- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]
remDr$navigate("http://pesquisa.in.gov.br/imprensa/core/start.action")

## Busca palavra chave
while(has_error(remDr$findElement(using = 'css', "#input-search"))==TRUE)
{
  remDr$refresh()
  Sys.sleep(sample(1:60))
}

for(j in sigla_programa)
{

  wordkey<-remDr$findElement(using = 'css', "#txtPesquisa_avancada")
  wordkey$sendKeysToElement(list('"',j, '"'))
  
  
  ## Pressiona botão de pesquisa
  press_button <- remDr$findElement(using = 'css', "#pesquisa02_0")
  press_button$clickElement()
  
  teste <- remDr$findElement(using = 'css', "#navigation")
  teste$getElementText()[[1]]
  
  msg <- "Nenhum item encontrado para a pesquisa solicitada."
  ## Checa se há resultados disponíveis
  if(remDr$findElement(using = 'css', "#navigation")$getElementText()[[1]]==msg)
  {
    text_body <- paste0(subject, "\n\nNão foram encontrados resultados que contenham as palavras-chave: ", j)
  
  } else {  
    ## Check para quantidade de resultados
  # } else if(has_error(remDr$findElement(using = 'css', ".search-results"))==FALSE)
  # {
  #   qtd_results <- remDr$findElement(using = 'css', ".search-results")
  #   n_results <- as.numeric( substr(qtd_results$getElementText()[[1]], 
  #                                   stop  = nchar(qtd_results$getElementText()[[1]])-1, 
  #                                   start = nchar(qtd_results$getElementText()[[1]])-1))
  #   
  #   text_body <- paste0(subject, "\n\nPortarias, Resoluções publicadas, referentes ao Fundo Nacional de Desenvolvimento da Educação/ FIES/MEC\n\n")
  #   links <- NULL
  #   
  #   # Para quando houver mais de 1 resultado
  #   for(i in 1:n_results)
  #   {
  #     element <- paste0("tr:nth-child(", i, ") a")
  #     corpo <- paste0("tr:nth-child(", i, ")", " .asset-entry-summary")
  #     
  #     result<-remDr$findElement(using = 'css', element)
  #     titulo_materia <- result$getElementText()[[1]]
  #     
  #     texto <- remDr$findElement(using = 'css', corpo)
  #     corpo_materia <- texto$getElementText()[[1]]
  #     
  #     result$clickElement()
  #     links[i] <- remDr$getCurrentUrl()
  #     
  #     # Corpo do Email
  #     text_body <- paste0(text_body, "\n\n *", titulo_materia, "\n\n", corpo_materia, "\n\n", "Link - ", links[[1]])
  #     
  #     remDr$goBack()
  #   }
  #   
  # } else # Para apenas um resultado
    
    teste <- remDr$findElement(using = 'css', "#navigation")
    n_results <- as.numeric(substr(teste$getElementText()[[1]], 1, 1))
    n <- rep(c(".displayTagLinhaBranca", ".displayTagLinhaAzul"), n_results-round((n_results/2)))
    
    for(i in 1:n_results)
      {
        
        corpo <- paste0(n[i], ":nth-child(", i, ")", " .titulo_jornal")

        texto <- remDr$findElement(using = 'css', corpo)
        texto$clickElement()

        
        result$clickElement()
        links[i] <- remDr$getCurrentUrl()

        # Corpo do Email
        text_body <- paste0(text_body, "\n\n *", titulo_materia, "\n\n", corpo_materia, "\n\n", "Link - ", links[[1]])

        remDr$goBack()
      }
    teste2 <- remDr$findElement(using = 'css', ".displayTagLinhaBranca:nth-child(1) .titulo_jornal")
    teste2$clickElement()
    link <- remDr$getCurrentUrl()
    
    # Corpo do Email
    
    body <- "\n\nPortarias, Resoluções publicadas, referentes ao Fundo Nacional de Desenvolvimento da Educação/ FIES/MEC\n\n"
    text_body <- paste0(subject, body,
                        "* ", titulo_materia, "\n\n", corpo_materia, "\n\n", "Link - ", links[[1]])
    remDr$goBack() 
  }
  remDr$goBack()
  print(j)
  
  #text_body <- iconv(text_body, "UTF-8", "latin1")
  cat(text_body)
  body_email[j,1] <- j
  body_email[j,2] <- text_body  
  #outMail[["body"]] = text_body
  #outMail$Send()
}

## Enviando Email


## Schedule
#taskscheduler_create(taskname = "Web_Scraping_Diario_Oficial_da_Uniao", 
#                     rscript = "C:/Users/08259760495/Documents/Web Scraping/Web Scraping - DOU/scrpit_web_scraping_dou.R", 
#                     schedule = "WEEKLY", starttime = "06:00", days = c("MON", "TUE", "WED", "THU", "FRI"))

remDr$mouseMoveToLocation(webElement = texto) # move to the required element
remDr$click(2)


webElem$sendKeysToElement(list(key = "control", "w"))

remDr$getWindowHandles()



remDr$getCurrentWindowHandle()

remDr$switchToWindow("6442450950")
remDr$getCurrentWindowHandle()

teste <- remDr$findElement(using = "xpath", "//*[@id='ResultadoConsulta']/tbody/tr[3]/td/nav/a")
teste$clickElement()


library(XML)
fileUrl <- "http://pesquisa.in.gov.br/imprensa/core/consulta.action"
doc <- htmlTreeParse(fileUrl, useInternal=T)
xpathSApply(doc, "//a[@href]", xmlGetAttr, "href")


teste$getElementAttribute("href")
