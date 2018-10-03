
## Web Scraping Diario Oficial da União ##


library(RSelenium)
library(testit)
library(RDCOMClient)
library(magrittr)

# Titulo do Email
subject <- paste0("Diário Oficial da União - ", format(Sys.Date(), "%d %B %Y"))

driver<- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]
remDr$navigate("http://portal.imprensanacional.gov.br/web/guest/inicio")

## Busca palavra chave
while(has_error(remDr$findElement(using = 'css', "#input-search"))==TRUE)
{
  remDr$refresh()
}
wordkey<-remDr$findElement(using = 'css', "#input-search")
wordkey$sendKeysToElement(list("fies"))


## Pressiona botão de pesquisa
press_button <- remDr$findElement(using = 'css', ".btn span")
press_button$clickElement()


## Checa se há resultados disponíveis
if(has_error(remDr$findElement(using = 'css', "tr:nth-child(1) a"))==TRUE)
{
  text_body <- paste0(subject, "\n\nNão foram encontrados resultados que contenham as palavras-chave: fies.")
  
  ## Check para quantidade de resultados
} else if(has_error(remDr$findElement(using = 'css', ".search-results"))==FALSE)
{
  qtd_results <- remDr$findElement(using = 'css', ".search-results")
  n_results <- as.numeric( substr(qtd_results$getElementText()[[1]], 
                                  stop  = nchar(qtd_results$getElementText()[[1]])-1, 
                                  start = nchar(qtd_results$getElementText()[[1]])-1))
  
  text_body <- paste0(subject, "\n\nPortarias, Resoluções publicadas, referentes ao Fundo Nacional de Desenvolvimento da Educação/ FIES/MEC\n\n")
  links <- NULL
  
  # Para quando houver mais de 1 resultado
  for(i in 1:n_results)
  {
    element <- paste0("tr:nth-child(", i, ") a")
    corpo <- paste0("tr:nth-child(", i, ")", " .asset-entry-summary")
    
    result<-remDr$findElement(using = 'css', element)
    titulo_materia <- result$getElementText()[[1]]
    
    texto <- remDr$findElement(using = 'css', corpo)
    corpo_materia <- texto$getElementText()[[1]]
    
    result$clickElement()
    links[i] <- remDr$getCurrentUrl()
    
    # Corpo do Email
    text_body <- paste0(text_body, "\n\n *", titulo_materia, "\n\n", corpo_materia, "\n\n", "Link - ", links[[1]])
    
    remDr$goBack()
  }
  
} else # Para apenas um resultado
{
  element <- "tr:nth-child(1) a"
  corpo <- "tr:nth-child(1) .asset-entry-summary"
  
  result <- remDr$findElement(using = 'css', element)
  titulo_materia <- result$getElementText()[[1]]
  
  texto <- remDr$findElement(using = 'css', corpo)
  corpo_materia <- texto$getElementText()[[1]]
  
  result$clickElement()
  links <- remDr$getCurrentUrl()
  
  # Corpo do Email
  
  body <- "\n\nPortarias, Resoluções publicadas, referentes ao Fundo Nacional de Desenvolvimento da Educação/ FIES/MEC\n\n"
  text_body <- paste0(subject, body,
                      "* ", titulo_materia, "\n\n", corpo_materia, "\n\n", "Link - ", links[[1]])
  
}


## Enviando Email

OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "rodrigo.arruda@fnde.gov.br; cinara.bessa@fnde.gov.br"
outMail[["subject"]] = subject
outMail[["body"]] = text_body
outMail$Send()

## Schedule
#taskscheduler_create(taskname = "Web_Scraping_Diario_Oficial_da_Uniao", 
#                     rscript = "C:/Users/08259760495/Documents/Web Scraping/Web Scraping - DOU/scrpit_web_scraping_dou.R", 
#                     schedule = "WEEKLY", starttime = "06:00", days = c("MON", "TUE", "WED", "THU", "FRI"))

