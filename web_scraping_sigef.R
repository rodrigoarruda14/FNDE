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

subject <- paste0("Diário Oficial da União - ", format(Sys.Date(), "%d %B %Y"))
corpo_email <- "Portarias, Resoluções publicadas, referentes ao Fundo Nacional de Desenvolvimento da Educação\n"

driver<- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]
remDr$navigate("http://pesquisa.in.gov.br/imprensa/core/start.action")

## Busca palavra chave
while(has_error(remDr$findElement(using = 'css', "#txtPesquisa_avancada"))==TRUE)
{
  remDr$refresh()
  Sys.sleep(sample(1:60))
}

for(j in sigla_programa)
{

  remDr$navigate("http://pesquisa.in.gov.br/imprensa/core/start.action")
  
  wordkey<-remDr$findElement(using = 'css', "#txtPesquisa_avancada")
  wordkey$sendKeysToElement(list('"',j, '"'))
  
  
  ## Pressiona botão de pesquisa
  press_button <- remDr$findElement(using = 'css', "#pesquisa02_0")
  press_button$clickElement()
  
  Sys.sleep(sample(1:2))
  
  msg <- "Nenhum item encontrado para a pesquisa solicitada."
  
  ## Checa se há resultados disponíveis
  if(remDr$findElement(using = 'css', "#navigation")$getElementText()[[1]]==msg)
  {
    msg2 <- "Não foram encontrados resultados que contenham as palavras-chave"
    corpo_email <- paste0(corpo_email, "\n", j, " - ", msg2, "\n")
  } else {  

    navigation <- remDr$findElement(using = 'css', "#navigation")
    n_results <- as.numeric(substr(navigation$getElementText()[[1]], 1, 1))
    links <- data.frame()
    
    for(i in 1:n_results)
    {
      
      valor <- paste0("//*[@id='ResultadoConsulta']/tbody/tr[", i, "]/td/nav/a")
      link <- remDr$findElement(using = "xpath", value = valor)
      
      links[i,1] <- j
      links[i,2] <- link$getElementText()
      links[i,3] <- link$getElementAttribute("href")
      
      corpo_email <- paste0(corpo_email, "\n", j, " - ", link$getElementText(),"\n\n", "* ", link$getElementAttribute("href"), "\n")

    }
      
  }
  
}

remDr$close()
driver$server$stop()
driver$server$process

teste <- iconv(corpo_email, "UTF-8", "latin1")

OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "rodrigo.arruda@fnde.gov.br;fabio.gomes@fnde.gov.br; eriane.dantas@fnde.gov.br; ana.barreto@fnde.gov.br; eliete.oliveira@fnde.gov.br; ana.botelho@fnde.gov.br; gerson.flores@fnde.gov.br"
outMail[["subject"]] = subject
outMail[["body"]] = teste
outMail$Send()


## Schedule
# taskscheduler_create(taskname = "Web_Scraping_Diario_Oficial_da_Uniao_SIGEF", 
#                      rscript = "C:/Users/08259760495/Documents/Projetos - Git/FNDE/web_scraping_sigef.R", 
#                      schedule = "WEEKLY", starttime = "06:30", days = c("MON", "TUE", "WED", "THU", "FRI"))


