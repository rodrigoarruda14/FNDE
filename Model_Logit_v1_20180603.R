## Instalação dos Pacotes

install.packages(c("caret","readr","RODBC","odbc","sqldf","janitor","dplyr",
                   "abjutils","caret","ROCR","e1071"))


library(readr)
library(utils)
library(odbc)
library(sqldf)
library(janitor)
library(dplyr)
library(abjutils)
library(caret)
library(ROCR)
library(e1071)
library(MASS)

##---------------- Leitura da base de dados ----------------##
#Ta�ssa
base_ini <- read_delim("C:/Users/03590072105/Desktop/velhasT.csv", 
                      ";", escape_double = FALSE, col_types = cols(CodCurso = col_character(), 
                                                                   CodigoBanco = col_character(), DtAssinaturaContratoAF = col_date(format = "%Y-%m-%d"), 
                                                                   DtNascimento = col_date(format = "%Y-%m-%d"), 
                                                                   NuSemestreReferencia = col_character(), 
                                                                   PercentualFinanciado = col_number(), 
                                                                   ValorRendaComprometidaFiadores = col_double(), 
                                                                   ValorRendaComprovadaFiadores = col_double(), 
                                                                   ValorRendaInformadaFiadores = col_double()), 
                      locale = locale(decimal_mark = ","), 
                      na = "NA", trim_ws = TRUE)

## Criando a conexão
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};",server="velhas", encoding = "latin1")

## Lendo a tabela
query <- dbSendQuery(conn = con, statement = "SELECT * 
                     FROM [09_2017].[dbo].TB_Gestao_Carteira 
                     WHERE FaseSISFIES in ('amortizacao','liquidado')")

base_ini <- dbFetch(query)

## Fechando a conexão
dbDisconnect(con)

## Convertendo variáveis para data
base_ini$DtAssinaturaContratoAF = as.Date(base_ini$DtAssinaturaContratoAF)
base_ini$DtNascimento = as.Date(base_ini$DtNascimento)

##---------------- Criação de Variáveis ----------------##

## Criação da variável idade
base_ini <- base_ini %>% mutate(idade = floor(as.numeric(difftime(base_ini$DtAssinaturaContratoAF, base_ini$DtNascimento, units = "days"))/365.25))

## Criação da Variável Resposta
base_ini <- base_ini %>% mutate(resposta = ifelse(base_ini$DiasAtraso > 365, 1, 0))


## Criação da variável Região da IES
base_ini <- base_ini %>% mutate(Regiao_UF = if_else(UfIES %in% c("AC","AP","AM","PA","RO","RR","TO"),"Norte",
                                            if_else(UfIES %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE"),"Nordeste",
                                            if_else(UfIES %in% c("DF","MT","MS","GO"),"Centro-Oeste",
                                            if_else(UfIES %in% c("RS","SC","PR"),"Sul","Sudoeste")))))


## Removendo variáveis não utilizadas

base_trat <- base_ini %>% dplyr::select(-c(CodigoBanco,Cep,CidadeAluno, FaseBanco, FaseSISFIES,Risco,NuSemestreReferencia,SafraEntradaSISFIES,
                                           SafraSaidaSISFIES,SafraEntradaAF,CodCurso,CodTurno,MunicipioIes,CnpjIES,CnpjMant,Contagem,DtNascimento,
                                           DtAssinaturaContratoAF,ValorDividaAF,ValorLiberadoAF, ValorRendaInformadaFiadores, RendaPercapita, 
                                           ValorLiberadoSISFIES, UfAluno,ValorRendaComprometidaFiadores))


##---------------- Tratamento das Variáveis ----------------##


## Convertendo variáveis para strings e numéricas

cols_to_char <- c("Banco","Sexo","CursouEnsinoMedioEscolaPublica","Deficiente",
                  "Prouni","SituacaoInscricao","NomeTurno","UfIES","TipoGarantia",
                  "NomeCurso","EstadoCivil","resposta")

cols_to_num <- c("RendaFamiliarMensalBruta","RendaPessoalMensalBruta","ValorFinanciadoSISFIES",
                 "ValorRendaComprovadaFiadores","PercentualFinanciado","QtdeSemestreContratado",
                 "TaxaDeJuros","idade")

base_trat[cols_to_char] <- sapply(base_trat[cols_to_char], as.character)
base_trat[cols_to_num] <- sapply(base_trat[cols_to_num], as.numeric)


## A variável flag se cursou o ensino m?dio em escola pública("S","N") apresentava o valor "P"
## Essa categoria foi recategorizada para "S"
base_trat[base_trat$CursouEnsinoMedioEscolaPublica =="P" ,"CursouEnsinoMedioEscolaPublica"] <- "S"


## Padronização dos nomes dos cursos
base_trat$NomeCurso <- toupper(base_trat$NomeCurso)

## Remoção dos outliers
base_trat <- base_trat %>% filter(RendaFamiliarMensalBruta <= 20000)
base_trat <- base_trat %>% filter(RendaPessoalMensalBruta <= 20000)
base_trat <- base_trat %>% filter(idade > 15 & idade <= 80)


##------------ Join para obter o código do curso ------------##

curso_DISTINCT <- read.csv2("Z:/CGFIN/CGFIN/Gestão da Carteira/Modelos/setembro2017/curso_DISTINCT.csv",encoding = "Latin-1")

#Ta�ssa
curso_DISTINCT <- read.csv2("C:/Users/03590072105/Desktop/curso_DISTINCT.csv",encoding = "Latin-1")
curso_DISTINCT <- curso_DISTINCT %>% filter(NomeCurso.1!='')

#n�o usa para o taissa
curso_DISTINCT <- curso_DISTINCT %>% filter(CodCurso!='')

curso_DISTINCT$NomeCurso=toupper(curso_DISTINCT$NomeCurso)

base_cat <- left_join(base_trat, curso_DISTINCT , by = "NomeCurso", copy = FALSE)



##------------ Categorização das variáveis ------------##


base_cat <- base_cat %>% mutate(qtdsemestre_cat = ifelse((QtdeSemestreContratado >= 1 & QtdeSemestreContratado <= 4),"a", 
                                                  ifelse((QtdeSemestreContratado >= 5 & QtdeSemestreContratado <= 8),"a",
                                                  ifelse((QtdeSemestreContratado >= 9 & QtdeSemestreContratado <= 11),"b",
                                                  ifelse((QtdeSemestreContratado >= 12 & QtdeSemestreContratado <= 16),"b",
                                                  ifelse((QtdeSemestreContratado >= 17 & QtdeSemestreContratado <= 19),"c",
                                                  ifelse((QtdeSemestreContratado >= 20 & QtdeSemestreContratado <= 26),"c","0")))))),
                                
                                rendafamiliarmensalbruta_cat = ifelse((RendaFamiliarMensalBruta <= 1100 ),"a", 
                                                               ifelse((RendaFamiliarMensalBruta > 1100 & RendaFamiliarMensalBruta <= 2500),"b",
                                                               ifelse((RendaFamiliarMensalBruta > 2500 & RendaFamiliarMensalBruta <= 12269902),"c","0"))),
                                
                                rendapessoalmensalbruta_cat = ifelse((RendaPessoalMensalBruta <= 450 ),"a", 
                                                              ifelse((RendaPessoalMensalBruta > 450 & RendaPessoalMensalBruta <= 815),"b",
                                                              ifelse((RendaPessoalMensalBruta > 815 & RendaPessoalMensalBruta <= 270186),"c","0"))),
                                
                                valorfinaciados_cat = ifelse((ValorFinanciadoSISFIES <= 7671 ),"a", 
                                                      ifelse((ValorFinanciadoSISFIES > 7671 & ValorFinanciadoSISFIES <= 14355),"b",
                                                      ifelse((ValorFinanciadoSISFIES > 14355 & ValorFinanciadoSISFIES <= 25395),"b",
                                                      ifelse((ValorFinanciadoSISFIES > 25395 & ValorFinanciadoSISFIES <= 17348803),"c","0")))),
                                
                                valorrendafiadoress_cat = ifelse(is.na(ValorRendaComprovadaFiadores),"f",
                                                          ifelse(ValorRendaComprovadaFiadores <= 1710,"a", 
                                                          ifelse((ValorRendaComprovadaFiadores > 1710 & ValorRendaComprovadaFiadores <= 2692),"b",
                                                          ifelse((ValorRendaComprovadaFiadores > 2692 & ValorRendaComprovadaFiadores <= 4683),"c",
                                                          ifelse(ValorRendaComprovadaFiadores > 4683,"d","e"))))),
                                
                                idade_cat = ifelse((idade <= 20 ),"a", 
                                            ifelse((idade > 20 ),"b","0")),
                                
                                taxadejuros_cat = ifelse((TaxaDeJuros <= 340 ),"a", 
                                                  ifelse((TaxaDeJuros > 340 ),"b","0")),
                                
                                garantia_cat = ifelse(TipoGarantia %in% c("FGEDUC","FIANCA SOLIDARIA+FGEDUC"),"a",
                                               ifelse(TipoGarantia %in% c("","FIAN? CONVENCIONAL"),"b", 
                                               ifelse(TipoGarantia %in% c("FIANCA CONVENCIONAL+FGEDUC"),"c","d"))),
                                
                                turno_cat = ifelse(NomeTurno %in% c("INTEGRAL","N?fO APLICA"),"a","b"),
                                
                                ufies_cat = ifelse(UfIES %in% c(""),"a",
                                            ifelse(UfIES %in% c("CentroOeste","norte"),"b",     
                                            ifelse(UfIES %in% c("nordeste","sudeste"),"c","d"))),       
                                
                                estadocivil_cat = ifelse(EstadoCivil %in% c("Casado"),"c",
                                                  ifelse(EstadoCivil %in% c("Solteiro"),"b","a")),
                                
                                curso_cat = ifelse(NomeCurso.1 %in% c("1","2","8"),"a", 
                                            ifelse(NomeCurso.1 %in% c("3","4","7"),"b",
                                            ifelse(NomeCurso.1 %in% c("5","6"),"c","0"))),
                                
                                percentual_cat = ifelse((PercentualFinanciado == 10000 ),"a", "b")
                                
                                
)


##------------ Divisão da base em Treinamento e Teste ------------##

set.seed(120022541)
index <- sample(1:nrow(base_cat), trunc(0.3*nrow(base_cat)))
train <- base_cat[-index,]
test  <- base_cat[index,]

test$resposta <- as.factor(test$resposta)
train$resposta <- as.factor(train$resposta)

##------------ Ajuste do Modelo ------------##

lrcompleto= glm(resposta ~ idade_cat + Sexo + taxadejuros_cat + garantia_cat  + 
                valorrendafiadoress_cat + valorfinaciados_cat + rendapessoalmensalbruta_cat + rendafamiliarmensalbruta_cat +
                SituacaoInscricao + Prouni + CursouEnsinoMedioEscolaPublica + Banco , family = binomial("logit"), data= train)


stepLR = stepAIC(lrcompleto, direction="both")
summary (stepLR)

save(file="Z:/CGFIN/CGFIN/Gestão da Carteira/Modelos/logistic_v1.rda", stepLR)

##------------ Validação do Modelo ------------##


## Cálculo da importância das variáveis
varImp(stepLR)

## Escoragem da base
p <- predict(stepLR, test, type = "response")


## Cálculo da curva ROC
prLR1 <- prediction(p, test$resposta)
perf.LR1 <- performance(prLR1, measure = "tpr", x.measure = "fpr")
plot(perf.LR1, main = "ROC CURVE LOGISTIC REGRESSION", col= "orange",lwd =3)
abline (a = 0, b= 1, lwd =2, lty = 2)
dev.off()

## Cálculo do KS
x <- p[test$resposta== "1"]
y <- p[test$resposta== "0"]
x[is.na(x)=="FALSE"]
y[is.na(x)=="FALSE"]
ks.test(x,y,alternative = "two.sided")

## Cálculo da M?xima verossimilhan?a

#Quanto MAIOR melhor
logLik(stepLR)

#Quanto MENOR melhor
extractAIC(stepLR)

## Cálculo da razão de chance
exp(cbind(OR=coef(stepLR),confint(stepLR)))

## Cálculo da Desviância
deviance(stepLR)   # -2xloglik pois no modelo binomial com ni=1, l_saturado = 1 => D=2xloglik
summary( residuals(stepLR, type = "deviance"))

## Cálculo do Coefieciente de person
sum(residuals(stepLR, type = "pearson")^2)   # X2p

test <- test %>% mutate(p_30 = ifelse(( p <= 0.30 ),0, 
                                      ifelse((p > 0.30) ,1, "erro")))


## Cálculo da Matriz de Confusão
confusionMatrix(data= test$p_30,
                reference= test$resposta,
                positive='1')
100*prop.table(table(test$p_30))
