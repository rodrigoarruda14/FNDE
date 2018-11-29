## Instalacao dos Pacotes

install.packages(c("caret","readr","RODBC","odbc","sqldf","janitor","dplyr",
                   "abjutils","caret","ROCR","e1071"))

library(magrittr)
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

## Criando a conexÃ£o
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};",server="velhas", encoding = "latin1")

## Lendo a tabela
query <- dbSendQuery(conn = con, statement = "SELECT * 
                     FROM [2017_09].[dbo].TB_Gestao_Carteira 
                     WHERE FaseSISFIES in ('amortizacao','liquidado')")

base_ini <- dbFetch(query)

## Fechando a conexÃ£o
dbDisconnect(con)

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

## Convertendo variaveis para data

base_ini$DtAssinaturaContratoAF <- as.Date(base_ini$DtAssinaturaContratoAF)
base_ini$DtNascimento <- as.Date(base_ini$DtNascimento)


##---------------- Criacao de Variaveis ----------------##

## Criacao da variavel idade
base_ini %<>% mutate(idade = floor(as.numeric(difftime(base_ini$DtAssinaturaContratoAF, base_ini$DtNascimento, units = "days"))/365.25))

## Criacao da Variavel Resposta
base_ini %<>% mutate(resposta = ifelse(base_ini$DiasAtraso > 365, 1, 0))


## Criacao da variavel Regiao da IES
base_ini %<>% mutate(Regiao_UF = if_else(UfIES %in% c("AC","AP","AM","PA","RO","RR","TO"),"Norte",
                                 if_else(UfIES %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE"),"Nordeste",
                                 if_else(UfIES %in% c("DF","MT","MS","GO"),"Centro-Oeste",
                                 if_else(UfIES %in% c("RS","SC","PR"),"Sul","Sudeste")))))


## Removendo variaveis nao utilizadas

base_trat <- base_ini %>% dplyr::select(-c(CodigoBanco,Cep,CidadeAluno, FaseBanco, FaseSISFIES,Risco,NuSemestreReferencia,SafraEntradaSISFIES,
                                           SafraSaidaSISFIES,SafraEntradaAF,CodCurso,CodTurno,MunicipioIes,CnpjIES,CnpjMant,Contagem,DtNascimento,
                                           DtAssinaturaContratoAF,ValorDividaAF,ValorLiberadoAF, ValorRendaInformadaFiadores, RendaPercapita, 
                                           ValorLiberadoSISFIES, UfAluno,ValorRendaComprometidaFiadores))


##---------------- Tratamento das Variaveis ----------------##


## Convertendo variaveis para strings e numericas

cols_to_char <- c("Banco","Sexo","CursouEnsinoMedioEscolaPublica","Deficiente",
                  "Prouni","SituacaoInscricao","NomeTurno","UfIES","TipoGarantia",
                  "NomeCurso","EstadoCivil","resposta")

cols_to_num <- c("RendaFamiliarMensalBruta","RendaPessoalMensalBruta","ValorFinanciadoSISFIES",
                 "ValorRendaComprovadaFiadores","PercentualFinanciado","QtdeSemestreContratado",
                 "TaxaDeJuros","idade")

base_trat[cols_to_char] <- sapply(base_trat[cols_to_char], as.character)
base_trat[cols_to_num] <- sapply(base_trat[cols_to_num], as.numeric)


## A variavel flag se cursou o ensino medio em escola publica("S","N") apresentava o valor "P"
## Essa categoria foi recategorizada para "S"
base_trat[base_trat$CursouEnsinoMedioEscolaPublica =="P" ,"CursouEnsinoMedioEscolaPublica"] <- "S"


## Padronizacao dos nomes dos cursos
base_trat$NomeCurso <- toupper(base_trat$NomeCurso)

## Remocao dos outliers
base_trat %<>% filter(RendaFamiliarMensalBruta <= 20000)
base_trat %<>% filter(RendaPessoalMensalBruta <= 20000)
base_trat %<>% filter(idade > 15 & idade <= 80)


##------------ Join para obter o codigo do curso ------------##

curso_DISTINCT <- read.csv2("Z:/CGFIN/CGFIN/Gestão da Carteira/Modelos/setembro2017/curso_DISTINCT.csv",encoding = "Latin-1")

#não usa para o taissa
curso_DISTINCT <- curso_DISTINCT %>% filter(CodCurso!='')

curso_DISTINCT$NomeCurso=toupper(curso_DISTINCT$NomeCurso)

base_cat <- left_join(base_trat, curso_DISTINCT , by = "NomeCurso", copy = FALSE)


##------------ Categorizacao das variaveis ------------##


base_cat %<>% mutate(qtdsemestre_cat = ifelse((QtdeSemestreContratado >= 0 & QtdeSemestreContratado <= 8),"a", 
                                       ifelse((QtdeSemestreContratado >= 9),"b", "0")),
                                
                     rendafamiliarmensalbruta_cat = ifelse((RendaFamiliarMensalBruta <= 1100 ),"a", 
                                                    ifelse((RendaFamiliarMensalBruta > 1100 & RendaFamiliarMensalBruta <= 2500),"b",
                                                    ifelse((RendaFamiliarMensalBruta > 2500),"c","0"))),
                                
                     rendapessoalmensalbruta_cat = ifelse((RendaPessoalMensalBruta <= 450 ),"a", 
                                                   ifelse((RendaPessoalMensalBruta > 450 & RendaPessoalMensalBruta <= 815),"b",
                                                   ifelse((RendaPessoalMensalBruta > 815),"c","0"))),
                                
                     valorfinaciados_cat = ifelse((ValorFinanciadoSISFIES <= 7671 ),"a", 
                                           ifelse((ValorFinanciadoSISFIES > 7671 & ValorFinanciadoSISFIES <= 14355),"b",
                                           ifelse((ValorFinanciadoSISFIES > 14355 & ValorFinanciadoSISFIES <= 25395),"b",
                                           ifelse((ValorFinanciadoSISFIES > 25395),"c","0")))),
                                
                     valorrendafiadoress_cat = ifelse(is.na(ValorRendaComprovadaFiadores),"f",
                                               ifelse(ValorRendaComprovadaFiadores <= 1710,"a", 
                                               ifelse((ValorRendaComprovadaFiadores > 1710 & ValorRendaComprovadaFiadores <= 2692),"b",
                                               ifelse((ValorRendaComprovadaFiadores > 2692),"c", "d")))),
                                
                     idade_cat = ifelse((idade <= 20 ),"a", 
                                 ifelse((idade > 20 ),"b","0")),
                                
                     taxadejuros_cat = ifelse((TaxaDeJuros <= 340 ),"a", 
                                       ifelse((TaxaDeJuros > 340 ),"b","0")),
                                
                     garantia_cat = ifelse(TipoGarantia %in% c("","FIANÇA CONVENCIONAL","FIANCA CONVENCIONAL+FGEDUC"),"a",
                                    ifelse(TipoGarantia %in% c("FGEDUC","FIANCA SOLIDARIA+FGEDUC"),"b", "c")),
                                
                     turno_cat = ifelse(NomeTurno %in% c("NOTURNO"),"a","b"),
                                
                     ufies_cat = ifelse(Regiao_UF %in% c("", "Nordeste", "Sudeste"),"a",
                                 ifelse(Regiao_UF %in% c("Centro-Oeste","Norte"),"b", "c" )),       
                                
                     estadocivil_cat = ifelse(EstadoCivil %in% c("Divorciado","Separado","União estável", "Viúvo"),"a",
                                       ifelse(EstadoCivil %in% c("Solteiro"),"b", "c")),
                                
                     curso_cat = ifelse(CodCurso %in% c("1","2","8"),"a", 
                                 ifelse(CodCurso %in% c("3","4","7"),"b",
                                 ifelse(CodCurso %in% c("5","6"),"c","0"))),
                                
                     percentual_cat = ifelse((PercentualFinanciado == 10000 ),"a", "b" )
                                
)


##------------ Divisao da base em Treinamento e Teste ------------##

set.seed(120022541)
amostra <- base_cat[runif(593883,1,nrow(base_cat)),]

index <- 1:nrow(amostra)
testindex <- sample(index,trunc(length(index)*30/100))

test <- amostra[testindex,]
train <- amostra[-testindex,]

test$resposta <- as.factor(test$resposta)
train$resposta <- as.factor(train$resposta)


##------------ Ajuste do Modelo ------------##

#Eu tirei: Deficiente
#p-valor tirou:Sexo+ estadocivil_cat  +  idade_cat + curso_cat+


lrcompleto= glm(resposta ~ qtdsemestre_cat +ufies_cat + garantia_cat + valorfinaciados_cat +
                           taxadejuros_cat + percentual_cat + valorrendafiadoress_cat + 
                           rendapessoalmensalbruta_cat+ rendafamiliarmensalbruta_cat + Banco 
                           SituacaoInscricao + turno_cat + Prouni + CursouEnsinoMedioEscolaPublica, family = binomial("logit"), data= train)

summary (lrcompleto)

save(file="Z:/CGFIN/CGFIN/Gestão da Carteira/Modelos/logistic_v2.rda", lrcompleto)


##------------ Validacao do Modelo ------------##

#Como que interpreta???
varImp(lrcompleto)

p = predict(lrcompleto, test, type = "response")
p

##ROC
prLR1 = prediction(p, test$resposta)
perf.LR1 = performance(prLR1, measure = "tpr", x.measure = "fpr")
plot(perf.LR1, main = "CURVA ROC DO MODELO DE REGRESSÃO LOGÍSTICA", col= "orange",lwd =3, xlab= "Verdadeiro positivo", ylab= "Falso positivo ")
abline (a = 0, b= 1, lwd =2, lty = 2)
dev.off()

pr_lr= prediction(p,test$resposta)
per_auc = performance(pr_lr, measure = "auc")

#KS
x= p[test$resposta== "1"]
y= p[test$resposta== "0"]
x[is.na(x)=="FALSE"]
y[is.na(x)=="FALSE"]
ks.test(x,y,alternative = "two.sided")


#maxima verossimilhança
#Quanto maior melhor
#logLik(lrcompleto)
#qUANTO MENOR MELHOR.
#extractAIC(lrcompleto)
#Razão de chance
#exp(cbind(OR=coef(lrcompleto),confint(lrcompleto)))
#Desviancia
#deviance(lrcompleto)   # -2xloglik pois no modelo binomial com ni=1, l_saturado = 1 => D=2xloglik
#summary( residuals(lrcompleto, type = "deviance") )
#Coefieciente de person
#sum(residuals(lrcompleto, type = "pearson")^2)   # X2p


test <- test %>% mutate(p_30 = ifelse(( p <= 0.30 ),0, 
                               ifelse((p > 0.30) ,1, "erro")))


####MATRIZ DE CONFUSÃO)
confusionMatrix(data= test$p_30,
                reference= test$resposta,
                positive='1')
100*prop.table(table(test$p_30))
100*prop.table(table(resposta))

#178164
(95353/178164)
38333/178164
12556/178164
31922/178164



####################################



#######################################################
######################Aplicando os dados no rf#########
#######################################################
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wolpert/4/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()














