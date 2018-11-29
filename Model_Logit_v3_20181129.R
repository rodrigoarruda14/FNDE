## Instalacao dos Pacotes

install.packages(c("caret","readr","RODBC","odbc","sqldf","janitor","dplyr",
                   "abjutils","caret","ROCR","e1071"))

library(magrittr)
library(readr)
library(utils)
library(odbc)
library(janitor)
library(dplyr)
library(abjutils)
library(caret)
library(e1071)
library(MASS)
library(ROCR)
library(stringi)

## Criando a conexao
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};",server="velhas", encoding = "latin1")

## Lendo a tabela
query <- dbSendQuery(conn = con, statement = "SELECT * 
                     FROM [2017_09].[dbo].TB_Gestao_Carteira 
                     WHERE FaseSISFIES in ('amortizacao','liquidado')")

base_ini <- dbFetch(query)

base_ini %<>% filter(!is.na(DiasAtraso))

## Fechando a conexao
dbDisconnect(con)

base_ini <- read_delim("C:/Users/08259760495/Downloads/velhasT.csv", 
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



# Importando tabela com porte das mantenedoras

consolidado_mant <- read_delim("C:/Users/08259760495/Downloads/consolidado_mant.csv", 
                               ";", escape_double = FALSE, col_types = cols(`CNPJ Mantenedora` = col_character()), 
                               trim_ws = TRUE)

consolidado_mant$`CNPJ Mantenedora` <- stri_pad(consolidado_mant$`CNPJ Mantenedora`,14, pad = "0")

base_ini %<>% left_join(dplyr::select(consolidado_mant, "Porte", "CNPJ Mantenedora", "QTD alunos por Mantenedora"), 
                               by = c("CnpjMant" = "CNPJ Mantenedora"))


## Removendo variaveis nao utilizadas

base_trat <- base_ini %>% dplyr::select(-c(CodigoBanco,Cep,CidadeAluno, FaseBanco, FaseSISFIES,Risco,NuSemestreReferencia,SafraEntradaSISFIES,
                                           SafraSaidaSISFIES,SafraEntradaAF,CodCurso,CodTurno,MunicipioIes,CnpjIES,CnpjMant,Contagem,DtNascimento,
                                           DtAssinaturaContratoAF,ValorDividaAF,ValorLiberadoAF, ValorRendaInformadaFiadores, RendaPercapita, 
                                           ValorLiberadoSISFIES, UfAluno,ValorRendaComprometidaFiadores))


##---------------- Tratamento das Variaveis ----------------##


## Convertendo variaveis para strings e numericas

cols_to_char <- c("Banco","Sexo","CursouEnsinoMedioEscolaPublica","Deficiente",
                  "Prouni","SituacaoInscricao","NomeTurno","UfIES","TipoGarantia",
                  "NomeCurso","EstadoCivil","resposta", "Porte")

cols_to_num <- c("RendaFamiliarMensalBruta","RendaPessoalMensalBruta","ValorFinanciadoSISFIES",
                 "ValorRendaComprovadaFiadores","PercentualFinanciado","QtdeSemestreContratado",
                 "TaxaDeJuros","idade", "QTD alunos por Mantenedora")

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

curso_DISTINCT <- curso_DISTINCT %>% filter(CodCurso!='')

curso_DISTINCT$NomeCurso=toupper(curso_DISTINCT$NomeCurso)

base_cat <- left_join(base_trat, curso_DISTINCT , by = "NomeCurso", copy = FALSE)


##------------ Categorizacao das variaveis ------------##


base_cat %<>% mutate(nome_curso_cat = ifelse(NomeCurso =='ADMINISTRAÇÃO DE EMPRESAS', 'ADMINISTRAÇÃO',
                                      ifelse(NomeCurso =='DESIGN DE MODA - ESTILISMO', 'DESIGN DE MODA',
                                      ifelse(NomeCurso =='ENGENHARIA AMBIENTAL E SANITÁRIA', 'ENGENHARIA AMBIENTAL',
                                      ifelse(NomeCurso =='ENGENHARIA DE PETRÓLEO E GÁS', 'ENGENHARIA DE PETRÓLEO',
                                      ifelse(NomeCurso =='ENGENHARIA ELÉTRICA E ELETRÔNICA', 'ENGENHARIA ELÉTRICA',
                                      ifelse(NomeCurso =='ENGENHARIA INDUSTRIAL MECÂNICA', 'ENGENHARIA MECÂNICA',
                                      ifelse(NomeCurso %in% c('ESTÉTICA E COSMÉTICA','ESTÉTICA E COSMETOLOGIA'),'ESTÉTICA',
                                      ifelse(NomeCurso =='MARKETING E PROPAGANDA', 'MARKETING',
                                      ifelse(NomeCurso =='SISTEMA DE INFORMAÇÃO', 'SISTEMAS DE INFORMAÇÃO', 
                                      ifelse(NomeCurso %in% c('COMUNICAÇÃO SOCIAL - JORNALISMO',
                                                              'COMUNICAÇÃO SOCIAL - PUBLICIDADE E PROPAGANDA', 
                                                              'COMUNICAÇÃO SOCIAL - RADIO E TELEVISÃO'), 'COMUNICAÇÃO SOCIAL',
                                      ifelse(NomeCurso %in% c('LETRAS - LÍNGUA PORTUGUESA', 'LETRAS - PORTUGUÊS',
                                                              'LETRAS - PORTUGUÊS E INGLÊS'), 'LETRAS',
                                                              
                                             NomeCurso))))))))))))
                                             
# Base auxiliar com os cursos

base_cursos <- base_cat %>% group_by(nome_curso_cat) %>% summarise(count=n()) %>% arrange(count) %>%
  mutate(novo_curso = ifelse(count<100, "OUTROS", nome_curso_cat))

base_cat %<>% left_join(dplyr::select(base_cursos, nome_curso_cat, novo_curso), by = "nome_curso_cat")
#

base_cat %<>% mutate(novo_curso2 = ifelse(novo_curso %in% c('MEDICINA', 'ENGENHARIA BIOMÉDICA', 'ENGENHARIA METALÚRGICA'), 'grupoa',
                                   ifelse(novo_curso %in% c('ENGENHARIA DE TELECOMUNICAÇÕES', 'ENGENHARIA ELETRÔNICA E DE TELECOMUNICAÇÕES'), 'grupoa',
                                   ifelse(novo_curso %in% c('ODONTOLOGIA', 'CIÊNCIAS ECONÔMICAS', 'ENGENHARIA DE COMPUTAÇÃO','DESIGN DE PRODUTO',
                                                            'ENGENHARIA QUÍMICA','QUÍMICA INDUSTRIAL', 'ENGENHARIA DE MINAS'), 'grupoc',       
                                   ifelse(novo_curso %in% c('RELAÇÕES PÚBLICAS', 'ENGENHARIA AMBIENTAL', 'RÁDIO, TV E INTERNET',
                                                            'ENGENHARIA DE PRODUÇÃO MECÂNICA', 'FARMÁCIA', 'ENGENHARIA MECÂNICA',
                                                            'ENGENHARIA CIVIL', 'ENGENHARIA MECATRÔNICA', 'ENGENHARIA AGRONÔMICA',
                                                            'ARQUITETURA E URBANISMO', 'ENGENHARIA ELÉTRICA', 'ENGENHARIA DE ALIMENTOS',
                                                            'MEDICINA VETERINÁRIA', 'ENGENHARIA FLORESTAL', 'FABRICAÇÃO MECÂNICA',
                                                            'TERAPIA OCUPACIONAL', 'RELAÇÕES INTERNACIONAIS', 'AGRONOMIA',
                                                            'CIÊNCIAS AERONÁUTICAS', 'ENGENHARIA ELETRÔNICA', 'AVIAÇÃO CIVIL'), 'grupod',
                                   ifelse(novo_curso %in% c('JOGOS DIGITAIS', 'MANUTENÇÃO INDUSTRIAL', 'PSICOLOGIA',
                                                            'ENGENHARIA DE PRODUÇÃO', 'ENGENHARIA DE CONTROLE E AUTOMAÇÃO',
                                                            'ENGENHARIA DA COMPUTAÇÃO', 'ENGENHARIA DE PETRÓLEO', 'DESIGN',
                                                            'ENGENHARIA', 'DESENHO INDUSTRIAL', 'ZOOTECNIA')  , 'grupoe',
                                   ifelse(novo_curso %in% c('PRODUÇÃO MULTIMÍDIA', 'BIOMEDICINA', 'FARMÁCIA E BIOQUÍMICA',               
                                                            'CIÊNCIA DA COMPUTAÇÃO', 'FÍSICA', 'DIREITO', 'MODA',
                                                            'CIÊNCIAS SOCIAIS', 'QUÍMICA', 'FONOAUDIOLOGIA'), 'grupof',
                                   ifelse(novo_curso %in% c('ARTES VISUAIS', 'JORNALISMO', 'CIÊNCIAS CONTÁBEIS',
                                                            'PROCESSOS QUÍMICOS', 'NUTRIÇÃO', 'FISIOTERAPIA',
                                                            'TEATRO', 'SISTEMAS PARA INTERNET', 'SECRETARIADO EXECUTIVO',
                                                            'ENFERMAGEM', 'COMUNICAÇÃO SOCIAL',  'FILOSOFIA',
                                                            'OUTROS', 'PROPAGANDA E MARKETING', 'SISTEMAS DE INFORMAÇÃO'), 'grupog',
                                   ifelse(novo_curso %in% c('LETRAS - INGLÊS', 'TEOLOGIA', 'GESTÃO PORTUÁRIA',
                                                            'DESIGN GRÁFICO', 'MATEMÁTICA', 'ESTÉTICA',
                                                            'ADMINISTRAÇÃO', 'FOTOGRAFIA', 'LETRAS - ESPANHOL',
                                                            'CIÊNCIAS BIOLÓGICAS', 'AUTOMAÇÃO INDUSTRIAL', 'CINEMA E AUDIOVISUAL'), 'grupoh',
                                   ifelse(novo_curso %in% c('PEDAGOGIA', 'GESTÃO DE TURISMO', 'SEGURANÇA DA INFORMAÇÃO',
                                                            'DESIGN DE INTERIORES', 'HISTÓRIA', 'GESTÃO DA TECNOLOGIA DA INFORMAÇÃO',
                                                            'GESTÃO DA PRODUÇÃO INDUSTRIAL', 'GEOGRAFIA', 'PUBLICIDADE E PROPAGANDA',
                                                            'MECATRÔNICA INDUSTRIAL', 'AGRONEGÓCIO', 'MANUTENÇÃO DE AERONAVES',
                                                            'SECRETARIADO', 'EDUCAÇÃO FÍSICA', 'GASTRONOMIA', 'PRODUÇÃO FONOGRÁFICA', 
                                                            'ANÁLISE E DESENVOLVIMENTO DE SISTEMAS', 'CONSTRUÇÃO DE EDIFÍCIOS', 
                                                            'LETRAS - PORTUGUÊS E ESPANHOL', 'SERVIÇO SOCIAL', 'COMÉRCIO EXTERIOR'), 'grupoi',
                                   ifelse(novo_curso %in% c('PILOTAGEM PROFISSIONAL DE AERONAVES', 'GESTÃO DA QUALIDADE',
                                                            'REDES DE COMPUTADORES', 'GESTÃO PÚBLICA', 'GESTÃO FINANCEIRA',                                                            
                                                            'COMPUTAÇÃO', 'HOTELARIA', 'DESIGN DE MODA'), 'grupoj',
                                   ifelse(novo_curso %in% c('PRODUÇÃO AUDIOVISUAL', 'PETRÓLEO E GÁS', 'LETRAS',
                                                            'MÚSICA', 'EVENTOS', 'RADIOLOGIA', 'GESTÃO COMERCIAL'), 'grupok',
                                   ifelse(novo_curso %in% c('MARKETING', 'GESTÃO DE SEGURANÇA PRIVADA', 'LOGÍSTICA',
                                                            'GESTÃO DE RECURSOS HUMANOS', 'GESTÃO HOSPITALAR', 'GESTÃO AMBIENTAL',
                                                            'PROCESSOS GERENCIAIS', 'PRODUÇÃO PUBLICITÁRIA', 'TURISMO'), 'grupol', 'grupom')))))))))))))
                                                            

base_cat %<>% mutate(qtdsemestre_cat = ifelse(QtdeSemestreContratado >= 0 & QtdeSemestreContratado <= 9,"a", "b"),
                     
                     rendafamiliarmensalbruta_cat = ifelse((RendaFamiliarMensalBruta <= 1000 ),"a", 
                                                    ifelse((RendaFamiliarMensalBruta > 1000 & RendaFamiliarMensalBruta <= 2000),"b",
                                                    ifelse((RendaFamiliarMensalBruta > 2000),"c","0"))),
                     
                     rendapessoalmensalbruta_cat = ifelse((RendaPessoalMensalBruta <= 2000 ),"a", 
                                                   ifelse((RendaPessoalMensalBruta >  2000 & RendaPessoalMensalBruta <= 3000),"b",
                                                   ifelse((RendaPessoalMensalBruta >  3000),"c","0"))),
                     
                     valorfinaciados_cat = ifelse((ValorFinanciadoSISFIES <= 80000 ),"a", 
                                           ifelse((ValorFinanciadoSISFIES > 80000),"b","0")),
                     
                     valorrendafiadoress_cat = ifelse(is.na(ValorRendaComprovadaFiadores),"a",
                                               ifelse(ValorRendaComprovadaFiadores <= 1500,"b", 
                                               ifelse((ValorRendaComprovadaFiadores > 1500 & ValorRendaComprovadaFiadores <= 2500),"c",
                                               ifelse((ValorRendaComprovadaFiadores > 2500),"d", "e")))),
                     
                     idade_cat = ifelse((idade <= 20 ),"b", 
                                 ifelse((idade > 20 ),"a","0")),
                     
                     taxadejuros_cat = ifelse(TaxaDeJuros == "650","b", "a"),
                     
                     garantia_cat = ifelse(TipoGarantia == 'FIANCA CONVENCIONAL+FGEDUC', "d",
                                    ifelse(TipoGarantia %in% c("","FIAN? CONVENCIONAL"),"b",
                                    ifelse(TipoGarantia == "SEM FIAN?","c", "a"))),
                     
                     turno_cat = ifelse(NomeTurno %in% c("INTEGRAL"),"b","a"),
                     
                     ufies_cat = ifelse(Regiao_UF == "Sul", "c",
                                        ifelse(Regiao_UF == "Norte","a", "b")),       
                     
                     estadocivil_cat = ifelse(EstadoCivil == "Solteiro", "b", "a"),
                     
                     curso_cat = ifelse(CodCurso %in% c("5","6"),"c", 
                                 ifelse(CodCurso %in% c("4","7"),"b", "a")),
                     
                     percentual_cat = ifelse((PercentualFinanciado == 10000 ),"a", "b" ),
                     
                     porte_cat = ifelse(Porte == "GM", "GM", "PMM"),
                     
                     escola_cat = ifelse(CursouEnsinoMedioEscolaPublica == "N", "b", "a"),
                     
                     deficiente_cat = ifelse(Deficiente == "N         ", "a", "b")
                     
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


lrcompleto= glm(resposta ~ qtdsemestre_cat + rendafamiliarmensalbruta_cat + 
                  rendapessoalmensalbruta_cat + valorfinaciados_cat + 
                  valorrendafiadoress_cat + idade_cat + taxadejuros_cat + 
                  garantia_cat + ufies_cat + curso_cat + 
                  percentual_cat + porte_cat + escola_cat + novo_curso2
                , family = binomial("logit"), data= train)

stepmodel <- stepAIC(lrcompleto)

summary (stepmodel)

save(file="Z:/CGFIN/CGFIN/Gestão da Carteira/Modelos/logistic_v2.rda", lrcompleto)


lrcompleto = glm(resposta ~ rendafamiliarmensalbruta_cat + rendapessoalmensalbruta_cat + 
                 valorfinaciados_cat + valorrendafiadoress_cat + 
                 taxadejuros_cat + garantia_cat + ufies_cat +  
                 porte_cat + escola_cat + novo_curso2, family = binomial("logit"), 
                 data = train)


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
x <- x[is.na(x)=="FALSE"]
y <- y[is.na(x)=="FALSE"]
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


test <- test %>% mutate(p_30 = ifelse((p <= 0.35),0, 
                               ifelse((p > 0.35) ,1, "erro")))


####MATRIZ DE CONFUSÃO)
confusionMatrix(table(test$p_30,test$resposta), positive = "1")

100*prop.table(table(test$p_30))
100*prop.table(table(test$resposta))




####################################





View(head(base_cat))

## Bivariadas

table(base_cat$Sexo, base_cat$resposta) %>% prop.table(1)
table(base_cat$CursouEnsinoMedioEscolaPublica, base_cat$resposta) %>% prop.table(1)
table(base_cat$Deficiente, base_cat$resposta) %>% prop.table(1)
table(base_cat$EstadoCivil, base_cat$resposta) %>% prop.table(1)
table(base_cat$Prouni, base_cat$resposta) %>% prop.table(1)
table(base_cat$SituacaoInscricao, base_cat$resposta) %>% prop.table(1)
table(base_cat$NomeTurno, base_cat$resposta) %>% prop.table(1)
table(base_cat$UfIES, base_cat$resposta) %>% prop.table(1)
table(base_cat$TipoGarantia, base_cat$resposta) %>% prop.table(1)
table(base_cat$TaxaDeJuros, base_cat$resposta) %>% prop.table(1)
table(base_cat$Regiao_UF, base_cat$resposta) %>% prop.table(1)
table(base_cat$Porte, base_cat$resposta) %>% prop.table(1)
table(base_cat$CodCurso, base_cat$resposta) %>% prop.table(1)
table(base_cat$QtdeSemestreContratado, base_cat$resposta) %>% prop.table(1)



base_cat %<>% mutate(qtdsemestre_cat = ifelse(QtdeSemestreContratado >= 0 & QtdeSemestreContratado <= 9,"a", "b"),
                     
                     rendafamiliarmensalbruta_cat = ifelse((RendaFamiliarMensalBruta <= 1000 ),"a", 
                                                           ifelse((RendaFamiliarMensalBruta > 1000 & RendaFamiliarMensalBruta <= 2000),"b",
                                                                  ifelse((RendaFamiliarMensalBruta > 2000),"c","0"))),
                     
                     rendapessoalmensalbruta_cat = ifelse((RendaPessoalMensalBruta <= 2000 ),"a", 
                                                          ifelse((RendaPessoalMensalBruta >  2000 & RendaPessoalMensalBruta <= 3000),"b",
                                                                 ifelse((RendaPessoalMensalBruta >  3000),"c","0"))),
                     
                     valorfinaciados_cat = ifelse((ValorFinanciadoSISFIES <= 80000 ),"a", 
                                                  ifelse((ValorFinanciadoSISFIES > 80000),"b","0")),
                     
                     valorrendafiadoress_cat = ifelse(is.na(ValorRendaComprovadaFiadores),"a",
                                                      ifelse(ValorRendaComprovadaFiadores <= 1500,"b", 
                                                             ifelse((ValorRendaComprovadaFiadores > 1500 & ValorRendaComprovadaFiadores <= 2500),"c",
                                                                    ifelse((ValorRendaComprovadaFiadores > 2500),"d", "e")))),
                     
                     idade_cat = ifelse((idade <= 20 ),"b", 
                                        ifelse((idade > 20 ),"a","0")),
                     
                     taxadejuros_cat = ifelse(TaxaDeJuros == "6.5","b", "a"),
                     
                     garantia_cat = ifelse(TipoGarantia == 'FIANCA CONVENCIONAL+FGEDUC', "d",
                                           ifelse(TipoGarantia %in% c("","FIAN? CONVENCIONAL"),"c",
                                                  ifelse(TipoGarantia == "SEM FIAN?","b", "a"))),
                     
                     turno_cat = ifelse(NomeTurno %in% c("INTEGRAL"),"b","a"),
                     
                     ufies_cat = ifelse(Regiao_UF == "Sul", "c",
                                        ifelse(Regiao_UF == "Norte","a", "b")),       
                     
                     estadocivil_cat = ifelse(EstadoCivil == "Solteiro", "b", "a"),
                     
                     curso_cat = ifelse(CodCurso %in% c("5","6"),"c", 
                                        ifelse(CodCurso %in% c("4","7"),"b", "a")),
                     
                     percentual_cat = ifelse((PercentualFinanciado == 100 ),"a", "b" ),
                     
                     porte_cat = ifelse(Porte == "GM", "GM", "PMG"),
                     
                     escola_cat = ifelse(CursouEnsinoMedioEscolaPublica == "N", "b", "a"),
                     
                     deficiente_cat = ifelse(Deficiente == "N         ", "a", "b")
                     
)


table(base_cat$qtdsemestre_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$rendafamiliarmensalbruta_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$rendapessoalmensalbruta_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$valorfinaciados_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$valorrendafiadoress_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$idade_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$taxadejuros_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$garantia_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$turno_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$ufies_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$estadocivil_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$curso_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$percentual_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$porte_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$escola_cat, base_cat$resposta) %>% prop.table(1)
table(base_cat$deficiente_cat, base_cat$resposta) %>% prop.table(1)





