esus<-function(){

  
  
  # Carregue as bibliotecas necessárias
  
  # Carregue seus dados em um DataFrame
  # Substitua 'seu_dataframe' pelo nome do seu DataFrame
  
  
  devtools::install_github("https://github.com/cran/klaR/tree/master")
  
  library(readr)
  
  
  # Lista de bibliotecas desejadas
  bibliotecas <- c(
    "tidyverse",
    "caret",
    "flextable",
    "gplots",
    "ggplot2",
    "dplyr",
    "factoextra",
    "FactoMineR",
    "ROSE",
    "Amelia",
    "pROC",
    "neuralnet",
    "ROSE",
    "randomForest",
    "e1071",
    "gridExtra",
    "vcd",
    "lubridate",
    "klaR"
  )
  
  # Remover duplicatas
  bibliotecas_unicas <- unique(bibliotecas)
  
  # Verificar e instalar as bibliotecas ausentes
  bibliotecas_instaladas <- installed.packages()[, "Package"]
  bibliotecas_a_instalar <- bibliotecas_unicas[!bibliotecas_unicas %in% bibliotecas_instaladas]
  
  if (length(bibliotecas_a_instalar) > 0) {
    install.packages(bibliotecas_a_instalar)
  }
  
  # Carregar as bibliotecas
  library(tidyverse)
  library(caret)
  library(flextable)
  library(gplots)
  library(ggplot2)
  library(dplyr)
  library(factoextra)
  library(FactoMineR)
  library(ROSE)
  library(Amelia)
  library(pROC)
  library(neuralnet)
  library(randomForest)
  library(e1071)
  library(gridExtra)
  library(vcd)
  library(lubridate)
  library(klaR)



  library(readr)
r <- read_delim("https://raw.githubusercontent.com/eduardompeixoto/atualiza_esus_notifica/main/inst/planilha_esus.csv",
delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()),
trim_ws = TRUE)

  set.seed(123)
  #r[is.na(r)] <- "99"

  dataset<-r
dataset$testes<-NULL
dataset$testes_list<-NULL


dataset$obito[stringr::str_detect(dataset$evolucaoCaso,"ITO")]<-1
dataset$interna<-0
dataset$interna[dataset$evolucaoCaso=="Internado"]<-1
dataset$cardiopatia<-0
dataset$cardiopatia[stringr::str_detect(dataset$condicoes,"Card")|stringr::str_detect(dataset$condicoes,"iperten")|stringr::str_detect(dataset$condicoes,"HAS")]<-1
dataset$cardiopatia[stringr::str_detect(dataset$outrasCondicoes,"Card")|stringr::str_detect(dataset$outrasCondicoes,"iperten")|stringr::str_detect(dataset$outrasCondicoes,"HAS")]<-1
#dataset$cardiopatia[is.na(dataset$condicoes)]<-0
dataset$obesidade<-0
dataset$obesidade[stringr::str_detect(dataset$condicoes,"Obesidade")]<-1
dataset$obesidade[stringr::str_detect(dataset$outrasCondicoes,"Obesidade")]<-1

#dataset$obesidade[is.na(dataset$condicoes)]<-0
dataset$anosmia<-0
dataset$anosmia[stringr::str_detect(dataset$sintomas,"fativos")]<-1
dataset$anosmia[stringr::str_detect(dataset$outrosSintomas,"fativos")]<-1
dataset$anosmia[stringr::str_detect(dataset$outrosSintomas,"anosmia")]<-1
dataset$anosmia[stringr::str_detect(dataset$outrosSintomas,"cheiro")]<-1

#dataset$anosmia[is.na(dataset$condicoes)]<-0
dataset$dm<-0
dataset$dm[stringr::str_detect(dataset$condicoes,"Diabetes")]<-1
dataset$dm[stringr::str_detect(dataset$outrasCondicoes,"Diabetes")]<-1

#dataset$dm[is.na(dataset$condicoes)]<-0
dataset$rc<-0
dataset$rc[stringr::str_detect(dataset$condicoes,"enais")]<-1
dataset$rc[stringr::str_detect(dataset$outrasCondicoes,"enais")]<-1

#dataset$rc[is.na(dataset$condicoes)]<-0
dataset$drespc<-0
dataset$drespc[stringr::str_detect(dataset$condicoes,"espirat")]<-1
dataset$drespc[stringr::str_detect(dataset$outrasCondicoes,"espirat")]<-1

#dataset$drespc[is.na(dataset$condicoes)]<-0
dataset$gest<-0
dataset$gest[stringr::str_detect(dataset$condicoes,"estante")]<-1
dataset$gest[stringr::str_detect(dataset$outrasCondicoes,"estante")|stringr::str_detect(toupper(dataset$outrasCondicoes),"GRAVIDA")|stringr::str_detect(toupper(dataset$outrasCondicoes),"GRÁVIDA")|stringr::str_detect(toupper(dataset$outrasCondicoes),"GRAVIDEZ")]<-1

#dataset$gest[is.na(dataset$condicoes)]<-0
#dataset$outras_como<-0
#dataset$outras_como[dataset$cardiopatia==0&dataset$obesidade==0&dataset$obesidade==0&dataset$dm==0&dataset$rc==0&dataset$drespc==0&is.na(dataset$condicoes)==F&dataset$gest==0]<-1
#dataset$outras_como[is.na(dataset$condicoes)]<-0
dataset$febre<-0
dataset$febre[stringr::str_detect(dataset$sintomas,"ebre")]<-1
dataset$febre[stringr::str_detect(toupper(dataset$outrosSintomas),"FEBRE")]<-1

#dataset$febre[is.na(dataset$sintomas)]<-NA
dataset$coriza<-0
dataset$coriza[stringr::str_detect(dataset$sintomas,"Coriza")]<-1
dataset$coriza[stringr::str_detect(toupper(dataset$outrosSintomas),"CORIZA")]<-1

#dataset$coriza[is.na(dataset$sintomas)]<-NA
dataset$tosse<-0
dataset$tosse[stringr::str_detect(dataset$sintomas,"Tosse")]<-1
dataset$tosse[stringr::str_detect(toupper(dataset$outrosSintomas),"TOSSE")]<-1

#dataset$tosse[is.na(dataset$sintomas)]<-NA
dataset$dispneia<-0
dataset$dispneia[stringr::str_detect(dataset$sintomas,"Dispneia")]<-1
dataset$dispneia[stringr::str_detect(toupper(dataset$outrosSintomas),"DISPNEIA")]<-1
dataset$dispneia[stringr::str_detect(toupper(dataset$outrosSintomas),"DISPNÉIA")]<-1
dataset$dispneia[stringr::str_detect(toupper(dataset$outrosSintomas),"DISPNÉIA")]<-1

dataset$assintomatico<-0
dataset$assintomatico[stringr::str_detect(dataset$sintomas,"Assintomático")]<-1

#dataset$dispneia[is.na(dataset$sintomas)]<-NA
dataset$garganta<-0
dataset$garganta[stringr::str_detect(dataset$sintomas,"Garganta")]<-1
#dataset$garganta[is.na(dataset$sintomas)]<-NA
dataset$cefaleia<-0
dataset$cefaleia[stringr::str_detect(dataset$sintomas,"Cabe")]<-1
#dataset$cefaleia[is.na(dataset$sintomas)]<-NA
dataset$outro_sin<-0
dataset$outro_sin[stringr::str_detect(dataset$sintomas,"Outro")]<-1
#dataset$outro_sin[is.na(dataset$sintomas)]<-NA



dataset$dataInicioSintomas<-ex_date(dataset$dataInicioSintomas)
dataset$dataInicioSintomas<-str_replace(dataset$dataInicioSintomas,"20-","2020-")
dataset$dataInicioSintomas<-str_replace(dataset$dataInicioSintomas,"21-","2021-")
dataset$dataInicioSintomas<-str_replace(dataset$dataInicioSintomas,"22-","2022-")
dataset$dataInicioSintomas<-str_replace(dataset$dataInicioSintomas,"23-","2023-")
dataset$ano_sin<-lubridate::year((dataset$dataInicioSintomas))
dataset$mes_sin<-lubridate::month(dataset$dataInicioSintomas)
dataset$capital<-0
dataset$capital[stringr::str_detect(dataset$municipio,"aneiro")]<-1
#dataset$capital[is.na(dataset$municipio)]<-NA
dataset$recebeuVacina<- (as.character(dataset$codigoDosesVacina))
dataset$recebeuVacina[str_detect(dataset$recebeuVacina,"5")]<-5
dataset$recebeuVacina[str_detect(dataset$recebeuVacina,"4")]<-4
dataset$recebeuVacina[str_detect(dataset$recebeuVacina,"3")]<-3
dataset$recebeuVacina[str_detect(dataset$recebeuVacina,"2")]<-2
dataset$recebeuVacina[str_detect(dataset$recebeuVacina,"1")]<-1
dataset$recebeuVacina[str_detect(dataset$recebeuVacina,"0")]<-0
dataset$recebeuVacina[as.character(dataset$codigoRecebeuVacina)==2]<-0
dataset$recebeuVacina[dataset$recebeuVacina=="list()"]<-NA
dataset$sexo[dataset$sexo=="Indefinido"]<-NA
dataset$sexo[dataset$sexo=="Nao informar"]<-NA
dataset$sexo[dataset$sexo=="Feminino"]<-0
dataset$sexo[dataset$sexo=="Masculino"]<-1


dataset[ , colnames(dataset)] <- lapply(dataset[ , colnames(dataset)], as.character)


a<-subset(dataset, select = c(id,tipoTeste1,tipoTeste2,tipoTeste3,dataTeste1,dataTeste2,dataTeste3,resultado1,resultado2,resultado3,estado1,estado2,estado3,sexo,outrosSintomas,condicoes,outrasCondicoes,estadoNotificacao,evolucaoCaso,municipio,classificacaoFinal,estado,sintomas,dataInicioSintomas,idade,estrangeiro,profissionalSaude,municipioNotificacaoIBGE,racaCor,dataNotificacao,n_testes,obito,interna,cardiopatia,obesidade,anosmia,dm,rc,drespc,gest,febre,coriza,tosse,dispneia,assintomatico,garganta,cefaleia,outro_sin,ano_sin,mes_sin,capital,recebeuVacina))

a$tipoTeste1[str_detect(a$tipoTeste1,"ANT")]<-"ANTÍGENO"
a$tipoTeste2[str_detect(a$tipoTeste2,"ANT")]<-"ANTÍGENO"
a$tipoTeste3[str_detect(a$tipoTeste3,"ANT")]<-"ANTÍGENO"
a$tipoTeste1[str_detect(a$tipoTeste1,"PCR")]<-"PCR"
a$tipoTeste2[str_detect(a$tipoTeste2,"PCR")]<-"PCR"
a$tipoTeste3[str_detect(a$tipoTeste3,"PCR")]<-"PCR"
a$tipoTeste1[str_detect(a$tipoTeste1,"SORO")]<-"SOROLOGIA"
a$tipoTeste2[str_detect(a$tipoTeste2,"SORO")]<-"SOROLOGIA"
a$tipoTeste3[str_detect(a$tipoTeste3,"SORO")]<-"SOROLOGIA"
a$tipoTeste1[str_detect(a$tipoTeste1,"LAMP")]<-"RT-LAMP"
a$tipoTeste2[str_detect(a$tipoTeste2,"LAMP")]<-"RT-LAMP"
a$tipoTeste3[str_detect(a$tipoTeste3,"LAMP")]<-"RT-LAMP"

a$dataTeste1<-ex_date(a$dataTeste1)
a$dataTeste1<-str_replace(a$dataTeste1,"20-","2020-")
a$dataTeste1<-str_replace(a$dataTeste1,"21-","2021-")
a$dataTeste1<-str_replace(a$dataTeste1,"22-","2022-")
a$dataTeste1<-str_replace(a$dataTeste1,"23-","2023-")

a$dataTeste2<-ex_date(a$dataTeste2)
a$dataTeste2<-str_replace(a$dataTeste2,"20-","2020-")
a$dataTeste2<-str_replace(a$dataTeste2,"21-","2021-")
a$dataTeste2<-str_replace(a$dataTeste2,"22-","2022-")
a$dataTeste2<-str_replace(a$dataTeste2,"23-","2023-")

a$dataTeste3<-ex_date(a$dataTeste3)
a$dataTeste3<-str_replace(a$dataTeste3,"20-","2020-")
a$dataTeste3<-str_replace(a$dataTeste3,"21-","2021-")
a$dataTeste3<-str_replace(a$dataTeste3,"22-","2022-")
a$dataTeste3<-str_replace(a$dataTeste3,"23-","2023-")

a$dataNotificacao<-as.Date(a$dataNotificacao)
a$obito<-NULL
a$evolucaoCaso<-NULL

a$resultado1[str_detect(a$resultado1,"Detec")|str_detect(a$resultado1,'resultadoTeste = "Reagente"')]<-"positivo"
a$resultado1[str_detect(a$resultado1,"Não")]<-"negativo"
a$resultado1[str_detect(a$resultado1,"Incon")]<-"Inconclusivo"
a$resultado1[str_detect(a$resultado1,"NULL")]<-"NA"

a$resultado2[str_detect(a$resultado2,"Detec")|str_detect(a$resultado2,'resultadoTeste = "Reagente"')]<-"positivo"
a$resultado2[str_detect(a$resultado2,"Não")]<-"negativo"
a$resultado2[str_detect(a$resultado2,"Incon")]<-"Inconclusivo"
a$resultado2[str_detect(a$resultado2,"NULL")]<-"NA"

a$resultado3[str_detect(a$resultado3,"Detec")|str_detect(a$resultado3,'resultadoTeste = "Reagente"')]<-"positivo"
a$resultado3[str_detect(a$resultado3,"Não")]<-"negativo"
a$resultado3[str_detect(a$resultado3,"Incon")]<-"Inconclusivo"
a$resultado3[str_detect(a$resultado3,"NULL")]<-"NA"

a$estado1<-str_replace(a$estado1,'estadoTeste = ','' )
a$estado1<-str_replace(a$estado1,'\\,',"" )
a$estado1<-str_replace(a$estado1,'\\"',"" )
a$estado1<-str_replace(a$estado1,'\\"' ,"" )

a$estado2<-str_replace(a$estado2,'estadoTeste = ','' )
a$estado2<-str_replace(a$estado2,'\\,',"" )
a$estado2<-str_replace(a$estado2,'\\"',"" )
a$estado2<-str_replace(a$estado2,'\\"' ,"" )

a$estado3<-str_replace(a$estado3,'estadoTeste = ','' )
a$estado3<-str_replace(a$estado3,'\\,',"" )
a$estado3<-str_replace(a$estado3,'\\"',"" )
a$estado3<-str_replace(a$estado3,'\\"' ,"" )

a[ , colnames(a)] <- lapply(a[ , colnames(a)], as.character)


a[a=='NULL']<-NA
a[a==""]<-NA

a1<-subset(a,select=c(id,tipoTeste1,dataTeste1,resultado1,estado1,sexo,outrosSintomas,condicoes,outrasCondicoes,estadoNotificacao,municipio,classificacaoFinal,estado,sintomas,dataInicioSintomas,idade,estrangeiro,profissionalSaude,municipioNotificacaoIBGE,racaCor,dataNotificacao,n_testes,interna,cardiopatia,obesidade,anosmia,dm,rc,drespc,gest,febre,coriza,tosse,dispneia,assintomatico,garganta,cefaleia,outro_sin,ano_sin,mes_sin,capital,recebeuVacina))
a2<-subset(a,select=c(id,tipoTeste2,dataTeste2,resultado2,estado2,sexo,outrosSintomas,condicoes,outrasCondicoes,estadoNotificacao,municipio,classificacaoFinal,estado,sintomas,dataInicioSintomas,idade,estrangeiro,profissionalSaude,municipioNotificacaoIBGE,racaCor,dataNotificacao,n_testes,interna,cardiopatia,obesidade,anosmia,dm,rc,drespc,gest,febre,coriza,tosse,dispneia,assintomatico,garganta,cefaleia,outro_sin,ano_sin,mes_sin,capital,recebeuVacina))
a3<-subset(a,select=c(id,tipoTeste3,dataTeste3,resultado3,estado3,sexo,outrosSintomas,condicoes,outrasCondicoes,estadoNotificacao,municipio,classificacaoFinal,estado,sintomas,dataInicioSintomas,idade,estrangeiro,profissionalSaude,municipioNotificacaoIBGE,racaCor,dataNotificacao,n_testes,interna,cardiopatia,obesidade,anosmia,dm,rc,drespc,gest,febre,coriza,tosse,dispneia,assintomatico,garganta,cefaleia,outro_sin,ano_sin,mes_sin,capital,recebeuVacina))

colnames(a1)<-c('id','tipoTeste','dataTeste','resultado','estado','sexo','outrosSintomas','condicoes','outrasCondicoes','estadoNotificacao','municipio','classificacaoFinal','estado','sintomas','dataInicioSintomas','idade','estrangeiro','profissionalSaude','municipioNotificacaoIBGE','racaCor','dataNotificacao','n_testes','interna','cardiopatia','obesidade','anosmia','dm','rc','drespc','gest','febre','coriza','tosse','dispneia','assintomatico','garganta','cefaleia','outro_sin','ano_sin','mes_sin','capital','recebeuVacina')
colnames(a2)<-c('id','tipoTeste','dataTeste','resultado','estado','sexo','outrosSintomas','condicoes','outrasCondicoes','estadoNotificacao','municipio','classificacaoFinal','estado','sintomas','dataInicioSintomas','idade','estrangeiro','profissionalSaude','municipioNotificacaoIBGE','racaCor','dataNotificacao','n_testes','interna','cardiopatia','obesidade','anosmia','dm','rc','drespc','gest','febre','coriza','tosse','dispneia','assintomatico','garganta','cefaleia','outro_sin','ano_sin','mes_sin','capital','recebeuVacina')
colnames(a3)<-c('id','tipoTeste','dataTeste','resultado','estado','sexo','outrosSintomas','condicoes','outrasCondicoes','estadoNotificacao','municipio','classificacaoFinal','estado','sintomas','dataInicioSintomas','idade','estrangeiro','profissionalSaude','municipioNotificacaoIBGE','racaCor','dataNotificacao','n_testes','interna','cardiopatia','obesidade','anosmia','dm','rc','drespc','gest','febre','coriza','tosse','dispneia','assintomatico','garganta','cefaleia','outro_sin','ano_sin','mes_sin','capital','recebeuVacina')

a<-rbind(a1,a2,a3)
a$classificacaoFinal<-NULL

a<-subset(a,is.na(a$dataTeste)| a$dataTeste > Sys.Date()-7)
a<-distinct(a)
a[a=="NA"]<-NA
a$resultado[a$resultado=="Inconclusivo"]<-NA

a$racaCor[a$racaCor=="Amarela"]<-3
a$racaCor[a$racaCor=="Branca"]<-1
a$racaCor[a$racaCor=="Branco"]<-1
a$racaCor[a$racaCor=="Ignorado"]<-NA
a$racaCor[a$racaCor=="Parda"]<-4
a$racaCor[a$racaCor=="Preta"]<-2
a$racaCor[str_detect(a$racaCor,"Ind")]<-5
a$racaCor<-as.character(a$racaCor)

a$confirmado<-'0'
a$confirmado[a$resultado=="positivo"&a$tipoTeste=="ANTÍGENO"]<-'1'
a$confirmado[a$resultado=="positivo"&a$tipoTeste=="PCR"]<-'1'
a$confirmado[a$resultado=="positivo"&a$tipoTeste=="RT-LAMP"]<-'1'
a<-arrange(a,desc(a$resultado))
a<-arrange(a,desc(a$confirmado))
a<-distinct(a,a$id,.keep_all = T)
a$confirmado<-as.numeric(a$confirmado)
a$idade<-as.numeric(a$idade)
a$semvacina<-a$recebeuVacina
a$semvacina[a$recebeuVacina>=1]<-'0'
a$semvacina[a$recebeuVacina<1]<-'1'


r<- subset(a,select = c(sexo,dataInicioSintomas,idade,estrangeiro,profissionalSaude,racaCor,cardiopatia,obesidade,anosmia,dm,rc,drespc,gest,febre,coriza,tosse,dispneia,assintomatico,garganta,cefaleia,capital,confirmado,semvacina,recebeuVacina))
r$raca2<-r$racaCor
r$raca2[r$racaCor!="1"]<-"Não branca"
r$raca2[r$racaCor=="1"]<-"Branca"
  
  r<- read_delim("https://raw.githubusercontent.com/eduardompeixoto/atualiza_esus_notifica/main/inst/planilha_esus.csv", 
                 delim = ";", escape_double = FALSE, col_types = cols(dataInicioSintomas = col_date(format = "%Y-%m-%d")), 
                 trim_ws = TRUE)
  
  
  #r <- ovun.sample(confirmado~., data=r, method = "over")$data
  
  
  r<-r %>% mutate(  idade= case_when(idade <= 1 ~ "Até 1",
                                     idade > 1 & idade < 4 ~ "1-4",
                                     idade >= 5 & idade < 9 ~ "5-9",
                                     idade >= 10 & idade < 19 ~ "10-19",
                                     idade >= 20 & idade < 29 ~ "20-29",
                                     idade >= 30 & idade < 39 ~ "30-39",
                                     idade >= 40 & idade < 49 ~ "40-49",
                                     idade >= 50 & idade < 59 ~ "50-59",
                                     idade >= 60 & idade < 69 ~ "60-69",
                                     idade >= 70 & idade < 79 ~ "70-79",
                                     idade >= 80 ~ "80+"))
  
  
  r$semvacina<-NULL
  
  seu_dataframe <- r
  #seu_dataframe$dataInicioSintomas[seu_dataframe$dataInicioSintomas == "99"] <- NA
  
  
  
  # Calcule a semana epidemiológica a partir da data
  seu_dataframe$dataInicioSintomas <- as.Date(seu_dataframe$dataInicioSintomas)
  seu_dataframe$semanaEpidemiologica <- week(seu_dataframe$dataInicioSintomas)
  
  seu_dataframe<-subset(seu_dataframe,seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-6|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-5|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-4|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-3|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-2|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-1)
  
  seu_dataframe <- na.omit(seu_dataframe)
  
  # Divida seus dados em dois subconjuntos (confirmado 0 e 1)
  dados_confirmados_0 <- seu_dataframe %>% filter(confirmado == 0)
  dados_confirmados_1 <- seu_dataframe %>% filter(confirmado == 1)
  
  # Aplique k-modes clustering aos subconjuntos de dados
  # Defina o número de modas (clusters) desejado
  k_modas <- 1
  
  # Exemplo de k-modes clustering para confirmados 0
  set.seed(123)  # Defina uma semente para reproducibilidade
  modelo_cluster_0 <- klaR::kmodes(dados_confirmados_0, modes = k_modas)
  dados_confirmados_0$cluster <- modelo_cluster_0$cluster
  
  # Exemplo de k-modes clustering para confirmados 1
  set.seed(123)  # Defina uma semente para reproducibilidade
  modelo_cluster_1 <- klaR::kmodes(dados_confirmados_1, modes = k_modas)
  dados_confirmados_1$cluster <- modelo_cluster_1$cluster
  
  # Contagem manual dos clusters
  contagem_clusters_0 <- table(dados_confirmados_0$cluster)
  contagem_clusters_1 <- table(dados_confirmados_1$cluster)
  
  
  
  
  # Função para calcular a moda de um vetor
  Mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }
  
  # Para o subconjunto de confirmados 0
  moda_clusters_0 <- dados_confirmados_0 %>%
    group_by(cluster) %>%
    summarise_all(Mode)
  
  # Para o subconjunto de confirmados 1
  moda_clusters_1 <- dados_confirmados_1 %>%
    group_by(cluster) %>%
    summarise_all(Mode)
  
  # Visualização das modas em gráficos de barras
  # Gráfico para confirmados 0
  
  moda_clusters_0[ , colnames(moda_clusters_0)] <- lapply(moda_clusters_0[ , colnames(moda_clusters_0)], as.character)
  
  
  moda_clusters_0$dataInicioSintomas<-NULL
  moda_clusters_0$semanaEpidemiologica<-NULL
  moda_clusters_0$confirmado<-as.character(moda_clusters_0$confirmado)
  moda_clusters_0_long <- tidyr::pivot_longer(moda_clusters_0, cols = -cluster)
  
  
  moda_clusters_1[ , colnames(moda_clusters_1)] <- lapply(moda_clusters_1[ , colnames(moda_clusters_1)], as.character)
  
  moda_clusters_1$dataInicioSintomas<-NULL
  moda_clusters_1$semanaEpidemiologica<-NULL
  moda_clusters_1$confirmado<-as.character(moda_clusters_1$confirmado)
  moda_clusters_1_long <- tidyr::pivot_longer(moda_clusters_1, cols = -cluster)
  
  dados_confirmados_0$cluster<-0
  
  dados_combinados <- bind_rows(dados_confirmados_0, dados_confirmados_1)
  
  moda_clusters_combinados <- dados_combinados %>%
    group_by(semanaEpidemiologica, confirmado) %>%
    summarise_all(Mode)
  
  moda_clusters_combinados[ , colnames(moda_clusters_combinados)] <- lapply(moda_clusters_combinados[ , colnames(moda_clusters_combinados)], as.character)
  
  
  moda_clusters_combinados$dataInicioSintomas<-NULL
  moda_clusters_combinados$confirmado<-as.character(moda_clusters_combinados$confirmado)
  moda_clusters_combinados_long <- tidyr::pivot_longer(moda_clusters_combinados, cols = -c(semanaEpidemiologica, cluster))
  
  
  
  moda_clusters_combinados_long$valor<-paste(moda_clusters_combinados_long$name,moda_clusters_combinados_long$value)
  
  
  moda_clusters_combinados_long$cluster<-as.factor(moda_clusters_combinados_long$cluster)
  moda_clusters_combinados_long$semanaEpidemiologica<-as.factor(moda_clusters_combinados_long$semanaEpidemiologica)
  
  
  # Função para calcular a moda de um vetor
  Mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }
  
  # Calcule a moda das variáveis em cada combinação de semana epidemiológica e estado de confirmação
  moda_por_semana <- moda_clusters_combinados %>%
    group_by(semanaEpidemiologica, confirmado) %>%
    summarise_all(Mode) %>%
    ungroup()
  
  moda_clusters_combinados_long$name<-NULL
  moda_clusters_combinados_long$value<-NULL
  
  # Load the vcd library
  
  
  # Create a data frame with your categorical data
  data <- moda_clusters_combinados_long
  
  result <- anti_join(subset(data,data$cluster==1),subset(data,data$cluster==0), by = c("semanaEpidemiologica", "valor"))
  
  result<-subset(result,result$valor!="confirmado 1")
  result<-subset(result,result$valor!="confirmado 0")
  
  result2 <- anti_join(subset(data,data$cluster==0),subset(data,data$cluster==1), by = c("semanaEpidemiologica", "valor"))
  
  result2<-subset(result2,result2$valor!="confirmado 1")
  result2<-subset(result2,result2$valor!="confirmado 0")
  
  
  
  
  result$cluster<-"confirmado"
  result2$cluster<-"não confirmado"
  
  result<-subset(result,stringr::str_detect(result$valor,"racaCor")==F)
  result2<-subset(result2,stringr::str_detect(result2$valor,"racaCor")==F)

  # Seu código para o primeiro gráfico com coordenadas invertidas
  bar_chart <- ggplot(result, aes(x = semanaEpidemiologica, y = 1, fill = as.factor(valor))) +
    geom_bar(stat = "identity", position = "dodge", color = "white", fill = "white") +
    coord_flip() +  # Invert the coordinates
    labs(title = "", x = "", y = "Semana") +  # Suprimir o eixo X e configurar o eixo Y
    geom_text(aes(label = valor, color = "black"), position = position_dodge(width = 1), vjust = 0.4, hjust = 3.5) +  # Add value labels with colored text
    scale_color_manual(values = viridis::inferno(length(unique(result$valor)))) +  # Define text color scale
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),  # Suprimir rótulos do eixo X
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),  # Suprimir título do eixo X
          plot.background=element_blank(),
          legend.background = element_blank(),
          legend.position = "none")
  
  # # Seu código para o segundo gráfico com coordenadas invertidas
  # bar_chart2 <- ggplot(result2, aes(x = semanaEpidemiologica, y = 1, fill = as.factor(valor))) +
  #   geom_bar(stat = "identity", position = "dodge", color = "white", fill = "white") +
  #   coord_flip() +  # Invert the coordinates
  #   labs(title = "", x = "", y = "Semana") +  # Suprimir o eixo X e configurar o eixo Y
  #   geom_text(aes(label = valor, color = "black"), position = position_dodge(width = 1), vjust = 0.4, hjust = 3.5) +  # Add value labels with colored text
  #   scale_color_manual(values = viridis::inferno(length(unique(result2$valor)))) +  # Define text color scale
  #   theme(axis.line=element_blank(),
  #         axis.text.x=element_blank(),  # Suprimir rótulos do eixo X
  #         axis.ticks=element_blank(),
  #         axis.title.x=element_blank(),  # Suprimir título do eixo X
  #         plot.background=element_blank(),
  #         legend.background = element_blank(),
  #         legend.position = "none")  # Suprimir o título do eixo Y
  # require(cowplot )
  # Use grid.arrange para organizar os gráficos lado a lado
  
  # Use grid.arrange para organizar os gráficos lado a lado com o título
  
  

  
  
  
  bar_chart
  


  
  }
  
