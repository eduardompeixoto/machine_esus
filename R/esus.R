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
  
  set.seed(123)
  #r[is.na(r)] <- "99"
  
  r<- read_delim("https://raw.githubusercontent.com/eduardompeixoto/atualiza_esus_notifica/main/inst/planilha_esus.csv", 
                 delim = ";", escape_double = FALSE, col_types = cols(dataInicioSintomas = col_date(format = "%Y-%m-%d")), 
                 trim_ws = TRUE)
  
  r$...1<-NULL
  
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
  
  seu_dataframe<-subset(seu_dataframe,seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-4|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-3|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-2|seu_dataframe$semanaEpidemiologica==lubridate::epiweek(Sys.Date())-1)
  
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
  
