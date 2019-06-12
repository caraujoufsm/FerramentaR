#install.packages("highcharter")
#install.packages("htmltools")
#install.packages("magrittr")
library(rgdal)
library(magrittr)
library(shiny)
library(leaflet)
library(stringr)
library(ggplot2)
library(gridExtra)
library(highcharter)
library(htmltools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)

# Carregamento dos dados
dados2017 <- read.csv2("data\\PRF2017RS.csv", sep = ";", encoding = "UTF-8")
dados2018 <- read.csv2("data\\PRF2018RS.csv", sep = ";", encoding = "UTF-8")
dados2019 <- read.csv2("data\\PRF2019RS.csv", sep = ";", encoding = "UTF-8")

# Eliminacao das colunas que nao serao utilizadas
dados2018$uop <- NULL
dados2018$delegacia <- NULL
dados2018$regional <- NULL

dados2019$uop <- NULL
dados2019$delegacia <- NULL
dados2019$regional <- NULL

# Separacao de acidentes por condutor(contagem total de acidentes) e passageiros envolvidos
dados2017Veiculos <- dados2017[dados2017$tipo_envolvido == "Condutor",]
dados2017Veiculos <- unique(dados2017Veiculos)

dados2018Veiculos <- dados2018[dados2018$tipo_envolvido == "Condutor",]
dados2018Veiculos <- unique(dados2018Veiculos)

dados2019Veiculos <- dados2019[dados2019$tipo_envolvido == "Condutor",]
dados2019Veiculos <- unique(dados2019Veiculos)

dados2017Passageiros <- dados2017[dados2017$tipo_envolvido == "Passageiro",]
dados2017Passageiros <- unique(dados2017Passageiros)

dados2018Passageiros <- dados2018[dados2018$tipo_envolvido == "Passageiro",]
dados2018Passageiros <- unique(dados2018Passageiros)

dados2019Passageiros <- dados2019[dados2019$tipo_envolvido == "Passageiro",]
dados2019Passageiros <- unique(dados2019Passageiros)

# Agrupar os acidentes por terceiros envolvidos
TotalAcidentesTerceiros <- rbind(dados2017Passageiros,dados2018Passageiros,dados2019Passageiros)
# Selecao apenas dos acidentes que passageiros morreram
TotalAcidentesTerceiros <- TotalAcidentesTerceiros[TotalAcidentesTerceiros$mortos == 1,]
TotalAcidentesTerceiros <- TotalAcidentesTerceiros[TotalAcidentesTerceiros$ordem_tipo_acidente == 1,]
TotalAcidentesTerceiros <- TotalAcidentesTerceiros[!duplicated(TotalAcidentesTerceiros$pesid),]

# Agrupar todos os acidentes em um unico dataframe
TotalAcidentes <- rbind(dados2017Veiculos,dados2018Veiculos,dados2019Veiculos)
TotalAcidentes <- unique(TotalAcidentes)
TotalAcidentes <- TotalAcidentes[TotalAcidentes$idade > 0,]
TotalAcidentes <- TotalAcidentes[TotalAcidentes$idade < 100,]
TotalAcidentes <- na.omit(TotalAcidentes)

# Adição de mes no dataframe do total de acidentes
TotalAcidentes$mes <- "mes" 
for (i in 1:22662) {
  mes <- str_sub(TotalAcidentes$data_inversa[i], start = 6, end = -4)
  if(mes == "01")
  {
    TotalAcidentes$mes[i] <- "01-Janeiro"
  }else if(mes == "02")
  {
    TotalAcidentes$mes[i] <- "02-Fevereiro"
  }else if(mes == "03")
  {
    TotalAcidentes$mes[i] <- "03-Marco"
  }else if(mes == "04")
  {
    TotalAcidentes$mes[i] <- "04-Abril"
  }else if(mes == "05")
  {
    TotalAcidentes$mes[i] <- "05-Maio"
  }else if(mes == "06")
  {
    TotalAcidentes$mes[i] <- "06-Junho"
  }else if(mes == "07")
  {
    TotalAcidentes$mes[i] <- "07-Julho"
  }else if(mes == "08")
  {
    TotalAcidentes$mes[i] <- "08-Agosto"
  }else if(mes == "09")
  {
    TotalAcidentes$mes[i] <- "09-Setembro"
  }else if(mes == "10")
  {
    TotalAcidentes$mes[i] <- "10-Outubro"
  }else if(mes == "11")
  {
    TotalAcidentes$mes[i] <- "11-Novembro"
  }else if(mes == "12")
  {
    TotalAcidentes$mes[i] <- "12-Dezembro"
  }else
  {
    TotalAcidentes$mes[i] <- "Valor Invalido"
  }
}
TotalAcidentes$mes <- as.factor(TotalAcidentes$mes)

# Verificacao de acidentes por ano
TotalAcidentes$ano <- "ano"
for (i in 1:22662) {
  ano <- str_sub(TotalAcidentes$data_inversa[i], end = -7, start = 3)
  if(ano == "17")
  {
    TotalAcidentes$ano[i] <- "2017"
  }else if(ano == "18")
  {
    TotalAcidentes$ano[i] <- "2018"
  }
  else if(ano =="19")
  {
    TotalAcidentes$ano[i] <- "2019"
  }else{
    TotalAcidentes$ano[i] <- "Ano invalido"
  }
}

# Trocar a forma de verdadeiro e falso do codigo, sendo 1 = sim e 0 = nao
for(i in 1:22662){ 
  if(TotalAcidentes$ilesos[i] == 1){
    TotalAcidentes$ilesos[i] <- 'Sim'
  }else {
    TotalAcidentes$ilesos[i] <- 'Não'
  }
}

#Adicao da coluna feriado
TotalAcidentes$feriado <- 'Nao'
#Verificacao de Ocorrências nos Feriados Federais
feriados <- list("2017-01-01","2017-04-14","2017-04-16","2017-04-21","2017-05-01","2017-09-07","2017-10-12","2017-11-02","2017-11-15","2017-12-25",
                 "2018-01-01","2018-03-30","2018-04-01","2018-04-21","2018-05-01","2018-09-07","2018-10-12","2018-11-02","2018-11-15","2018-12-25","2019-01-01")

vespera <- list("2017-04-13","2017-04-15","2017-04-20","2017-04-30","2017-09-06","2017-10-11","2017-11-01","2017-11-14","2017-12-24",
                "2017-12-31","2018-03-29","2018-03-31","2018-04-20","2018-04-30","2018-09-06","2018-10-11","2018-11-01","2018-11-14","2018-12-24","2018-12-31")

for (i in 1:22662) {
  if(TotalAcidentes$data_inversa[i] %in% feriados)
  {
    TotalAcidentes$feriado[i] <- 'Sim'
  }
  else if(TotalAcidentes$data_inversa[i] %in% vespera)
  {
    TotalAcidentes$feriado[i] <- 'Vespera'
  }
}


# Total de Acidentes causados pelo Álcool
TotalAcidentesLeiSeca <- TotalAcidentes[TotalAcidentes$causa_acidente == "Ingestão de Álcool",]
TotalAcidentesLeiSeca <- TotalAcidentesLeiSeca[TotalAcidentesLeiSeca$ordem_tipo_acidente == 1,]
TotalAcidentesLeiSeca <- TotalAcidentesLeiSeca[!duplicated(TotalAcidentesLeiSeca$pesid),]

# Adicao da coluna total de obitos que ocorreram no veiculo
TotalAcidentesLeiSeca$total_obitos <- TotalAcidentesLeiSeca$mortos

for(j in 1:187){
  for (i in 1:1134) {
    if(TotalAcidentesTerceiros$id[j] == TotalAcidentesLeiSeca$id[i])
    {
      if(TotalAcidentesTerceiros$id_veiculo[j] == TotalAcidentesLeiSeca$id_veiculo[i])
      {
        TotalAcidentesLeiSeca$total_obitos[i] <- TotalAcidentesLeiSeca$total_obitos[i] + 1
      }
    }
  }
}

# Criacao do Icone para as mortes
cavIcon = makeIcon(iconUrl = "https://cdn4.iconfinder.com/data/icons/security-overcolor/512/death-512.png",
                   iconWidth = 20, iconHeight = 20)
# Total de Acidentes para o maio amarelo
TotalAcidentesMaio <- TotalAcidentes[TotalAcidentes$mes == "05-Maio",]
TotalAcidentesMaio <- TotalAcidentesMaio[TotalAcidentesMaio$ordem_tipo_acidente == 1,]
TotalAcidentesMaio <-TotalAcidentesMaio[!duplicated(TotalAcidentesMaio$pesid),]

# Adicao da coluna total de obitos que ocorreram no veiculo
TotalAcidentesMaio$total_obitos <- TotalAcidentesMaio$mortos

for(j in 1:187){
  for (i in 1:1273) {
    if(TotalAcidentesTerceiros$id[j] == TotalAcidentesMaio$id[i])
    {
      if(TotalAcidentesTerceiros$id_veiculo[j] == TotalAcidentesMaio$id_veiculo[i])
      {
        TotalAcidentesMaio$total_obitos[i] <- TotalAcidentesMaio$total_obitos[i] + 1
      }
    }
  }
}

# Total de Acidentes para a epoca de veraneio
TotalAcidentesDezembro <- TotalAcidentes[TotalAcidentes$mes == "12-Dezembro",]
TotalAcidentesJaneiro <- TotalAcidentes[TotalAcidentes$mes == "01-Janeiro",]
TotalAcidentesFevereiro <- TotalAcidentes[TotalAcidentes$mes == "02-Fevereiro",]

TotalAcidentesVeraneio <- rbind(TotalAcidentesJaneiro,TotalAcidentesFevereiro,TotalAcidentesDezembro)
TotalAcidentesVeraneio <- unique(TotalAcidentesVeraneio)
TotalAcidentesVeraneio <- TotalAcidentesVeraneio[TotalAcidentesVeraneio$ordem_tipo_acidente == 1,]
TotalAcidentesVeraneio <-TotalAcidentesVeraneio[!duplicated(TotalAcidentesVeraneio$pesid),]

# Adicao da coluna total de obitos que ocorreram no veiculo
TotalAcidentesVeraneio$total_obitos <- TotalAcidentesVeraneio$mortos

for(j in 1:187){
  for (i in 1:4659) {
    if(TotalAcidentesTerceiros$id[j] == TotalAcidentesVeraneio$id[i])
    {
      if(TotalAcidentesTerceiros$id_veiculo[j] == TotalAcidentesVeraneio$id_veiculo[i])
      {
        TotalAcidentesVeraneio$total_obitos[i] <- TotalAcidentesVeraneio$total_obitos[i] + 1
      }
    }
  }
}

#Passagem das colunas latitude e longitude para numeric(necessario para o mapa)
TotalAcidentesMaio$latitude <- as.numeric(as.character(TotalAcidentesMaio$latitude))
TotalAcidentesMaio$longitude <- as.numeric(as.character(TotalAcidentesMaio$longitude))
TotalAcidentesLeiSeca$latitude <- as.numeric(as.character(TotalAcidentesLeiSeca$latitude))
TotalAcidentesLeiSeca$longitude <- as.numeric(as.character(TotalAcidentesLeiSeca$longitude))
TotalAcidentesVeraneio$latitude <- as.numeric(as.character(TotalAcidentesVeraneio$latitude))
TotalAcidentesVeraneio$longitude <- as.numeric(as.character(TotalAcidentesVeraneio$longitude))

# Criacao dos dataframe com morte e sem morte
TotalAcidentesLeiSeca <- na.omit(TotalAcidentesLeiSeca)
TotalAcidentesLeiSecaComMortes <- TotalAcidentesLeiSeca[TotalAcidentesLeiSeca$total_obitos >= 1,]
TotalAcidentesLeiSecaSemMortes <- TotalAcidentesLeiSeca[TotalAcidentesLeiSeca$total_obitos == 0,]

TotalAcidentesMaio <- na.omit(TotalAcidentesMaio)
TotalAcidentesMaioComMortes <- TotalAcidentesMaio[TotalAcidentesMaio$total_obitos >= 1,]
TotalAcidentesMaioSemMortes <- TotalAcidentesMaio[TotalAcidentesMaio$total_obitos == 0,]

TotalAcidentesVeraneio <- na.omit(TotalAcidentesVeraneio)
TotalAcidentesVeraneioComMortes <- TotalAcidentesVeraneio[TotalAcidentesVeraneio$total_obitos >= 1,]
TotalAcidentesVeraneioSemMortes <- TotalAcidentesVeraneio[TotalAcidentesVeraneio$total_obitos == 0,]

# -------- Geracao de graficos para maio amarelo -------- #

# Grafico de Pizza das Causas
ContaCausasMaio <- with(TotalAcidentesMaio,table(causa_acidente))
ContaCausasMaio <- as.data.frame(ContaCausasMaio)
colnames(ContaCausasMaio) <- c("Causa_do_Acidente","Ocorrencia")

PizzaCausaMaio <- hchart(ContaCausasMaio, "pie",
                         hcaes(x = Causa_do_Acidente, y = Ocorrencia),
                         name = "Quantidade de Acidentes") %>%
  hc_title(text = "Causas de Acidentes no mês de maio em 2017 e 2018")

# Grafico de Barras por BR
ContaBRMaio <- with(TotalAcidentesMaio,table(br))
ContaBRMaio <- as.data.frame(ContaBRMaio)
colnames(ContaBRMaio) <- c("BR","Ocorrencia")

BarraBRMaio <- hchart(ContaBRMaio, "column",
                      hcaes(x = BR, y = Ocorrencia),
                      name = "Quantidade de Acidentes") %>%
  hc_title(text = "Acidentes por BR no mês de maio em 2017 e 2018")

# Grafico de Pizza dos Dias Semanais
ContaDiaMaio <- with(TotalAcidentesMaio,table(dia_semana))
ContaDiaMaio <- as.data.frame(ContaDiaMaio)
colnames(ContaDiaMaio) <- c("Dia","Ocorrencia")

BarraDiaMaio <- hchart(ContaDiaMaio, "column",
                       hcaes(x = Dia, y = Ocorrencia),
                       name = "Quantidade de Acidentes") %>%
  hc_title(text = "Acidentes por dia semanal no mês de maio em 2017 e 2018")

# -------- Geracao de graficos para a lei seca --------- #

# Grafico de Barra por ano no mes de janeiro lei seca 
ContaAcidentesAno <- with(TotalAcidentesLeiSeca,table(mes,ano))
ContaAcidentesAno <- as.data.frame(ContaAcidentesAno)
colnames(ContaAcidentesAno) <- c("Mês","Ano","Quantidade")
ContaAcidentesAno <- ContaAcidentesAno[ContaAcidentesAno$Mês == "01-Janeiro",]

BarraAcidentesAnoLeiSeca <- hchart(ContaAcidentesAno, "column",
                                   hcaes(x = Ano, y = Quantidade),
                                   name = "Quantidade de Acidentes") %>%
  hc_title(text = "Comparativo de Acidentes em Janeiro de 2017,2018 e 2019")

# Grafico de barra anos de 2017 e 2018 por total
ContaAcidentes2017LeiSeca <- with(TotalAcidentesLeiSeca, table(ano))
ContaAcidentes2017LeiSeca <- as.data.frame(ContaAcidentes2017LeiSeca)
colnames(ContaAcidentes2017LeiSeca) <- c("Ano","Quantidade")
ContaAcidentes2017LeiSeca <- ContaAcidentes2017LeiSeca[ContaAcidentes2017LeiSeca$Ano == "2017",]

ContaAcidentes2018LeiSeca <- with(TotalAcidentesLeiSeca, table(ano))
ContaAcidentes2018LeiSeca <- as.data.frame(ContaAcidentes2018LeiSeca)
colnames(ContaAcidentes2018LeiSeca) <- c("Ano","Quantidade")
ContaAcidentes2018LeiSeca <- ContaAcidentes2018LeiSeca[ContaAcidentes2018LeiSeca$Ano == "2018",]

ContaAcidentesTotaisLeiSeca <- rbind(ContaAcidentes2017LeiSeca,ContaAcidentes2018LeiSeca)

BarraAcidentesAnosLeiSeca <- hchart(ContaAcidentesTotaisLeiSeca, "column",
                                    hcaes(x = Ano, y = Quantidade),
                                    name = "Quantidade de Acidentes") %>%
  hc_title(text = "Comparativo de acidentes causados nos anos 2017 e 2018")

# Lista para armazenar os dois graficos
lst <- list(BarraAcidentesAnoLeiSeca,BarraAcidentesAnosLeiSeca)

# Gráfico de Pizza por Sexo Lei Seca
ContaSexoLeiSeca <- with(TotalAcidentesLeiSeca,table(sexo))
ContaSexoLeiSeca <- as.data.frame(ContaSexoLeiSeca)
colnames(ContaSexoLeiSeca) <- c("Sexo","Pessoas")

PizzaSexoLeiSeca <- hchart(ContaSexoLeiSeca, "pie",
                           hcaes(x = Sexo, y = Pessoas),
                           name = "Quantidade de Acidentes") %>%
  hc_title(text = "Causadores de acidente após ingestão de álcool nos anos de 2017, 2018 e 2019")

# Grafico de Barras por Feriado
ContaDiaLeiSeca <- with(TotalAcidentesLeiSeca,table(dia_semana))
ContaDiaLeiSeca <- as.data.frame(ContaDiaLeiSeca)
colnames(ContaDiaLeiSeca) <- c("Dia","Quantidade")

BarraDiaLeiSeca <- hchart(ContaDiaLeiSeca, "column",
                                    hcaes(x = Dia, y = Quantidade),
                                    name = "Quantidade de Acidentes") %>%
  hc_title(text = "Comparativo de acidentes por dia semanal - Se beber não dirija")


# -------- Geracao de graficos para epoca de veraneio --------- #
# Grafico de Pizza por Causa nas epocas de Veraneio
ContaCausasVeraneio <- with(TotalAcidentesVeraneio,table(causa_acidente))
ContaCausasVeraneio <- as.data.frame(ContaCausasVeraneio)
colnames(ContaCausasVeraneio) <- c("Causa_do_Acidente","Ocorrencia")

PizzaCausaVeraneio <- hchart(ContaCausasVeraneio, "pie",
                         hcaes(x = Causa_do_Acidente, y = Ocorrencia),
                         name = "Quantidade de Acidentes") %>%
  hc_title(text = "Causas de acidentes nas epocas de veraneio em 2017 e 2018")

# Grafico de barras por dia da epoca de veraneio
ContaDiaVeraneio <- with(TotalAcidentesVeraneio,table(dia_semana))
ContaDiaVeraneio <- as.data.frame(ContaDiaVeraneio)
colnames(ContaDiaVeraneio) <- c("Dia","Ocorrencia")

BarraDiaVeraneio <- hchart(ContaDiaVeraneio, "column",
                       hcaes(x = Dia, y = Ocorrencia),
                       name = "Quantidade de Acidentes") %>%
  hc_title(text = "Acidentes por dia semanal na época de veraneio")

# Grafico de barras por BR da epoca de veraneio
ContaBRVeraneio <- with(TotalAcidentesVeraneio,table(br))
ContaBRVeraneio <- as.data.frame(ContaBRVeraneio)
colnames(ContaBRVeraneio) <- c("BR","Ocorrencia")

BarraBRVeraneio <- hchart(ContaBRVeraneio, "column",
                      hcaes(x = BR, y = Ocorrencia),
                      name = "Quantidade de Acidentes") %>%
  hc_title(text = "Acidentes por BR na época de veraneio em 2017 e 2018")

#----------- Geracao das Arvores de Decisao para cada campo ----------- #

# Criacao da Arvore da Lei Seca
Indice = sample(1:nrow(TotalAcidentesLeiSeca), 0.6*nrow(TotalAcidentesLeiSeca), replace = FALSE)
TreinoLeiSeca = data.frame() 
TreinoLeiSeca = TotalAcidentesLeiSeca[Indice,]
TreinoLeiSeca <- TreinoLeiSeca[TreinoLeiSeca$idade != 0,]
TesteLeiSeca = data.frame() 
TesteLeiSeca = TotalAcidentesLeiSeca[-Indice,]
TesteLeiSeca <- TesteLeiSeca[TesteLeiSeca$idade != 0,]
TotalAcidentesLeiSeca <- TotalAcidentesLeiSeca[TotalAcidentesLeiSeca$idade != 0,]

# Arvore para Treino 
ArvoreTreinoLeiSeca <- rpart(ilesos ~ mes + dia_semana + idade, data = TreinoLeiSeca)
rpart.plot(ArvoreTreinoLeiSeca, type = 2,  clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTreinoLeiSeca, cover = TRUE)

# Arvore para Teste
ArvoreTesteLeiSeca <- rpart(ilesos ~ idade + mes + dia_semana, data = TesteLeiSeca)
rpart.plot(ArvoreTesteLeiSeca, main = "Probabilidade de sair ileso em acidentes com base em idade, dia de semana e mês", type = 2, clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTesteLeiSeca, cover = TRUE)

# Teste naive bayes para lei seca
TreinoSeca <- na.omit(TreinoLeiSeca)
TesteSeca <- na.omit(TesteLeiSeca)
TotalSeca <- na.omit(TotalAcidentesLeiSeca)

TreinoSeca <- na.omit(TreinoSeca, cols="ilesos")
TreinoSeca <- na.omit(TreinoSeca, cols="idade")

TreinoSeca$idade <- as.factor(TreinoSeca$idade)
TreinoSeca$ilesos <- as.factor(TreinoSeca$ilesos)

TesteSeca <- na.omit(TesteSeca, cols="ilesos")
TesteSeca <- na.omit(TesteSeca, cols="idade")

TesteSeca$idade <- as.factor(TesteSeca$idade)
TesteSeca$ilesos <- as.factor(TesteSeca$ilesos)

NBclassfier=naiveBayes(ilesos ~ idade + mes + dia_semana, data=TreinoSeca)
print(NBclassfier)

TreinoPredicaoSeca <- predict(NBclassfier, newdata = TreinoSeca, type = "class")
TreinoTabelaSeca = table(TreinoSeca$ilesos, TreinoPredicaoSeca)

TestePredicaoSeca <- predict(NBclassfier, newdata = TesteSeca, type = "class")
TesteTabelaSeca = table(TesteSeca$ilesos, TestePredicaoSeca)

TreinoAcc <-(TreinoTabelaSeca[1,1]+TreinoTabelaSeca[2,2])/sum(TreinoTabelaSeca)
TesteAcc <-(TesteTabelaSeca[1,1]+TesteTabelaSeca[2,2])/sum(TesteTabelaSeca)

print(round(cbind(PrecisaoTreino=TreinoAcc, PrecisaoTeste=TesteAcc),2))

# Criacao da Arvore do Maio Amarelo
Indice1 <- sample(1:nrow(TotalAcidentesMaio), 0.6*nrow(TotalAcidentesMaio), replace = FALSE)
TreinoMaioAmarelo = data.frame() 
TreinoMaioAmarelo = TotalAcidentesMaio[Indice1,]
TreinoMaioAmarelo <- TreinoMaioAmarelo[TreinoMaioAmarelo$idade != 0,]
TesteMaioAmarelo = data.frame() 
TesteMaioAmarelo = TotalAcidentesMaio[-Indice1,]
TesteMaioAmarelo <- TesteMaioAmarelo[TesteMaioAmarelo$idade != 0,]
TotalAcidentesMaio <- TotalAcidentesMaio[TotalAcidentesMaio$idade != 0,]

# Arvore para Treino 
ArvoreTreinoMaioAmarelo <- rpart(ilesos ~ idade + tipo_acidente + dia_semana, data = TreinoMaioAmarelo)
rpart.plot(ArvoreTreinoMaioAmarelo, type = 0,  clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTreinoMaioAmarelo, cover = TRUE)

# Arvore para Teste
ArvoreTesteMaioAmarelo <- rpart(ilesos ~ idade + dia_semana + tipo_acidente,  data = TesteMaioAmarelo)
rpart.plot(ArvoreTesteMaioAmarelo, type = 3, clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTesteMaioAmarelo, cover = TRUE)

# Teste Naive Bayes Maio Amarelo - Exemplo de precisao ileso baseado na idade
TreinoMaio <- na.omit(TreinoMaioAmarelo)
TesteMaio <- na.omit(TesteMaioAmarelo)

TreinoMaio <- na.omit(TreinoMaio, cols="ilesos")
TreinoMaio <- na.omit(TreinoMaio, cols="idade")

TreinoMaio$ilesos <- as.factor(TreinoMaio$ilesos)
TreinoMaio$idade <- as.factor(TreinoMaio$idade)

TesteMaio <- na.omit(TesteMaio, cols="ilesos")
TesteMaio <- na.omit(TesteMaio, cols="idade")

TesteMaio$ilesos <- as.factor(TesteMaio$ilesos)
TesteMaio$idade <- as.factor(TesteMaio$idade)

NBclassfierMaio=naiveBayes(ilesos ~ idade + dia_semana + tipo_acidente, data=TreinoMaio)
print(NBclassfierMaio)

TreinoPredicaoMaio <- predict(NBclassfierMaio, newdata = TreinoMaio, type = "class")
TreinoTabelaMaio = table(TreinoMaio$ilesos, TreinoPredicaoMaio)

TestePredicaoMaio <- predict(NBclassfierMaio, newdata = TesteMaio, type = "class")
TesteTabelaMaio = table(TesteMaio$ilesos, TestePredicaoMaio)

TreinoAccMaio <-(TreinoTabelaMaio[1,1]+TreinoTabelaMaio[2,2])/sum(TreinoTabelaMaio)
TesteAccMaio <-(TesteTabelaMaio[1,1]+TesteTabelaMaio[2,2])/sum(TesteTabelaMaio)

print(round(cbind(PrecisaoTreino=TreinoAccMaio, PrecisaoTeste=TesteAccMaio),2))

#--------------------- Comparativo pelo geral dos acidentes --------------------#
TotalAcidentesValidos <- TotalAcidentes
TotalAcidentesValidos <- na.omit(TotalAcidentesValidos)

TotalAcidentesValidos <- TotalAcidentesValidos[TotalAcidentesValidos$ano < 2019,]
TotalAcidentesValidos <-TotalAcidentesValidos[!duplicated(TotalAcidentesValidos$pesid),]
TotalAcidentesValidos <- TotalAcidentesValidos[TotalAcidentesValidos$tipo_envolvido == "Condutor",]

# Levantamento Mensal
ContaAcidentesMes <- with(TotalAcidentesValidos,table(mes))
ContaAcidentesMes <- as.data.frame(ContaAcidentesMes)
colnames(ContaAcidentesMes) <- c("Mes","Ocorrencia")

BarraMesTotal <- hchart(ContaAcidentesMes, "column",
                          hcaes(x = Mes, y = Ocorrencia),
                          name = "Quantidade de Acidentes") %>%
  hc_title(text = "Contagem de Acidentes por Mês no Geral - (2017-2018)")

# Levantamento por Tipo
ContaAcidentesTipo <- with(TotalAcidentesValidos, table(causa_acidente))
ContaAcidentesTipo <- as.data.frame(ContaAcidentesTipo)
colnames(ContaAcidentesTipo) <- c("Causa","Ocorrencia")

PizzaCausaTotal <- hchart(ContaAcidentesTipo, "pie",
                          hcaes(x = Causa, y = Ocorrencia),
                          name = "Quantidade de Acidentes") %>%
  hc_title(text = "Contagem de Acidentes por Causa no Geral - (2017-2018)")

ListaCausas <- list(PizzaCausaMaio,PizzaCausaVeraneio)


# Levantamento por Dia Semanal
ContaAcidentesDia <- with(TotalAcidentesValidos, table(dia_semana))
ContaAcidentesDia <- as.data.frame(ContaAcidentesDia)
colnames(ContaAcidentesDia) <- c("Dia","Ocorrencia")

BarraDiaTotal <- hchart(ContaAcidentesDia, "column",
                        hcaes(x=Dia, y=Ocorrencia),
                        name = "Quantidade de Acidentes") %>%
  hc_title(text = "Contagem de Acidentes por Dia Semanal - (2017-2018)")

ListaDias <- list(BarraDiaLeiSeca,BarraDiaMaio,BarraDiaVeraneio)
# Criacao da Arvore para Epoca de Veraneio
Indice2 <- sample(1:nrow(TotalAcidentesVeraneio), 0.6*nrow(TotalAcidentesVeraneio), replace = FALSE)
TreinoEpocaVeraneio = data.frame() 
TreinoEpocaVeraneio = TotalAcidentesVeraneio[Indice2,]
TreinoEpocaVeraneio <- TreinoEpocaVeraneio[TreinoEpocaVeraneio$idade != 0,]
TesteEpocaVeraneio = data.frame() 
TesteEpocaVeraneio = TotalAcidentesVeraneio[-Indice2,]
TesteEpocaVeraneio <- TesteEpocaVeraneio[TesteEpocaVeraneio$idade != 0,]
TotalAcidentesVeraneio <- TotalAcidentesVeraneio[TotalAcidentesVeraneio$idade != 0,]

ArvoreTreinoVeraneio <- rpart(ilesos ~ idade + dia_semana + tipo_acidente, data = TreinoEpocaVeraneio)
rpart.plot(ArvoreTreinoVeraneio, type = 2,  clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTreinoVeraneio, cover = TRUE)

ArvoreTesteVeraneio <- rpart(ilesos ~ idade + dia_semana + tipo_acidente, data = TesteEpocaVeraneio)
rpart.plot(ArvoreTesteVeraneio, type = 2,  clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTesteVeraneio, cover = TRUE)

# Teste Naive Bayes Veraneio - Exemplo de precisao ileso baseado no veiculo e tipo de acidente
TreinoVeraneio <- na.omit(TreinoEpocaVeraneio)
TesteVeraneio <- na.omit(TesteEpocaVeraneio)

TreinoVeraneio <- na.omit(TreinoVeraneio, cols="ilesos")
TreinoVeraneio <- na.omit(TreinoVeraneio, cols="tipo_veiculo")
TreinoVeraneio <- na.omit(TreinoVeraneio, cols="tipo_acidente")

TreinoVeraneio$ilesos <- as.factor(TreinoVeraneio$ilesos)
TreinoVeraneio$tipo_veiculo <- as.factor(TreinoVeraneio$tipo_veiculo)
TreinoVeraneio$tipo_acidente <- as.factor(TreinoVeraneio$tipo_acidente)

TesteVeraneio <- na.omit(TesteVeraneio, cols="ilesos")
TesteVeraneio <- na.omit(TesteVeraneio, cols="tipo_veiculo")
TesteVeraneio <- na.omit(TesteVeraneio, cols="tipo_acidente")

TesteVeraneio$ilesos <- as.factor(TesteVeraneio$ilesos)
TesteVeraneio$tipo_veiculo <- as.factor(TesteVeraneio$tipo_veiculo)
TesteVeraneio$tipo_acidente <- as.factor(TesteVeraneio$tipo_acidente)

NBclassfierVeraneio=naiveBayes(ilesos ~ idade + dia_semana + tipo_acidente, data=TreinoVeraneio)
print(NBclassfierVeraneio)

TreinoPredicaoVeraneio <- predict(NBclassfierVeraneio, newdata = TreinoVeraneio, type = "class")
TreinoTabelaVeraneio = table(TreinoVeraneio$ilesos, TreinoPredicaoVeraneio)

TestePredicaoVeraneio <- predict(NBclassfierVeraneio, newdata = TesteVeraneio, type = "class")
TesteTabelaVeraneio = table(TesteVeraneio$ilesos, TestePredicaoVeraneio)

TreinoAccVeraneio <-(TreinoTabelaVeraneio[1,1]+TreinoTabelaVeraneio[2,2])/sum(TreinoTabelaVeraneio)
TesteAccVeraneio <-(TesteTabelaVeraneio[1,1]+TesteTabelaVeraneio[2,2])/sum(TesteTabelaVeraneio)

print(round(cbind(PrecisaoTreino=TreinoAccVeraneio, PrecisaoTeste=TesteAccVeraneio),2))

TotalAcidentesLeiSeca$total_obitos <- c(paste("Óbitos = ", TotalAcidentesLeiSeca$total_obitos))

coresLeiSeca <- colorFactor(topo.colors(4),TotalAcidentesLeiSeca$total_obitos)

coresLeiSecaSexo <- colorFactor(
  palette = c("red","black"),
  domain = TotalAcidentesLeiSeca$sexo)

gruposLeiSeca <- as.character(unique(TotalAcidentesLeiSeca$total_obitos))
gruposLeiSeca1 <- as.character(unique(TotalAcidentesLeiSeca$sexo))

coresMaioAmarelo <- colorFactor(topo.colors(27), TotalAcidentesMaio$causa_acidente)

gruposMaioAmarelo <- as.character(unique(TotalAcidentesMaio$causa_acidente))

coresVerao <- colorFactor(topo.colors(32), TotalAcidentesVeraneio$causa_acidente)

gruposVerao <- as.character(unique(TotalAcidentesVeraneio$causa_acidente))

# Criação da Situação Hipotética
VeraoSabado <- TotalAcidentesVeraneio[TotalAcidentesVeraneio$dia_semana == "sábado",]
TreinoVeraoSabado <- VeraoSabado[Indice2,]
TesteVeraoSabado <- VeraoSabado[-Indice2,]

NBclassfierVeraneioSabado=naiveBayes(ilesos ~ idade + sexo, data=TreinoVeraoSabado)
print(NBclassfierVeraneioSabado)

TreinoPredicaoVeraoSabado <- predict(NBclassfierVeraneio, newdata = TreinoVeraoSabado, type = "class")
TreinoTabelaVeraoSabado = table(TreinoVeraoSabado$ilesos, TreinoPredicaoVeraoSabado)

TestePredicaoVeraoSabado <- predict(NBclassfierVeraneio, newdata = TesteVeraoSabado, type = "class")
TesteTabelaVeraoSabado = table(TesteVeraoSabado$ilesos, TestePredicaoVeraoSabado)

TreinoAccVeraoSabado <-(TreinoTabelaVeraoSabado[1,1]+TreinoTabelaVeraoSabado[2,2])/sum(TreinoTabelaVeraoSabado)
TesteAccVeraoSabado <-(TesteTabelaVeraoSabado[1,1]+TesteTabelaVeraoSabado[2,2])/sum(TesteTabelaVeraoSabado)

print(round(cbind(PrecisaoTreino=TreinoAccVeraoSabado, PrecisaoTeste=TesteAccVeraoSabado),2))

ArvoreTreinoVeraoSabado <- rpart(ilesos ~ idade + sexo, data = TreinoVeraoSabado)
rpart.plot(ArvoreTreinoVeraoSabado, type = 2,  clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTreinoVeraoSabado, cover = TRUE)

ArvoreTesteVeraoSabado <- rpart(ilesos ~ idade + sexo, data = TesteVeraoSabado)
rpart.plot(ArvoreTesteVeraoSabado, type = 2,  clip.right.labs = FALSE, branch = .3, under = TRUE)
rpart.rules(ArvoreTesteVeraoSabado, cover = TRUE)