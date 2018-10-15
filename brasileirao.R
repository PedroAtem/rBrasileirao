rm(list = ls())
setwd(file.path("~", "project", "rBrasileirao"))
source("tabela.R")
source("rodada.R")

caminho <- file.path("brasileirao2018.xlsx")
sheets <- openxlsx::getSheetNames(caminho)
brasileirao <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=caminho, colNames = FALSE)
names(brasileirao) <- sheets

timesRodadas <- c("Palmeiras", "São Paulo", "Corinthians", "Flamengo", "Botafogo")
coresTimes <- c("#1cb52f", "#FF0000", "#000000", "#550000", "#c4c4c4")

gerarTabela(brasileirao$Jogos)
gerarGraficoPontuacaoRodada(timesRodadas, brasileirao[timesRodadas], coresTimes)