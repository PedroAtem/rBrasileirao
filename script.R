library(gridExtra)

# limpando variaveis
rm(list = ls())
setwd(file.path("~", "project", "r-tests", "brasileirao2018"))

# jogos
caminho <- file.path("~", "project", "r-tests", "brasileirao2018", "jogos.csv")
jogosDisputados <- read.table(caminho, header = FALSE, sep = ",", stringsAsFactors = FALSE)

# tabela
times = jogosDisputados[,1][-1]
pontos = seq(0, by=0, length=20)
jogos = seq(0, by=0, length=20)
vitorias = seq(0, by=0, length=20)
empates = seq(0, by=0, length=20)
derrotas = seq(0, by=0, length=20)
golspro = seq(0, by=0, length=20)
golscontra = seq(0, by=0, length=20)
saldogols = seq(0, by=0, length=20)
aproveitamento = seq(0, by=0, length=20)
nomesColunas = c("TIME", "P", "J", "V", "E", "D", "GP", "GC", "SG", "AP")
tabela = data.frame(times, pontos, jogos, vitorias, empates, derrotas, golspro, golscontra, saldogols, aproveitamento)
colnames(tabela) <- nomesColunas

# percorrendo matriz de jogos
contadorVisitante = 0
for (linha in jogosDisputados) {
  if (contadorVisitante > 0) {
    visitante = linha[1]
    contadorMandante = 1
    for (partida in linha[2:21]) {
      if (partida != "" && partida != "-") {
        mandante = times[contadorMandante]
        resultado <- unlist(strsplit(partida, "x"))
        
        # jogos
        tabela$J[contadorMandante] <- tabela$J[contadorMandante] + 1
        tabela$J[contadorVisitante] <- tabela$J[contadorVisitante] + 1
        
        # vitorias, empates, derrotas, pontos
        if (resultado[1] > resultado[2]) { # time mandante venceu
          tabela$V[contadorMandante] <- tabela$V[contadorMandante] + 1
          tabela$D[contadorVisitante] <- tabela$D[contadorVisitante] + 1
          tabela$P[contadorMandante] <- tabela$P[contadorMandante] + 3
        }
        else if (resultado[1] < resultado[2]) { # time mandante perdeu
          tabela$D[contadorMandante] <- tabela$D[contadorMandante] + 1
          tabela$V[contadorVisitante] <- tabela$V[contadorVisitante] + 1
          tabela$P[contadorVisitante] <- tabela$P[contadorVisitante] + 3
        }
        else { # empate
          tabela$E[contadorMandante] <- tabela$E[contadorMandante] + 1
          tabela$E[contadorVisitante] <- tabela$E[contadorVisitante] + 1
          tabela$P[contadorMandante] <- tabela$P[contadorMandante] + 1
          tabela$P[contadorVisitante] <- tabela$P[contadorVisitante] + 1
        }
        
        # gols
        tabela$GP[contadorMandante] <- tabela$GP[contadorMandante] + as.integer(resultado[1])
        tabela$GP[contadorVisitante] <- tabela$GP[contadorVisitante] + as.integer(resultado[2])
        tabela$GC[contadorMandante] <- tabela$GC[contadorMandante] + as.integer(resultado[2])
        tabela$GC[contadorVisitante] <- tabela$GC[contadorVisitante] + as.integer(resultado[1])
        
        # print(paste(mandante, resultado[1], resultado[2], visitante))
      }
      
      contadorMandante <- contadorMandante + 1
    } 
  }
  contadorVisitante <- contadorVisitante + 1
}

tabelaOrdenada <- data.frame(tabela[order(-tabela$P, -tabela$V, -tabela$SG, -tabela$GP),], row.names = seq(20))

theme1 <- ttheme_default(core=list(
  bg_params = list(fill=c(
    rep(c("#32CD32"), length.out=4),
    rep(c("#7CFC00"), length.out=2),
    rep(c("#F4A460"), length.out=6),
    rep(c("#FFFFFF"), length.out=4),
    rep(c("#FA8072"), length.out=4)
  ))
))

png("tabela.png", height = 25*nrow(tabela), width = 40*ncol(tabelaOrdenada))
grid.table(tabelaOrdenada, theme = theme1)
dev.off()