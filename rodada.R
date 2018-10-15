library(gridExtra)
library(grid)

gerarGraficoPontuacaoRodada <- function(times, times_rodadas, cores) {
  # arquivo png
  png("rodadas.png", height = 600, width = 800)
  # criando grafica em branco definindo os eixos
  plot(c(1,38), c(1,38*3), type = 'n', xlab = "Rodada", ylab = "Pontos", main = "Gráfico de evolução")
  tamanho_linha = 4 # tamanho da linha dos graficps
  contador_time <- 1
  legenda_times <- c()
  for (rodadas in times_rodadas) {
    names(rodadas) <- c("Mandante", "Resultado", "Visitante")
    time <- times[contador_time] # time da rodada
    pontos <- c(0) # vetor de pontos onde o indíce é a rodada a partir do 2
    contador_rodadas = 2 # iniciando o contador para controlar as rodadas
    for (nRodada in c(1:38)) {
      rodada <- rodadas[nRodada,]
      resultado <- as.integer(unlist(strsplit(rodada$Resultado, "x")))
      if (!is.na(resultado) && length(resultado) == 2) {
        if (resultado[1] == resultado[2]) { # verificando empate
          pontos[contador_rodadas] <- pontos[contador_rodadas-1] + 1
        }
        else if (rodada$Mandante == time && resultado[1] > resultado[2]) { # verificando vitória do time como mandante
          pontos[contador_rodadas] <- pontos[contador_rodadas-1] + 3
        }
        else if (rodada$Visitante == time && resultado[2] > resultado[1]) { # verificando vitória do time como visitante
          pontos[contador_rodadas] <- pontos[contador_rodadas-1] + 3
        }
        else { # derrota do time
          pontos[contador_rodadas] <- pontos[contador_rodadas-1]
        }
        contador_rodadas <- contador_rodadas + 1
      }
      mandante <- rodada$Mandante # time mandante
      visitante <- rodada$Visitante # time visitante
    }
    legenda_times[contador_time] = paste(time, pontos[length(pontos)])
    lines(pontos[-1], col = cores[contador_time], lwd = tamanho_linha) # plotando a linha dos pontos do time
    text(x = length(pontos)-1, y = pontos[length(pontos)], pos = 4, labels = pontos[length(pontos)]) # pontuação atual
    contador_time <- contador_time + 1
  }
  legend(1, 38*3, legend=legenda_times, col=cores, lwd = tamanho_linha) # criando legenda no gráfico
  dev.off()
}