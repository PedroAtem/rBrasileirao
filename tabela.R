library(gridExtra)
library(grid)

gerarTabela <- function(jogosDisputados) {
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
  colnames(tabela) <- nomesColunas # nomeando as colunas da tabela
  
  contador_visitante = 1 # contador do time visitante
  for (coluna in jogosDisputados[,-1]) {
    contador_mandante = 1 # contador do time mandante
    visitante <- times[contador_visitante] # time visitante
    for (partida in coluna[-1]) {
      mandante <- times[contador_mandante] # time mandante
      resultado <- unlist(strsplit(partida, "x")) # resultado da partida
      if (!is.na(resultado) && length(resultado) == 2) { # verificando se o jogo já ocorreu
        # somando numero de jogos para o mandante e visitante
        tabela$J[contador_mandante] <- tabela$J[contador_mandante] + 1
        tabela$J[contador_visitante] <- tabela$J[contador_visitante] + 1
        if (resultado[1] > resultado[2]) { # time mandante venceu
          tabela$V[contador_mandante] <- tabela$V[contador_mandante] + 1 # 1 vitória para o mandante
          tabela$D[contador_visitante] <- tabela$D[contador_visitante] + 1 # 1 derrota para o visitante
          tabela$P[contador_mandante] <- tabela$P[contador_mandante] + 3 # 3 pontos para o mandante
        }
        else if (resultado[1] < resultado[2]) { # time mandante perdeu
          tabela$V[contador_visitante] <- tabela$V[contador_visitante] + 1 # 1 vitória para o visitante
          tabela$D[contador_mandante] <- tabela$D[contador_mandante] + 1 # 1 derrota para o mandante
          tabela$P[contador_visitante] <- tabela$P[contador_visitante] + 3 # somando 3 pontos para o visitante
        }
        else { # empate
          tabela$E[contador_mandante] <- tabela$E[contador_mandante] + 1 # 1 empate para o mandante
          tabela$E[contador_visitante] <- tabela$E[contador_visitante] + 1 # 1 empate para o visitante
          tabela$P[contador_mandante] <- tabela$P[contador_mandante] + 1 # 1 partida para o mandante
          tabela$P[contador_visitante] <- tabela$P[contador_visitante] + 1 # 1 partida para o visitante
        }
        tabela$GP[contador_mandante] <- tabela$GP[contador_mandante] + as.integer(resultado[1]) # gols pro mandante
        tabela$GP[contador_visitante] <- tabela$GP[contador_visitante] + as.integer(resultado[2]) # gols pro visitante
        tabela$GC[contador_mandante] <- tabela$GC[contador_mandante] + as.integer(resultado[2]) # gols contra mandante
        tabela$GC[contador_visitante] <- tabela$GC[contador_visitante] + as.integer(resultado[1]) # gols contra visitante
      }
      contador_mandante <- contador_mandante + 1
    }
    contador_visitante <- contador_visitante + 1
  }
  for (i in 1:length(times)) {
    tabela[i,]$SG <- tabela[i,]$GP - tabela[i,]$GC # calculando saldo de gols do time
    maxPontos <- tabela[i,]$J * 3 # máximo de pontos que o time poderia fazer
    aproveitamento = (100 * tabela[i,]$P) / maxPontos # calculando aproveitamento do time
    tabela[i,]$AP <- paste(format(round(aproveitamento, 2), nsmall = 2),"%") # formatando aproveitamento
  }
  # ordenando tabela
  tabelaOrdenada <- data.frame(tabela[order(-tabela$P, -tabela$V, -tabela$SG, -tabela$GP),], row.names = seq(20))
  
  png("tabela.png", height = 600, width = 500)
  
  classificacao <- c("Libertadores", "Pré-Libertadores", "Sulamericana", "Rebaixamento")
  cor_legenda_classificacao <- c("#32CD32", "#7CFC00", "#F4A460", "#FA8072")
  cor_tabela_classificacao <- c(
    rep(c("#32CD32"), length.out=4), # libertadores
    rep(c("#7CFC00"), length.out=2), # pré-libertadores
    rep(c("#F4A460"), length.out=6), # sulamericana
    rep(c("#EEEEEE"), length.out=4), # nada
    rep(c("#FA8072"), length.out=4) # rebaixamento
  )
  legenda <-data.frame(classificacao)
  names(legenda) <- c("Legenda")
  
  # tema da tabela
  tema_tabela <- ttheme_minimal(
    core=list(bg_params = list(fill = cor_tabela_classificacao), fg_params=list(fontface=3)),
    colhead=list(fg_params=list(col="black", fontface=4L)),
    rowhead=list(fg_params=list(col="white", fontface=1L))
  )
  
  # tema da legenda
  tema_legenda <- ttheme_minimal(
    core=list(bg_params = list(fill = cor_legenda_classificacao), fg_params=list(fontface=3)),
    colhead=list(fg_params=list(col="black", fontface=4L)),
    rowhead=list(fg_params=list(col="white", fontface=1L))
  )
  
  tb_tabela <- tableGrob(tabelaOrdenada, theme = tema_tabela)
  tb_legenda <- tableGrob(legenda, theme = tema_legenda)
  valigned <- gtable_combine(tb_tabela, tb_legenda, along=2)
  tabelas <- arrangeGrob(valigned, ncol=1)
  
  grid.draw(tabelas)
  
  dev.off()
}