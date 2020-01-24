#' Denne lager en figur som er en sammenstilling av alle kvalitetsindikatorene
#' til NRA ved snm.
#'
#' @inheritParams nraFigAndeler
#'
#' @return En figur med kvalitetsindikatorer med måloppnåelse
#' @export
#'
nraIndikatorSNM <- function(outfile='', dekngrad=85, posTestBedr75=73, stmark9=30, stmarks12=44, infeksjon=1.6,
                            width = 7.5, height = 6){

  plotmatrise <- rev(c(dekngrad, posTestBedr75, stmark9, stmarks12, infeksjon))
  maalniva <- rev(c('>60%', '>70%', '>30%', '>50%', '<4%'))
  maaloppnaaelse <- rev(c(dekngrad>=60, posTestBedr75>=70, stmark9>=30, stmarks12>=50, infeksjon<=4))
  etiketter <- rev(c('Dekningsgrad', 'Andel med positiv test med (minst) 75% bedring',
                     'Andel med St.Marks score mindre enn 9, 1 år etter operasjon',
                     'Andel med St.Marks score mindre enn 12, 1 år etter operasjon', 'Infeksjonsrate'))
  etiketter <- muskel::wrap.it(etiketter, 25)

  FigTypUt <- rapbase::figtype(outfile='', pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  windows(width = width, height = height)
  oldpar_mar <- par()$mar
  par(mar=c(5.1,10.1,4.1,6.1))

  ypos <- barplot(plotmatrise, horiz = T, col = farger[3], border = F, xlim = c(0,100), xlab = 'Andel (%)')
  points(rep(100,length(ypos[maaloppnaaelse])), ypos[maaloppnaaelse], xpd=T, cex=2, col='green', pch=19)
  points(rep(100,length(ypos[!maaloppnaaelse])), ypos[!maaloppnaaelse], xpd=T, cex=2, col='red', pch=19)

  mtext(etiketter, side=2, line=0.2, las=1, at=ypos, col=1)
  mtext(maalniva , side=4, line=5, las=1, at=ypos, col=1, cex=1, adj = 1)
  mtext('Mål', side=4, line=5, las=1, at=ypos[length(ypos)]+diff(ypos)[1], col=1, cex=1, adj = 1)
  text(plotmatrise, ypos, labels = paste0(plotmatrise, '%'), pos = 4)

  par('mar'= oldpar_mar)
  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
}

