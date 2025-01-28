#' Gi en visuell fremstilling av registerets indikatorer over tid
#'
#' @param indikatordata En dataramme med følgende kolonner:
#'                 - AvdRESH
#'                 - year
#'                 - var
#'                 - SenterKortNavn
#'
#' @export
#'
nraFigIndikator_v4 <- function(
    indikatordata, tittel='', terskel=30, minstekrav = NA,
    maal = NA, skriftStr=1.3, pktStr=1.4,
    legPlass='top', minstekravTxt='Min.', maalTxt='Mål',
    graaUt=NA, decreasing=F, outfile = '',
    lavDG=NA, width=800, height=700, maalretn='hoy',
    desimal=FALSE, xmax=NA,
    lavDGtekst='Dekningsgrad < 60 %')
{

  # indikatordata <- plotdata; tittel='testtittel'; terskel=5;
  # minstekrav = NA; maal = 30; skriftStr=1.3; pktStr=1.4;
  # legPlass='top'; minstekravTxt='Min.'; maalTxt='Mål';
  # graaUt=NA; decreasing=F; outfile = '';
  # lavDG=NA; width=800; height=700; inkl_konf=T; maalretn='hoy';
  # lavDGtekst='Dekningsgrad < 60 %'

  indikatordata <- indikatordata[indikatordata$year >
                                   max(indikatordata$year)-3, ] # behold bare siste 3 år

  Tabell <- indikatordata %>% dplyr::group_by(SenterKortNavn, year) %>%
    dplyr::summarise(Antall = sum(var),
                     N = dplyr::n(),
                     Andel = Antall/N*100)


  AntTilfeller <- tidyr::spread(Tabell[, -c(4,5)], 'year', 'Antall')
  AntTilfeller <- dplyr::bind_cols(
    SenterKortNavn=c(as.character(AntTilfeller[["SenterKortNavn"]]),
                     "Nasjonalt"),
    dplyr::bind_rows(AntTilfeller[,-1], colSums(AntTilfeller[,-1],
                                                na.rm = T)))

  N <- tidyr::spread(Tabell[, -c(3,5)], 'year', 'N')
  N <- dplyr::bind_cols(
    SenterKortNavn=c(as.character(N[["SenterKortNavn"]]), "Nasjonalt"),
    dplyr::bind_rows(N[,-1], colSums(N[,-1], na.rm = T)))
  N[is.na(N)] <- 0

  AntTilfeller[,paste0(names(AntTilfeller)[2], '-',
                       names(AntTilfeller)[dim(AntTilfeller)[2]])] <-
    rowSums(AntTilfeller[,-1], na.rm = T)
  AntTilfeller <- AntTilfeller[, c(1, (dim(AntTilfeller)[2]-1):dim(AntTilfeller)[2])]
  AntTilfeller <- as.data.frame(AntTilfeller)
  row.names(AntTilfeller) <- AntTilfeller[["SenterKortNavn"]]
  AntTilfeller <- AntTilfeller[, -1]
  N[,paste0(names(N)[2], '-', names(N)[dim(N)[2]])] <-
    rowSums(N[,-1], na.rm = T)
  N <- N[, c(1, (dim(N)[2]-1):dim(N)[2])]
  N <- as.data.frame(N)
  row.names(N) <- N[["SenterKortNavn"]]
  N <- N[, -1]

  andeler <- AntTilfeller/N * 100

  andeler[N < terskel] <- NA
  andeler[rownames(andeler) %in% lavDG, ] <- NA

  if (decreasing){
    rekkefolge <- order(andeler[, dim(andeler)[2]],
                        decreasing = decreasing, na.last = F)
  } else {
    rekkefolge <- order(andeler[, dim(andeler)[2]],
                        decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width,
                                  height=height, pointsizePDF=11,
                                  fargepalett='BlaaOff')
  farger <- FigTypUt$farger

  if (max(N[,2], na.rm = T) < terskel) {
    plot.new()
    text(0.5, 0.6, paste0("Det er færre enn ", terskel,
                          " registreringer av indikatoren nasjonalt i ",
                          names(N)[2], "."), cex=1.2)
  } else {

    andeler[N[, dim(andeler)[2]]<terskel, 1:2] <- NA
    KI <- binomkonf(AntTilfeller[rekkefolge, dim(andeler)[2]],
                    N[, dim(andeler)[2]])*100
    KI[, is.na(andeler[, dim(andeler)[2]])] <- NA
    pst_txt <- paste0(sprintf('%.0f', andeler[, dim(andeler)[2]]), ' %')
    pst_txt[N[, dim(andeler)[2]]<terskel] <- paste0('N<', terskel)
    pst_txt[rownames(andeler) %in% lavDG] <- lavDGtekst
    pst_txt <- c(NA, pst_txt, NA, NA)


    soyleFarger <- rep(farger[3], length(andeler[,dim(andeler)[2]]))
    soyleFarger[which(rownames(andeler)=='Nasjonalt')] <- farger[4]
    if (!is.na(graaUt[1])) {soyleFarger[which(rownames(andeler) %in% graaUt)] <-
      'gray88'}
    soyleFarger <- c(NA, soyleFarger)

    oldpar_mar <- par()$mar
    oldpar_fig <- par()$fig
    oldpar_oma <- par()$oma

    cexgr <- skriftStr
    rownames(andeler) <- paste0(rownames(andeler), " ")
    andeler <- rbind(andeler, c(NA,NA,NA))
    KI <- cbind(c(NA, NA), KI, c(NA, NA))

    par('mar'=c(5.1, 7.1, 5.1, 7.1))
    xmax <- min(max(KI, max(andeler, na.rm = T), na.rm = T)*1.15,100)

    andeler <- rbind(c(NA,NA), andeler, c(NA,NA))
    rownames(andeler)[dim(andeler)[1]] <- '  '
    rownames(andeler)[dim(andeler)[1]-1] <- '   '
    rownames(andeler)[1] <- ' '

    ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                     xlim=c(0,xmax),
                     names.arg=rep('',dim(andeler)[1]),
                     horiz=T, axes=F, space=c(0,0.3),
                     col=soyleFarger, border=NA) # '#96BBE7'

    fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')

    if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
      rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=max(ypos)-1.6,
           col = fargerMaalNiva[2], border = NA)
      rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6,
           col = fargerMaalNiva[1], border = NA)}
    if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
      rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=max(ypos)-1.6,
           col = fargerMaalNiva[2], border = NA)
      rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6,
           col = fargerMaalNiva[1], border = NA)}
    if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
      rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6,
           col = fargerMaalNiva[1], border = NA)}
    if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
      rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6,
           col = fargerMaalNiva[1], border = NA)}

    barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, add=TRUE)

    title(main = tittel, cex.main=skriftStr*1.1)
    ypos <- as.numeric(ypos) #as.vector(ypos)
    yposOver <- max(ypos)-2 + 0.5*diff(ypos)[1]

    arrows(x0 = KI[1,], y0 = ypos, x1 = KI[2,], y1 = ypos,
           length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray')
    legend('bottom', cex=0.9*cexgr, bty='n',
           lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
           legend=paste0('Konfidensintervall ', names(N)[dim(N)[2]]))

    axis(1,cex.axis=0.9)
    mtext( rownames(andeler), side=2, line=0.2, las=1,
           at=ypos, col=1, cex=cexgr)
    antAar <- dim(andeler)[2]

    if (dim(andeler)[2]==2) {
      par(xpd=TRUE)
      points(y=ypos, x=andeler[,1],cex=pktStr, pch= 19)
      par(xpd=FALSE)
      if (legPlass=='nede'){
        legend('bottomright', cex=0.9*cexgr, bty='n',
               lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8),
               col=c('black',farger[3]),
               legend=names(N), ncol = 1)}
      if (legPlass=='top'){
        legend('top', cex=0.9*cexgr, bty='n',
               lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8),
               col=c('black',farger[3]),
               legend=names(N), ncol = dim(andeler)[2])
      }
      if (legPlass=='topleft'){
        legend('topleft', cex=0.9*cexgr, bty='n',
               lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8),
               col=c('black',farger[3]),
               legend=names(N), ncol = dim(andeler)[2])}
      if (legPlass=='topright'){
        legend('topright', cex=0.9*cexgr, bty='n',
               lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8),
               col=c('black',farger[3]),
               legend=names(N), ncol = dim(andeler)[2])}

    } else {

      par(xpd=TRUE)
      points(y=ypos, x=andeler[,1],cex=pktStr)
      points(y=ypos, x=andeler[,2],cex=pktStr,pch= 19)
      par(xpd=FALSE)
      if (legPlass=='nede'){
        legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n',
               lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8),
               col=c('black','black',farger[3]),
               legend=names(N) )}
      if (legPlass=='top'){
        legend('top', cex=0.9*cexgr, bty='n',
               lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8),
               col=c('black','black',farger[3]),
               legend=names(N), ncol = dim(andeler)[2])
      }
    }

    mtext( c(NA, N[,1], names(N)[1]), side=4, line=2.5, las=1, at=ypos,
           col=1, cex=cexgr*0.6, adj = 1, xpd=TRUE)
    mtext( c(NA, N[,2], names(N)[2]), side=4, line=6.5, las=1, at=ypos,
           col=1, cex=cexgr*0.6, adj = 1, xpd=TRUE)
    mtext( 'N', side=4, line=4.0, las=1, at=max(ypos), col=1,
           cex=cexgr*0.7, adj = 1)

    legPos <- ifelse(dim(andeler)[1] < 8, -1.1, -1.7)
    legend(x=0, y=legPos, pch=c(NA, 15, 15), col=c(NA, fargerMaalNiva[2:1]),
           ncol=3, xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
           legend=c('Måloppnåelse:', 'Moderat', 'Høy'))
    mtext('Andel (%)', side=1, line=1.7, cex = 0.8, xpd=TRUE)

    text(x=0, y=ypos, labels = pst_txt, cex=skriftStr, pos=4)#

    par('mar'= oldpar_mar)
    par('fig'= oldpar_fig)
    par('oma'= oldpar_oma)

    if ( outfile != '') {dev.off()}
  }
}
