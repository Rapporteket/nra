#' Provide global dataframe for NRA
#'
#' Provides NRA data from staging
#'
#' @return RegData data frame
#' @export

nraHentRegData <- function() {

  registryName <- "nra"
  dbType <- "mysql"

  query <- paste0("SELECT
                  allevarnum.Ukjent,
                  allevarnum.AnnenBekkenKirurgi,
                  allevarnum.AnnetTraume,
                  allevarnum.Hemoroidereksjon,
                  allevarnum.NevrologiskSykdom,
                  allevarnum.ObsteriskSkade,
                  allevarnum.PeriferNervskade,
                  allevarnum.PerinealAbscess,
                  allevarnum.Rectumreseksjon,
                  allevarnum.Sfinkterotomi,
                  allevarnum.KraftBehandling,
                  allevarnum.AnnetEtiologi,
                  allevarnum.Konservativ,
                  allevarnum.Irrigasjon,
                  allevarnum.Tibialisstimulering,
                  allevarnum.AnalInjection,
                  allevarnum.SNM,
                  allevarnum.Sfinkterplastikk,
                  allevarnum.Rectopexi,
                  allevarnum.KirurgiForRectumprolaps,
                  allevarnum.Gracilisplastikk,
                  allevarnum.Stomi,
                  allevarnum.AnnetTidligereBeh,
                  allevarnum.SenterKortNavn,
                  allevarnum.Symtomvarighet,
                  allevarnum.Ultralyd,
                  allevarnum.PartiellDefekt,
                  allevarnum.FullveggsdefektYtreSfinkter,
                  allevarnum.FullveggsdefektIndreSfinkter,
                  allevarnum.GenQol,
                  allevarnum.StMarksTotalScore,
                  allevarnum.QolSexualitet,
                  allevarnum.Tilfredshet,
                  allevarnum.Urinlekkasje,
                  allevarnum.Komplikasjon,
                  allevarnum.KomplikasjonT2,
                  allevarnum.PostopKomplikasjoner,
                  allevarnum.Bloedning,
                  allevarnum.Saarinfeksjon,
                  allevarnum.Saardehisens,
                  allevarnum.InkontinensFoerTest,
                  allevarnum.UrgencyFoerTest,
                  allevarnum.AvfoeringerFoerTest,
                  allevarnum.LekkasjedagerFoer,
                  allevarnum.InkontinensUnderTest,
                  allevarnum.UrgencyUnderTest,
                  allevarnum.AvfoeringerUnderTest,
                  allevarnum.LekkasjedagerUnder,
                  allevarnum.WexnerTotalScore,
                  allevarnum.Testprosedyre,
                  allevarnum.UrgencyFoerTestUtenLekkasje,
                  allevarnum.UrgencyFoerTestMedLekkasje,
                  allevarnum.UrgencyFoerTestPassivLekkasje,
                  allevarnum.UrgencyUnderTestPassivLekkasje,
                  allevarnum.UrgencyUnderUtenTestUtenLekkasje,
                  allevarnum.UrgencyUnderTestMedLekkasje,
                  allevarnum.LekasjeFriFoerTest,
                  allevarnum.LekasjeFriUnderTest,
                  allevarnum.OppfoelgingMulig,
                  allevarnum.ICIQ_hyppighet,
                  allevarnum.ABD65,
                  allevarnum.ABD652AT2,
                  allevarnum.ABD60,
                  allevarnum.WexFastAvfoering,
                  allevarnum.WexBind,
                  allevarnum.WexFlytendeAvfoering,
                  allevarnum.WexLuft,
                  allevarnum.WexLivsstilsendring,
                  allevarnum.Onestage,
                  allevarnum.KirurgiForRectumprolaps_v2,
                  allevarnum.KunstigLukkMuskel,
                  allevarnum.PGICEndring,
                  allevarnum.PGICEndringLekkasje,
                  allevarnum.EQ5DSkore,
                  allevarnum.EQ5DHelsetilstand,
                  allevarnum.EQ5DAngst,
                  allevarnum.EQ5DPersonligStell,
                  allevarnum.EQ5DSmerte,
                  allevarnum.EQ5DGange,
                  allevarnum.EQ5DVanligeGjoeremaal,
                  forlopsoversikt.KobletForlopsID,
                  forlopsoversikt.ForlopsID,
                  forlopsoversikt.HovedDato,
                  forlopsoversikt.PasientAlder,
                  forlopsoversikt.PasientID,
                  forlopsoversikt.AvdRESH,
                  forlopsoversikt.Sykehusnavn,
                  forlopsoversikt.ForlopsType1Num,
                  forlopsoversikt.ForlopsType2Num,
                  forlopsoversikt.ForlopsType1,
                  forlopsoversikt.ForlopsType2,
                  forlopsoversikt.ErMann,
                  forlopsoversikt.OppflgRegStatus
                  FROM allevarnum INNER JOIN forlopsoversikt
                  ON allevarnum.ForlopsID = forlopsoversikt.ForlopsID")

  RegData <- rapbase::loadRegData(registryName, query, dbType)

  return(RegData)
}

#' Fetch chosen table for NRA
#'
#' Provides NRA data from staging
#'
#' @inheritParams nraFigAndeler
#'
#' @return RegData data frame
#' @export
nraHentTabell <- function(tabnavn) {

  if (Sys.getenv("R_RAP_INSTANCE") %in% c("QAC", "PRODUCTIONC")){
    registryName <- "data"
  } else {
    registryName <- "nra"
  }
  dbType <- "mysql"
  query <- paste0("SELECT * FROM ", tabnavn)

  RegData <- rapbase::loadRegData(registryName, query, dbType)

  return(RegData)
}
