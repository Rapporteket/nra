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
                  alleVarNum.Ukjent,
                  alleVarNum.AnnenBekkenKirurgi,
                  alleVarNum.AnnetTraume,
                  alleVarNum.Hemoroidereksjon,
                  alleVarNum.NevrologiskSykdom,
                  alleVarNum.ObsteriskSkade,
                  alleVarNum.PeriferNervskade,
                  alleVarNum.PerinealAbscess,
                  alleVarNum.Rectumreseksjon,
                  alleVarNum.Sfinkterotomi,
                  alleVarNum.KraftBehandling,
                  alleVarNum.AnnetEtiologi,
                  alleVarNum.Konservativ,
                  alleVarNum.Irrigasjon,
                  alleVarNum.Tibialisstimulering,
                  alleVarNum.AnalInjection,
                  alleVarNum.SNM,
                  alleVarNum.Sfinkterplastikk,
                  alleVarNum.Rectopexi,
                  alleVarNum.KirurgiForRectumprolaps,
                  alleVarNum.Gracilisplastikk,
                  alleVarNum.Stomi,
                  alleVarNum.AnnetTidligereBeh,
                  alleVarNum.SenterKortNavn,
                  alleVarNum.Symtomvarighet,
                  alleVarNum.Ultralyd,
                  alleVarNum.PartiellDefekt,
                  alleVarNum.FullveggsdefektYtreSfinkter,
                  alleVarNum.FullveggsdefektIndreSfinkter,
                  alleVarNum.GenQol,
                  alleVarNum.StMarksTotalScore,
                  alleVarNum.QolSexualitet,
                  alleVarNum.Tilfredshet,
                  alleVarNum.Urinlekkasje,
                  alleVarNum.Komplikasjon,
                  alleVarNum.KomplikasjonT2,
                  alleVarNum.PostopKomplikasjoner,
                  alleVarNum.Bloedning,
                  alleVarNum.Saarinfeksjon,
                  alleVarNum.Saardehisens,
                  alleVarNum.InkontinensFoerTest,
                  alleVarNum.UrgencyFoerTest,
                  alleVarNum.AvfoeringerFoerTest,
                  alleVarNum.LekkasjedagerFoer,
                  alleVarNum.InkontinensUnderTest,
                  alleVarNum.UrgencyUnderTest,
                  alleVarNum.AvfoeringerUnderTest,
                  alleVarNum.LekkasjedagerUnder,
                  alleVarNum.WexnerTotalScore,
                  alleVarNum.Testprosedyre,
                  alleVarNum.UrgencyFoerTestUtenLekkasje,
                  alleVarNum.UrgencyFoerTestMedLekkasje,
                  alleVarNum.UrgencyFoerPassivLekkasje,
                  alleVarNum.UrgencyUnderPassivLekkasje,
                  alleVarNum.UrgencyUnderUtenTestMedLekkasje,
                  alleVarNum.UrgencyUnderTestLekkasje,
                  alleVarNum.OppfoelgingMulig,
                  alleVarNum.ICIQ_hyppighet,
                  alleVarNum.ABD65,
                  alleVarNum.ABD652AT2,
                  alleVarNum.ABD60,
                  alleVarNum.WexFastAvfoering,
                  alleVarNum.WexBind,
                  alleVarNum.WexFlytendeAvfoering,
                  alleVarNum.WexLuft,
                  alleVarNum.WexLivsstilsendring,
                  alleVarNum.Onestage,
                  alleVarNum.KirurgiForRectumprolaps_v2,
                  alleVarNum.KunstigLukkMuskel,
                  ForlopsOversikt.KobletForlopsID,
                  ForlopsOversikt.ForlopsID,
                  ForlopsOversikt.HovedDato,
                  ForlopsOversikt.PasientAlder,
                  ForlopsOversikt.PasientID,
                  ForlopsOversikt.AvdRESH,
                  ForlopsOversikt.Sykehusnavn,
                  ForlopsOversikt.ForlopsType1Num,
                  ForlopsOversikt.ForlopsType2Num,
                  ForlopsOversikt.ForlopsType1,
                  ForlopsOversikt.ForlopsType2,
                  ForlopsOversikt.ErMann,
                  ForlopsOversikt.OppflgRegStatus
                  FROM alleVarNum INNER JOIN ForlopsOversikt
                  ON alleVarNum.ForlopsID = ForlopsOversikt.ForlopsID")

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

  registryName <- "nra"
  dbType <- "mysql"

  query <- paste0("SELECT * FROM ", tabnavn)

  RegData <- rapbase::loadRegData(registryName, query, dbType)

  return(RegData)
}
