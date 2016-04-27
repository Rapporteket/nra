#' Provide global dataframe for NRA
#'
#' Provides NRA data from staging
#'
#' @inheritParams nraFigAndeler
#'
#' @return RegData data frame
#' @export

nraHentRegData <- function() {

  registryName <- "nra"
  dbType <- "mysql"

  query <- paste0("SELECT
                  alleVarNum.AvdRESH,
                  alleVarNum.Sykehusnavn,
                  alleVarNum.PatientID,
                  alleVarNum.FodselsDato,
                  ForlopsOversikt.ErMann,
                  ForlopsOversikt.PasientAlder,
                  ForlopsOversikt.HovedDato,
                  ForlopsOversikt.BasisRegStatus,
                  ForlopsOversikt.ForlopsID
                  FROM alleVarNum INNER JOIN ForlopsOversikt
                  ON alleVarNum.ForlopsID = ForlopsOversikt.ForlopsID")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
