devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)

Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nra/data-raw/config")


nra::nraApp()


RegData <- rapbase::loadRegData(
  registryName = "data",
  query="SELECT * FROM eq5dlformdatacontract",
  dbType="mysql")

tmp_yml <- yaml::read_yaml("./dev/test.yml")
tmp_json <- jsonlite::serializeJSON(tmp_yml)
query <- paste0("INSERT INTO `autoreport` VALUES ('", tmp_json, "');")

con <- rapbase::rapOpenDbConnection("autoreport")$con
DBI::dbExecute(con, query)
rapbase::rapCloseDbConnection(con)


