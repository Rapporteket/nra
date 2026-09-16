library(nra)
library(dplyr)
library(tidyr)
library(irr)

reldata <- readxl::read_xlsx(
  "C:/Users/kth200/regdata/nra/reliabilitet/reliabilitet.allevarnum_utflatet_NRA_20260319.xlsx") |>
  dplyr::rename(CASENUMBER = PatientID,
                CREATEDBY = RegistratorID) |>
  dplyr::filter(!duplicated(paste(CASENUMBER, CREATEDBY))) |>
  dplyr::select(where(~ !all(is.na(.x))))


get_mode <- function(v) {
  # Create a frequency table
  freq_table <- table(v)

  # Find the name (value) that corresponds to the maximum frequency
  most_frequent_value <- names(freq_table)[which.max(freq_table)]

  # Coerce the result back to the original data type of the vector
  # This handles cases where the vector contains numbers
  if (is.numeric(v)) {
    return(as.numeric(most_frequent_value))
  } else {
    return(most_frequent_value)
  }
}

samsvar_naiv <- function(data, var) {
  irr_data <- data |>
    dplyr::select(CASENUMBER, CREATEDBY, dplyr::all_of(var)) |>
    tidyr::pivot_wider(names_from = CREATEDBY,
                       values_from = all_of(var))

  ratings <- irr_data[,-1]

  ant_enige <- apply(
    ratings, 1,
    function(x) {
      mode_dim <- x |> unlist() |> get_mode()
      ant_enige <- sum(x == mode_dim, na.rm = T)
    })
  ikke_na <- apply(
    ratings, 1,
    function(x) {
      ikke_na <- sum(!is.na(x))
    })

  data.frame(var = var,
             ant_enige = sum(ant_enige),
             N = sum(ikke_na))
}


samsvar_reg <- data.frame(
  var = NULL, ant_enige = NULL,  N = NULL)
for (var in setdiff(names(reldata), c("CASENUMBER", "CREATEDBY"))) {
  analyse <- samsvar_naiv(reldata, var)
  samsvar_reg <- dplyr::bind_rows(samsvar_reg, analyse)
}
samsvar_reg <- samsvar_reg |>
  dplyr::mutate(pst_samsvar = round(ant_enige/N*100,1))

kolnavn <- names(reldata)
kolnavn <- kolnavn[kolnavn!="ForlopsType2"]
reldata_no_na <- reldata %>%
  mutate(across(all_of(kolnavn), ~replace_na(., -1)))

samsvar_reg_no_na <- data.frame(
  var = NULL, ant_enige = NULL,  N = NULL)
for (var in setdiff(names(reldata_no_na), c("CASENUMBER", "CREATEDBY"))) {
  analyse <- samsvar_naiv(reldata_no_na, var)
  samsvar_reg_no_na <- dplyr::bind_rows(samsvar_reg_no_na, analyse)
}
samsvar_reg_no_na <- samsvar_reg_no_na |>
  dplyr::mutate(pst_samsvar = round(ant_enige/N*100,1))



# Function to filter columns based on first n rows having no NA
keep_non_na_columns <- function(df, n) {
  # Check each column: are the first n elements all non-NA?
  cols_to_keep <- sapply(df, function(col) all(!is.na(head(col, n))))

  # Subset the data frame to keep only those columns
  df_filtered <- df[, cols_to_keep, drop = FALSE]

  return(df_filtered)
}

# 3. Function to compute IRR for categorical variables
compute_categorical_IRR <- function(data, var, n) {
  cat("\n=== Inter-Rater Reliability for", var, "===\n")

  irr_data <- data %>%
    select(CASENUMBER, CREATEDBY, all_of(var)) %>%
    pivot_wider(names_from = CREATEDBY, values_from = all_of(var))

  ratings <- irr_data[,-1]

  # Drop rows with missing values
  ratings <- na.omit(ratings)
  #
  samsvar = agree(ratings)

  # Krippendorff's alpha (transpose required)
  kripp_result <- kripp.alpha(t(ratings), method = "nominal")

  list(kripp_result = kripp_result,
       samsvar = samsvar)
}

# analyse <- compute_categorical_IRR(
#   data |> filter(CASENUMBER %in% 1:5), var)

data <- reldata_no_na |> filter(CASENUMBER %in% 1:5)

# 5. Run analysis
tabell_kategorisk <- data.frame(Tabell = NULL,
                                Variabel = NULL,
                                Krippendorff = NULL,
                                Samsvar = NULL,
                                ant_ratere = NULL,
                                ant_caser = NULL)
k <- 0
for (var in setdiff(names(data),
                    c("CASENUMBER", "CREATEDBY", "ForlopsType2",
                      "ForlopsType2Num"))) {
  k <- k+1
  analyse <- compute_categorical_IRR(
    data, var)
  tabell_kategorisk <- bind_rows(
    tabell_kategorisk,
    data.frame(Tabell = "Registrering",
               Variabel = var,
               Krippendorff = analyse$kripp_result$value,
               Samsvar = analyse$samsvar$value,
               ant_ratere = analyse$samsvar$raters,
               ant_caser = analyse$samsvar$subjects)
  )
}

data <- reldata_no_na |> filter(CASENUMBER %in% 6:10)

# 5. Run analysis
tabell_kategorisk2 <- data.frame(Tabell = NULL,
                                 Variabel = NULL,
                                 Krippendorff = NULL,
                                 Samsvar = NULL,
                                 ant_ratere = NULL,
                                 ant_caser = NULL)
k <- 0
for (var in setdiff(names(data),
                    c("CASENUMBER", "CREATEDBY", "ForlopsType2",
                      "ForlopsType2Num"))) {
  k <- k+1
  analyse <- compute_categorical_IRR(
    data, var)
  tabell_kategorisk2 <- bind_rows(
    tabell_kategorisk2,
    data.frame(Tabell = "Registrering",
               Variabel = var,
               Krippendorff = analyse$kripp_result$value,
               Samsvar = analyse$samsvar$value,
               ant_ratere = analyse$samsvar$raters,
               ant_caser = analyse$samsvar$subjects)
  )
}


write.csv2(
  samsvar_reg,
  "C:/Users/kth200/regdata/nra/reliabilitet/samsvar_reg.csv",
  row.names = F, fileEncoding = "Latin1")
write.csv2(
  samsvar_reg_no_na,
  "C:/Users/kth200/regdata/nra/reliabilitet/samsvar_reg_na_inkludert.csv",
  row.names = F, fileEncoding = "Latin1")


#### MERK: irr::agree() gir andel caser med 100 % samsvar. Muligens lite egnet
#### når det er mange ratere og få caser




