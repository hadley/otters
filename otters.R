library(nanoparquet)

otters <- read.csv("seot_morphometricsReproStatus_ak_monson.csv", na.strings = c("", "NA"))

# Replace -9 sentinel values with NA in numeric columns
num_cols <- c(
  "WEIGHT", "TAIL_LGTH_1", "TAIL_LGTH_2", "TAIL_LGTH_3",
  "mean_tail_lgth", "LGTH1", "LGTH2", "LGTH3", "mean_lgth",
  "true_standard_lgth", "body_lgth", "CURVE_LGTH1", "CURVE_LGTH2",
  "GIRTH1", "GIRTH2", "mean_girth", "PAW", "PUP_WGHT", "PUP_LGTH",
  "PUP_CURVLGTH", "FETUS_NUM", "FETUS_WT", "FETUS_LTH",
  "CAN_DIA", "FINAL_AGE", "AGE_CATEGORY", "BACULA_LGTH"
)
for (col in num_cols) {
  otters[[col]][otters[[col]] == -9] <- NA
}

# Replace "." sentinel values with NA in string columns
str_cols <- c(
  "W_PUP", "PUP_NUMBER", "PUP_SEX", "FETUS_PRES", "FETUS_SEX",
  "FE_REP_CON", "FE_REP_STA", "PREGNANCY_STATUS", "comments"
)
for (col in str_cols) {
  otters[[col]][otters[[col]] == "."] <- NA
}

# Parse dates
otters$DATE <- as.Date(otters$DATE)

# Standardise column names to lower_snake_case
names(otters) <- tolower(gsub("[. ]+", "_", names(otters)))

write_parquet(otters, "otters.parquet")
