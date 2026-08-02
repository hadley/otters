library(nanoparquet)

otters <- read.csv("seot_morphometricsReproStatus_ak_monson.csv")

# One tail length uses a comma as the decimal separator ("27,5"), which makes
# the whole column read as text.
otters$TAIL_LGTH_2 <- as.numeric(gsub(",", ".", otters$TAIL_LGTH_2, fixed = TRUE))

# Missing values are coded as -9 in the numeric columns and "." in the text
# columns.
num_cols <- c(
  "WEIGHT", "TAIL_LGTH_1", "TAIL_LGTH_2", "TAIL_LGTH_3", "mean_tail_lgth",
  "LGTH1", "LGTH2", "LGTH3", "mean_lgth", "true_standard_lgth", "body_lgth",
  "CURVE_LGTH1", "CURVE_LGTH2", "GIRTH1", "GIRTH2", "mean_girth", "PAW",
  "PUP_WGHT", "PUP_LGTH", "PUP_CURVLGTH", "FETUS_NUM", "FETUS_WT",
  "FETUS_LTH", "CAN_DIA", "FINAL_AGE", "AGE_CATEGORY", "BACULA_LGTH"
)
for (col in num_cols) {
  otters[[col]][otters[[col]] == -9] <- NA
}

chr_cols <- c(
  "W_PUP", "PUP_NUMBER", "PUP_SEX", "FETUS_PRES", "FETUS_SEX", "FE_REP_CON",
  "FE_REP_STA", "PREGNANCY_STATUS", "comments"
)
for (col in chr_cols) {
  otters[[col]][otters[[col]] == "."] <- NA
}

# One fetus is recorded as present but weighing 0 g.
otters$FETUS_WT[otters$FETUS_WT == 0] <- NA

otters$DATE <- as.Date(otters$DATE)

# The CSV mixes upper and lower case, and read.csv turned the space in
# "OTTER NO" into a period. Use snake_case throughout.
names(otters) <- tolower(gsub(".", "_", names(otters), fixed = TRUE))

# state is "Alaska" in every row, and year just repeats the year of date.
otters$state <- NULL
otters$year <- NULL

# A female carrying more than one fetus gets one row per fetus, with every
# other column repeated identically. Give each capture event an id, then move
# the per-fetus columns into their own table.
fetus_cols <- c("fetus_num", "fetus_sex", "fetus_wt", "fetus_lth")
capture_cols <- setdiff(names(otters), fetus_cols)

# Numbering the events with cumsum() relies on each repeated row sitting
# directly below the row it repeats.
repeated <- duplicated(otters[capture_cols])
stopifnot(isTRUE(all.equal(
  otters[which(repeated), capture_cols],
  otters[which(repeated) - 1L, capture_cols],
  check.attributes = FALSE
)))
otters$measurement_id <- cumsum(!repeated)

fetuses <- otters[otters$fetus_pres %in% c("Y", "M"), c("measurement_id", fetus_cols)]
otters <- otters[!repeated, c("measurement_id", capture_cols)]

# region and area describe the place, not the capture: each of the 68
# locations falls in exactly one region and one area. Coordinates vary from
# capture to capture within a location, so they stay put.
locations <- unique(otters[c("location", "region", "area")])
stopifnot(!any(duplicated(locations$location)))
locations <- locations[order(locations$location), ]
otters[c("region", "area")] <- NULL

# sex and recap describe the animal, not the capture, and are constant across
# the 117 otters that were caught more than once. Everything else varies from
# capture to capture, so it becomes a measurement.
otter_cols <- c("otter_no", "sex", "recap")
measurements <- otters[setdiff(names(otters), c("sex", "recap"))]
otters <- unique(otters[otter_cols])
stopifnot(!any(duplicated(otters$otter_no)))
otters <- otters[order(otters$otter_no), ]

write_parquet(otters, "otters.parquet")
write_parquet(measurements, "measurements.parquet")
write_parquet(locations, "locations.parquet")
write_parquet(fetuses, "fetuses.parquet")
