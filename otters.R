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

# The measurements are recorded to one decimal place and the means are kept to
# two, which is exact for a mean of two and spares us guessing which way the
# source rounded a half.

# The stored means don't always average the replicates taken at the capture.
# mean_lgth sometimes holds the mean across all of an otter's captures: of the
# 69 otters whose lgth1 differs between captures, 20 carry an identical
# mean_lgth on every one. Two tail means average a subset of their replicates
# and one was left blank. Recompute all three from the capture's replicates.
# Four captures record a length only as mean_lgth and keep what they have.
mean_of <- list(
  mean_tail_lgth = c("tail_lgth_1", "tail_lgth_2", "tail_lgth_3"),
  mean_lgth      = c("lgth1", "lgth2", "lgth3"),
  mean_girth     = c("girth1", "girth2")
)
for (mean_col in names(mean_of)) {
  rep_cols <- mean_of[[mean_col]]
  measured <- rowSums(!is.na(otters[rep_cols])) > 0
  otters[[mean_col]][measured] <-
    round(rowMeans(otters[rep_cols], na.rm = TRUE), 2)[measured]
}

# Standard-length captures take their true standard length from mean_lgth, so
# it follows. Curvilinear ones derive it from curve_lgth1 and are left alone.
standard <- !is.na(otters$mean_lgth)
otters$true_standard_lgth[standard] <-
  round(otters$mean_lgth * otters$curvilinear_correction, 2)[standard]

# body_lgth is the true standard length less the tail.
tailed <- !is.na(otters$true_standard_lgth) & !is.na(otters$mean_tail_lgth)
otters$body_lgth[tailed] <-
  round(otters$true_standard_lgth - otters$mean_tail_lgth, 2)[tailed]

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

# A pup travelling with its mother was measured on her row: pup_wght, pup_lgth
# and pup_curvlgth are the pup's own weight, true standard length and
# curvilinear length. Where the pup was captured in its own right the same day
# those numbers duplicate its record; otherwise they are the only trace of the
# animal. Give every named pup a measurement of its own.

# One female has her own id typed into pup_number, and one record measures a
# pup nobody named. Both pups need an id we make up.
measured <- with(measurements, !is.na(pup_wght) | !is.na(pup_lgth) | !is.na(pup_curvlgth))
nameless <- measured &
  (is.na(measurements$pup_number) | measurements$pup_number == measurements$otter_no)
measurements$pup_number[nameless] <- paste0("synthetic-", seq_len(sum(nameless)))

# The mother's copy is redundant only when the pup already has a row that day.
new_pup <- !is.na(measurements$pup_number) &
  !paste(measurements$pup_number, measurements$date) %in%
    paste(measurements$otter_no, measurements$date)

pups <- measurements[new_pup, ]
carried <- c("date", "location", "lat", "long", "cause_of_death_capture_method")
for (col in setdiff(names(pups), carried)) {
  pups[[col]] <- pups[[col]][NA_integer_]  # blank, but keep the column's type
}
pups$otter_no <- measurements$pup_number[new_pup]
pups$weight <- measurements$pup_wght[new_pup]
pups$true_standard_lgth <- measurements$pup_lgth[new_pup]
pups$curve_lgth1 <- measurements$pup_curvlgth[new_pup]
pups$w_pup <- "D"
pups$measurement_id <- max(measurements$measurement_id) + seq_len(nrow(pups))

measurements <- rbind(measurements, pups)

# Pups met only through their mother join the otter table; their sex was
# recorded as pup_sex. recap follows from how many times they were measured.
new_otters <- data.frame(
  otter_no = measurements$pup_number[new_pup],
  sex = measurements$pup_sex[new_pup]
)
new_otters <- unique(new_otters[!new_otters$otter_no %in% otters$otter_no, ])
stopifnot(!any(duplicated(new_otters$otter_no)))
new_otters$recap <- as.integer(table(pups$otter_no)[new_otters$otter_no] > 1)
otters <- rbind(otters, new_otters)
otters <- otters[order(otters$otter_no), ]

measurements[c("pup_sex", "pup_wght", "pup_lgth", "pup_curvlgth")] <- NULL

# Tail length, length and girth were each measured up to three times, but the
# repeat columns are nearly empty: girth2 holds 4 values. Move the individual
# measurements into their own table and leave the capture holding the mean.
replicate_cols <- list(
  tail_lgth = c("tail_lgth_1", "tail_lgth_2", "tail_lgth_3"),
  lgth      = c("lgth1", "lgth2", "lgth3"),
  girth     = c("girth1", "girth2")
)
replicates <- do.call(rbind, lapply(names(replicate_cols), function(quantity) {
  cols <- replicate_cols[[quantity]]
  long <- data.frame(
    measurement_id = rep(measurements$measurement_id, times = length(cols)),
    quantity = quantity,
    replicate = rep(seq_along(cols), each = nrow(measurements)),
    value = unlist(measurements[cols], use.names = FALSE)
  )
  long[!is.na(long$value), ]
}))
replicates <- replicates[order(replicates$measurement_id, replicates$quantity, replicates$replicate), ]
row.names(replicates) <- NULL
measurements[unlist(replicate_cols)] <- NULL

# The mean is now the only version of each, so it can drop the mean_ prefix.
# curve_lgth1 is the sole curvilinear measurement: curve_lgth2 is not a
# replicate of it, so neither joins the replicates table.
renames <- c(mean_tail_lgth = "tail_lgth", mean_lgth = "lgth",
             mean_girth = "girth", curve_lgth1 = "curve_lgth",
             curve_lgth2 = "curve_lgth_alt")
names(measurements) <- ifelse(names(measurements) %in% names(renames),
                              renames[names(measurements)], names(measurements))

write_parquet(otters, "otters.parquet")
write_parquet(measurements, "measurements.parquet")
write_parquet(locations, "locations.parquet")
write_parquet(fetuses, "fetuses.parquet")
write_parquet(replicates, "replicates.parquet")
