library(nanoparquet)

otters <- read.csv("seot_morphometricsReproStatus_ak_monson.csv")

write_parquet(otters, "otters.parquet")
