
library("data.table")

wdt <- data.table::data.table(
  agegroup = c("0-4", "5-9", "10-14", "15-19", "20-24",
               "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
               "60-64", "65-69", "70-74", "75-79", "80-84", "85"),
  world = c(12000L,
            10000L, 9000L, 9000L, 8000L, 8000L, 6000L, 6000L, 6000L, 6000L,
            5000L, 4000L, 4000L, 3000L, 2000L, 1000L, 500L, 500L),
  europe = c(8000L,
             7000L, 7000L, 7000L, 7000L, 7000L, 7000L, 7000L, 7000L, 7000L,
             7000L, 6000L, 5000L, 4000L, 3000L, 2000L, 1000L, 1000L),
  nordic = c(5900L,
             6600L, 6200L, 5800L, 6100L, 6800L, 7300L, 7300L, 7000L, 6900L,
             7400L, 6100L, 4800L, 4100L, 3900L, 3500L, 2400L, 1900L)
)

wdt <- data.table::melt(
  data = wdt,
  id.vars = "agegroup",
  measure.vars = c("world", "europe", "nordic"),
  variable.name = "schema_name",
  value.name = "weight"
)
wdt[, "age_low" := rep(seq(0, 85, 5), 3)]
wdt[, "n_age_groups" := .N, by = "schema_name"]

integrated_weight_schemas <- wdt[
  j = .(schema_name, n_age_groups, age_low, weight)
]

usethis::use_data(integrated_weight_schemas, internal = FALSE, overwrite = TRUE)
