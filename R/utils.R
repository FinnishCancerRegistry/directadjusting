tmp_nms <- function(
  prefixes = "tmp_nm_",
  suffixes = "",
  avoid = "",
  pool = letters,
  n_random_elems = 10L,
  n_max_tries = 100L
) {
  df <- data.frame(prefix = prefixes, suffx = suffixes, tmp_nm = "",
                   stringsAsFactors = FALSE)
  n_nms <- nrow(df)
  avoid <- c(avoid, "")
  for (i in seq(n_nms)) {
    while (df[["tmp_nm"]][i] %in% avoid) {
      random_part <- sample(pool, size = n_random_elems, replace = TRUE)
      random_part <- paste0(random_part, collapse = "")
      df[["tmp_nm"]][i] <- paste0(
        df[["prefix"]][i], random_part, df[["suffix"]][i]
      )
    }
    avoid <- c(avoid, df[["tpm_nm"]][i])
  }
  df[["tmp_nm"]]
}
