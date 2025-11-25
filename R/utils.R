




dt_rep <- function(dt, times = 1L, each = 1L) {
  big_dt <- data.table::setDT(lapply(dt, function(col) {
    rep(col, times = times, each = each)
  }))
  idx <- rep(seq_len(nrow(dt)), each = each, times = times)
  big_dt <- dt[idx, ]
  return(big_dt[])
}





partial_cross_join <- function(
  dt = data.table::data.table(
    a1 = c(1, 1, 2, 2),
    a2 = 1:4,
    b1 = c(1, 1, 2, 2),
    b2 = 1:4
  ),
  noncj_col_nm_sets = list(c("a1", "a2"), c("b1", "b2"))
) {

  level_dts <- lapply(noncj_col_nm_sets, function(col_nm_set) {
    dt <- data.table::setDT(lapply(col_nm_set, function(col_nm) {
      dt[[col_nm]]
    }))
    data.table::setnames(dt, names(dt), col_nm_set)
    dt <- unique(dt, by = col_nm_set)
    data.table::setkeyv(dt, col_nm_set)
    dt[]
  })
  level_dt_rows <- vapply(level_dts, nrow, integer(1))
  total_rows <- as.integer(prod(level_dt_rows))

  rep_args <- lapply(seq_along(level_dts), function(i) {
    total_repeats <- total_rows / level_dt_rows[i]
    each <- as.integer(prod(level_dt_rows[i:length(level_dt_rows)]))
    each <- each / level_dt_rows[i]
    times <- total_repeats / each
    mget(c("each", "times"))
  })

  out <- data.table::data.table(
    .____TMP = rep(NA, total_rows)
  )
  total_cols <- prod(vapply(level_dts, ncol, integer(1)))
  data.table::alloc.col(out, n = total_cols + 1024L)
  lapply(seq_along(level_dts), function(i) {
    level_dt <- level_dts[[i]]
    rep_args <- rep_args[[i]]
    level_dt <- dt_rep(level_dt, each = rep_args[["each"]],
                       times = rep_args[["times"]])
    data.table::set(
      x = out,
      j = names(level_dt),
      value = level_dt
    )
    NULL
  })
  data.table::set(out, j = ".____TMP", value = NULL)
  data.table::setkeyv(out, names(out))
  out[]
}





tmp_nms <- function(
  prefixes = "tmp_nm_",
  suffixes = "",
  avoid = "",
  pool = letters,
  n_random_elems = 10L,
  n_max_tries = 100L
) {
  # assertions -----------------------------------------------------------------
  assert_is_character_vector(prefixes)
  assert_is_character_vector(suffixes)
  assert_is_character_vector(avoid)
  stopifnot(
    length(prefixes) %% length(suffixes) == 0L ||
      length(suffixes) %% length(prefixes) == 0L
  )
  assert_is_character_nonNA_vector(pool)
  assert_is_integer_gtezero_atom(n_random_elems)
  assert_is_integer_gtezero_atom(n_max_tries)

  # sample random names --------------------------------------------------------

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



















