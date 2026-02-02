weights_arg_to_weights_dt <- function(
  weights,
  adjust_col_nms,
  stats_dt
) {
  UseMethod("weights_arg_to_weights_dt")
}

#' @export
weights_arg_to_weights_dt.numeric <- function(
  weights,
  adjust_col_nms,
  stats_dt
) {
  stopifnot(
    length(weights) == data.table::uniqueN(stats_dt[[adjust_col_nms]])
  )

  weights_dt <- data.table::data.table(
    adjust_col = sort(unique(stats_dt[[adjust_col_nms]])),
    weight = weights
  )
  data.table::setnames(weights_dt, "adjust_col", adjust_col_nms)

  weights_dt[]
}

assert_is_weights_dt <- function(
  weights_dt,
  adjust_col_nms,
  stats_dt
) {
  lapply(adjust_col_nms, function(col_nm) {
    miss_levels <- setdiff(stats_dt[[col_nm]], weights_dt[[col_nm]])
    if (length(miss_levels) > 0) {
      stop("Levels in stats_dt$", col_nm, " not in weights_dt$", col_nm, ": ",
           deparse(miss_levels))
    }
  })
  invisible(NULL)
}

#' @export
weights_arg_to_weights_dt.data.table <- function(
  weights,
  adjust_col_nms,
  stats_dt
) {
  assert_is_weights_dt(weights_dt = weights,
                       adjust_col_nms = adjust_col_nms,
                       stats_dt = stats_dt)
  weights
}

add_weights_column <- function(
  stats_dt,
  stratum_col_nms,
  weights_dt,
  adjust_col_nms = setdiff(names(weights_dt), "weight")
) {
  tmp_col_nms <- tmp_nms(
    prefixes = c("tmp_w_", "tmp_w_sum_"),
    avoid = union(names(stats_dt), names(weights_dt)),
  )
  tmp_w_col_nm <- tmp_col_nms[1]
  tmp_w_sum_col_nm <- tmp_col_nms[2]
  # @codedoc_comment_block directadjusting:::add_weights_column
  #   + Weights are merged into `stats_dt` in-place by making a left join
  #     on `weights_dt` using `stats_dt` and adding column `weight` resulting
  #     from this join into `stats_dt`.
  # @codedoc_comment_block directadjusting:::add_weights_column
  data.table::set(
    x = stats_dt,
    j = tmp_w_col_nm,
    value = weights_dt[
      i = stats_dt,
      on = eval(adjust_col_nms),
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = "weight"
    ]
  )
  # @codedoc_comment_block directadjusting:::add_weights_column
  #   + Re-scale weights to sum to one within each stratum defined by
  #     `stratum_col_nms`.
  # @codedoc_comment_block directadjusting:::add_weights_column
  stats_dt[
    #' @importFrom data.table .SD :=
    j = (tmp_w_sum_col_nm) := lapply(.SD, sum),
    .SDcols = tmp_w_col_nm,
    by = eval(stratum_col_nms)
  ]
  data.table::set(
    stats_dt,
    j = tmp_w_col_nm,
    value = stats_dt[[tmp_w_col_nm]] / stats_dt[[tmp_w_sum_col_nm]]
  )
  data.table::set(stats_dt, j = tmp_w_sum_col_nm, value = NULL)
  data.table::setattr(stats_dt, "tmp_w_col_nm", tmp_w_col_nm)
  invisible(stats_dt)
}
