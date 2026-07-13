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
      stop(
        "Levels in stats_dt$",
        col_nm,
        " not in weights_dt$",
        col_nm,
        ": ",
        deparse(miss_levels)
      )
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
  assert_is_weights_dt(
    weights_dt = weights,
    adjust_col_nms = adjust_col_nms,
    stats_dt = stats_dt
  )
  weights
}

add_weight_columns__ <- function(
  stats_dt,
  stratum_col_nms,
  weights_dt,
  adjust_col_nms = setdiff(names(weights_dt), "weight")
) {
  weight_col_nms <- names(weights_dt)[grepl("^weight", names(weights_dt))]
  tmp_weight_col_nms <- tmp_nms(
    prefixes = paste0(weight_col_nms, "_"),
    avoid = union(names(stats_dt), names(weights_dt)),
  )
  tmp_weight_sum_col_nm <- tmp_nms(
    prefixes = "tmp_w_sum_",
    avoid = union(names(stats_dt), names(weights_dt)),
  )
  # @codedoc_comment_block directadjusting:::add_weight_columns__
  #   + Weights are merged into `stats_dt` in-place by making a left join
  #     of `stats_dt` and `weights_dt`.
  # @codedoc_comment_block directadjusting:::add_weight_columns__
  data.table::set(
    x = stats_dt,
    j = tmp_weight_col_nms,
    value = weights_dt[
      i = stats_dt,
      on = adjust_col_nms,
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = weight_col_nms
    ]
  )
  # @codedoc_comment_block directadjusting:::add_weight_columns__
  #   + Re-scale weights to sum to one within each stratum defined by
  #     `stratum_col_nms`.
  # @codedoc_comment_block directadjusting:::add_weight_columns__
  lapply(tmp_weight_col_nms, function(tmp_weight_col_nm) {
    stats_dt[
      #' @importFrom data.table .SD :=
      j = (tmp_weight_sum_col_nm) := lapply(.SD, sum),
      .SDcols = tmp_weight_col_nm,
      by = eval(stratum_col_nms)
    ]
    data.table::set(
      stats_dt,
      j = tmp_weight_col_nm,
      value = stats_dt[[tmp_weight_col_nm]] / stats_dt[[tmp_weight_sum_col_nm]]
    )
  })
  data.table::set(stats_dt, j = tmp_weight_sum_col_nm, value = NULL)
  names(tmp_weight_col_nms) <- sub("weight", "", weight_col_nms)
  data.table::setattr(stats_dt, "tmp_weight_col_nms", tmp_weight_col_nms)
  invisible(stats_dt)
}
