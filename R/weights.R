




weights_arg_to_weights_dt <- function(
  weights,
  adjust_col_nms,
  stats_dt
) {
  UseMethod("weights_arg_to_weights_dt")
}

#' @importFrom data.table uniqueN setnames data.table
weights_arg_to_weights_dt.numeric <- function(
  weights,
  adjust_col_nms,
  stats_dt
) {
  assert_is_double_nonNA_gtzero_vector(weights)
  assert_is_character_nonNA_atom(adjust_col_nms)
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
  assert_is_data_table_with_required_names(
    x = weights_dt,
    required_names = c(adjust_col_nms, "weight")
  )
  assert_is_data_table_with_required_names(
    x = stats_dt,
    required_names = c(adjust_col_nms)
  )
  assert_is_double_nonNA_gtezero_vector(weights_dt[["weight"]])
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

#' @export
weights_arg_to_weights_dt.character <- function(
  weights,
  adjust_col_nms,
  stats_dt
) {
  weight_vector <- weightschemastring_to_weights(weights)
  weights_arg_to_weights_dt(weights, adjust_col_nms, stats_dt)
}

weightschemastring_to_weights <- function(string) {
  stop("TODO")
}

#' @importFrom data.table set := setattr
add_weights_column <- function(
  stats_dt,
  stratum_col_nms,
  weights_dt,
  adjust_col_nms = setdiff(names(weights_dt), "weight")
) {
  assert_is_data_table_with_required_names(
    stats_dt,
    required_names = c(stratum_col_nms, adjust_col_nms)
  )
  assert_is_data_table_with_required_names(
    weights_dt,
    required_names = c(adjust_col_nms, "weight")
  )

  tmp_col_nms <- tmp_nms(
    prefixes = c("tmp_w_", "tmp_w_sum_"),
    avoid = union(names(stats_dt), names(weights_dt)),
  )
  tmp_w_col_nm <- tmp_col_nms[1]
  tmp_w_sum_col_nm <- tmp_col_nms[2]
  data.table::set(
    stats_dt,
    j = tmp_w_col_nm,
    value = weights_dt[
      i = stats_dt,
      on = eval(adjust_col_nms),
      j = .SD,
      .SDcols = "weight"
      ]
  )
  stats_dt[
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















