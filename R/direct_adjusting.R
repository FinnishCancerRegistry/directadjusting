#' @title Directly Adjusted Estimates
#' @description Compute direct adjusted estimates from a table of statistics.
#' @param stats_dt `[data.frame]` (no default)
#'
#' a `data.frame` containing estimates and variance estimates of statistics
#' @param stat_col_nms `[character]` (no default)
#'
#' names of columns in `stats_dt` containing estimates (statistics);
#' `NA` statistics values cause also `NA` confidence intervals
#' @param var_col_nms `[character]` (default `NULL`)
#'
#' - if `NULL`, no confidence intervals can (will) be computed
#' - if `character` vector, names of columns in `stats_dt` containing variance
#'   estimates of the statistics specified in `stat_col_nms` with one-to-one
#'   correspondence; `NA` elements in `var_col_nms` cause no confidence
#'   intervals to computed for those statistics;
#'   `NA` variance estimates in `stats_dt` cause `NA` confidence intervals;
#'   negative values cause an error; `Inf` values cause `c(-Inf, Inf)`
#'   intervals with confidence interval method `"identity"`, etc.
#' @param conf_lvls `[numeric]` (default `0.95`)
#'
#' confidence levels for confidence intervals; you may specify each statistic
#' (see `stat_col_nms`) its own level by supplying a vector of values;
#' values other than between `(0, 1)` cause an error
#' @param conf_methods `[character, list]` (default `"identity"`)
#'
#' Method(s) to compute confidence intervals. Either one method for all stats
#' (`stat_col_nms`) or otherwise this must be of length
#' (`length(stat_col_nms)`). Each element is passed to
#' `[delta_method_confidence_intervals]` separately.
#'
#' Can also be `"none"`: This causes no confidence intervals to be calculated
#' for the respective `stat_col_nms` element(s).
#' @param stratum_col_nms `[NULL, character]` (default `NULL`)
#'
#' names of columns in `stats_dt` by which statistics are stratified (and they
#' should be stratified by these columns after direct adjusting)
#' @param adjust_col_nms `[NULL, character]` (default `NULL`)
#'
#' Names of columns in `stats_dt` by which statistics are currently stratified
#' and by which the statistics should be adjusted (e.g. `"agegroup"`).
#'
#' - `NULL`: If `weights` is a `data.table`, use its stratifying columns. Else
#'   no adjusting is performed.
#' - `character`: Adjust by these columns.
#' @template weights_arg
#' @examples
#'
#' # directadjusting::directly_adjusted_estimates
#' library("data.table")
#' set.seed(1337)
#'
#' offsets <- rnorm(8, mean = 1000, sd = 100)
#' baseline <- 100
#' hrs_by_sex <- rep(1:2, each = 4)
#' hrs_by_ag <- rep(c(0.75, 0.90, 1.10, 1.25), times = 2)
#' counts <- rpois(8, baseline * hrs_by_sex * hrs_by_ag)
#'
#' # raw estimates
#' my_stats <- data.table::data.table(
#'   sex = rep(1:2, each = 4),
#'   ag = rep(1:4, times = 2),
#'   e = counts / offsets,
#'   v = counts / (offsets**2)
#' )
#'
#' # adjusted by age group
#' my_adj_stats <- directly_adjusted_estimates(
#'   stats_dt = my_stats,
#'   stat_col_nms = "e",
#'   var_col_nms = "v",
#'   conf_lvls = 0.95,
#'   conf_methods = "log",
#'   stratum_col_nms = "sex",
#'   adjust_col_nms = "ag",
#'   weights = c(200, 300, 400, 100)
#' )
#'
#' # adjusted by smaller age groups, stratified by larger age groups
#' my_stats[, "ag2" := c(1, 1, 2, 2, 1, 1, 2, 2)]
#' my_adj_stats <- directly_adjusted_estimates(
#'   stats_dt = my_stats,
#'   stat_col_nms = "e",
#'   var_col_nms = "v",
#'   conf_lvls = 0.95,
#'   conf_methods = "log",
#'   stratum_col_nms = c("sex", "ag2"),
#'   adjust_col_nms = "ag",
#'   weights = c(200, 300, 400, 100)
#' )
#'
#' # with no adjusting columns defined you get the same table as input
#' # but with confidence intervals. this for the sake of
#' # convenience for programming cases where sometimes you want to adjust,
#' # sometimes not.
#' stats_dt_2 <- data.table::data.table(
#'   sex = 0:1,
#'   e = 0.0,
#'   v = 0.1
#' )
#' dt_2 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_2,
#'   stat_col_nms = "e",
#'   var_col_nms = "v",
#'   conf_lvls = 0.95,
#'   conf_methods = "identity",
#'   stratum_col_nms = "sex"
#' )
#' stopifnot(
#'   dt_2[["e"]] == stats_dt_2[["e"]],
#'   dt_2[["v"]] == stats_dt_2[["v"]],
#'   dt_2[["sex"]] == stats_dt_2[["sex"]]
#' )
#'
#' # sometimes when adjusting rates or counts, there can be strata where the
#' # statistic is zero. these should be included in your statistics dataset
#' # if you still want the weighted average be influenced by the zero.
#' # otherwise you will get the wrong result. sometimes when naively tabulating
#' # a dataset with e.g. dt[, .N, keyby = "stratum"] one does not get a result
#' # row for a stratum that does not appear in the dataset even if we know that
#' # the stratum exists, for instance only the age groups 1-17 are present in
#' # the dataset.
#' stats_dt_3 <- data.table::data.table(
#'   age_group = 1:18,
#'   count = 17:0,
#'   var = 17:0
#' )
#'
#' # this goes as intended
#' dt_3 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_3,
#'   stat_col_nms = "count",
#'   var_col_nms = "var",
#'   stratum_col_nms = NULL,
#'   adjust_col_nms = "age_group",
#'   weights = data.table::data.table(
#'     age_group = 1:18,
#'     weight = 18:1
#'   )
#' )
#'
#' # this does not
#' dt_4 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_3[1:17, ],
#'   stat_col_nms = "count",
#'   var_col_nms = "var",
#'   stratum_col_nms = NULL,
#'   adjust_col_nms = "age_group",
#'   weights = data.table::data.table(
#'     age_group = 1:18,
#'     weight = 18:1
#'   )
#' )
#'
#' # the weighted average that included the zero is smaller
#' stopifnot(
#'   dt_3[["count"]] < dt_4[["count"]]
#' )
#'
#' # NAs are allowed and produce in turn NAs silently.
#' stats_dt_5 <- data.table::data.table(
#'   age_group = 1:18,
#'   count = c(NA, 16:0),
#'   var = c(NA, 16:0)
#' )
#' dt_5 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_5,
#'   stat_col_nms = "count",
#'   var_col_nms = "var",
#'   adjust_col_nms = "age_group",
#'   weights = data.table::data.table(
#'     age_group = 1:18,
#'     weight = 18:1
#'   )
#' )
#' stopifnot(
#'   is.na(dt_5)
#' )
#'
#' stats_dt_6 <- data.table::data.table(
#'   age_group = 1:4,
#'   survival = c(0.20, 0.40, 0.60, 0.80),
#'   survival_var = 0.05^2
#' )
#' # you can use conf_methods to pass whatever to
#' # `delta_method_confidence_intervals`.
#' dt_6 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_6,
#'   stat_col_nms = "survival",
#'   var_col_nms = "survival_var",
#'   adjust_col_nms = "age_group",
#'   weights = data.table::data.table(
#'     age_group = 1:4,
#'     weight = 1:4
#'   ),
#'   conf_methods = list(
#'     list(
#'       g = quote(stats::qnorm(theta)),
#'       g_inv = quote(stats::pnorm(g))
#'     )
#'   )
#' )
#'
#' # the wame weights data.table can contain more than one weight column
#' dt_7 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_6,
#'   stat_col_nms = "survival",
#'   var_col_nms = "survival_var",
#'   adjust_col_nms = "age_group",
#'   weights = data.table::data.table(
#'     age_group = 1:4,
#'     weight_1 = 1:4,
#'     weight2 = 4:1
#'   )
#' )
#' stopifnot(
#'   c("survival_1", "survival2") %in% names(dt_7)
#' )
#'
#' # ... and multiple statistics columns as well
#' data.table::set(
#'   x = stats_dt_6,
#'   j = c("risk", "risk_var"),
#'   value = list(
#'     1 - stats_dt_6[["survival"]],
#'     stats_dt_6[["survival_var"]]
#'   )
#' )
#' dt_8 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_6,
#'   stat_col_nms = c("survival", "risk"),
#'   var_col_nms = c("survival_var", "risk_var"),
#'   adjust_col_nms = "age_group",
#'   weights = data.table::data.table(
#'     age_group = 1:4,
#'     weight_1 = 1:4,
#'     weight2 = 4:1
#'   )
#' )
#' stopifnot(
#'   c("survival_1", "survival2", "risk_1", "risk2") %in% names(dt_8)
#' )
#'
#' @eval codedoc::pkg_doc_fun("directadjusting::directly_adjusted_estimates")
directly_adjusted_estimates <- function(
  stats_dt,
  stat_col_nms,
  var_col_nms,
  stratum_col_nms = NULL,
  adjust_col_nms = NULL,
  conf_lvls = 0.95,
  conf_methods = "identity",
  weights = NULL
) {
  # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2026-01-22", "0.5.0")
  # `directadjusting::direct_adjusted_estimates` option `conf_methods = "boot"`
  # removed. Only delta method confidence intervals now possible. Making use of
  # the delta method is now more flexible and accepts e.g.
  # `list("log", list(g = quote(qnorm(theta)), g_inv = quote(pnorm(g))))`.
  # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2026-01-22", "0.5.0")
  # assertions -----------------------------------------------------------------
  stopifnot(
    is.data.frame(stats_dt),
    stat_col_nms %in% names(stats_dt),
    is.null(stratum_col_nms) || all(stratum_col_nms %in% names(stats_dt)),
    length(var_col_nms) %in% c(0L, length(stat_col_nms)),
    var_col_nms %in% c(names(stats_dt), NA),

    data.table::between(conf_lvls, lower = 0.0, upper = 1.0, incbounds = FALSE),
    length(conf_lvls) %in% c(1L, length(stat_col_nms)),

    length(conf_methods) %in% c(1L, length(stat_col_nms)),
    inherits(conf_methods, c("character", "list"))
  )
  if (!is.null(var_col_nms)) {
    lapply(setdiff(var_col_nms, NA_character_), function(var_col_nm) {
      eval(substitute(
        stopifnot(
          stats_dt[[VCN]] >= 0 | is.na(stats_dt[[VCN]])
        ),
        list(VCN = var_col_nm)
      ))
    })
  } else {
    var_col_nms <- rep(NA_character_, length(stat_col_nms))
  }
  # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2024-12-12", "0.4.0")
  # `directadjusting::direct_adjusted_estimates` now correctly uses
  # the same `conf_lvls` and `conf_methods` for all statistics when their
  # length is one.
  # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2024-12-12", "0.4.0")
  if (length(conf_lvls) == 1) {
    conf_lvls <- rep(conf_lvls, length(stat_col_nms))
  }
  lapply(seq_along(conf_methods), function(i) {
    if (!is.character(conf_methods[[i]])) {
      return(NULL)
    }
    eval(substitute(
      stopifnot(
        conf_methods[i] %in% ALLOWED
      ),
      list(ALLOWED = allowed_conf_method_strings__())
    ))
  })
  if (length(conf_methods) == 1) {
    conf_methods <- rep(conf_methods, length(stat_col_nms))
  }

  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  # `directadjusting::directly_adjusted_estimates` computes weighted
  # averages and their confidence intervals. Performs the following steps:
  #
  # - Makes a new `data.table` with data from `stats_dt` without copying any
  #   column data to avoid modifying `stats_dt` itself.
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  work_dt <- local({
    keep_col_nms <- setdiff(
      c(stratum_col_nms, adjust_col_nms, stat_col_nms, var_col_nms),
      NA_character_
    )
    work_dt <- data.table::setDT(as.list(stats_dt)[keep_col_nms])
    work_dt[]
  })
  tmp_stratum_col_nm <- NULL
  if (length(stratum_col_nms) == 0L) {
    tmp_stratum_col_nm <- tmp_nms(
      prefixes = "tmp_stratum_col_",
      avoid = names(work_dt)
    )
    stratum_col_nms <- tmp_stratum_col_nm
    data.table::set(work_dt, j = tmp_stratum_col_nm, value = TRUE)
  }

  # prepare data for adjusted estimates and CIs --------------------------------
  if (is.null(adjust_col_nms) && is.data.frame(weights)) {
    # @codedoc_comment_block news("directadjusting::directly_adjusted_estimates", "2026-07-10", "0.7.0")
    # `directadjusting::directly_adjusted_estimates` argument `adjust_col_nms` is
    # now automatically
    # determined based on `weights` if `weights` is a `data.frame` /
    # `data.table`.
    # @codedoc_comment_block news("directadjusting::directly_adjusted_estimates", "2026-07-10", "0.7.0")
    adjust_col_nms <- names(weights)[!grepl("^weight", names(weights))]
  }
  if (length(adjust_col_nms) == 0) {
    # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2025-11-25", "0.5.0")
    # `directadjusting::direct_adjusted_estimates` now allows for the sake of
    # convenience to be called with no `adjust_col_nms` defined. This results
    # in no adjusting.
    # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2025-11-25", "0.5.0")
    adjust_col_nms <- tmp_nms(
      prefixes = "tmp_adjust_stratum_",
      avoid = names(work_dt)
    )
    data.table::set(
      x = work_dt,
      j = adjust_col_nms,
      value = TRUE
    )
    weights_dt <- data.table::data.table(
      x = TRUE,
      weight = 1.0
    )
    data.table::setnames(weights_dt, "x", adjust_col_nms)
  } else {
    # @codedoc_comment_block directadjusting::directly_adjusted_estimates
    # - Handle argument `weights` in order to produce a `data.table` of weights
    #   if it wasn't one already.
    # @codedoc_comment_block directadjusting::directly_adjusted_estimates
    weights_dt <- weights_arg_to_weights_dt(
      weights = weights,
      stats_dt = work_dt,
      adjust_col_nms = adjust_col_nms
    )
  }

  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  # - Insert the weights into `stats_dt`.
  # @codedoc_insert_comment_block directadjusting:::add_weight_columns__
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  add_weight_columns__(
    stats_dt = work_dt,
    stratum_col_nms = stratum_col_nms,
    weights_dt = weights_dt,
    adjust_col_nms = adjust_col_nms
  )
  tmp_weight_col_nms <- attr(work_dt, "tmp_weight_col_nms")

  # prep output ----------------------------------------------------------------
  out <- work_dt[
    i = !duplicated(work_dt, by = stratum_col_nms),
    j = .SD,
    .SDcols = stratum_col_nms
  ]

  # delta method confidence intervals ------------------------------------------
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  # - Compute weighted averages of `stat_col_nms` and `var_col_nms`
  #   (the latter with squared weights because they are variances)
  #   over `adjust_col_nms`. This results in a `data.table` without column(s)
  #   `adjust_col_nms`.
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  meta_dt <- lapply(seq_along(stat_col_nms), function(i) {
    # @codedoc_comment_block news("directadjusting::directly_adjusted_estimates", "2026-07-13", "0.7.0")
    # `directadjusting::directly_adjusted_estimates` can now handle `weights`
    # `data.table` with more than one column containing weights, e.g.
    # `weight_europe`, `weight_world`.
    # @codedoc_comment_block news("directadjusting::directly_adjusted_estimates", "2026-07-13", "0.7.0")
    suffix_set <- names(tmp_weight_col_nms)
    meta_dt <- lapply(seq_along(suffix_set), function(j) {
      tmp_weight_col_nm_ij <- tmp_weight_col_nms[j]
      stat_col_nms_ij <- paste0(stat_col_nms[i], suffix_set[j])
      var_col_nms_ij <- paste0(var_col_nms[i], suffix_set[j])
      data.table::set(
        x = work_dt,
        j = stat_col_nms_ij,
        value = stats_dt[[stat_col_nms[i]]] * work_dt[[tmp_weight_col_nm_ij]]
      )
      data.table::set(
        x = work_dt,
        j = var_col_nms_ij,
        value = stats_dt[[var_col_nms[i]]] *
          (work_dt[[tmp_weight_col_nm_ij]]^2)
      )
      data.table::data.table(
        stat_col_nm = stat_col_nms[i],
        stat_col_nm_w = stat_col_nms_ij,
        var_col_nm = var_col_nms[i],
        var_col_nm_w = var_col_nms_ij
      )[]
    })
    meta_dt <- data.table::rbindlist(meta_dt)
    return(meta_dt[])
  })
  meta_dt <- data.table::rbindlist(meta_dt)
  local({
    value_col_nms <- c(meta_dt[["stat_col_nm_w"]], meta_dt[["var_col_nm_w"]])
    adjusted_stats_dt <- work_dt[
      #' @importFrom data.table .SD
      j = lapply(.SD, sum),
      .SDcols = value_col_nms,
      keyby = eval(stratum_col_nms)
    ]
    data.table::set(
      x = out,
      j = value_col_nms,
      value = as.list(adjusted_stats_dt)[value_col_nms]
    )
  })
  data.table::set(
    x = meta_dt,
    j = c("conf_method", "conf_lvl"),
    value = list(
      as.list(rep(conf_methods, each = length(tmp_weight_col_nms))),
      rep(conf_lvls, each = length(tmp_weight_col_nms))
    )
  )
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  # - For each statistic:
  #   + If the corresponding `conf_methods` element is `"none"`, skip confidence
  #     intervals computation.
  #   + Otherwise call `[delta_method_confidence_intervals]`.
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  lapply(seq_len(nrow(meta_dt)), function(i) {
    if (
      is.character(meta_dt[["conf_method"]][[i]]) &&
        meta_dt[["conf_method"]][[i]] == "none"
    ) {
      return(NULL)
    }
    ci_dt <- delta_method_confidence_intervals(
      statistics = out[[meta_dt[["stat_col_nm_w"]][[i]]]],
      variances = out[[meta_dt[["var_col_nm_w"]][[i]]]],
      conf_lvl = meta_dt[["conf_lvl"]][[i]],
      conf_method = meta_dt[["conf_method"]][[i]]
    )

    ci_col_nms <- paste0(meta_dt[["stat_col_nm_w"]][[i]], c("_lo", "_hi"))
    data.table::set(
      x = out,
      j = ci_col_nms,
      value = as.list(ci_dt)[c("ci_lo", "ci_hi")]
    )
    data.table::set(
      x = meta_dt,
      i = i,
      j = c("ci_lo_col_nm_w", "ci_hi_col_nm_w"),
      value = as.list(ci_col_nms)
    )
    return(NULL)
  })

  # final touches --------------------------------------------------------------
  ordered_stat_col_nms <- unlist(
    lapply(seq_len(nrow(meta_dt)), function(i) {
      unlist(meta_dt[
        i = i,
        j = .SD,
        .SDcols = c(
          "stat_col_nm_w",
          "var_col_nm_w",
          "ci_lo_col_nm_w",
          "ci_hi_col_nm_w"
        )
      ])
    }),
    use.names = FALSE
  )
  data.table::setcolorder(out, c(stratum_col_nms, ordered_stat_col_nms))
  data.table::setkeyv(out, stratum_col_nms)
  if (identical(stratum_col_nms, tmp_stratum_col_nm)) {
    data.table::set(out, j = tmp_stratum_col_nm, value = NULL)
    stratum_col_nms <- NULL
  }
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  # - Set attribute `directly_adjusted_estimates_meta`. It is a list
  #   containing:
  #   + `call`: The call to `directadjusting::directly_adjusted_estimates`.
  #   + `stat_col_nms`: The argument as given by the user.
  #   + `var_col_nms`: The argument as given by the user.
  #   + `stratum_col_nms`: The argument as given by the user.
  #   + `adjust_col_nms`: The argument as given by the user.
  #   + `conf_lvls`: The argument, but always of length `length(stat_col_nms)`.
  #   + `conf_methods`: The argument, but always of length
  #     `length(stat_col_nms)`.
  #   + `meta_dt`: Contains columns
  #     * `stat_col_nm`: Elements of argument `stat_col_nms`.
  #     * `stat_col_nm_w`: Corresponding name of column containing weighted
  #       statistics. These columns names are in the output of
  #       `directadjusting::directly_adjusted_estimates`.
  #     * `var_col_nm`: Elements of argument `var_col_nms`.
  #     * `var_col_nm_w`: Like `stat_col_nm_w` but for variance column names.
  #     * `conf_method`: Correspoding `conf_methods` elements.
  #     * `conf_lvl`: Correspoding `conf_lvls` elements.
  #     * `ci_lo_col_nm_w`: Name of confidence interval lower bound of the
  #        weighted statistic.
  #     * `ci_hi_col_nm_w`: Name of confidence interval upper bound of the
  #        weighted statistic.
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  call <- match.call()
  data.table::setattr(
    out,
    "directly_adjusted_estimates_meta",
    mget(c(
      "call",
      "stat_col_nms",
      "var_col_nms",
      "stratum_col_nms",
      "adjust_col_nms",
      "conf_lvls",
      "conf_methods",
      "meta_dt"
    ))
  )

  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  # - Return a `data.table`. Returned columns are those given via
  #   `stratum_col_nms`, `stat_col_nms`, and `var_col_nms` and additional
  #   confidence interval columns. If `weights` was a
  #   `data.table`, the names of the stats, variances, and confidence bounds
  #   columns are identified by the respective suffix of the weight column,
  #   e.g. `stat_col_nms = c("s", "r")` and
  #   `weights = data.table(age_group = 1:4, weight_1 = 1:4, weight2 = 4:1)`
  #   will result in output columns `s_1`, `s2`, `r_1`, and `r2` etc.
  # @codedoc_comment_block directadjusting::directly_adjusted_estimates
  #' @return
  #' Returns a `data.table`. Returned columns are those given via
  #' `stratum_col_nms`, `stat_col_nms`, and `var_col_nms` and additional
  #' confidence interval columns. If `weights` was a
  #' `data.table`, the names of the stats, variances, and confidence bounds
  #' columns are identified by the respective suffix of the weight column,
  #' e.g. `stat_col_nms = c("s", "r")` and
  #' `weights = data.table(age_group = 1:4, weight_1 = 1:4, weight2 = 4:1)`
  #' will result in output columns `s_1`, `s2`, `r_1`, and `r2` etc.
  return(out[])
}

# @codedoc_comment_block news("directadjusting", "2024-12-10", "0.3.0")
# Remove deprecated `directadjusting::direct_adjusted_estimates`. Use
# `directadjusting::directly_adjusted_estimates`.
# @codedoc_comment_block news("directadjusting", "2024-12-10", "0.3.0")
