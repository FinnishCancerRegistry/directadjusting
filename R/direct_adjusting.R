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
#' - `NULL`: No adjusting is performed.
#' - `character`: Adjust by these columns.
#' @template weights_arg
#' @section Weights:
#'
#' The weights are scaled internally to sum to one, but they need to be positive
#' numbers (or zero). The scaling is performed separately by each unique
#' combination of `stratum_col_nms` columns. This allows you to have e.g.
#' two hierarchical variables, one used for adjusting and one for stratifying
#' output (such as 18 age groups of 5 years for adjusting and 4 larger age
#' groups for stratifying output). See **Examples**.
#'
#' @examples
#'
#' # suppose we have poisson rates that we want to adjust for by age group.
#' # they are stratified by sex.
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
#'   v = counts / (offsets ** 2)
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
#' my_stats[, "ag2" := c(1,1, 2,2, 1,1, 2,2)]
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
#' # survival example; see help("survival.formula")
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   library("survival")
#'   library("data.table")
#'   fit <- survfit(Surv(time, status) ~ x, data = aml)
#'   surv_stats <- summary(fit, times = 0:40)
#'   surv_dt <- data.table::data.table(
#'     x = surv_stats[["strata"]],
#'     time = surv_stats[["time"]],
#'     surv = surv_stats[["surv"]],
#'     var = surv_stats[["std.err"]] ** 2
#'   )
#'   surv_dt_adj <- directly_adjusted_estimates(
#'     stats_dt = surv_dt,
#'     stat_col_nms = "surv",
#'     var_col_nms = "var",
#'     conf_lvls = 0.95,
#'     conf_methods = "log-log",
#'     stratum_col_nms = "time",
#'     adjust_col_nms = "x",
#'     weights = c(600, 400)
#'   )
#'   print(surv_dt_adj, nrows = 10)
#'   matplot(
#'     y = surv_dt_adj[, list(surv, surv_lo, surv_hi)],
#'     x = surv_dt_adj[["time"]], type = "s", col = 1, lty = 1,
#'     xlab = "time", ylab = "survival",
#'     main = "Survival with 95 % CIs"
#'   )
#' }
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
#'   var = 0.05 ^ 2
#' )
#'
#' # you can use conf_method to pass whatever to
#' # `delta_method_confidence_intervals`.
#' dt_6 <- directadjusting::directly_adjusted_estimates(
#'   stats_dt = stats_dt_6,
#'   stat_col_nms = "survival",
#'   var_col_nms = "var",
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
#' @export
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
      eval(substitute(stopifnot(
        stats_dt[[VCN]] >= 0 | is.na(stats_dt[[VCN]])
      ), list(VCN = var_col_nm)))
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
    eval(substitute(stopifnot(
      conf_methods[i] %in% ALLOWED
    ), list(ALLOWED = allowed_conf_method_strings__())))
  })
  if (length(conf_methods) == 1) {
    conf_methods <- rep(conf_methods, length(stat_col_nms))
  }

  work_dt <- local({
    keep_col_nms <- setdiff(
      c(stratum_col_nms, adjust_col_nms, stat_col_nms, var_col_nms),
      NA_character_
    )
    work_dt <- data.table::setDT(as.list(stats_dt)[keep_col_nms])
    work_dt[]
  })
  tmp_stratum_col_nm <- tmp_nms(
    prefixes = "tmp_stratum_col_", avoid = names(work_dt)
  )
  if (length(stratum_col_nms) == 0L) {
    stratum_col_nms <- tmp_stratum_col_nm
    on.exit(data.table::set(out, j = tmp_stratum_col_nm, value = NULL))
    data.table::set(work_dt, j = tmp_stratum_col_nm, value = TRUE)
  }

  # prepare data for adjusted estimates and CIs --------------------------------
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
      weight = 1L
    )
    data.table::setnames(weights_dt, "x", adjust_col_nms)
  } else {
    weights_dt <- weights_arg_to_weights_dt(weights = weights,
                                            stats_dt = work_dt,
                                            adjust_col_nms = adjust_col_nms)
  }

  add_weights_column(
    stats_dt = work_dt,
    stratum_col_nms = stratum_col_nms,
    weights_dt = weights_dt,
    adjust_col_nms = adjust_col_nms
  )
  tmp_w_col_nm <- attr(work_dt, "tmp_w_col_nm")

  # prep output ----------------------------------------------------------------
  if (!is.null(stratum_col_nms)) {
    out <- subset(
      work_dt,
      subset = !duplicated(work_dt, by = stratum_col_nms)
    )
    data.table::setkeyv(out, stratum_col_nms)
  } else {
    out <- data.table::data.table(
      stat = NA_integer_
    )
    data.table::setnames(out, "stat", stat_col_nms[1])
  }

  # delta method confidence intervals ------------------------------------------
  local({
    data.table::set(
      work_dt,
      j = stat_col_nms,
      value = lapply(stat_col_nms, function(col_nm) {
        work_dt[[col_nm]] * work_dt[[tmp_w_col_nm]]
      })
    )
    data.table::set(
      x = work_dt,
      j = var_col_nms,
      value = lapply(var_col_nms, function(vcn) {
        work_dt[[vcn]] * (work_dt[[tmp_w_col_nm]] ^ 2)
      })
    )
    adjusted_stats_dt <- work_dt[
      #' @importFrom data.table .SD
      j = lapply(.SD, sum),
      .SDcols = c(stat_col_nms, var_col_nms),
      keyby = eval(stratum_col_nms)
    ]
    data.table::set(
      x = out,
      j = c(stat_col_nms, var_col_nms),
      value = lapply(
        c(stat_col_nms, var_col_nms),
        function(col_nm) {
          adjusted_stats_dt[[col_nm]]
        }
      )
    )

    lapply(seq_along(stat_col_nms), function(i) {
      stat_col_nm <- stat_col_nms[i]
      var_col_nm <- var_col_nms[i]
      conf_lvl <- conf_lvls[i]
      conf_method <- conf_methods[[i]]
      if (is.character(conf_method) && conf_method == "none") {
        return(NULL)
      }
      ci_dt <- delta_method_confidence_intervals(
        statistics = adjusted_stats_dt[[stat_col_nm]],
        variances = adjusted_stats_dt[[var_col_nm]],
        conf_lvl = conf_lvl,
        conf_method = conf_method
      )

      ci_col_nms <- paste0(stat_col_nm, c("_lo", "_hi"))
      data.table::set(
        x = out,
        j = ci_col_nms,
        value = lapply(c("ci_lo", "ci_hi"), function(col_nm) {
          ci_dt[[col_nm]]
        })
      )
      NULL
    })
  })


  # final touches --------------------------------------------------------------
  ordered_stat_col_nms <- unlist(lapply(
    seq_along(stat_col_nms),
    function(i) {
      stat_col_nm <- stat_col_nms[i]
      var_col_nm <- var_col_nms[i]
      ci_col_nms <- paste0(stat_col_nm, c("_lo", "_hi"))

      order_col_nms <- intersect(c(stat_col_nm, var_col_nm, ci_col_nms),
                                 names(out))
      order_col_nms
    }
  ))
  keep_col_nms <- c(stratum_col_nms, ordered_stat_col_nms)
  del_col_nms <- setdiff(names(out), keep_col_nms)
  if (length(del_col_nms)) {
    data.table::set(out, j = del_col_nms, value = NULL)
  }
  data.table::setcolorder(out, keep_col_nms)
  data.table::setkeyv(out, stratum_col_nms)
  if (identical(stratum_col_nms, tmp_stratum_col_nm)) {
    stratum_col_nms <- NULL
  }
  call <- match.call()
  data.table::setattr(
    out,
    "direct_adjusting_meta",
    mget(c("call", "stat_col_nms", "var_col_nms", "stratum_col_nms",
           "adjust_col_nms", "conf_lvls", "conf_methods"))
  )

  out[]
}

# @codedoc_comment_block news("directadjusting", "2024-12-10", "0.3.0")
# Remove deprecated `directadjusting::direct_adjusted_estimates`. Use
# `directadjusting::directly_adjusted_estimates`.
# @codedoc_comment_block news("directadjusting", "2024-12-10", "0.3.0")
