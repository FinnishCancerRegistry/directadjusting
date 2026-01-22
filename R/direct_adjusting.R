




#' @md
#' @title Direct Adjusted Estimates
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
#' @param conf_methods `[character]` (default `"identity"`)
#'
#' method to compute confidence intervals; either one string (to be used for
#' all statistics) or a vector of strings, one for each element of
#' `stat_col_nms`; see \code{\link{delta_method_confidence_intervals}} for
#' supported delta method confidence intervals; other allowed values:
#'
#' - `"none"`: for statistics for which you do not want
#'    confidence intervals to be calculated (or `NA` in `var_col_nms`)
#' - `"boot"`: bootstrapped confidence intervals --- see args `boot_arg_list`,
#'   `boot_ci_arg_list` and section **Bootstrap**
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
#' @param boot_arg_list `[list]` (default `list(R = 1000)`)
#'
#' arguments passed to \code{\link[boot]{boot}} with `data`, `statistic`, and
#' `stype` overridden internally
#' @param boot_ci_arg_list `[list]` (default `list(type = "perc")`)
#'
#' arguments passed to \code{\link[boot]{boot.ci}} with `boot.out` and `conf`
#' overridden internally (latter comes from `conf_levels`)
#' @section Weights:
#'
#' The weights are scaled internally to sum to one, but they need to be positive
#' numbers (or zero). The scaling is performed separately by each unique
#' combination of `stratum_col_nms` columns. This allows you to have e.g.
#' two hierarchical variables, one used for adjusting and one for stratifying
#' output (such as 18 age groups of 5 years for adjusting and 4 larger age
#' groups for stratifying output). See **Examples**.
#'
#' @section Tabulation:
#'
#' Currently every pair of columns in `union(stratum_col_nms, adjust_col_nms)`
#' must be either
#'
#' - hierarchical: each level of B exists under exactly one level of A (or
#'   converse); e.g. regions `c(1, 1, 2, 2)` and sub-regions `c(1, 2, 3, 4)`;
#'   sub-regions `c(1, 2, 2, 3)` would not be hierarchical
#' - cross-joined: every level of B is repeated for every level of A; e.g.
#'   sexes `c(1, 1, 2, 2)` and regions `c(1, 2, 1, 2)`;
#'   regions `c(1, 2, 2, 3)` would not be cross-joined
#'
#' This ensures that adjusting will be performed properly, i.e. the weights
#' are merged and used as intended.
#'
#' @section Bootstrap:
#'
#' Confidence intervals can be approximated using bootstrap.
#' \code{\link[boot]{boot.ci}} is called with `stype = "w"`, i.e. the
#' bootstrap is based on random sampling of weights within each stratum
#' defined by `stratum_col_nms` for each stratification defined by
#' `adjust_col_nms`. You have to know whether this is appropriate or not.
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
#' # adjusted by age group, bootstrapped CIs
#' my_adj_stats <- directly_adjusted_estimates(
#'   stats_dt = my_stats,
#'   stat_col_nms = "e",
#'   var_col_nms = "v",
#'   conf_lvls = 0.95,
#'   conf_methods = "boot",
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
#' # but with confidence intervals if you want them. this for the sake of
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
#' @export
directly_adjusted_estimates <- function(
  stats_dt,
  stat_col_nms,
  var_col_nms = NULL,
  stratum_col_nms = NULL,
  adjust_col_nms = NULL,
  conf_lvls = 0.05,
  conf_methods = "identity",
  weights = NULL,
  boot_arg_list = list(R = 1000),
  boot_ci_arg_list = list(type = "perc")
) {
  # assertions -----------------------------------------------------------------
  stopifnot(
    is.data.frame(stats_dt),
    stat_col_nms %in% names(stats_dt),
    is.null(stratum_col_nms) || all(stratum_col_nms %in% names(stats_dt)),
    is.null(var_col_nms) || (
      length(var_col_nms) == length(stat_col_nms) &&
        all(var_col_nms %in% names(stats_dt))
    ),
    data.table::between(conf_lvls, lower = 0.0, upper = 1.0, incbounds = FALSE),
    length(conf_lvls) %in% c(1L, length(stat_col_nms)),
    length(conf_methods) %in% c(1L, length(stat_col_nms)),
    inherits(boot_arg_list, "list"),
    inherits(boot_ci_arg_list, "list")
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
  eval(substitute(stopifnot(
    conf_methods %in% ALLOWED
  ), list(ALLOWED = allowed_conf_methods())))
  if (length(conf_methods) == 1) {
    conf_methods <- rep(conf_methods, length(stat_col_nms))
  }

  # check that stratification makes sense --------------------------------------
  keep_col_nms <- setdiff(
    c(stratum_col_nms, adjust_col_nms, stat_col_nms, var_col_nms), NA_character_
  )
  stats_dt <- data.table::setDT(lapply(keep_col_nms, function(col_nm) {
    stats_dt[[col_nm]]
  }))
  data.table::setnames(stats_dt, keep_col_nms)
  tmp_stratum_col_nm <- tmp_nms(
    prefixes = "tmp_stratum_col_", avoid = names(stats_dt)
  )
  if (length(stratum_col_nms) == 0L) {
    stratum_col_nms <- tmp_stratum_col_nm
    #' @importFrom data.table :=
    on.exit(stats_dt[, (tmp_stratum_col_nm) := NULL])
    stats_dt[, (tmp_stratum_col_nm) := NA]
  }
  test_col_nms <- setdiff(
    union(stratum_col_nms, adjust_col_nms),
    tmp_stratum_col_nm
  )
  call <- match.call()
  if (length(test_col_nms) > 1) {
    stratum_col_nm_pairs <- utils::combn(test_col_nms, m = 2L)
    lapply(seq_len(ncol(stratum_col_nm_pairs)), function(pair_no) {
      pair <- stratum_col_nm_pairs[, pair_no]
      udt <- unique(stats_dt, by = pair)

      un1 <- data.table::uniqueN(udt[[pair[1]]])
      un2 <- data.table::uniqueN(udt[[pair[2]]])
      is_cj <- nrow(udt) == un1 * un2
      if (is_cj) {
        return(NULL)
      }

      is_hierachical <- nrow(udt) %in% c(un1, un2)
      if (!is_hierachical) {
        stop(simpleError(
          paste0(
            "stratum / adjust column pair ", deparse(pair),
            " in stats_dt are not ",
            "hierarchical nor cross-joined; see ",
            "?directly_adjusted_estimates section Tabulation"
          ),
          call = call
        ))
      }
    })
  }


  # prepare data for adjusted estimates and CIs --------------------------------
  if (length(adjust_col_nms) == 0) {
    # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2025-11-25", "0.5.0")
    # `directadjusting::direct_adjusted_estimates` now allows for the sake of
    # convenience to be called with no `adjust_col_nms` defined. This results
    # in no adjusting and confidence interval if you want them.
    # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2025-11-25", "0.5.0")
    adjust_col_nms <- tmp_nms(
      prefixes = "tmp_adjust_stratum_",
      avoid = names(stats_dt)
    )
    data.table::set(
      x = stats_dt,
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
                                            stats_dt = stats_dt,
                                            adjust_col_nms = adjust_col_nms)
  }

  add_weights_column(
    stats_dt = stats_dt,
    stratum_col_nms = stratum_col_nms,
    weights_dt = weights_dt,
    adjust_col_nms = adjust_col_nms
  )
  tmp_w_col_nm <- attr(stats_dt, "tmp_w_col_nm")

  # bootstrapped confidence intervals ------------------------------------------
  wh_boot_ci <- which(conf_methods == "boot")
  wh_nonboot_ci <- setdiff(seq_along(conf_methods), wh_boot_ci)
  boot_stat_col_nms <- character(0)
  if (length(wh_boot_ci)) {
    boot_stat_col_nms <- stat_col_nms[wh_boot_ci]
    #' @importFrom data.table .SD
    boot_stats_dt <- stats_dt[
      j = .SD,
      .SDcols = c(stratum_col_nms, boot_stat_col_nms, tmp_w_col_nm)
    ]
    boot_stats_dt <- bootstrap_confidence_intervals(
      stats_dt = boot_stats_dt,
      stat_col_nms = boot_stat_col_nms,
      stratum_col_nms = stratum_col_nms,
      conf_lvls = conf_lvls,
      adjust_weight_col_nm = tmp_w_col_nm,
      boot_arg_list = boot_arg_list,
      boot_ci_arg_list = boot_ci_arg_list
    )
  }

  # delta method confidence intervals ------------------------------------------
  nonboot_stats_dt <- stats_dt
  if (length(wh_nonboot_ci) > 0) {
    if (length(wh_boot_ci) > 0) {
      data.table::set(nonboot_stats_dt, j = boot_stat_col_nms, value = NULL)
    }

    nonboot_stat_col_nms <- stat_col_nms[wh_nonboot_ci]

    data.table::set(
      nonboot_stats_dt,
      j = stat_col_nms,
      value = lapply(stat_col_nms, function(col_nm) {
        nonboot_stats_dt[[col_nm]] * nonboot_stats_dt[[tmp_w_col_nm]]
      })
    )
    usable_var_col_nms <- setdiff(var_col_nms, NA)
    if (length(usable_var_col_nms) > 0) {
      data.table::set(
        x = nonboot_stats_dt,
        j = usable_var_col_nms,
        value = lapply(usable_var_col_nms, function(col_nm) {
          nonboot_stats_dt[[col_nm]] * (nonboot_stats_dt[[tmp_w_col_nm]] ^ 2)
        })
      )
    }
    #' @importFrom data.table .SD
    nonboot_stats_dt <- nonboot_stats_dt[
      j = lapply(.SD, sum),
      .SDcols = c(nonboot_stat_col_nms, usable_var_col_nms),
      keyby = eval(stratum_col_nms)
    ]

    lapply(wh_nonboot_ci, function(i) {
      stat_col_nm <- stat_col_nms[i]
      var_col_nm <- var_col_nms[i]
      conf_lvl <- conf_lvls[i]
      conf_method <- conf_methods[i]
      if (is.na(var_col_nm) || conf_method == "none") {
        return(NULL)
      }
      ci_dt <- delta_method_confidence_intervals(
        statistics = nonboot_stats_dt[[stat_col_nm]],
        variances = nonboot_stats_dt[[var_col_nm]],
        conf_lvl = conf_lvl,
        conf_method = conf_method
      )

      ci_col_nms <- paste0(stat_col_nm, c("_lo", "_hi"))
      data.table::set(
        nonboot_stats_dt,
        j = ci_col_nms,
        value = lapply(c("ci_lo", "ci_hi"), function(col_nm) {
          ci_dt[[col_nm]]
        })
      )
      data.table::alloc.col(nonboot_stats_dt)
      NULL
    })
  }


  # final touches --------------------------------------------------------------
  #' @importFrom data.table .SD
  stats_dt <- nonboot_stats_dt[, .SD, .SDcols = stratum_col_nms]
  stats_dt <- unique(stats_dt, by = stratum_col_nms)
  data.table::setkeyv(stats_dt, stratum_col_nms)
  if (length(wh_nonboot_ci)) {
    stats_dt <- stats_dt[nonboot_stats_dt, on = eval(stratum_col_nms)]
  }
  if (length(wh_boot_ci)) {
    stats_dt <- stats_dt[boot_stats_dt, on = eval(stratum_col_nms)]
  }

  ordered_stat_col_nms <- unlist(lapply(
    seq_len(length(stat_col_nms)),
    function(i) {
      stat_col_nm <- stat_col_nms[i]
      var_col_nm <- var_col_nms[i]
      ci_col_nms <- paste0(stat_col_nm, c("_lo", "_hi"))

      stat_col_nms <- intersect(c(stat_col_nm, var_col_nm, ci_col_nms),
                                names(stats_dt))
      stat_col_nms
    }
  ))
  keep_col_nms <- c(stratum_col_nms, ordered_stat_col_nms)
  del_col_nms <- setdiff(names(stats_dt), keep_col_nms)
  if (length(del_col_nms)) {
    data.table::set(stats_dt, j = del_col_nms, value = NULL)
  }
  data.table::setcolorder(stats_dt, keep_col_nms)
  data.table::setkeyv(stats_dt, stratum_col_nms)
  if (identical(stratum_col_nms, tmp_stratum_col_nm)) {
    stratum_col_nms <- NULL
  }
  data.table::setattr(
    stats_dt, "direct_adjusting_meta",
    mget(c("call", "stat_col_nms", "var_col_nms", "stratum_col_nms",
           "adjust_col_nms", "conf_lvls", "conf_methods"))
  )

  stats_dt[]
}

# @codedoc_comment_block news("directadjusting", "2024-12-10", "0.3.0")
# Remove deprecated `directadjusting::direct_adjusted_estimates`. Use
# `directadjusting::directly_adjusted_estimates`.
# @codedoc_comment_block news("directadjusting", "2024-12-10", "0.3.0")
