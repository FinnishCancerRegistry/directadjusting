




#' @md
#' @title Direct Adjusted Estimates
#' @description Compute direct adjusted estimates from a table of statistics.
#' @param stats_dt `[data.table]` (no default)
#'
#' a `data.table` containing estimates and variance estimates of statistics
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
#' @param adjust_col_nms `[character]` (no default)
#'
#' names of columns in `stats_dt` by which statistics are currently stratified
#' and by which the statistics should be adjusted (e.g. `"agegroup"`)
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
#' @export
#' @importFrom data.table setDT := .SD set alloc.col setcolorder setkeyv
#' setnames uniqueN
#' @importFrom utils combn
directly_adjusted_estimates <- function(
  stats_dt,
  stat_col_nms,
  var_col_nms,
  stratum_col_nms = NULL,
  adjust_col_nms,
  conf_lvls,
  conf_methods,
  weights,
  boot_arg_list = list(R = 1000),
  boot_ci_arg_list = list(type = "perc")
) {

  call <- match.call()

  # assertions -----------------------------------------------------------------
  assert_is_data_table(stats_dt)
  assert_is_character_nonNA_vector(stat_col_nms)
  assert_is_data_table_with_required_names(
    stats_dt,
    required_names = stat_col_nms
  )
  assert_is_one_of(
    var_col_nms,
    fun_nms = c("assert_is_character_vector", "assert_is_NULL")
  )
  if (!is.null(var_col_nms)) {
    assert_is_data_table_with_required_names(
      stats_dt,
      required_names = setdiff(var_col_nms, NA_character_)
    )
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
  assert_is_double_nonNA_vector(conf_lvls)
  stopifnot(
    conf_lvls > 0, conf_lvls < 1
  )
  if (length(conf_lvls) == 1) {
    conf_lvls <- rep(conf_lvls, length(stat_col_nms))
  }
  assert_is_character_nonNA_vector(conf_methods)
  eval(substitute(stopifnot(
    conf_methods %in% ALLOWED
  ), list(ALLOWED = allowed_conf_methods())))
  if (length(conf_methods) == 1) {
    conf_methods <- rep(conf_methods, length(stat_col_nms))
  }
  assert_is_list(boot_arg_list)
  assert_is_list(boot_ci_arg_list)

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
  weights_dt <- weights_arg_to_weights_dt(weights = weights,
                                          stats_dt = stats_dt,
                                          adjust_col_nms = adjust_col_nms)

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
        nonboot_stats_dt,
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

delta_method_conf_methods <- function() {
  c("identity", "log", "log-log")
}
allowed_conf_methods <- function() {
  c("none", delta_method_conf_methods(), "boot")
}
confidence_interval_expression <- function(conf_method) {
  assert_is_character_nonNA_atom(conf_method)
  stopifnot(
    conf_method %in% allowed_conf_methods()
  )
  math <- switch(
    conf_method,
    identity = quote(STAT + Z * STD_ERR),
    log = quote(STAT * exp(Z * STD_ERR / STAT)),
    `log-log` = quote(STAT ** exp(-Z * (STD_ERR / (abs(log(STAT)) * STAT)))),
    stop("No math defined for conf_method = ", deparse(conf_method))
  )
  return(math)
}

doc_ci_methods <- function() {
  conf_methods <- setdiff(allowed_conf_methods(), c("none", "boot"))
  maths <- vapply(conf_methods, function(conf_method) {
    paste0(deparse(confidence_interval_expression(conf_method)), collapse = "")
  }, character(1))
  lines <- c(
    "@section Confidence interval methods:",
    "Currently supported confidence interval methods and their formulae:",
    paste0(" - ", conf_methods, ": ", maths, collapse = ""),
    "",
    "Where",
    " - `STAT` is the statistic,",
    " - `Z` is the quantile from the standard normal distribution for the ",
    "   lower or upper bound, and",
    " - `STD_ERR`` is the standard error (square root of the variance)"
  )
  return(lines)
}

#' @md
#' @title Confidence Intervals
#' @description
#' Computes different kinds of confidence intervals given the statistics
#' and their variance estimates.
#' @param statistics `[numeric]` (no default)
#'
#' statistics for which to calculate confidence intervals
#' @param variances `[numeric]` (no default)
#'
#' variance estimates of `statistics` used to compute confidence intervals
#' @param conf_lvl `[numeric]` (default `0.95`)
#'
#' confidence level of confidence intervals in `]0, 1[`
#' @param conf_method `[character]` (default `"identity"`)
#'
#' see section **Confidence interval methods**
#' @eval doc_ci_methods()
#' @return
#' `data.table` with columns
#' - `statistic`: the values you supplied via argument `statistics`
#' - `variance`: the values you supplied via argument `variances`
#' - `ci_lo`: lower bound of confidence interval
#' - `ci_hi`: upper bound of confidence interval
#' @export
#' @importFrom data.table := setattr setnames set
#' @importFrom stats qnorm
#' @importFrom boot boot boot.ci
delta_method_confidence_intervals <- function(
  statistics,
  variances,
  conf_lvl = 0.95,
  conf_method = "identity"
) {
  assert_is_number_vector(statistics)
  assert_is_number_vector(variances)
  assert_is_double_nonNA_atom(conf_lvl)
  assert_is_character_nonNA_atom(conf_method)
  eval(substitute(stopifnot(
    variances >= 0 | is.na(variances),
    conf_lvl > 0,
    conf_lvl < 1,
    conf_method %in% ALLOWED
  ), list(ALLOWED = setdiff(allowed_conf_methods(), "none"))))

  math <- confidence_interval_expression(conf_method = conf_method)

  dt <- data.table::setDT(list(STAT = statistics, STD_ERR = sqrt(variances)))
  Z <- stats::qnorm(p = (1 - conf_lvl) / 2)
  expr <- substitute(dt[, "ci_lo" := MATH], list(MATH = math))
  eval(expr)
  Z <- stats::qnorm(p = conf_lvl + (1 - conf_lvl) / 2)
  expr <- substitute(dt[, "ci_hi" := MATH], list(MATH = math))
  eval(expr)

  meta_list <- mget(c("conf_lvl", "conf_method", "math"))
  if (conf_method == "boot") {
    meta_list <- c(meta_list, mget(c("boot_arg_list", "boot_ci_arg_list")))
  }
  data.table::setattr(
    dt,
    name = "ci_meta",
    value = meta_list
  )
  data.table::setnames(dt, c("STAT", "STD_ERR"), c("statistic", "variance"))
  data.table::set(
    dt,
    j = "variance",
    value = dt[["variance"]] ** 2
  )
  data.table::setcolorder(dt, c("statistic", "variance", "ci_lo", "ci_hi"))
  dt[]
}





#' @importFrom data.table setkeyv .SD := set
#' @importFrom boot boot boot.ci
bootstrap_confidence_intervals <- function(
  stats_dt,
  stat_col_nms,
  stratum_col_nms,
  conf_lvls,
  adjust_weight_col_nm = "weight",
  boot_arg_list = list(R = 1000),
  boot_ci_arg_list = list(type = "perc")
) {
  this_call <- match.call()

  assert_is_data_table_with_required_names(
    stats_dt,
    required_names = c(stat_col_nms, stratum_col_nms, adjust_weight_col_nm)
  )
  assert_is_list(boot_arg_list)
  assert_is_list(boot_ci_arg_list)

  #' @importFrom data.table .SD
  out <- stats_dt[, .SD, .SDcols = stratum_col_nms]
  out <- unique(out, by = stratum_col_nms)
  data.table::setkeyv(out, stratum_col_nms)
  ci_list <- lapply(seq_along(stat_col_nms), function(i) {
    stat_col_nm <- stat_col_nms[i]
    boot_stat_fun <- function(d, w) {
      w <- w * d[[adjust_weight_col_nm]]
      w <- w / sum(w)
      sum(d[[stat_col_nm]] * w)
    }
    boot_arg_list[["statistic"]] <- boot_stat_fun
    boot_arg_list[["stype"]] <- "w"
    boot_ci_arg_list[["conf"]] <- conf_lvls[i]
    stat_ci_col_nms <- paste0(stat_col_nm, c("_lo", "_hi"))
    stratum_value_counts <- stats_dt[
      #' @importFrom data.table .SD
      j = list(n = data.table::uniqueN(.SD)),
      .SDcols = stat_col_nm,
      keyby = eval(stratum_col_nms)
    ]
    if (any(stratum_value_counts[["n"]] == 1)) {
      warning(simpleWarning(
        paste0(
          "some strata had only one unique value of statistic ",
          deparse(stat_col_nm), " so bootstrapping was not possible; ",
          "returning NA confidence intervals in such cases"
        ),
        call = this_call
      ))
    }
    strata_with_variance <- stats_dt[
      i = stratum_value_counts[stratum_value_counts[["n"]] > 1, ],
      on = eval(stratum_col_nms),
    ]
    if (nrow(strata_with_variance) > 0) {
      ci_dt <- strata_with_variance[
        j = {
          #' @importFrom data.table .SD
          .__DT <- .SD
          boot_arg_list[["data"]] <- quote(.__DT)
          b <- do.call(boot::boot, boot_arg_list)
          est <- b[["t0"]]
          boot_ci_arg_list[["boot.out"]] <- quote(b)
          ci <- do.call(boot::boot.ci, boot_ci_arg_list)
          ci_list <- as.list(ci[["percent"]][4:5])
          names(ci_list) <- stat_ci_col_nms
          out <- c(list(est = est), ci_list)
          names(out)[1] <- stat_col_nm
          out
        },
        keyby = eval(stratum_col_nms)
      ]
    }

    data.table::set(
      x = out,
      j = c(stat_col_nm, stat_ci_col_nms),
      value = ci_dt[
        i = out,
        on = eval(stratum_col_nms),
        #' @importFrom data.table .SD
        j = .SD,
        .SDcols = c(stat_col_nm, stat_ci_col_nms)
      ]
    )
    NULL
  })
  out[]
}
