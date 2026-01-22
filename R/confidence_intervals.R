predefined_conf_method_math_list <- list(
  "identity" = quote(est + z * std_err),
  "log" = quote(est * exp(z * std_err / est)),
  "log-log" = quote(est ** exp(-z * (std_err / (abs(log(est)) * est))))
)
allowed_conf_methods <- function() {
  c("none", names(predefined_conf_method_math_list), "boot")
}
confidence_interval_expression <- function(conf_method) {
  if (is.language(conf_method)) {
    return(conf_method)
  }
  method_list <- predefined_conf_method_math_list
  if (!conf_method %in% names(method_list)) {
    stop("No math defined for `conf_method = ", deparse(conf_method), "`")
  } else {
    math <- method_list[[conf_method]]
  }
  return(math)
}

#' @md
#' @title Confidence Intervals
#' @description
#' Functions to compute confidence intervals.
#' @name confidence_intervals
NULL

#' @eval codedoc::pkg_doc_fun(
#'   regex = "directadjusting::delta_method_confidence_intervals",
#'   rdname = "confidence_intervals"
#' )
#' @examples
#'
#' # directadjusting::delta_method_confidence_intervals
#' dt_1 <- directadjusting::delta_method_confidence_intervals(
#'   statistics = 0.0,
#'   variances = 0.1,
#'   conf_lvl = 0.95,
#'   conf_method = "identity"
#' )
#' # you can also supply your own math for computing the confidence intervals
#' dt_2 <- directadjusting::delta_method_confidence_intervals(
#'   statistics = 0.0,
#'   variances = 0.1,
#'   conf_lvl = 0.95,
#'   conf_method = quote(est + z * std_err)
#' )
#' stopifnot(
#'   all.equal(dt_1, dt_2, check.attributes = FALSE)
#' )
delta_method_confidence_intervals <- function(
  statistics,
  variances,
  conf_lvl = 0.95,
  conf_method = "identity"
) {
  stopifnot(
    #' @param statistics `[numeric]` (no default)
    #'
    #' Statistics for which to calculate confidence intervals.
    is.numeric(statistics),
    #' @param variances `[numeric]` (no default)
    #'
    #' Variance estimates of `statistics` used to compute confidence intervals.
    is.numeric(variances),
    variances >= 0 | is.na(variances),
    length(statistics) == length(variances),
    #' @param conf_lvl `[numeric]` (default `0.95`)
    #'
    #' Confidence level of confidence intervals in `]0, 1[`.
    length(conf_lvl) == 1,
    is.double(conf_lvl),
    data.table::between(conf_lvl, lower = 0.0, upper = 1.0, incbounds = FALSE),
    #' @param conf_method `[character]` (default `"identity"`)
    #'
    #' See **Details**.
    is.language(conf_method) || length(conf_method) == 1,
    length(conf_lvl) == 1
  )

  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # `directadjusting::delta_method_confidence_intervals` can be used to
  # compute confidence intervals using the delta method. The following steps
  # are performed:
  #
  # - Collect mathematical expression `math` for computing the lower and upper
  #   end of the confidence interval based on `conf_method`.
  #   The following `conf_method` options have pre-determined math within
  #   this R package: `${deparse1(names(predefined_conf_method_math_list))}`.
  #   Alternatively, `conf_method` can be quoted R expression --- see `?quote`.
  #   In that case it will be used as-is.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  math <- confidence_interval_expression(conf_method = conf_method)

  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Collect variables `est = statistics`, `var = variances`, and
  #   `std_err = sqrt(variance)` into a single `data.table`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  dt <- data.table::setDT(list(
    est = statistics,
    var = variances,
    std_err = sqrt(variances)
  ))
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Add `z = stats::qnorm(p = (1 - conf_lvl) / 2)` into the `data.table`.
  #   E.g. `z = -1.959964` with `conf_lvl = 0.95`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::set(
    x = dt,
    j = "z",
    value = stats::qnorm(p = (1 - conf_lvl) / 2)
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Evaluate the mathematical expression `math` such that the columns in the
  #   `data.table` are available in evaluation. This creates column `ci_lo`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::set(
    x = dt,
    j = "ci_lo",
    value = eval(math, envir = dt)
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Re-set `z = stats::qnorm(p = conf_lvl + (1 - conf_lvl) / 2)`.
  #   E.g. `z = 1.959964` with `conf_lvl = 0.95`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::set(
    x = dt,
    j = "z",
    value = stats::qnorm(p = conf_lvl + (1 - conf_lvl) / 2)
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Evaluate `math` again. This yields `ci_hi`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::set(
    x = dt,
    j = "ci_hi",
    value = eval(math, envir = dt)
  )

  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Add attribute named `ci_meta` to the `data.table`.
  #   This attribute is a list which contains elements `conf_lvl`,
  #   `conf_method`, and `math`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  meta_list <- mget(c("conf_lvl", "conf_method", "math"))
  data.table::setattr(
    x = dt,
    name = "ci_meta",
    value = meta_list
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Delete column `std_err`. Rename `est` to `statistic` and `var` to
  #   `variance`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::setnames(dt, c("est", "var"), c("statistic", "variance"))
  data.table::set(
    x = dt,
    j = "std_err",
    value = NULL
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Return `data.table` with columns
  #   `c("statistic", "variance", "ci_lo", "ci_hi")`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::setcolorder(dt, c("statistic", "variance", "ci_lo", "ci_hi"))
  return(dt[])
}

#' @eval codedoc::pkg_doc_fun(
#'   regex = "directadjusting::bootstrap_confidence_intervals",
#'   rdname = "confidence_intervals"
#' )
bootstrap_confidence_intervals <- function(
  stats_dt,
  stat_col_nms,
  stratum_col_nms,
  conf_lvls,
  adjust_weight_col_nm = "weight",
  boot_arg_list = list(R = 1000),
  boot_ci_arg_list = list(type = "perc")
) {
  stopifnot(
    is.data.frame(stats_dt),
    c(stat_col_nms, stratum_col_nms, adjust_weight_col_nm) %in% names(stats_dt),
    inherits(boot_arg_list, "list"),
    inherits(boot_ci_arg_list, "list")
  )

  # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
  # `directadjusting::bootstrap_confidence_intervals` can be used to
  # compute confidence intervals using the bootstrap. The following steps are
  # performed:
  #
  # - Collect a `data.table` containing the (non-duplicated) strata in
  #   `stats_dt` by `stratum_col_nms`.
  # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
  out <- subset(
    stats_dt,
    subset = !duplicated(stats_dt, by = stratum_col_nms),
    select = stratum_col_nms
  )
  data.table::setkeyv(out, stratum_col_nms)
  this_call <- match.call()
  ci_list <- lapply(seq_along(stat_col_nms), function(i) {
    # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
    # - For each `stat_col_nms` element:
    #   + Set `boot_arg_list` elements `statistic`, `stype = "w"` and
    #     `boot_ci_arg_list[["conf"]] <- conf_lvls[i]` internally.
    #     `statistic` is a function which multiplies `adjust_weight_col_nm` with
    #     the bootstrap weight and returns the weighted average of the statistic
    #     in question.
    # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
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
      # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
      #   + Set `boot_arg_list[["data"]]` and
      #     `boot_ci_arg_list[["boot.out"]]` internally.
      #   + Call `boot::boot` and `boot::boot.ci` within each stratum.
      # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
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

    # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
    #   + Add the resulting confidence intervals into the output `data.table`.
    # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
    data.table::set(
      x = out,
      j = c(stat_col_nm, stat_ci_col_nms),
      value = ci_dt[
        i = out,
        on = stratum_col_nms,
        #' @importFrom data.table .SD
        j = .SD,
        .SDcols = c(stat_col_nm, stat_ci_col_nms)
      ]
    )
    NULL
  })
  # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
  # - Returns a `data.table` with columns `stratum_col_nms` and for each `i`
  #   `stat_col_nms[i]`, `paste0(stat_col_nms[i], "_lo")`, and
  #   `paste0(stat_col_nms[i], "_hi")`.
  # @codedoc_comment_block directadjusting::bootstrap_confidence_intervals
  out[]
}
