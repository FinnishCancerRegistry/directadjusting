predefined_conf_method_math_list <- list(
  "identity" = quote(est + z * std_err),
  "log" = quote(est * exp(z * std_err / est)),
  "log-log" = quote(est ** exp(-z * (std_err / (abs(log(est)) * est))))
)
allowed_conf_methods <- function() {
  c("none", names(predefined_conf_method_math_list))
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
  # - Delete columns `std_err`, `z`. Rename `est` to `statistic` and `var` to
  #   `variance`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::setnames(dt, c("est", "var"), c("statistic", "variance"))
  data.table::set(
    x = dt,
    j = c("std_err", "z"),
    value = NULL
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Return `data.table` with columns
  #   `c("statistic", "variance", "ci_lo", "ci_hi")`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  data.table::setcolorder(dt, c("statistic", "variance", "ci_lo", "ci_hi"))
  return(dt[])
}
