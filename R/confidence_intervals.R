confidence_interval_eval__ <- function(
  conf_method,
  theta,
  theta_variance,
  conf_lvl = 0.95
) {
  stopifnot(
    inherits(conf_method, c("character", "list", "call"))
  )
  if (is.character(conf_method)) {
    # @codedoc_comment_block directadjusting:::confidence_interval_eval__
    #   + If `conf_method` is a string, a pre-defined set of mathematical
    #     expressions are used to compute the confidenve intervals.
    # @codedoc_comment_block directadjusting:::confidence_interval_eval__
    delta_method_confidence_interval_analytical__(
      transform = conf_method,
      theta = theta,
      theta_variance = theta_variance,
      conf_level = conf_lvl
    )
  } else if (is.call(conf_method)) {
    # @codedoc_comment_block directadjusting:::confidence_interval_eval__
    #   + If `conf_method` is a `call`, it is evaluated with the variables
    #     `theta`, `theta_variance`, `theta_standard_error`, and `z`. This is
    #     done once for the lower and once for the upper bound of the confidence
    #     interval, so for the lower bound and `conf_level = 0.95`
    #     we use `z = stats::qnorm(p = (1 - conf_lvl) / 2)`.
    # @codedoc_comment_block directadjusting:::confidence_interval_eval__
    data <- list(
      theta = theta,
      theta_variance = theta_variance,
      theta_standard_error = sqrt(theta_variance)
    )
    a <- (1 - conf_lvl)
    data[["z"]] <- stats::qnorm(p = a / 2)
    ci_lo <- eval(conf_method, data)
    data[["z"]] <- stats::qnorm(p = 1 - a / 2)
    ci_hi <- eval(conf_method, data)
    return(data.table::setDT(list(ci_lo = ci_lo, ci_hi = ci_hi))[])
  } else {
    # @codedoc_comment_block directadjusting:::confidence_interval_eval__
    #   + If `conf_method` is a `list`, it must contain elements `g` and
    #     `g_inv`, e.g. `list(g = quote(log(theta)), g_inv = quote(exp(g)))`.
    # @codedoc_insert_comment_block directadjusting:::delta_method_confidence_interval_algorithmic__
    # @codedoc_comment_block directadjusting:::confidence_interval_eval__
    delta_method_confidence_interval_algorithmic__(
      g = conf_method[["g"]],
      g_inv = conf_method[["g_inv"]],
      theta = theta,
      theta_variance = theta_variance,
      conf_level = conf_lvl
    )
  }
}

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
#'   statistics = 0.9,
#'   variances = 0.1,
#'   conf_lvl = 0.95,
#'   conf_method = "log"
#' )
#'
#' # you can also supply your own math for computing the confidence intervals
#' dt_2 <- directadjusting::delta_method_confidence_intervals(
#'   statistics = 0.9,
#'   variances = 0.1,
#'   conf_lvl = 0.95,
#'   conf_method = quote(theta * exp(z * theta_standard_error / theta))
#' )
#'
#' dt_3 <- directadjusting::delta_method_confidence_intervals(
#'   statistics = 0.9,
#'   variances = 0.1,
#'   conf_lvl = 0.95,
#'   conf_method = list(
#'     g = quote(log(theta)),
#'     g_inv = quote(exp(g))
#'   )
#' )
#'
#' dt_4 <- directadjusting::delta_method_confidence_intervals(
#'   statistics = 0.9,
#'   variances = 0.1,
#'   conf_lvl = 0.95,
#'   conf_method = list(
#'     g = quote(stats::qnorm(theta)),
#'     g_inv = quote(stats::pnorm(g))
#'   )
#' )
#' stopifnot(
#'   all.equal(dt_1, dt_2, check.attributes = FALSE),
#'   all.equal(dt_1, dt_3, check.attributes = FALSE)
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
    conf_lvl < 1.0,
    conf_lvl > 0.0,

    #' @param conf_method `[character, call, list]` (default `"identity"`)
    #'
    #' Delta method transformation to be applied.
    #'
    #' - `character`: Use one of the pre-defined transformations. Table of
    #'   options with the correspoding math:
    #'
    #'   `r knitr::kable(delta_method_analytical_expressions_table__())`
    #'
    #' - `call`: A quoted R expression which produces the lower / upper limit
    #'   when evaluated. E.g.
    #'   `quote(theta * exp(z * theta_standard_error / theta))`.
    #' - `list`: Contains both the transformation and its inverse.
    #'   E.g. `list(g = quote(log(theta)), g_inv = quote(exp(g)))`.
    inherits(conf_method, c("character", "list", "call"))
  )

  # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2026-01-23", "0.5.0")
  # `directadjusting::delta_method_confidence_intervals` made a lot more
  # flexible. It now accepts via `conf_method` a string, a call, and a list
  # of calls that produce the desired confidence intervals.
  # @codedoc_comment_block news("directadjusting::direct_adjusted_estimates", "2026-01-23", "0.5.0")
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # `directadjusting::delta_method_confidence_intervals` can be used to
  # compute confidence intervals using the delta method. The following steps
  # are performed:
  #
  # - Compute confidence intervals based on `conf_method`, `statistics`,
  #   `variances`, and `conf_lvl`.
  # @codedoc_insert_comment_block directadjusting:::confidence_interval_eval__
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  dt <- confidence_interval_eval__(
    conf_method = conf_method,
    theta = statistics,
    theta_variance = variances,
    conf_lvl = conf_lvl
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Collect a `data.table` with the confidence intervals and with also
  #   the columns `statistics = statistics` and `variance = variances`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  dt <- cbind(
    statistic = statistics,
    variance = variances,
    dt
  )

  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Add attribute named `ci_meta` to the `data.table`.
  #   This attribute is a list which contains elements `conf_lvl` and
  #   `conf_method`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  meta_list <- mget(c("conf_lvl", "conf_method"))
  data.table::setattr(
    x = dt,
    name = "ci_meta",
    value = meta_list
  )
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  # - Return `data.table` with columns
  #   `c("statistic", "variance", "ci_lo", "ci_hi")`.
  # @codedoc_comment_block directadjusting::delta_method_confidence_intervals
  return(dt[])
}
