delta_method_variance_algorithmic__ <- function(
  g,
  theta,
  theta_variance
) {
  stopifnot(
    is.language(g),
    identical("theta", all.vars(g, functions = FALSE, unique = TRUE)),
    is.numeric(theta),
    length(theta) == length(theta_variance),
    is.numeric(theta_variance)
  )
  # @codedoc_comment_block directadjusting:::delta_method_variance_algorithmic__
  #     * `g` is passed to `[deriv]`. If that fails, a numerical derivative
  #       is computed.
  # @codedoc_comment_block directadjusting:::delta_method_variance_algorithmic__
  g_gradient_expr <- tryCatch(
    stats::deriv(expr = g, "theta"),
    error = function(e) e
  )
  if (inherits(g_gradient_expr, "error")) {
    g_gradient_value <- numerical_derivative__(
      expr = g,
      theta = theta
    )
  } else {
    g_gradient_value <- eval(g_gradient_expr, list(theta = theta))
    g_gradient_value <- as.numeric(attr(g_gradient_value, "gradient"))
  }
  # @codedoc_comment_block directadjusting:::delta_method_variance_algorithmic__
  #     * With the derivative known the variance after the transformation
  #       is `variance * g_gradient ^ 2`.
  # @codedoc_comment_block directadjusting:::delta_method_variance_algorithmic__
  return(theta_variance * (g_gradient_value ^ 2))
}

delta_method_variance_analytical__ <- function(
  transform = "log",
  theta,
  theta_variance
) {
  stopifnot(
    is.numeric(theta),
    length(theta) == length(theta_variance),

    is.numeric(theta_variance),
    ifelse(
      is.na(theta_variance),
      TRUE,
      theta_variance >= 0
    )
  )
  g_gradient_expr <-
    delta_method_analytical_expressions__(
      transform
    )[["g_gradient"]]
  g_gradient_value <- eval(g_gradient_expr, list(theta = theta))
  out <- theta_variance * (g_gradient_value ^ 2)
  out[theta_variance == 0.0] <- 0.0
  return(out)
}

delta_method_confidence_interval_eval__ <- function(
  g,
  g_inv,
  theta,
  g_variance,
  conf_level = 0.95
) {
  transform_value <- eval(g, list(theta = theta))
  a <- (1 - conf_level)
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_eval__
  #     * With the transformed variance known the transform confidence interval
  #       is calculated simply via `g(theta) + g_standard_error * z`.
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_eval__
  transform_standard_error_value <- sqrt(g_variance)
  transform_ci_lo <- transform_value + stats::qnorm(p = a / 2) *
    transform_standard_error_value
  transform_ci_hi <- transform_value + stats::qnorm(p = 1 - a / 2) *
    transform_standard_error_value
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_eval__
  #     * These transformation-scale confidence intervals are then converted
  #       back to the original scale using `g_inv`.
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_eval__
  theta_ci_lo <- eval(g_inv, list(g = transform_ci_lo))
  theta_ci_hi <- eval(g_inv, list(g = transform_ci_hi))
  out <- data.table::setDT(list(ci_lo = theta_ci_lo, ci_hi = theta_ci_hi))
  out[
    i = out[["ci_lo"]] > out[["ci_hi"]],
    #' @importFrom data.table := .SD
    j = c("ci_lo", "ci_hi") := list(.SD[["ci_hi"]], .SD[["ci_lo"]])
  ]
  return(out[])
}

delta_method_confidence_interval_algorithmic__ <- function(
  g,
  g_inv,
  theta,
  theta_variance,
  conf_level = 0.95
) {
  stopifnot(
    is.call(g_inv),
    identical(all.vars(g_inv, functions = FALSE, unique = TRUE), "g"),
    all.equal(
      eval(g_inv, list(g = eval(g, list(theta = theta)))),
      theta
    )
  )
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_algorithmic__
  # @codedoc_insert_comment_block directadjusting:::delta_method_variance_algorithmic__
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_algorithmic__
  g_variance <- delta_method_variance_algorithmic__(
    g = g,
    theta = theta,
    theta_variance = theta_variance
  )
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_algorithmic__
  # @codedoc_insert_comment_block directadjusting:::delta_method_confidence_interval_eval__
  # @codedoc_comment_block directadjusting:::delta_method_confidence_interval_algorithmic__
  out <- delta_method_confidence_interval_eval__(
    g = g,
    g_inv = g_inv,
    theta = theta,
    g_variance = g_variance,
    conf_level = conf_level
  )
  return(out[])
}

delta_method_analytical_expressions_list__ <- list(
  "identity" = list(
    g = quote(theta),
    g_inv = quote(g),
    g_gradient = quote(1)
  ),
  "log" = list(
    g = quote(log(theta)),
    g_inv = quote(exp(g)),
    g_gradient = quote(1 / theta)
  ),
  "log-log" = list(
    g = quote(log(-log(theta))),
    g_inv = quote(exp(-exp(g))),
    g_gradient = quote(1 / (theta * log(theta)))
  ),
  "logit" = list(
    g = quote(log(theta) - log(1 - theta)),
    g_inv = quote(1 / (1 + exp(-g))),
    g_gradient = quote(1 / (theta - theta ^ 2))
  )
)

delta_method_analytical_expressions_table__ <- function() {
  dt <- data.table::data.table(
    name = names(delta_method_analytical_expressions_list__)
  )
  for (elem_nm in names(delta_method_analytical_expressions_list__[[1]])) {
    for (i in seq_len(nrow(dt))) {
      data.table::set(
        x = dt,
        i = i,
        j = elem_nm,
        value = deparse1(
          delta_method_analytical_expressions_list__[[i]][[elem_nm]]
        )
      )
    }
  }
  return(dt[])
}

allowed_conf_method_strings__ <- function() {
  c("none", names(delta_method_analytical_expressions_list__))
}

delta_method_analytical_expressions__ <- function(
  transform
) {
  stopifnot(
    is.character(transform),
    length(transform) == 1
  )
  if (!transform %in% names(delta_method_analytical_expressions_list__)) {
    stop("No such transformation pre-defined: ", transform)
  }
  return(delta_method_analytical_expressions_list__[[transform]])
}

numerical_derivative__ <- function(
  expr,
  theta,
  h = sqrt(.Machine$double.eps)
) {
  lo <- eval(expr, list(theta = theta - h))
  hi <- eval(expr, list(theta = theta + h))
  deriv <- (hi - lo) / (2 * h)
  return(deriv)
}

delta_method_confidence_interval_analytical__ <- function(
  transform,
  theta,
  theta_variance,
  conf_level = 0.95
) {
  g_variance <- delta_method_variance_analytical__(
    transform = transform,
    theta = theta,
    theta_variance = theta_variance
  )
  expressions <- delta_method_analytical_expressions__(
    transform = transform
  )
  out <- delta_method_confidence_interval_eval__(
    g = expressions[["g"]],
    g_inv = expressions[["g_inv"]],
    theta = theta,
    g_variance = g_variance,
    conf_level = conf_level
  )
  return(out[])
}
