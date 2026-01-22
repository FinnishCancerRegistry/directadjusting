#' @param weights `[double, data.table, character]`
#'
#' The weights need not sum to one as this is ensured internally. You may
#' supply weights in one of the following ways:
#'
#' - `double`: A vector of weights, the length of which must match the number
#'   of strata defined by adjusting variables.
#' - `data.table`: With one or more columns with names matching to those
#'   variables that are used to adjust estimates, and one column named
#'   `weight`. E.g. `data.table(agegroup = 1:3, weight = c(100, 500, 400))`.
