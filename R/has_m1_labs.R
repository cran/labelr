#' Is This an `add_m1_lab()` Many-to-One-Style Value-labeled Variable (Column)?
#'
#' @description
#' Determine whether a specific variable of a data.frame has many-to-one-style
#' value labels associated with it (i.e., via `add_m1_lab()` or `add1m1()`).
#'
#' @details
#' `hm1l` is a compact alias for `has_m1_labs`: they do the same thing, and the
#' former is easier to type
#'
#' @param data a data.frame.
#' @param var the unquoted name of the variable (column) to check for the
#' presence of many-to-one-style value labels.
#' @return A 1L logical.
#' @export
#' @examples
#' # add many-to-one style labels for "carb" and one-to-one style for "am"
#' df <- mtcars
#'
#' df <- add_m1_lab(df,
#'   vars = "carb",
#'   vals = 1:3,
#'   lab = "<=3",
#'   max.unique.vals = 10
#' )
#'
#' df <- add_m1_lab(df,
#'   vars = "carb",
#'   vals = c(4, 6, 8),
#'   lab = ">=4",
#'   max.unique.vals = 10
#' )
#'
#' df <- add_val_labs(df,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("autom", "manu"),
#'   max.unique.vals = 10
#' )
#'
#' has_m1_labs(df, carb) # TRUE, carb has m1-style value labels
#'
#' has_val_labs(df, am) # TRUE, am does have value labels
#'
#' has_m1_labs(df, am) # FALSE, am's value labels are not not m1-style labels
has_m1_labs <- function(data, var) {
  type <- "m1"

  # capture var argument
  vars <- deparse(substitute(var))
  test_quote <- any(grepl("\"", vars))
  if (test_quote && is.character(vars)) vars <- gsub("\"", "", vars)
  vars <- gsub("c\\(", "", vars)
  vars <- gsub("\\(", "", vars)
  vars <- gsub("\\)", "", vars)

  # test for presence of var in data.frame
  if (!all(vars %in% names(data)) || length(vars) != 1) {
    stop("
\nInvalid var argument specification: var arg should be a single, unquoted
name of a variable that is present in the data.frame.
         ")
  }

  att <- paste0("val.labs.", vars)
  att_list <- get_all_lab_atts(data)
  any_val <- check_labs_att(data, att)
  m1_val <- FALSE
  q_val <- FALSE

  if (any_val) {
    m1_val <- length(unique(att_list[att][[1]])) != length(att_list[att][[1]])

    q_unique_labs <- length(unique(unname(get_labs_att(
      data,
      paste0(
        "val.labs.",
        vars
      )
    )[[1]])))
    q_unique_vals <- unname(vapply(
      data[vars],
      function(x) length(unique(x)),
      integer(1)
    ))

    q_val <- q_unique_vals > q_unique_labs
  }

  out_val <- FALSE

  if (type == "any") {
    out_val <- any_val
  } else if (type == "m1" && any_val && !q_val) {
    out_val <- m1_val
  } else if (type == "q" && !m1_val && any_val) {
    out_val <- q_val
  } else if (type == "1to1" && !q_val && !m1_val) {
    out_val <- any_val
  }

  return(out_val)
}

#' @export
#' @rdname has_m1_labs
hm1l <- has_m1_labs
