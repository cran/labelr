#' Is This a Value-labeled Variable (Column)?
#'
#' @description
#' Determine whether a specific variable of a data.frame has value labels
#' associated with it.
#'
#' @details
#' `hvl` is a compact alias for `has_val_labs`: they do the same thing, and the
#' former is easier to type
#'
#' @param data a data.frame.
#' @param var the unquoted name of the variable (column) to check for the
#' presence of value labels.
#' @param type the type of value label to check the variable for: "any,"
#' "1to1," "m1," or "q." If "1to1," check will be for value labels added via
#' `add_val_labs()` or `add_val1()`. If "m1," check will be for value labels
#' added via `add_m1_lab()` or `add1m1()`. If "q," check will be for value
#' labels added via `add_quant_labs()` or `add_quant1()`. If "any," will return
#' TRUE if any of the foregoing value label types is found.
#' @return A 1L logical.
#' @export
#' @examples
#' # add val labs to multiple variables at once
#' # make a "Likert"-type fake data set to demo
#' # note, by default, add_val_labs() "vars" arg will do partial matching
#' # in this case, we catch all vars with "y" in their name, except "y3"
#' set.seed(272)
#' dflik <- make_likert_data(scale = 1:7)
#' vals2label <- 1:7
#' labs2use <- c(
#'   "VSD",
#'   "SD",
#'   "D",
#'   "N",
#'   "A",
#'   "SA",
#'   "VSA"
#' )
#'
#' dflik <- add_val_labs(
#'   data = dflik, vars = c("y"), # note the vars args
#'   not.vars = "y3",
#'   vals = vals2label,
#'   labs = labs2use,
#'   partial = TRUE
#' )
#'
#' has_val_labs(dflik, y1) # TRUE
#'
#' has_val_labs(dflik, y3) # FALSE, see not.vars arg above
has_val_labs <- function(data, var, type = "any") {
  type <- tolower(type)

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
#' @rdname has_val_labs
hvl <- has_val_labs
