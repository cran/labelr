#' Filter Rows Using Variable Value Labels
#'
#' @description
#' `flab` ("filter using labels") allows one to filter-subset a data.frame
#' using value or numerical range labels.
#'
#' @details
#' `flab` accepts a labelr value-labeled data.frame, followed by condition-
#' based row-filtering instructions (in the fashion of base::subset or
#' dplyr::filter) expressed in terms of variable value labels that exist only
#' as meta-data (i.e., not visible using View(), head(), etc.), and returns the
#' filtered data.frame in terms of the values themselves.
#'
#' For example, if value labels were added to the integer nominal variable
#' "raceth" of (notional) data.frame df (using `add_val_labs` or `add_val1`), one
#' could then use flab to filter down to only raceth==3 rows using a call like
#' flab(df, raceth=="African-American") (assuming here that the integer value
#' raceth==3 has previously been given the value label "African-American". As
#' another example, flab(mtcars, am=="automatic") would return (only) those rows
#' of mtcars where automatic==0 if the value label "automatic" has been uniquely
#' associated with the mtcars$am value of 0 (again, via a prior call to
#' `add_val_labs` or `add_val1`). This functionality may be useful for
#' interactively subsetting a data.frame, where character value labels may be
#' more intuitive and easily recalled than the underlying variable values
#' themselves (e.g., raceth=="White" & gender="F" may be more intuitive or readily
#' recalled than raceth==3 & gender==2).
#'
#' Note that `flab` (and labelr more broadly) is intended for moderate-sized (or
#' smaller) data.frames, defined loosely as those with a few million or fewer
#' rows. With a conventional (c. 2023) laptop, labelr operations on modest-
#' sized (~100K rows) take seconds (or less); with larger (> a few million rows)
#' data.frames, labelr may take several minutes (or run out of memory and fail
#' altogether!), depending on specifics.
#'
#' See also `slab`, `use_val_labs`, `add_val_labs`, `add_val1`, `add_quant_labs`, `add_quant1`, \cr
#' `get_val_labs`, `drop_val_labs`. For label-preserving subsetting tools that subset in
#' terms of raw values (not value labels), see `sfilter`, `sbrac`, `ssubset`, `sdrop`.
#'
#' @param data the data.frame from which columns will be selected.
#' @param condition row-filtering conditions along the lines of base::subset()
#' and/or dplyr::filter(), which may involve a combination of value labels
#' (for value-labeled variables only) and actual values (for non-value-labeled
#' variables only).
#' @return a labelr label attribute-preserving data.frame consisting of the
#' selected rows that meet the filtering condition(s).
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000) # another labelr:: function
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
#' # let's add variable VALUE labels for variable "gender"
#' # note that, if we are labeling a single variable, we can use add_val1()
#' # distinction between add_val1() and add_val_labs() will become more meaningful
#' # when we get to our Likert example
#' df <- add_val1(
#'   data = df, gender, vals = c(0, 1, 2),
#'   labs = c("M", "F", "O"), max.unique.vals = 50
#' )
#'
#' # see what we did
#' # get_val_labs(df)
#' get_val_labs(df, "gender")
#' get_val_labs(df, "raceth")
#'
#' # use --labels-- to filter w/ flab() ("*F*ilter *lab*el")
#' dflab <- flab(df, raceth == "Asian" & gender == "F")
#' head(dflab, 4)
#'
#' # equivalently, use --values--- to filter w/ sfilter() ("*S*afe filter")
#' dfsf <- sfilter(df, raceth == 3 & gender == 1)
#' head(dfsf, 4)
flab <- function(data, condition) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  row_names <- rownames(data)
  initial_lab_atts <- get_all_lab_atts(data)
  data2 <- use_val_labs(data)
  cond_call <- substitute(condition)
  col_names <- names(data2)
  cond_ind <- eval(cond_call, data2, parent.frame())
  cond_ind[is.na(cond_ind)] <- FALSE
  rm(data2)

  # if no rows match filtering conditions
  if (!any(cond_ind)) {
    data <- data[FALSE, ]

    # if one or more rows match filtering conditions
  } else {
    data <- data[cond_ind, ]

    # to restore label attributes information
    data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
  }

  return(data)
}