% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_lab_col1.R
\name{add_lab_col1}
\alias{add_lab_col1}
\alias{alc1}
\title{Create a Value Labels Column for a Single Variable and Add to the Data Frame}
\usage{
add_lab_col1(data, var, suffix = "_lab")

alc1(data, var, suffix = "_lab")
}
\arguments{
\item{data}{a data.frame.}

\item{var}{the unquoted name of the column (variable) whose values you wish
to replace with the corresponding value labels.}

\item{suffix}{a suffix that will be appended to the name of the labels-on
column that is added to the data.frame (e.g., if suffix = "_lab," the
labels-on version of "x1" will be "x1_lab").}
}
\value{
A data.frame consisting of the originally supplied data.frame, along
with the labels-on column added to it.
}
\description{
For a single value-labeled column of a data.frame, create a copy of that
column that replaces all of its values with the corresponding value labels
and added that copy to the supplied data.frame.
}
\details{
Note 1: \code{add_lab_col1} is a variant of \code{add_lab_cols} that allows you to
specify only one variable at a time but that allows you to pass its name
without quoting it (compare add_lab_col1(mtcars, am) to
add_lab_cols(mtcars, "am")).

Note 2: \code{alc1} is a compact alias for \code{add_lab_col1}: they do the same thing,
and the former is easier to type.

Note 3: This command is intended exclusively for interactive use. In
particular, the var argument must be the literal name of a single variable
(column) found in the supplied data.frame and may NOT be, e.g., the name of a
character vector that contains the variable (column name) of interest. If you
wish to supply a character vector with the names of variables (columns) of
interest, use \code{add_lab_cols()}.

\code{add_lab_col1} creates a "labels-on" version of a value-labeled column and
adds that new column to the supplied data.frame. Here, "labels-on" means that
the column's original values are replaced with the corresponding value
labels. Note that this column does not replace but is added to its
parent/source columns in the returned data.frame. The resulting "labels-on"
column is a simple, self-contained character column that cannot itself be
converted or reverted to the original ("labels-off") values of its
parent/source column. See \code{add_lab_cols} for a list of other functions that
may be useful in working with value labels.
}
\examples{
# add "labels-on" version of "am" to copy of mtcars
df <- mtcars # copy of mtcars

# now, add value labels
df <- add_val1(
  data = df,
  var = am,
  vals = c(0, 1),
  labs = c("automatic", "manual")
)

# add value labels-on version of "am" to df, assign to df_plus
df_plus <- add_lab_col1(df, am)
head(df_plus[c("am", "am_lab")])
}
