% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/with_both_labs.R
\name{with_both_labs}
\alias{with_both_labs}
\alias{wbl}
\title{Overlay Variable Name and Value Labels Onto Arbitrary R Function Calls}
\usage{
with_both_labs(data, ...)

wbl(data, ...)
}
\arguments{
\item{data}{a data.frame with variable name labels and variable value labels.}

\item{...}{an additional expression passed to dots (quotes and dollar signs
are not needed or permitted).}
}
\value{
the value of the evaluated \code{expr}, with name and value labels
substituted for variable (column) names and values, respectively.
}
\description{
\code{with_both_labs} instructs R function calls to swap in variable name labels
for column names AND variable value labels for variable values in the objects
they return or side effects they produce.

Note: \code{wbl} is a compact alias for \code{with_both_labs}: they do the same thing,
and the former is easier to type
}
\details{
\code{with_both_labs} (see also alias \code{wbl}) is intended for interactive use. With
it, you pass a name-labeled data.frame followed, followed by a comma, followed
by an unquoted R expression (function call) to be evaluated within that
data.frame, and both name and value labels will be substituted for their
corresponding, respective column names and variable value in any returned
object or side effects. Your function call (expression) should refer to
columns of the data.frame passed via your data argument, NOT their name
labels, as the intent is to allow you to pass functions in terms of the
(typically much more concise and familiar) column names while having the
results displayed / presented in terms of the more informative (but more
verbose and typically non-standard) name labels. See examples.

Caution 1: Typically, \code{with_name_labs} will be more appropriate than
\code{with_both_labs}, since conversion of variables' values to their corresponding
labels frequently entails conversion from numeric to character.

Caution 2: \code{with_both_labs} is a rudimentary function that leverages basic
regular expressions and \code{eval(parse(text=))} to substitute name labels for
variable names behind the scenes. It appears to be robust to a range of the
most common commands and operators (e.g., formula or modeling operators, such
as ~ , * , +, :, =, and |). However, it is intended strictly as a convenience
for relatively simple, interactive, single-line-expression, data exploration,
description, or simple model-fitting use cases. It is expressly NOT intended
for: (1) multi-step workflows or pipes, (2) expressions that require or make
reference to objects existing outside the supplied data.frame, or (3) data
management operations aimed at modifying the supplied data.frame. Again, see
the examples for the types of expressions/use cases envisioned.
}
\examples{
# assign mtcars to new data.frame mt2
mt2 <- mtcars

# add name labs
mt2 <- add_name_labs(mt2,
  name.labs = c(
    "mpg" = "Miles/(US) gallon",
    "cyl" = "Number of cylinders",
    "disp" = "Displacement (cu.in.)",
    "hp" = "Gross horsepower",
    "drat" = "Rear axle ratio",
    "wt" = "Weight (1000 lbs)",
    "qsec" = "1/4 mile time",
    "vs" = "Engine (0 = V-shaped, 1 = straight)",
    "am" = "Transmission (0 = automatic, 1 = manual)",
    "gear" = "Number of forward gears",
    "carb" = "Number of carburetors"
  )
)


# add many-to-1 value labels
mt2 <- add_m1_lab(
  data = mt2,
  vars = "gear",
  vals = 4:5,
  lab = "4+"
)

# add many-to-1 value labels
mt2 <- add_val_labs(
  data = mt2,
  vars = "am",
  vals = c(0, 1),
  lab = c("auto", "man")
)
with_both_labs(mt2, t.test(mpg ~ am))
with_both_labs(mt2, lm(mpg ~ am))
with_both_labs(mt2, xtabs(~gear))
xtabs(~ mt2$gear)
}
