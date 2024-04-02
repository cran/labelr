## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE,
  comment = "### >"
)

## -----------------------------------------------------------------------------
opening_ding <- Sys.time() # to time labelr

library(labelr)
library(nycflights13)

## -----------------------------------------------------------------------------
df <- flights

nrow(df)

## -----------------------------------------------------------------------------
df <- add_frame_lab(df, frame.lab = "On-time data for all flights that
                    departed NYC (i.e. JFK, LGA or EWR) in 2013.")

## -----------------------------------------------------------------------------
attr(df, "frame.lab") # check for attribute

get_frame_lab(df) # return frame.lab alongside data.frame name as a data.frame

get_frame_lab(df)$frame.lab

## -----------------------------------------------------------------------------
names_labs_vec <- c(
  "year" = "Year of departure",
  "month" = "Month of departure",
  "year" = "Day of departure",
  "dep_time" = "Actual departure time (format HHMM or HMM), local tz",
  "arr_time" = "Actual arrival time (format HHMM or HMM), local tz",
  "sched_dep_time" = "Scheduled departure times (format HHMM or HMM)",
  "sched_arr_time" = "Scheduled arrival time (format HHMM or HMM)",
  "dep_delay" = "Departure delays, in minutes",
  "arr_delay" = "Arrival delays, in minutes",
  "carrier" = "Two letter airline carrier abbreviation",
  "flight" = "Flight number",
  "tailnum" = "Plane tail number",
  "origin" = "Flight origin airport code",
  "dest" = "Flight destination airport code",
  "air_time" = "Minutes spent in the air",
  "distance" = "Miles between airports",
  "hour" = "Hour of scheduled departure time",
  "minute" = "Minutes component of scheduled departure time",
  "time_hour" = "Scheduled date and hour of the flight as a POSIXct date"
)

df <- add_name_labs(df, name.labs = names_labs_vec)

get_name_labs(df) # show that they've been added

## -----------------------------------------------------------------------------
airlines <- nycflights13::airlines

head(airlines)

## -----------------------------------------------------------------------------
ny_val <- airlines$carrier

## -----------------------------------------------------------------------------
ny_lab <- airlines$name

## -----------------------------------------------------------------------------
df <- add_val1(df,
  var = carrier, vals = ny_val,
  labs = ny_lab,
  max.unique.vals = 20
)

## -----------------------------------------------------------------------------
ny_month_vals <- c(1:12) # values
ny_month_labs <- c(
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
) # labels

## -----------------------------------------------------------------------------
df <- add_val_labs(df,
  vars = "month",
  vals = ny_month_vals,
  labs = ny_month_labs,
  max.unique.vals = 20
)

## -----------------------------------------------------------------------------
df <- add_quant_labs(df, "dep_time", qtiles = 5)

## -----------------------------------------------------------------------------
get_val_labs(df)

## -----------------------------------------------------------------------------
head(df[c("origin", "dep_time", "dest", "year", "month", "carrier")])

## -----------------------------------------------------------------------------
df_swapd <- use_val_labs(df)

head(df_swapd[c("origin", "dep_time", "dest", "year", "month", "carrier")])

## -----------------------------------------------------------------------------
df_plus <- add_lab_cols(df, vars = c("carrier", "month", "dep_time"))

head(df_plus[c(
  "origin", "dest", "year",
  "month", "month_lab",
  "dep_time", "dep_time_lab",
  "carrier", "carrier_lab"
)])

## -----------------------------------------------------------------------------
# labels are not visible (they exist only as attributes() meta-data)
head(df[c("carrier", "arr_delay")])

# we still can use them to filter (note: we're filtering on "JetBlue Airways",
# ...NOT its obscure code "B6")
df_fl <- flab(df, carrier == "JetBlue Airways" & arr_delay > 20)

# here's what's returned when we filtered on "JetBlue Airways" using flab()
head(df_fl[c("carrier", "arr_delay")])

# double-check that this is JetBlue
head(use_val_labs(df_fl)[c("carrier", "arr_delay")])

## -----------------------------------------------------------------------------
the_buzzer <- Sys.time()
the_buzzer - opening_ding

## -----------------------------------------------------------------------------
mtbad <- mtcars

## -----------------------------------------------------------------------------
mtbad[1, 1:11] <- NA
rownames(mtbad)[1] <- "Missing Car"
mtbad[2, "am"] <- Inf
mtbad[3, "gear"] <- -Inf
mtbad[5, "carb"] <- NaN
mtbad[2, "mpg"] <- Inf
mtbad[3, "mpg"] <- NaN

# add a character variable, for demonstration purposes
# if it makes you feel better, you can pretend these are Consumer Reports or
# ...JD Power ratings or something
set.seed(9202) # for reproducibility
mtbad$grade <- sample(c("A", "B", "C"), nrow(mtbad), replace = TRUE)
mtbad[4, "grade"] <- NA
mtbad[5, "grade"] <- "NA"
mtbad[6, "grade"] <- "Inf"

# see where this leaves us
head(mtbad)

sapply(mtbad, class)

## -----------------------------------------------------------------------------
mtlabs <- mtbad |>
  add_val1(grade,
    vals = c("A", "B", "C"),
    labs = c("Gold", "Silver", "Bronze")
  ) |>
  add_val1(am,
    vals = c(0, 1),
    labs = c("auto", "stick")
  ) |>
  add_val1(carb,
    vals = c(1, 2, 3, 4, 6, 8), # not the most inspired use of labels
    labs = c(
      "1c", "2c", "3c",
      "4c", "6c", "8c"
    )
  ) |>
  add_val1(gear,
    vals = 3:5, # again, not the most compelling use case
    labs = c(
      "3-speed",
      "4-speed",
      "5-speed"
    )
  ) |>
  add_quant1(mpg, qtiles = 4) # add quartile-based value labels

## -----------------------------------------------------------------------------
get_val_labs(mtlabs, "am") # NA values were detected and dealt with

## -----------------------------------------------------------------------------
mtless <- sselect(mtlabs, mpg, cyl, am, gear, carb, grade) # safely select

head(mtless, 5) # note that the irregular values are still here

## -----------------------------------------------------------------------------
head(use_val_labs(mtless), 5) # but they all go to NA if we `use_val_labs`

## -----------------------------------------------------------------------------
mtlabs_plus <- add_lab_cols(mtlabs, c("mpg", "am")) # creates, adds "am_lab" col
mtlabs_plus <- sselect(mtlabs_plus, mpg, mpg_lab, am, am_lab) # select cols

head(mtlabs_plus) # where we landed

## -----------------------------------------------------------------------------
# Trying to Label an Irregular Value (-Inf)
mtbad <- add_val1(
  data = mtcars,
  var = gear,
  vals = -Inf,
  labs = c("neg.inf")
)

# Trying to Label an Irregular Value (NA)
mtbad <- add_val_labs(
  data = mtbad,
  vars = "grade",
  vals = NA,
  labs = c("miss")
)

# Trying to Label an Irregular Value (NaN)
mtbad <- add_val_labs(
  data = mtbad,
  vars = "carb",
  vals = NaN,
  labs = c("nan-v")
)

# labelr also treats "character variants" of irregular values as irregular values.
mtbad <- add_val1(
  data = mtbad,
  var = carb,
  vals = "NAN",
  labs = c("nan-v")
)

## -----------------------------------------------------------------------------
unique(iris$Species)

sapply(iris, class) # nothing up our sleeve -- "Species" is a factor

## -----------------------------------------------------------------------------
irlab <- add_val_labs(iris,
  vars = "Species",
  vals = c("setosa", "versicolor", "virginica"),
  labs = c("se", "ve", "vi")
)

# this also would've worked
# irlab_dos <- add_val1(iris, Species,
#   vals = c("setosa", "versicolor", "virginica"),
#   labs = c("se", "ve", "vi")
# )

## -----------------------------------------------------------------------------
summary(iris)

summary(irlab)

head(iris, 4)

head(irlab, 4)

lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

lm(Sepal.Length ~ Sepal.Width + Species, data = irlab) # values are same

## -----------------------------------------------------------------------------
sapply(irlab, class)

levels(irlab$Species)

## -----------------------------------------------------------------------------
get_val_labs(irlab, "Species")

## -----------------------------------------------------------------------------
head(use_val_labs(irlab))
ir_v <- flab(irlab, Species == "vi")
head(ir_v, 5)

## -----------------------------------------------------------------------------
irlab_aug <- add_lab_cols(irlab, vars = "Species")

## -----------------------------------------------------------------------------
set.seed(231)
sample_rows <- sample(seq_len(nrow(irlab)), 10, replace = FALSE)

irlab_aug[sample_rows, ]

sapply(irlab_aug, class)

with(irlab_aug, table(Species, Species_lab))

## -----------------------------------------------------------------------------
ir_char <- use_val_labs(irlab) # we assign this to a new data.frame
sapply(ir_char, class)

head(ir_char, 3)

class(ir_char$Species) # it's character

## -----------------------------------------------------------------------------
ir_fact <- use_val_labs(irlab)

ir_fact$Species <- factor(ir_char$Species,
  levels = c("se", "ve", "vi"),
  labels = c("se", "ve", "vi")
)
head(ir_fact, 3)

class(ir_fact$Species) # it's a factor

levels(ir_fact$Species) # it's a factor

## -----------------------------------------------------------------------------
with(ir_fact, tapply(Sepal.Width, Species, mean))
with(irlab, tapply(Sepal.Width, Species, mean))
with(iris, tapply(Sepal.Width, Species, mean))

## -----------------------------------------------------------------------------
ir_ord <- iris

set.seed(293)
qrating <- c("AAA", "AA", "A", "BBB", "AA", "BBB", "A")

ir_ord$qrat <- sample(qrating, 150, replace = TRUE)

ir_ord$qrat <- factor(ir_ord$qrat,
  ordered = TRUE,
  levels = c("AAA", "AA", "A", "BBB")
)

## -----------------------------------------------------------------------------
levels(ir_ord$qrat)

class(ir_ord$qrat)

## -----------------------------------------------------------------------------
ir_ord <- add_val_labs(ir_ord,
  vars = "qrat",
  vals = c("AAA", "AA", "A", "BBB"),
  labs = c(
    "unimpeachable",
    "excellent",
    "very good",
    "meh"
  )
)

## -----------------------------------------------------------------------------
ir_ord <- add_lab_cols(ir_ord, vars = "qrat")

head(ir_ord, 10)

with(ir_ord, table(qrat_lab, qrat))

class(ir_ord$qrat)

levels(ir_ord$qrat)

class(ir_ord$qrat_lab)

get_val_labs(ir_ord, "qrat") # labs are still there for qrat

get_val_labs(ir_ord, "qrat_lab") # no labs here; this is just a character var

## -----------------------------------------------------------------------------
class(iris[["Species"]])

iris_df <- factor_to_lab_int(iris, Species)

class(iris_df[["Species"]])

head(iris_df$Species)

get_val_labs(iris_df, "Species")

## -----------------------------------------------------------------------------
carb_orig <- mtcars

carb_orig <- add_val_labs(
  data = mtcars,
  vars = "carb",
  vals = c(1, 2, 3, 4, 6, 8),
  labs = c(
    "1c", "2c", # a tad silly, but these value labels will demo the principle
    "3c", "4c",
    "6c", "8c"
  )
)

# carb as labeled numeric
is.integer(carb_orig$carb) # note: carb not technically an "as.integer()" integer

class(carb_orig$carb) # but it IS numeric

has_decv(carb_orig$carb) # and does NOT have decimals; so, lab_int_to_fac() works

levels(carb_orig$carb) # none, not a factor

head(carb_orig$carb, 3) # remember to compare to carb_to_int (below)

mean(carb_orig$carb) # remember to compare to carb_to_int (below)

lm(mpg ~ carb, data = carb_orig) # remember to compare to carb_to_int (below)

# note this for comparison to below
(adj_r2_orig <- summary(lm(mpg ~ carb, data = carb_orig))$adj.r.squared)

# compare to counterparts below
AIC(lm(mpg ~ carb, data = carb_orig))

# Make carb a factor
carb_fac <- lab_int_to_factor(carb_orig, carb) # alias int2f() also works

class(carb_fac$carb) # now it's a factor

levels(carb_fac$carb) # like any good factor, it has levels

head(carb_fac$carb, 3)

lm(mpg ~ carb, data = carb_fac) # again: carb is a factor

# compare these model fit stats to counterparts above and below
(adj_r2_fac <- summary(lm(mpg ~ carb, data = carb_fac))$adj.r.squared)

# compare to counterparts above and below
AIC(lm(mpg ~ carb, data = carb_fac))

## -----------------------------------------------------------------------------
# ??"back"?? to integer? Not quite. Compare below to carb_orig above
carb_to_int <- factor_to_lab_int(carb_fac, carb) # alias f2int() also works

class(carb_to_int$carb) # Is an integer

levels(carb_to_int$carb) # NOT a factor

head(carb_to_int$carb, 3) # NOT the same as carb_orig

mean(carb_to_int$carb) # NOT the same as carb_orig

identical(carb_to_int$carb, carb_orig$carb) # really!

lm(mpg ~ carb, data = carb_to_int) # NOT the same as carb_orig

# Compare to counterpart calls from earlier iterations of carb (above)
(adj_r2_int <- summary(lm(mpg ~ carb, data = carb_to_int))$adj.r.squared)
AIC(lm(mpg ~ carb, data = carb_to_int))

## -----------------------------------------------------------------------------
get_val_labs(irlab, "Species")

## -----------------------------------------------------------------------------
irl_dumm <- add_lab_dummies(irlab, "Species")
head(irl_dumm) # they're there!
tail(irl_dumm) # again, they're there!

## -----------------------------------------------------------------------------
irl_dumm2 <- add_lab_dumm1(irlab, Species)
head(irl_dumm2) # again, they're there!
tail(irl_dumm2) # again, they're there!

## -----------------------------------------------------------------------------
set.seed(272) # for reproducibility
dflik <- make_likert_data(scale = 1:7) # another labelr function
head(dflik)

## -----------------------------------------------------------------------------
vals2label <- 1:7
labs2use <- c(
  "VSD",
  "SD",
  "D",
  "N",
  "A",
  "SA",
  "VSA"
)

## -----------------------------------------------------------------------------
dflik <- add_val_labs(
  data = dflik, vars = c("x", "y3"), ###  note the vars args
  vals = vals2label,
  labs = labs2use,
  partial = TRUE # applying to all cols with "x" or "y3" substring in names
)

## -----------------------------------------------------------------------------
head(dflik)

## -----------------------------------------------------------------------------
lik1 <- uvl(dflik) # assign to new object, since we can't "undo"
head(lik1) # we could have skipped previous call by using labelr::headl(dflik)

## -----------------------------------------------------------------------------
dfdrop <- drop_val_labs(dflik,
  c("x2", "y3"),
  partial = FALSE
)

## -----------------------------------------------------------------------------
get_val_labs(dfdrop, c("x2", "y3"))

## -----------------------------------------------------------------------------
get_val_labs(dfdrop, "x1")

## -----------------------------------------------------------------------------
dfxgone <- drop_val_labs(dflik,
  c("x"),
  partial = TRUE # note
)

## -----------------------------------------------------------------------------
get_val_labs(dfxgone)

## -----------------------------------------------------------------------------
set.seed(4847) # for reproducibility
df <- make_demo_data(n = 100) # make a fictional n = 100 data set

df <- add_val1(df, # data.frame
  var = raceth, # var to label, unquoted since this is add_val1()
  vals = c(1:7), # label values 1 through 7, inclusive
  labs = c(
    "White", "Black", "Hispanic", # ordered labels for sequential vals 1-7
    "Asian", "AIAN", "Multi", "Other"
  )
)

df <- add_val1(
  data = df,
  var = gender,
  vals = c(0, 1, 2), # the values to be labeled
  labs = c("Male", "Female", "Other"), # labs order should reflect vals order
  max.unique.vals = 10
)

# label values of var "x1" according to quantile ranges
df <- add_quant1(
  data = df,
  var = x1, # apply quantile range value labels to this var
  qtiles = 3 # first, second, and third tertiles
)

# apply many-vals-get-one-label labels to "edu" (note vals 3-5 all get same lab)
df <- add_m1_lab(df, "edu", vals = c(3:5), lab = "Some College+")
df <- add_m1_lab(df, "edu", vals = 1, lab = "Not HS Grad")
df <- add_m1_lab(df, "edu", vals = 2, lab = "HSG, No College")

# show value labels
get_val_labs(df)

## -----------------------------------------------------------------------------
tabl(df, vars = "gender", labs.on = FALSE)

## -----------------------------------------------------------------------------
tabl(df, vars = "gender", labs.on = TRUE) # labs.on = TRUE is the default

## -----------------------------------------------------------------------------
tabl(df, vars = c("gender", "edu"), prop.digits = 3)

## -----------------------------------------------------------------------------
head(tabl(df, vars = c("raceth", "edu"), wide.col = "gender"), 20)

## -----------------------------------------------------------------------------
tabl(iris, "Species") # explicit vars arg with one-var ("Species")

# many-valued numeric vars automatically converted to quantile categories
tabl(mtcars, c("am", "gear", "cyl", "disp", "mpg"),
  qtiles = 4, zero.rm = TRUE
)

