## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE,
  comment = "### >"
)

## ----setup--------------------------------------------------------------------
# install.packages("labelr") #CRAN version
# install.packages("devtools") # Step 1 to get GitHub version
# devtools::install_github("rhartmano/labelr") #Step 2 to get GitHub version
library(labelr)

## -----------------------------------------------------------------------------
set.seed(555) # for reproducibility
df <- make_demo_data(n = 1000) # you can specify the number of fictional obs.

# make a backup for later comparison
df_copy <- df

## -----------------------------------------------------------------------------
df <- add_frame_lab(df, frame.lab = "Demographic and reaction time test score
                    records collected by Royal Statistical Agency of
                    Fictionaslavica. Data fictionally collected in the year
                    1987. As published in A. Smithee (1988). Some Fictional Data
                    for Your Amusement. Mad Magazine, 10(1), 1-24.")


get_frame_lab(df)

## -----------------------------------------------------------------------------
df <- add_name_labs(df, name.labs = c(
  "age" = "Age in years",
  "raceth" = "Racial/ethnic identity group category",
  "gender" = "Gender identity category",
  "edu" = "Highest education level attained",
  "x1" = "Space Invaders reaction time test scores",
  "x2" = "Galaga reaction time test scores"
))

## -----------------------------------------------------------------------------
get_name_labs(df)

## -----------------------------------------------------------------------------
df <- add_val_labs(df, # data.frame with to-be-value-labeled column
  vars = "raceth", # quoted variable name of to-be-labeled col
  vals = c(1:7), # label values 1 through 7, inclusive
  labs = c(
    "White", "Black", "Hispanic", # ordered labels for vals 1-7
    "Asian", "AIAN", "Multi", "Other"
  ),
  max.unique.vals = 10 # max number of unique values permitted
)

## -----------------------------------------------------------------------------
df <- add_val1(
  data = df,
  var = gender, # contrast this var argument to the vars argument demo'd above
  vals = c(0, 1, 2), # the values to be labeled
  labs = c("Male", "Female", "Other"), # the labels, applied in order to the vals
  max.unique.vals = 10
)

## -----------------------------------------------------------------------------
get_val_labs(df)

## -----------------------------------------------------------------------------
df_temp <- add_quant_labs(
  data = df,
  vars = "x",
  qtiles = 5,
  partial = TRUE
)

get_val_labs(df_temp)

## -----------------------------------------------------------------------------
df_temp <- add_quant_labs(
  data = df_temp,
  vars = "x",
  vals = c(100, 150),
  partial = TRUE
)

get_val_labs(df_temp)

## -----------------------------------------------------------------------------
df <- add_quant1(df, # data.frame
  x1, # variable to value-label
  qtiles = 5
) # number quintiles to define numerical range labels

## -----------------------------------------------------------------------------
df <- add_m1_lab(df, "edu", vals = c(3:5), lab = "Some College+")
df <- add_m1_lab(df, "edu", vals = 1, lab = "Not HS Grad")
df <- add_m1_lab(df, "edu", vals = 2, lab = "HSG, No College")

get_val_labs(df)

## -----------------------------------------------------------------------------
head(df_copy, 3) # our pre-labeling copy of the data.frame

head(df, 3) # our latest, post-labeling version of same data.frame

## -----------------------------------------------------------------------------
labs.df <- get_all_lab_atts(df)

## -----------------------------------------------------------------------------
df <- strip_labs(df) # remove our labels
get_all_lab_atts(df) # show that they're gone

## -----------------------------------------------------------------------------
df <- add_lab_atts(df, labs.df)

get_all_lab_atts(df)

## -----------------------------------------------------------------------------
head(df, 5) # Base R function utils::head()

headl(df, 5) # labelr function headl() (note the "l")

tail(df, 5) # Base R function utils::tail()

taill(df, 5) # labelr function taill() (note the extra "l")

set.seed(293)
car::some(df, 5) # car package function car::some()

set.seed(293)
somel(df, 5) # labelr function somel() (note the "l")

## -----------------------------------------------------------------------------
use_val_labs(df)[1:20, ] # headl() is just a more compact shortcut for this

## -----------------------------------------------------------------------------
# `collapse::qsu()`
# with labels "off" (i.e., using regular values of "raceth" as by var)
(by_demog_val <- collapse::qsu(df, cols = c("x2"), by = ~raceth))

# with labels "on" (i.e., using labels, thanks to `uvl()`)
(by_demog_lab <- collapse::qsu(uvl(df), cols = c("x2"), by = ~raceth))

## -----------------------------------------------------------------------------
with(df, table(gender, raceth)) # base::with()

with_val_labs(df, table(gender, raceth)) # labelr::with_val_labs()

wvl(df, table(gender, raceth)) # labelr::wvl is a more compact alias

## -----------------------------------------------------------------------------
df_labd <- use_val_labs(df)
head(df_labd) # note, this is utils::head(), not labelr::headl()

## -----------------------------------------------------------------------------
df_plus_labs <- add_lab_cols(df)
head(df_plus_labs[c("gender", "gender_lab", "raceth", "raceth_lab")])

## -----------------------------------------------------------------------------
head(df)

df1 <- flab(df, raceth == "Asian" & gender == "Female")

head(df1, 5) # returned df1 is in terms of values, just like df

headl(df1, 5) # note use of labelr::headl; labels are there

## -----------------------------------------------------------------------------
df2 <- slab(df, raceth == "Black" & gender == "Male", gender, raceth)
head(df2, 10)

## -----------------------------------------------------------------------------
names_labs_vec <- c(
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

## -----------------------------------------------------------------------------
mt2 <- add_name_labs(mtcars,
  vars = names(names_labs_vec),
  labs = names_labs_vec
)

## -----------------------------------------------------------------------------
mt2 <- add_name_labs(mtcars,
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

## -----------------------------------------------------------------------------
mt2 <- use_name_labs(mt2)

head(mt2[c(1, 2)])

## -----------------------------------------------------------------------------
lm(`Miles/(US) gallon` ~ `Number of cylinders`, data = mt2) # pasting in var names
lm(mpg ~ cyl, data = use_var_names(mt2)) # same result if name labels are "off"

## -----------------------------------------------------------------------------
sapply(mt2, median) # get the median for every name-labeled variable

collapse::qsu(mt2) # use an external package for more informative descriptives

## -----------------------------------------------------------------------------
# invert our prior use_name_labs() call
mt2 <- use_var_names(mt2) # revert from name labels back to original colnames
head(mt2[c(1, 2)])

## -----------------------------------------------------------------------------
# first, show that mt2 now has original column names swapped back in
head(mt2)

# verify that the name labels are still present and available in the background
get_name_labs(mt2)

## -----------------------------------------------------------------------------
# demo with_name_labs() (note that with_name_labs() will achieve same result)
with_name_labs(mt2, t.test(mpg ~ am)) # wnl() is alias for with_name_labs()

with_name_labs(mt2, lm(mpg ~ am))

wnl(mt2, summary(mt2)) # wnl() is alias for with_name_labs()

wnl(mt2, xtabs(~gear)) # wnl() is alias for with_name_labs()

with(mt2, xtabs(~gear)) # compare this base::with() call to wnl() call above

