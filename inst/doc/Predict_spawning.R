## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  fig.align = 'center',
  fig.width = 6
)
options(tibble.print_min = 5, tibble.print_max = 5)

## ----setup, echo=FALSE--------------------------------------------------------
library(hatchR)

## -----------------------------------------------------------------------------
#select bull trout emergence model
bt_emerge_mod <- model_select(author = "Austin et al. 2019",
                                      species = "bull trout",
                                      model = "MM",
                                      development_type = "emerge"
                              )

## -----------------------------------------------------------------------------
#predict spawn timing using "2015-03-21" emergence date
bt_spawn <- predict_spawn(data = crooked_river,
              dates = date,
              temperature = temp_c,
              develop.date = "2015-03-21",
              model = bt_emerge_mod
              )

## -----------------------------------------------------------------------------
str(bt_spawn)

## -----------------------------------------------------------------------------
# development time
bt_spawn$days_to_develop

# spawning date
bt_spawn$dev_period$start

## -----------------------------------------------------------------------------
plot_phenology(bt_spawn)

## -----------------------------------------------------------------------------
bt_emerge <- predict_phenology(data = crooked_river,
              dates = date,
              temperature = temp_c,
              spawn.date = "2014-09-15",
              model = bt_emerge_mod
              )

## -----------------------------------------------------------------------------
# are they the same (yes!)
bt_emerge$dev_period == bt_spawn$dev_period

#print out values
bt_emerge$dev_period; bt_spawn$dev_period


## -----------------------------------------------------------------------------
library(purrr)

#vector of dates
emerge_days <- c("2015-02-15","2015-03-15", "2015-04-15")

# object for predicting spawn timing across three emergence days
bt_multiple_emerge <- map(emerge_days, # vector of emergence dates
                          predict_spawn, # predict_spawn function
                          # everything below are arguments to predict_spawn()
                          data = crooked_river,
                          dates = date,
                          temperature = temp_c,
                          model = bt_emerge_mod)

# we can access just the dev_periods using map_df
# the start column provides the predicted spawn dates
bt_multiple_emerge |> 
  map_df("dev_period")

