## ----include = FALSE----------------------------------------------------------
# rmd style
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.width = 6
)
options(tibble.print_min = 5, tibble.print_max = 5)

# load packages
library(hatchR)
library(tibble)
library(ggplot2)
library(lubridate)

## ----eval = FALSE-------------------------------------------------------------
# library(hatchR)    # for phenology modeling
# library(ggplot2)   # for additional plotting options
# library(lubridate) # for date manipulation
# library(tibble)    # for tidy tibbles and data manipulation
# library(dplyr)     # for data manipulation

## -----------------------------------------------------------------------------
woody_island
summary(woody_island)

## -----------------------------------------------------------------------------
p <- plot_check_temp(
  data = woody_island,
  dates = date,
  temperature = temp_c
  )
p

## -----------------------------------------------------------------------------
p +
  geom_rect(
    aes(
      xmin = ymd("1990-08-18"),
      xmax = ymd("1991-04-01"),
      ymin = -10, 
      ymax = 25
    ), 
    fill = "gray",
    alpha = 0.01
  )

## -----------------------------------------------------------------------------
sockeye_hatch_mod <- model_select(
  author = "Beacham and Murray 1990",
  species = "sockeye",
  model = 2,
  development_type = "hatch"
)

sockeye_emerge_mod <- model_select(
  author = "Beacham and Murray 1990",
  species = "sockeye",
  model = 2,
  development_type = "emerge"
)

## -----------------------------------------------------------------------------
sockeye_hatch_mod
sockeye_emerge_mod

## ----warning=TRUE-------------------------------------------------------------
WI_hatch <- predict_phenology(
  data = woody_island,
  dates = date,
  temperature = temp_c,
  spawn.date = "1990-08-18",
  model = sockeye_hatch_mod
)

## -----------------------------------------------------------------------------
WI_hatch$days_to_develop
WI_hatch$dev.period

## ----warning=TRUE-------------------------------------------------------------
WI_emerge <- predict_phenology(
  data = woody_island,
  dates = date,
  temperature = temp_c,
  spawn.date = "1990-08-18",
  model = sockeye_emerge_mod # notice we're using emerge model expression here
)

WI_emerge$days_to_develop
WI_emerge$dev.period

## -----------------------------------------------------------------------------
summary(WI_hatch)

## -----------------------------------------------------------------------------
plot_phenology(WI_hatch)

## ----eval=FALSE---------------------------------------------------------------
#  # shows a plot with just the ef cumulative sum values
# plot_phenology(WI_hatch, style = "ef_cumsum")
# # shows a plot with just the ef daily values
# plot_phenology(WI_hatch, style = "ef_daily")
# # turns off the labeling for a cleaner figure
# plot_phenology(WI_hatch, labels = FALSE)

## -----------------------------------------------------------------------------
# vector of temps from -5 to 15 by 0.5
x <- seq(from = -5, to = 15, by = 0.5)
x

# get effective values for those temperatures
# You can see the NaN warning that shows up in our past applications
demo_ef_vals <- eval(parse(text = sockeye_hatch_mod$expression))
demo_ef_vals

# bring together as a tibble
demo <- tibble(x, demo_ef_vals)
demo

# plot (note NaNs are removed from figure)
# rectangle added to highlight the approximate temperatures of interest
demo |>
  ggplot(aes(x = x, y = demo_ef_vals)) +
  geom_rect(
    aes(
      ymin = 0, ymax = 0.005,
      xmin = -5, xmax = 2
    ),
    fill = "dodgerblue", alpha = 0.25
  ) +
  geom_point() +
  geom_line() +
  labs(x = "Temperature (C)", y = "Effective Value") +
  theme_classic()

