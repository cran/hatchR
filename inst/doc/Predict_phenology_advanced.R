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
library(purrr)
library(dplyr)
library(lubridate)
library(tidyr)

## -----------------------------------------------------------------------------
library(hatchR) # for data and phenology modeling
library(ggplot2) # for additional plotting options
library(purrr) # for iteration
library(lubridate) # for date manipulation
library(tibble) # for data manipulation
library(dplyr) # for data manipulation
library(tidyr) # for data manipulation

## -----------------------------------------------------------------------------
WI_spawn_dates <- c("1990-08-14", "1990-08-18", "1990-09-3")

## -----------------------------------------------------------------------------
sockeye_hatch_mod <- model_select(
  author = "Beacham and Murray 1990",
  species = "sockeye",
  model_id = 2,
  development_type = "hatch"
)

## -----------------------------------------------------------------------------
# Loop storage objects
OUT_loop_all <- NULL
OUT_loop_d2h <- NULL

# Loop body
for (d in 1:length(WI_spawn_dates)) { # d will be our numerical iterator

  # subset the element d of the vector and assign to object
  WI_spawn <- WI_spawn_dates[d]

  # predict phenology
  WI_hatch <- predict_phenology(
    data = woody_island,
    dates = date,
    temperature = temp_c,
    spawn.date = WI_spawn,
    model = sockeye_hatch_mod
  )

  ### ALL output ###
  # do this if we want to maintain all info predict_phenology
  OUT_loop_all[[d]] <- WI_hatch

  ### A single element of output ###
  # alternatively, subsets out output
  temp <- tibble(matrix(data = NA, ncol = 2, nrow = 1)) # empty dataframe to add in data
  colnames(temp) <- c("spawn_date", "days_2_hatch") # change column names
  temp$spawn_date <- WI_spawn # assign spawn date
  temp$days_2_hatch <- WI_hatch$days_to_develop # assign days to hatch
  OUT_loop_d2h <- rbind(OUT_loop_d2h, temp) # row bind temp object and OUT object
}

## -----------------------------------------------------------------------------
glimpse(OUT_loop_all)

## -----------------------------------------------------------------------------
OUT_loop_d2h

## -----------------------------------------------------------------------------
# map works by applying a function over a list (our vector is a very simple list)
# if you are familiar with apply() functions, map is essentially the same

results_map <- map(
  WI_spawn_dates, # vector we are mapping over
  predict_phenology, # function we are mapping with (note no "()"),
  data = woody_island, # additional arguments required by predict_phenology
  dates = date,
  temperature = temp_c,
  model = sockeye_hatch_mod
)

# we now have a list of lists the same as OUT_loop_all
glimpse(results_map)

# we can then access days to hatch easily as such
results_map |> map_dbl("days_to_develop")

## -----------------------------------------------------------------------------
# lapply() is the equivalent function as map() in the apply family
# they both output lists

results_lapply <- lapply(WI_spawn_dates, # vector we are mapping over
  predict_phenology, # function we are mapping with (note no "()"),
  data = woody_island, # additional arguments required by predict phenology
  dates = date,
  temperature = temp_c,
  model = sockeye_hatch_mod
)

## -----------------------------------------------------------------------------
glimpse(results_lapply)

## -----------------------------------------------------------------------------
# look at data structure
glimpse(crooked_river)

# visually check data
plot_check_temp(
  data = crooked_river,
  dates = date,
  temperature = temp_c,
  temp_min = 0,
  temp_max = 12
)

## -----------------------------------------------------------------------------
crooked_river |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  tally()

## ----warning=TRUE-------------------------------------------------------------
# select bull trout hatch and emergence models
bull_hatch_mod <- model_select(
  author = "Austin et al. 2019",
  species = "bull trout",
  model = "MM",
  development_type = "hatch" # note we are using hatch model here
)

bull_emerge_mod <- model_select(
  author = "Austin et al. 2019",
  species = "bull trout",
  model = "MM",
  development_type = "emerge" # note we are using emerge model here
)

# set spawning dates in 2015
dates_2015 <- c("2015-09-01", "2015-09-15", "2015-09-30")

## hatch first
hatch_2015 <- dates_2015 |> purrr::map(
  predict_phenology,
  data = crooked_river,
  dates = date,
  temperature = temp_c,
  model = bull_hatch_mod
)

## -----------------------------------------------------------------------------
## mini loop to make vector of dates
spawn_dates <- NULL
for (year in 2011:2014) {
  temp_dates <- c(paste0(year, "-09-01"), paste0(year, "-09-15"), paste0(year, "-09-30"))
  spawn_dates <- c(spawn_dates, temp_dates)
}
spawn_dates

### loop for prediction
OUT <- NULL
i <- 1 # build a counter for adding to our list of lists (OUT)

for (mod in c("hatch", "emerge")) { # loop for hatch and emerge

  # here we just iterate model selection over the two hatch and emerge options
  # in the first for loop
  dev_mod <- model_select(
    author = "Austin et al. 2019",
    species = "bull trout",
    model = "MM",
    development_type = mod
  )

  for (d in 1:length(spawn_dates)) { # nested loop over spawn dates

    # here we iterate over every date of spawn_dates but run this loop twice,
    # once for each value of the first for loop

    spawn <- spawn_dates[d] # get spawn date

    temp <- predict_phenology(
      data = crooked_river,
      dates = date,
      temperature = temp_c,
      spawn.date = spawn,
      model = dev_mod
    ) # notice we are calling the mod we set in the fist loop


    OUT[[i]] <- temp

    i <- i + 1 # add to your counter
  }
}

## -----------------------------------------------------------------------------
mapped_results <- OUT |> purrr:::map_df("ef_table")
mapped_results

## -----------------------------------------------------------------------------
# slightly cleaner spawn dates with map output is a list of vectors
spawn_dates <- map(
  c(2011:2014), # year vector to map for custom function
  function(year) { # custom function to make dates
    c(
      paste0(year, "-09-01"),
      paste0(year, "-09-15"),
      paste0(year, "-09-30")
    )
    }
  ) |> 
  unlist()

dev_mods <- map(
  c("hatch", "emerge"),
  model_select,
  author = "Austin et al. 2019",
  species = "bull trout",
  model = "MM"
  )

# we then set up a variable grid for all combinations of our models and dates
# it is very important to make the names of the columns in var_grid to match the
# arguments of the predict_phenology function (e.i., model = and spawn.date =)

var_grid <- expand_grid(model = dev_mods, spawn.date = spawn_dates)
head(var_grid)

### multiple input mapping

crooked_predictions <- pmap(var_grid, # combos of variables to iterate over
  predict_phenology, # function
  data = crooked_river, # additional arguments required by function
  dates = date,
  temperature = temp_c
)

## -----------------------------------------------------------------------------
# loop predictions
preds_loop <- OUT |> map_dbl("days_to_develop")
preds_pmap <- crooked_predictions |> map_dbl("days_to_develop")

# everything matches!
preds_loop == preds_pmap

## -----------------------------------------------------------------------------
# get predictions
days <- crooked_predictions |>
  map_dbl("days_to_develop")

# make a vector of what type of phenology we were predicting
# remember we ran hatch over 15 spawn dates then emerge over those same 15
phenology <- c(rep("hatch", 12), rep("emerge", 12))

# make a vector of our spawn dates replicated twice and turn into a timepoint
spawning <- rep(spawn_dates, 2) |>
  ymd(tz = "UTC") # we do this because the crooked_river dataset is ymd_hms

# put them all together in an object
bull_trout_phenology <- tibble(phenology, spawning, days)
head(bull_trout_phenology)

## -----------------------------------------------------------------------------
# filter crooked_river to correct size
crooked_river_11_15 <- crooked_river |> filter(date < ymd_hms("2015-06-01 00:00:00"))

ggplot(data = crooked_river_11_15, aes(x = date, y = temp_c)) +
  geom_point(size = 0.25) +
  geom_line() +
  geom_point(
    data = bull_trout_phenology,
    aes(x = spawning, y = 10), color = "darkblue", shape = 25, size = 2.5
  ) +
  geom_point(
    data = bull_trout_phenology |> filter(phenology == "hatch"),
    aes(x = spawning + days(days), y = 0),
    color = "darkgreen", shape = 24, size = 2.5
  ) +
  geom_point(
    data = bull_trout_phenology |> filter(phenology == "emerge"),
    aes(x = spawning + days(days), y = 0),
    color = "darkorange", shape = 24, size = 2.5
  ) +
  labs(
    title = "Crooked River Bull Trout Developmental Phenology",
    subtitle = "Blue = Spawn, Green = Hatch, Orange = Emerge",
    x = "Date",
    y = "Temperature"
  ) +
  theme_classic()

## -----------------------------------------------------------------------------
# the tibble with all temperature and effective values for each phenological period
all_data <- crooked_predictions |>
  map_df("ef_table")
head(all_data)

# the phenological durations for each prediction set
development_period <- crooked_predictions |>
  map_df("dev.period")
head(development_period)

## -----------------------------------------------------------------------------
# to use the simple Iliamna Lake example within a single season
WI_named_list <- WI_spawn_dates |>
  set_names() |>
  map(predict_phenology, # note we leave out the input row (WI_spawn_dates) because we are piping it in as input
    data = woody_island, # additional arguments required by predict phenology
    dates = date,
    temperature = temp_c,
    model = sockeye_hatch_mod
  )

# you can now see each list is named according to its respective spawn date
glimpse(WI_named_list)

# therefore a single element could be accessed with its name and the $ operator
# if we wanted the dev.period for fish spawning on August 18th we would do the following

WI_named_list$`1990-08-18`$dev.period

