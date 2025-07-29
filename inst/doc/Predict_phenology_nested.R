## ----setup, include = FALSE---------------------------------------------------
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
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)

## ----eval=FALSE---------------------------------------------------------------
# library(hatchR)
# library(purrr)      # for mapping functions
# library(tidyr)      # for nesting data
# library(dplyr)      # for data manipulation
# library(lubridate)  # for date manipulation
# library(ggplot2)    # for plotting
# library(ggridges)   # for plotting

## ----echo = FALSE, out.height='70%'-------------------------------------------
knitr::include_graphics("img/isaak-2019.png")

## -----------------------------------------------------------------------------
# look at the first few rows
idaho

# count number of unique sites
idaho |> 
  pull(site) |>
  unique() |> 
  length()

## -----------------------------------------------------------------------------
# create a vector of site names with temps at or below 13 C
bull_trout_sites <- idaho |> 
  mutate(month = month(date)) |> #make a month column (numeric)
  filter(month == 8) |> # filter out Aug.
  group_by(site) |> # apply grouping by site
  summarise(mean_aug_temp = mean(temp_c)) |> 
  filter(mean_aug_temp <= 13) |> # keep only sites 13 C or cooler
  pull(site) |> 
  unique()

# we now have a list of 139 sites
length(bull_trout_sites)

# only keep sites in our vector of bull trout sites
idaho_bt <- idaho |> 
  filter(site %in% bull_trout_sites) 

# still lots of data!
idaho_bt

## -----------------------------------------------------------------------------
# lets look at a couple individual sites
PIBO_1345 <- idaho_bt |> filter(site == "PIBO_1345")

# looks nice
plot_check_temp(PIBO_1345,
                dates = date,
                temperature = temp_c)

# order by sample date
PIBO_1345 |> arrange(date)
# looks like there are multiple records per day 
# so we need to summarize those in the larger dataset (idaho_bt)


## -----------------------------------------------------------------------------
idaho_bt |> 
  group_by(site) |>  # we group by site
  nest() |> # nest our grouped data
  head()

## -----------------------------------------------------------------------------
isaak_summ_bt <- idaho_bt |> 
  group_by(site) |> 
  nest() |> 
  mutate(
    summ_obj = map(
      data, 
      summarize_temp,
      temperature = temp_c,
      dates = date
      )
    ) |> 
  select(site, summ_obj) 

# look at first rows of full object
isaak_summ_bt

# use purrr::pluck() the first site to see its structure
isaak_summ_bt |> pluck("summ_obj", 1)

## -----------------------------------------------------------------------------
isaak_summ_bt |> unnest(cols = summ_obj)

## ----warning=TRUE, message=TRUE-----------------------------------------------
# Pull data from one site
PIBO_1345_summ <- isaak_summ_bt |>
  filter(site == "PIBO_1345") |>
  unnest(cols = "summ_obj")

# We can then use the hatchR function to check that our data are continuous
# We see from the message they are!
check_continuous(data = PIBO_1345_summ, dates = date)

# To demonstrate an example of our data not being continuous, we remove the 100th row
check_continuous(data = PIBO_1345_summ[-100,], dates = date)


## ----warning=TRUE, message=TRUE-----------------------------------------------

# Map check_continuous() across isaak_summ_bt
# We'll only do the first 5 nested objects so the output doesn't get too long

isaak_summ_bt[1:5,] |> # run on first five nested dataframes for convenience
                       # remove [1:5,] to run on all nested objects 
  mutate(diff = map( # mutate a dummy diff column to run map
    summ_obj, # nested data object (akin to "data =" argument in normal function)
    check_continuous, # function name, no parentheses
    dates = date # specify dates argument with column name of dates in summ_obj
  )
  )


## -----------------------------------------------------------------------------
# spawn dataest
spawn_dates <- map(
  c(2011:2014), # year vector to map for custom function
  function(year) { # custom paste function
    c(
      paste0(year, "-09-01"),
      paste0(year, "-09-15"),
      paste0(year, "-09-30")
      )
    }
  ) |> 
  unlist()

# bull trout hatch model
bt_hatch <- model_select(
  development_type = "hatch",
  author = "Austin et al. 2019",
  species = "bull trout",
  model = "MM"
)

## -----------------------------------------------------------------------------
hatch_res <- isaak_summ_bt |> 
  mutate(
    dev_period = map2(
      summ_obj, spawn_dates, 
      predict_phenology,
      temperature = daily_temp,
      model = bt_hatch,
      dates = date
      ) |> 
      map_df("dev_period") |>
      list()
    ) |> 
  select(site, dev_period) |> # just select the columns we want
  unnest(cols = c(dev_period)) |> # unnest everything
  mutate(days_to_hatch = stop - start) # make a new column of days to hatch

hatch_res

## -----------------------------------------------------------------------------
hatch_res |>
  mutate(day = day(start)) |> 
  mutate(spawn_time = case_when(
    day == 1 ~ "Early",
    day == 15 ~ "Mid",
    day == 30 ~ "Late"
  )) |>
  mutate(spawn_time = factor(
    spawn_time, levels = c("Late", "Mid", "Early"), 
    ordered = TRUE)
    ) |> 
  ggplot(aes(
    x = as.integer(days_to_hatch), 
    y = spawn_time, 
    fill = spawn_time, 
    color = spawn_time
    )) + 
  geom_density_ridges(alpha = 0.9) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_color_brewer(palette = "Blues", direction = 1) +
  labs(x = "Days to hatch", y = "Spawn time") +
  theme_classic() +
  theme(legend.position = "none")

