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

## -----------------------------------------------------------------------------
# Pull data from one sits
PIBO_1345_summ <- isaak_summ_bt |>
  filter(site == "PIBO_1345") |>
  unnest(cols = "summ_obj")

# We create a column that either contains NA, TRUE, or FALSE
#   NA is for first data
#   TRUE is if the difference between one row and the row preceding it is 1
#   FALSE is the difference is not 1
PIBO_1345_summ |>
  mutate(diff = c(NA, diff(date)) == 1) |>
  filter(diff == FALSE) # since the output is empty there are no FALSE in diff

# We can do the same to our isaak_summ_bt dataset
#   only difference here is we are mapping with an anonymous function hence the
#   ~{..., .x$date...} syntax. 
#   ~{} tells us it's an anonymous function while the .x allows to us to 
#   call the column from whatever data is piped in
isaak_summ_bt |>
  mutate(diff = map(
    summ_obj, ~ {
      c(NA, diff(.x$date) == 1) 
      }
    )
    ) |>
  unnest(cols = c(summ_obj, diff)) |>
  filter(diff == FALSE) 
# all continuous!

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
      map_df("dev.period") |>
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

