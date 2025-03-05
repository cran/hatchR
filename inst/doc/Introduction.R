## ----setup, include = FALSE---------------------------------------------------
# rmd style
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE,
  message = FALSE,
  fig.align = 'center',
  fig.width = 6
)
options(tibble.print_min = 5, tibble.print_max = 5)

# load packages
library(hatchR)
library(lubridate)
library(readr)
library(dplyr)
library(nycflights13)
library(tibble)
library(ggplot2)

## ----echo = FALSE, out.width = '50%'------------------------------------------
knitr::include_graphics("img/workflow.png")

## ----eval = FALSE-------------------------------------------------------------
# library(lubridate)

## -----------------------------------------------------------------------------
today()
now()

## ----eval=FALSE---------------------------------------------------------------
# library(readr)

## -----------------------------------------------------------------------------
csv <- "
  date,datetime
  2022-01-02,2022-01-02 05:12
"
read_csv(csv)

## -----------------------------------------------------------------------------
ymd("2017-01-31")
mdy("January 31st, 2017")
mdy_hm("01/31/2017 08:01")
ymd_hms("2017-01-31 20:11:59")

## ----eval=FALSE---------------------------------------------------------------
# library(nycflights13)
# library(dplyr)

## -----------------------------------------------------------------------------
flights |> 
  select(year, month, day) |>
  mutate(date = make_date(year, month, day))

flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure = make_datetime(year, month, day, hour, minute))

## -----------------------------------------------------------------------------
Sys.timezone()

## -----------------------------------------------------------------------------
x1 <- ymd_hms("2024-06-01 12:00:00", tz = "America/New_York")
x1

## ----eval=FALSE---------------------------------------------------------------
# library(readr)

## -----------------------------------------------------------------------------
path_cr <- system.file("extdata/crooked_river.csv", package = "hatchR")
path_wi <- system.file("extdata/woody_island.csv", package = "hatchR")

## -----------------------------------------------------------------------------
crooked_river <- read_csv(path_cr)
woody_island <- read_csv(path_wi)

## ----eval=FALSE---------------------------------------------------------------
# library(tibble)

## -----------------------------------------------------------------------------
glimpse(crooked_river)
glimpse(woody_island)

## ----eval=FALSE---------------------------------------------------------------
# library(readr)
# library(tibble)
# your_data <- read_csv("data/your_data.csv")
# glimpse(your_data)

## -----------------------------------------------------------------------------
crooked_river <- read.csv(path_cr)
woody_island <- read.csv(path_wi)

glimpse(crooked_river) # note date column imported as a character (<chr>)
glimpse(woody_island) # note date column imported as a character (<chr>)

## -----------------------------------------------------------------------------
# if your date is in the form "2000-09-01 12:00:00"
crooked_river$date <- ymd_hms(crooked_river$date)

# if your date is in the form "2000-09-01"
woody_island$date <- mdy(woody_island$date) 

glimpse(crooked_river)
glimpse(woody_island)

## ----eval = FALSE-------------------------------------------------------------
# library(hatchR)

## -----------------------------------------------------------------------------
plot_check_temp(data = crooked_river, 
                dates = date, 
                temperature = temp_c, 
                temp_min = 0, 
                temp_max = 12)

## -----------------------------------------------------------------------------
# set seed for reproducibility
set.seed(123)

# create vector of date-times for a year at 30 minute intervals
dates <- seq(
  from = ymd_hms("2000-01-01 00:00:00"),
  to = ymd_hms("2000-12-31 23:59:59"), 
  by = "30 min"
  )

# simulate temperature data
fake_data <- tibble(
  date = dates,
  temp = rnorm(n = length(dates), mean = 10, sd = 3) |> abs()
)

# check it
glimpse(fake_data)

## -----------------------------------------------------------------------------
fake_data_sum <- summarize_temp(data = fake_data,
                                temperature = temp,
                                dates = date)

nrow(fake_data) #17568 records
nrow(fake_data_sum) #366 records; 2000 was a leap year :)

## -----------------------------------------------------------------------------
# note we use fake_data_sum instead of fake_data
plot_check_temp(data = fake_data_sum, 
                dates = date, 
                temperature = daily_temp, 
                temp_min = 5,
                temp_max = 15)

## ----warning=TRUE, message=TRUE-----------------------------------------------
check_continuous(data = crooked_river, dates = date)
check_continuous(data = woody_island, dates = date)

## ----warning=TRUE, message=TRUE-----------------------------------------------
check_continuous(data = crooked_river[-5,], dates = date)

