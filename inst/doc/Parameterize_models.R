## ----include = FALSE----------------------------------------------------------
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
library(tibble)
library(ggplot2)
library(lubridate)
library(dplyr)

## ----eval = FALSE-------------------------------------------------------------
# library(hatchR)
# library(tibble)
# library(ggplot2)

## -----------------------------------------------------------------------------
model_table

## -----------------------------------------------------------------------------
sockeye_hatch_mod <- model_select(
  author = "Beacham and Murray 1990", 
  species = "sockeye", 
  model = 2, 
  development_type = "hatch"
  )

sockeye_hatch_mod

## ----eval=FALSE---------------------------------------------------------------
# View(model_table)

## -----------------------------------------------------------------------------
bt_data <- tibble(
  temperature = c(2,5,8,11,14), 
  days_to_hatch = c(194,87,54,35,28)
  )
bt_data

## -----------------------------------------------------------------------------
bt_data |> 
  ggplot(aes(x = temperature, y = days_to_hatch)) +
  geom_point() +
  theme_classic()

## -----------------------------------------------------------------------------
bt_fit <- fit_model(temp = bt_data$temperature, 
                    days = bt_data$days_to_hatch, 
                    species = "brown_trout", 
                    development_type = "hatch")

## -----------------------------------------------------------------------------
bt_fit

## -----------------------------------------------------------------------------
bt_hatch_exp <- bt_fit$expression
bt_hatch_exp

## -----------------------------------------------------------------------------
###  make temp regime
set.seed(123)
# create random temps and corresponding dates
temps_sim <- sort(rnorm(n =30, mean = 16, sd = 1), decreasing = FALSE)
dates_sim <-  seq(from = ymd("2000-07-01"),
             to = ymd("2000-07-31"), length.out = 30)

data_sim <- matrix(NA, 30, 2) |> data.frame()
data_sim[,1] <- temps_sim
data_sim[,2] <- dates_sim

# change names so they aren't the same as the vector objects
colnames(data_sim) <- c("temp_sim", "date_sim")

## ----echo = TRUE--------------------------------------------------------------

### smallmouth mod
smallmouth <- matrix(NA, 10, 2) |> data.frame()
colnames(smallmouth) <- c("hours", "temp_F")
smallmouth$hours <- c(52, 54, 70, 78, 90, 98, 150, 167, 238, 234)
smallmouth$temp_F <- c(77, 75, 71, 70, 67, 65, 60, 59, 55, 55)

# change °F to °C and hours to days
smallmouth <- smallmouth |> 
  mutate(days = ceiling(hours/24),
         temp_C = (temp_F -32) * (5/9))

# model object for smallmouth bass 
smb_mod <- fit_model(temp = smallmouth$temp_C,
                     days = smallmouth$days,
                     species = "smb",
                     development_type = "hatch")

### catfish mod
catfish <- matrix(NA, 3, 2) |> data.frame()
colnames(catfish) <- c("days", "temp_C")
catfish$days <- c(16,21,26)
catfish$temp_C <- c(22,10,7)

cat_mod <- fit_model(temp = catfish$temp_C,
                     days = catfish$days,
                     species = "catfish",
                     development_type = "hatch")

### lake sturgeon mod
sturgeon <-  matrix(NA, 7, 2) |> data.frame()
colnames(sturgeon) <- c("days", "CTU")
sturgeon$days <- c(7,5,6,6,5,11,7)
sturgeon$CTU <- c(58.1, 62.2, 61.1, 57.5, 58.1, 71.4, 54.7)

sturgeon <- sturgeon |> 
  mutate(temp_C = CTU/days) # change CTUs to average temp and add column

sturgeon_mod <- fit_model(days = sturgeon$days,
                          temp = sturgeon$temp_C,
                          species = "sturgeon",
                          development_type = "hatch")




## ----echo = TRUE--------------------------------------------------------------
#model fits
smb_mod$r_squared; cat_mod$r_squared; sturgeon_mod$r_squared


## -----------------------------------------------------------------------------
### predict_phenology

#smallmouth bass
smb_res <- predict_phenology(data = data_sim,
                  dates = date_sim,
                  temperature = temp_sim,
                  spawn.date = "2000-07-01",
                  model = smb_mod$expression)

# catfish
catfish_res <- predict_phenology(data = data_sim,
                  dates = date_sim,
                  temperature = temp_sim,
                  spawn.date = "2000-07-01",
                  model = cat_mod$expression)

# sturgeon
# note that 16 C is pretty far out of range of temps for model fit, not best practice
sturgeon_res <- predict_phenology(data = data_sim,
                  dates = date_sim,
                  temperature = temp_sim,
                  spawn.date = "2000-07-01",
                  model = sturgeon_mod$expression)


## -----------------------------------------------------------------------------
# summary for all species
all_res <- data.frame(matrix(NA, 3, 2))
colnames(all_res) <- c("start", "stop")

all_res$start <- c(catfish_res$dev_period$start, 
                   smb_res$dev_period$start, 
                   sturgeon_res$dev_period$start)

all_res$stop <- c(catfish_res$dev_period$stop,
                  smb_res$dev_period$stop, 
                  sturgeon_res$dev_period$stop)


all_res <- all_res |> 
  mutate(days = ceiling(stop-start),
         index = c(17,16.5,16)) # index for our horizontal bars

all_res$Species <- c("Channel Catfish", "Smallmouth Bass", "Lake Sturgeon")


## -----------------------------------------------------------------------------
ggplot() +
  geom_point(data = data_sim, aes(x = date_sim, y = temp_sim )) + 
  geom_line(data = data_sim, aes(x = date_sim, y = temp_sim )) +
  geom_rect(data = all_res, aes(xmin = start, xmax = stop, ymax =index-.35, ymin = index-.5, fill = Species)) +
  geom_label(data = all_res, aes(x = start + (stop - start) / 1.25, y = (index -0.425), label = days)) +
  labs(x = "Date", y = "Temperature (°C)") +
  scale_fill_manual(values = c("deepskyblue4", "grey23", "darkolivegreen4")) +
  theme_classic() +
  theme(legend.position = c(0.75, 0.25))

