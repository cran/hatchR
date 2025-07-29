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

## ----setup--------------------------------------------------------------------
#libraries
library(hatchR)
library(patchwork)
library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)

## ----tailed-frog-model--------------------------------------------------------
### parameterize mod
ascaphus_data <- tibble(temp = c(7.6,9.8,11,13,14.5,15,18),
                        days = c(44,27.1,22.6,16.1,13.4,12.7,10.7))

ascaphus_mod <- fit_model(temp = ascaphus_data$temp,
                          days = ascaphus_data$days,
                          species = "ascaphus",
                          development_type = "hatch")


## ----tailed-frog-efs----------------------------------------------------------
### get effective values
temps <- c(6:20) # daily temps

#loop to calculate model expression at different temps
ef_vals <- NULL
for (x in temps) {
  ef <- eval(parse(text = ascaphus_mod$expression$expression)) # call model expression
  ef_vals <- rbind(ef_vals, ef)
}

# make data into plotable format
ascaphus_ef <- matrix(NA, 15, 2) |> tibble::tibble()
colnames(ascaphus_ef) <- c("temp", "ef")
ascaphus_ef$temp <- temps
ascaphus_ef$ef <- ef_vals[, 1]



## -----------------------------------------------------------------------------
### plot

fmt <- "~R^2 ==  %.4f" # format for R^2 val

lab1 <- sprintf(fmt, ascaphus_mod$r_squared) # R^2 label

# plot 1 of model fit
p1 <- ascaphus_mod$pred_plot +
  labs(x = "Incubation Temperature (°C)", y = "Days to Hatch") +
  lims(y = c(0, 50)) +
  annotate("text", x = 10, y = c(35), label = c(lab1),  hjust = 0, parse = TRUE)

# data table for 1 degree increase of temp for 0.01 increase in effective value for reference
data_1 <- tibble(t = c(0:20), e = seq(0, 0.20, by = 0.01))

#plot 2
p2 <- ascaphus_ef |>
  ggplot() +
  geom_point(aes(x = temp, y = ef)) +
  geom_line(aes(x = temp, y = ef)) +
  geom_line(data = data_1, aes(x = t, y = e), linetype = "dashed") +
  # geom_abline(intercept = 0, slope = .01, linetype = "dashed") +
  labs(x = "Daily Average Temperature (°C)", y = "Effective Value") +
  theme_classic()

p1 + p2 


## ----beetle-setup-------------------------------------------------------------
# vector of experimental temps
tang_temps <- c(16, 19, 22, 24, 26, 28)

# vectors of population specific developmental rates at the above temperatures
hb <- c(24.834, 19.481, 14.172, 11.205, 9.865, 8.570)

sy <- c(23.822, 19.129, 13.644, 10.897, 9.645, 8.306)

ta <- c(21.887, 18.381, 12.984, 10.809, 9.382, 8.130)

xy <- c(21.623, 18.337, 12.589, 10.633, 9.205, 8.085)

xs <- c(21.271, 16.666, 11.797, 9.929, 9.117, 6.942)

# make a list of pops
pop_list <- list(hb, sy, ta, xy, xs)

# map fit_model() over our list of pops
beetle_mods <- pop_list |>
  map(fit_model,
    temp = tang_temps,
    species = "cabbage beetle",
    development_type = "hatch"
  ) |>
  map("expression") |>
  map("expression") |>
  unlist()




## -----------------------------------------------------------------------------
beetle_mods

## ----beetle-loop--------------------------------------------------------------

# data set up
temps <- c(12:30) # temps to iterate throug
pops <- c( # pops to iterate through
  "Haerbin City",
  "Shenyang City",
  "Taian City",
  "Xinyang County",
  "Xiushui County"
)

ef_vals_pops <- NULL # NULL object to stor ef vals in 

# loop stepping over temps and populations to create
# temperature and population specific ef values

for (m in 1:length(beetle_mods)) {
  mod <- beetle_mods[m]
  pop <- pops[m]

  for (x in temps) {
    ef <- eval(parse(text = mod))
    temp_df <- data.frame(
      temperature = x,
      effective_value = ef,
      beetle_pops = pop
    )
    ef_vals_pops <- rbind(ef_vals_pops, temp_df)
  }
}


## ----beetle-plot--------------------------------------------------------------

ef_vals_pops |>
  tibble() |>
  ggplot(aes(x = temperature, y = effective_value, color = beetle_pops)) +
  geom_line() +
  geom_point() +
  labs(x = "Daily Average Temperature (°C)", y = "Effective Value") +
  scale_color_brewer(palette = "Dark2", name = "Beetle Populations") +
  theme_classic() +
  theme(legend.position = c(0.25, 0.75))

