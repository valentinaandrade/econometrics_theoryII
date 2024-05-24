
# Code 01: Process data ---------------------------------------------------

# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data ------------------------------------------------------------
data <- haven::read_dta(file = "01input/Ayudantia 6.dta")
data2 <- haven::read_dta(file = "01input/Ayudantia6v12.dta")
# 3. Explore --------------------------------------------------------------


