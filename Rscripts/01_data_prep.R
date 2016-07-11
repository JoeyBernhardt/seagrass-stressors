#### Data prep
#### JB July 10 2016


library(googlesheets)
library(dplyr)
library(ggplot2)


## import data
seagrass <- gs_title("Met_seagrass_pCO2_streamlined")

## read in correct sheet
data <- seagrass %>% 
	gs_read(ws = "temperature")


## get an initial subset
sample.raw <- data %>% 
	filter(Pmax_mean != "Pmax_mean") %>%
	select(starts_with("Pmax"), Obs_ID, temperature_treatment) %>% 
	as.data.frame() 

## get the columns in the right formats
sample <- sample.raw %>% 
	filter(!is.na(temperature_treatment)) %>%
	mutate(Pmax_mean = as.numeric(Pmax_mean),
				 Pmax_SE = as.numeric(Pmax_SE),
				 Pmax_std = as.numeric(Pmax_std),
				 temperature_treatment = as.factor(temperature_treatment),
				 Pmax_unit = as.factor(Pmax_unit))

## plot it
sample %>% 
	filter(!is.na(Pmax_mean)) %>% 
	ggplot(data =., aes(y = log(Pmax_mean), x = temperature_treatment, group = Pmax_unit, color = Pmax_unit)) + geom_point()






