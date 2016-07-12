#### Data prep
#### JB July 10 2016


library(googlesheets)
library(dplyr)
library(ggplot2)
library(metafor)
library(tidyr)
library(plyr)
library(dplyr)


## import data
seagrass <- gs_title("Met_seagrass_pCO2_streamlined")

## read in correct sheet
data.raw <- seagrass %>% 
	gs_read(ws = "temperature")


names(data.raw)

### for now, remove duplicated columns
data <- data.raw[ , !duplicated(colnames(data))]

data.processed <- data.raw %>% 
	unite(., all_growth_mean, starts_with("Growth_Mean")) %>% View

names(data.processed)

data.processed %>% 
	select(Obs_ID, all_growth_mean) %>% View


names(data)
### Pmax, NPP, ETRmax

## get an initial subset
sample.raw <- data %>% 
	select(Obs_ID, Temperature_Treatment, starts_with("Pmax"), starts_with("NPP"), starts_with("NPP")) %>%
	as.data.frame() %>% 
	
	## get the columns in the right formats
	sample <- sample.raw %>% 
	filter(!is.na(Temperature_Treatment)) %>%
	mutate(Pmax_mean = as.numeric(Pmax_mean),
				 Pmax_SE = as.numeric(Pmax_SE),
				 Pmax_std = as.numeric(Pmax_std),
				 Temperature_Treatment = as.factor(Temperature_Treatment),
				 Pmax_unit = as.factor(Pmax_unit))


### recode the typos in control
sample$Temperature_Treatment <- as.character(sample$Temperature_Treatment)
sample$Temperature_Treatment[sample$Temperature_Treatment == "Control"] <- "control"
sample$Temperature_Treatment[sample$Temperature_Treatment == "ambient"] <- "control"



sample %>% 
	plyr::revalue(Temperature_Treatment, c(Control = "control", ambient = "control")) %>% View



## plot it
sample %>% 
	filter(!is.na(Pmax_mean)) %>% 
	ggplot(data =., aes(y = log(Pmax_mean), x = Temperature_Treatment, group = Pmax_unit, color = Pmax_unit)) + geom_point()


#### playing around with multiple growth columns

growth <- read.csv("./growth.csv")

levels(growth$Treatment_designation)
levels(growth$Treatment_designation)[levels(growth$Treatment_designation)== "Control "] <- "Control"

growth %>% 
	select(Obs_ID, Treatment_designation, Growth_Mean, Growth_plantpart_units, Growth_n, Growth_SD) %>%
	gather(., "response_type", "value", starts_with("Growth")) %>%
	group_by(Obs_ID) %>% 
	spread(., response_type, value, fill = NA)


growth %>% 
	mutate(Paper_Num = as.factor(Paper_Num)) %>% 
	select(Obs_ID, Paper_Num, Treatment_designation, Growth_Mean, Growth_plantpart_units, Growth_n, Growth_SD) %>% 
	filter(!is.na(Growth_Mean)) %>% 
	group_by(Paper_Num, Treatment_designation) %>% View
summarise(mean_growth = mean(Growth_Mean)) %>%
	spread(., Treatment_designation, mean_growth) %>%
	group_by(Paper_Num) %>% 
	mutate(response_ratio = log(Treatment/Control)) %>% 
	ggplot(data = ., aes(x = Paper_Num, y = response_ratio)) + geom_point(size = 4) +
	geom_hline(yintercept = 0)




growth_pro <- growth %>% 
	select(Obs_ID, Paper_Num, Treatment_designation, starts_with("Growth")) %>% 
	gather(., "response_type", "value", starts_with("Growth")) %>% 
	mutate(value = as.numeric(value)) %>% 
	mutate(response_type = as.character(response_type))

## groups
Paper.number_species_life.stage_response.type_statistic_treatment.type1_treatment.type2_treatment.type3_time.point



unique((growth_pro$response_type))

?revalue

summary(growth_pro$value)

## changing names so all growth means are called the same thing
growth_pro_1 <- growth_pro %>% 
	mutate(response_type = revalue(response_type,
																 c(Growth_Mean.1 = "mean_growth", 
																 	Growth_Mean.2 = "mean_growth",
																 	Growth_Mean.3 = "mean_growth", 
																 	Growth_Mean.4 = "mean_growth",
																 	Growth_Mean.5 = "mean_growth",
																 	Growth_Mean.6 = "mean_growth",
																 	Growth_Mean.7 = "mean_growth",
																 	Growth_Mean.8 = "mean_growth",
																 	Growth_Mean.9 = "mean_growth",
																 	Growth_Mean.10 = "mean_growth",
																 	Growth_plantpart_units.1 = "Growth_plantpart_units",
																 	Growth_plantpart_units.2 = "Growth_plantpart_units",
																 	Growth_plantpart_units.3 = "Growth_plantpart_units", 
																 	Growth_plantpart_units.4 = "Growth_plantpart_units",
																 	Growth_plantpart_units.5 = "Growth_plantpart_units",
																 	Growth_plantpart_units.6 = "Growth_plantpart_units",
																 	Growth_plantpart_units.7 = "Growth_plantpart_units",
																 	Growth_plantpart_units.8 = "Growth_plantpart_units",
																 	Growth_plantpart_units.9 = "Growth_plantpart_units",
																 	Growth_plantpart_units.10 = "Growth_plantpart_units",
																 	Growth_SD.1 = "Growth_SD",
																 	Growth_SD.2 = "Growth_SD",
																 	Growth_SD.3 = "Growth_SD",
																 	Growth_SD.4 = "Growth_SD",
																 	Growth_SD.5 = "Growth_SD",
																 	Growth_SD.6 = "Growth_SD",
																 	Growth_SD.7 = "Growth_SD",
																 	Growth_SD.8 = "Growth_SD",
																 	Growth_SD.9 = "Growth_SD",
																 	Growth_SD.10 = "Growth_SD",
																 	Growth_SE.1 = "Growth_SE",
																 	Growth_SE.2 = "Growth_SE",
																 	Growth_SE.3 = "Growth_SE",
																 	Growth_SE.4 = "Growth_SE",
																 	Growth_SE.5 = "Growth_SE",
																 	Growth_SE.6 = "Growth_SE",
																 	Growth_SE.7 = "Growth_SE",
																 	Growth_SE.8 = "Growth_SE",
																 	Growth_SE.9 = "Growth_SE",
																 	Growth_SE.10 = "Growth_SE",
																 	Growth_n.1 = "Growth_n",
																 	Growth_n.2 = "Growth_n",
																 	Growth_n.3 = "Growth_n",
																 	Growth_n.4 = "Growth_n",
																 	Growth_n.5 = "Growth_n",
																 	Growth_n.6 = "Growth_n",
																 	Growth_n.7 = "Growth_n",
																 	Growth_n.8 = "Growth_n",
																 	Growth_n.9 = "Growth_n",
																 	Growth_n.10 = "Growth_n")))


str(growth_pro_1)

unique(growth_pro_1$response_type)
?revalue

## have to make sure that growth estimates are not decoupled from their units...


## treatments and controls have to get split

### Mary's attempt to re-integrate these column names
growth_pro_1 %>% 
	mutate(Paper_Num = as.factor(Paper_Num)) %>% 
	select(Obs_ID, Paper_Num, Treatment_designation, response_type, value) %>%
	filter(!is.na(value)) %>% 
	group_by(Obs_ID, Paper_Num, Treatment_designation) %>% 
filter(response_type == 'mean_growth') %>% View
#summarise(mean_growth = mean(value)) %>%
spread(., key = Treatment_designation, value = value, fill = NA) %>% View ## not sure why this isn't working
group_by(Paper_Num, Treatment_designation) %>% View
#summarise(mean_growth = mean(value)) %>%
spread(., Treatment_designation, mean_growth) %>%
	group_by(Paper_Num) %>% 
	mutate(response_ratio = log(Treatment/Control)) %>% 
	ggplot(data = ., aes(x = Paper_Num, y = response_ratio)) + geom_point(size = 4) +
	geom_hline(yintercept = 0)
	
	