#### Data prep
#### JB July 10 2016


library(googlesheets)
library(ggplot2)
library(metafor)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)

growth <- read.csv("./growth.csv")

#meta data file
temperature <- read.csv("./Met_seagrass_temperature.csv", fileEncoding = "UCS-2LE")

temperature_red <- temperature[,colSums(is.na(temperature))<nrow(temperature)]
data <- select(temperature, Obs_ID, Paper_Num, Species..seagrass., Ontogeny, Treatment.type_CO2, Treatment.type_N, Treatment.type_Temp, Treatment.type_HW.hours, Treatment.type_recovery, treatment.type_Light, Treatment.type_herbivory_removal_percent, duration.days)

## bring in Joey's output file
data.clean <- read.csv("./renamed_all_variables.csv")
data1 <- merge(data, data.clean, by = "Obs_ID")
data1$group <- paste(data1$Obs_ID, data1$Paper_Num.x, data1$Species..seagrass., data1$Ontogeny, data1$Treatment.type_CO2, data1$Treatment.type_N, data1$Treatment.type_Temp, data1$Treatment.type_HW.hours, data1$Treatment.type_recovery, data1$treatment.type_Light, data1$Treatment.type_herbivory_removal_percent, data1$duration.days, sep = '.')


data1 %>% 
	unite(group, Obs_ID, Paper_Num.x, Species..seagrass., Ontogeny, Treatment.type_CO2, Treatment.type_N, Treatment.type_Temp, Treatment.type_HW.hours, Treatment.type_recovery, treatment.type_Light, Treatment.type_herbivory_removal_percent, duration.days) %>% View



write.csv(data1, "merged.data.csv")

## have to spread it before we can calculate effect sizes. 
	
effect_sizes <- 	escalc(Growth_Mean/Growth_SD ~ factor(Treatment_designation) | factor(group),
												data = data1,
												weights = Growth_n,
												measure = "ROM", 
												slab = group)





### begin the long pipe of data manipulations!!

growth_rep <- growth %>% 
	filter(Treatment_designation == "Control" | Treatment_designation == "Treatment") %>%
	select(Paper_Num, Obs_ID, Treatment_designation, starts_with("Growth")) %>%
	gather(., "response_type", "value", starts_with("Growth")) %>%
	group_by(Obs_ID) %>%
	mutate(response_type = str_replace(response_type, "Growth_Mean.*", "Growth_Mean")) %>%
	mutate(response_type = str_replace(response_type, "Growth_SE.*", "Growth_SE")) %>%
	mutate(response_type = str_replace(response_type, "Growth_SD.*", "Growth_SD")) %>%
	mutate(response_type = str_replace(response_type, "Growth_n.*", "Growth_n")) %>%
	mutate(response_type = str_replace(response_type, "Growth_plantpart_units.*", "Growth_plantpart_units")) %>%
	filter(response_type == "Growth_Mean" | response_type == "Growth_n" | response_type == "Growth_SD") %>%
	mutate(value = as.numeric(value)) %>% 
	filter(!is.na(value)) %>% 
	as.data.frame() %>%
	mutate(response_type = as.factor(response_type)) %>%
	mutate(Paper_Num = as.factor(Paper_Num)) %>% 
	group_by(Paper_Num, Treatment_designation, response_type) %>%
	dplyr::summarise(mean = mean(value)) %>%
	spread(., response_type, mean, fill = NA)


effect_sizes <- 	escalc(Growth_Mean/Growth_SD ~ factor(Treatment_designation) | factor(Paper_Num),
												data = growth_rep,
												weights = Growth_n,
												measure = "ROM", 
												slab = Paper_Num)

effect_sizes$study_number <- unique(growth_rep$Paper_Num)


ggplot(data = effect_sizes, aes(y = yi, x = study_number)) + geom_point(size = 2) +
	geom_hline(yintercept = 0) +
	geom_errorbar(aes(ymin = yi - vi, ymax = yi + vi), width = 0.1) + ggtitle("Growth responses") +
	ylab("log response ratio") + xlab("study number")

#### code from tuesday afternoon: 


