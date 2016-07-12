growth <- read.csv("./growth.csv")

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