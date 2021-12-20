#Preprocessing 2

#Q7
table(is.na(starwars$species))
table(is.na(starwars$mass))

starwars %>% 
  filter(!is.na(species)) %>% 
  filter(!is.na(mass)) %>% 
  group_by(species) %>% 
  summarise(mean_star = mean(mass)) %>% 
  print(mean_star, n=31)

#Q8
#Average pulse of people above/below average age
colnames(survey)
old <- filter(survey, Age > mean(Age))
young <- filter(survey, Age <= mean(Age))

mean(old$Pulse, na.rm=TRUE)
mean(young$Pulse, na.rm=TRUE)


survey %>% 
  filter (Age > mean(Age)) %>% 
  summarise (old_pulse_mean = mean(Pulse, na.rm = T))
survey %>% 
  filter (Age <= mean(Age)) %>% 
  summarise (young_pulse_mean = mean(Pulse, na.rm = T))


#average height by sex
male <- filter(survey, Sex == "Male")
female <- filter(survey, Sex == "Female")
mean(male$Height, na.rm = TRUE)
mean(female$Height, na.rm = TRUE)

survey %>% 
  filter (Sex == "Female") %>% 
  summarise (female_height = mean(Height, na.rm = T)) 

survey %>% 
  filter (Sex == "Male") %>%
  summarise (male_height = mean(Height, na.rm = T))



#average age by “heavy”,”Regul”
smoker <- filter(survey, Smoke == 'Heavy' | Smoke == 'Regul')
mean(smoker$Age, na.rm = TRUE)

survey %>% 
  filter (Smoke == "Heavy" | Smoke == "Regul") %>% 
  summarise (smoke_age = mean(Age, na.rm = T))

#Q9
mpg <- as.data.frame(ggplot2::mpg) # import mpg data 
mpg[c(10, 14, 58, 93), "drv"] <- "k" # assign drv outlier
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42) #assign cty outlier

View(mpg)
table(mpg$drv)
mpg$drv <-ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)

table(mpg$drv) #check outlier

boxplot(mpg$cty)$stats
mpg$cty <-ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty) #outlier to NA
boxplot(mpg$cty) #outlier 점들 사라짐

mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% # exclude outlier
  group_by(drv) %>%                     # split by drv 
  summarise(mean_cty = mean(cty)) # average of cty


