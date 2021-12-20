# Preprocessing 1

#Q1
library(MASS)
View(UScereal) 
?UScereal 
more.sugar <- filter(UScereal, sugars >= mean(sugars)) 
less.sugar <- filter(UScereal, sugars < mean(sugars))
mean(more.sugar$sugars) 
mean(less.sugar$sugars) 
shelf1 <- filter(UScereal, shelf == 1)
shelf3 <- filter(UScereal, shelf == 3) 
mean(shelf3$fat) 
mean(shelf1$fat) 

v.cereal <- filter(UScereal, vitamins == "enriched" | vitamins == "100%")
mean(v.cereal$calories)
v.cereal <- filter(UScereal, vitamins %in% c("enriched", "100%")) 
mean(v.cereal$calories)

#Q2
install.packages('dplyr')
library('dplyr')
head(Animals)
View(Animals)
?Animals
MASS::Animals  
str(Animals)

Animals<-mutate(Animals, ratio = brain/(body * 1000)*100 )
View(Animals)
mean(Animals$brain)

Animals<-mutate(Animals, size = ifelse(brain > mean (brain), 'big', 'small'))
View (Animals) #new column added


#Q3
library(ggplot2)
library(dplyr)
mpg
mpg %>%
  group_by(manufacturer) %>% # seperate by manufacturer
  filter(class == "suv") %>% # extract suv 
  mutate(total = (cty+hwy)/2) %>% # derivated column
  summarise(mean_total = mean(total)) %>% # average
  arrange(desc(mean_total)) %>% # order desc, top 5
  head(5)

#Q4
?MASS :: Cars93
Cars93
df<- select(Cars93, Manufacturer, Model, MPG.highway)
head(df)

df <- MASS::Cars93 %>% 
  select(Manufacturer, Model, MPG.highway) 
head(df)

df_che <- df %>% 
  filter(Manufacturer == "Chevrolet")
head(df_che)

arrange(df_che, desc(MPG.highway))
head(arrange(df_che, desc(MPG.highway)),5)

# Do it at once
MASS::Cars93 %>%
  select(Manufacturer, Model, MPG.highway) %>%
  filter(Manufacturer == 'Chevrolet') %>%
  arrange(desc(MPG.highway)) %>%
  head(5)



#Q5
MASS::UScereal %>% 
  group_by(mfr) %>% 
  summarise(mean(calories))%>%
  head(6)

MASS::UScereal %>% 
  group_by(mfr) %>% 
  summarise( mean_cal = mean(calories)) %>% 
  arrange(desc(mean_cal))%>%
  head(3)

UScereal %>%
  filter(vitamins == "enriched") %>% 
  group_by(mfr) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  head(3)

#Q6

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty))
  
  
ggplot2 :: mpg%>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty))
  

  
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(3)

mpg %>% filter(class == "compact") %>% # extract "compact" 
  group_by(manufacturer) %>% # by manufacturer 
  summarise(count = n()) %>% # get frequency 
  arrange(desc(count))

