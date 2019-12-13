
library(tidyverse)
library(Hmisc)
library(readr)

crime <- read_csv("workshop_2/r_scripts/data_files/crime_dataset.csv")
crime <- separate(crime, 'City, State', into=c("City", "State"))
colnames(crime)[2] <- "House_Price"
colnames(crime)[6] <- "Violent_Crimes"
head(crime)

crime %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm")

rcorr(crime$Population, crime$Violent_Crimes)

#Now look at rate of violent crime without influence of cities with populations over 2 mil

crime_filtered <- filter(crime, Population <2000000)
crime_filtered %>%
  ggplot(aes(x= Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm")

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

#Looking at rate of Violent Crimes with a Focus on year 2015

crime_filtered1 <- filter(crime_filtered, Year == 2015)
crime_filtered1 %>%
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) +
  geom_point() + 
  geom_text(nudge_y = 500, check_overlap= TRUE) +
  geom_smooth(method = "lm") +
  xlim(0, 1800000)

rcorr(crime_filtered1$Population, crime_filtered1$Violent_Crimes)


#Work out how violent crime rate is predicted by population size 
#Model 1 uses mean of outcome variable as predictor
#Model 2 uses population size to predict violent crimes outcome

model1 <- lm(Violent_Crimes ~1, data = crime_filtered1)
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered1)

anova(model1, model2)

summary(model1)
summary(model2)


#Check whether same relationship holds for population size and robberies in 2015

crime_filtered2 <- filter(crime_filtered, Year == 2015)
crime_filtered2 %>%
  ggplot(aes(x= Population, y = Robberies, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap= TRUE) +
  geom_smooth(method = "lm") +
  xlim(0, 1800000)

rcorr(crime_filtered2$Population, crime_filtered2$Robberies)

model3 <- lm(Robberies ~1, data = crime_filtered2)
model4 <- lm(Robberies ~ Population, data = crime_filtered2)

anova(model3, model4)

summary(model3)
summary(model4)


#Are house prices predicted by the number of violent crimes in 2015?

model5 <- lm(House_Price ~1, data = crime_filtered2)
model6 <- lm(House_Price ~ Violent_Crimes, data = crime_filtered2)

anova(model5, model6)

summary(model5)
summary(model6)

#Are house prices predicted by population size in 2015?

model7 <- lm(House_Price ~1, data = crime_filtered2)
model8 <- lm(House_Price ~ Population, data = crime_filtered2)

anova(model7, model8)

summary(model8)
#To calculate prediction, multiply predictor and add intercept
#For a city with a population of about a million, house prices will be about 227.64
# (0.00002314) * 1,000,000 + intercept (204.5)

