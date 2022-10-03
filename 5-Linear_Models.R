#C.M. Gienger
#regression and ANOVA examples

library(dplyr)
library(ggplot2)
library(ggfortify)
library(emmeans)
library(multcomp)
library(multcompView)

plant_gr <- read.csv("plant.growth.rate.csv")
glimpse(plant_gr)

#as always, make a plot first
ggplot(plant_gr,
       aes(x = soil.moisture.content, y = plant.growth.rate)) +
  geom_point() +
  ylab("Plant Growth Rate (mm/week)") +
  theme_bw()

#fit the model; notice y variable define first, "as function of..."
model_pgr <- lm(plant.growth.rate ~ soil.moisture.content, data = plant_gr)

#check the assumptions
autoplot(model_pgr, smooth.colour = NA)

#interpret the model
anova(model_pgr) #does not perform ANOVA
summary(model_pgr)

#use coefficients to predict new values of plant growth rate
new <- data.frame(soil.moisture.content=c(1.75)) #new value of soil.moisture.content
predict.lm(model_pgr, new)

#add model prediction from coefficients (fit line) to the graph 
ggplot(plant_gr, aes(x = soil.moisture.content,
                     y = plant.growth.rate)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylab("Plant Growth Rate (mm/week)") +
  theme_bw()

#One-Way Analysis of Variance (ANOVA)

daphnia <- read.csv("Daphniagrowth.csv")
glimpse(daphnia)

#first we plot the data; boxplots are good for categorical predictors
ggplot(daphnia, aes(x = parasite, y = growth.rate)) +
  geom_boxplot() +
  theme_bw()

#flip axes so labels are not smashed
ggplot(daphnia, aes(x = parasite, y = growth.rate)) +
  geom_boxplot() +
  theme_bw() +
  coord_flip()

#construct linear model-check assumptions
model_grow <- lm(growth.rate ~ parasite, data = daphnia)

autoplot(model_grow, smooth.colour = NA)

#evaluate the linear model
anova(model_grow)

#treatment contrasts (pairwise comparisons)
summary(model_grow)

#recall calculating means using dplyr
# get the mean growth rates
sumDat<-daphnia %>%
  group_by(parasite) %>%
  summarise(meanGR = mean(growth.rate))

sumDat

#Multiple Contrasts using EMmeans package
emm_model_grow<- emmeans(model_grow, "parasite")
pairs(emm_model_grow)

#visually compare marginal means (LS means)
plot(emm_model_grow, comparisons = TRUE)+
  coord_flip()

#connecting letters differences report
multcomp::cld(emm_model_grow)
