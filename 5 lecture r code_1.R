#C.M. Gienger
#Intro Stats;X^2 and t-test
#Note error between GardenOzone.csv and ozone.csv input files
#Known issue

library(dplyr)
library(ggplot2)
library(emmeans)

lady <- read.csv("ladybirds_morph_colour.csv")

# Check it out
glimpse(lady)

#organize data for plotting/analysis

totals <- lady %>%
  group_by(Habitat, morph_colour) %>%
  summarise(total.number = sum(number))

View(totals)

ggplot(totals, aes(x = Habitat, y = total.number,
                   fill = morph_colour)) +
  geom_bar(stat = 'identity', position = 'dodge')


#Change the bar colors
ggplot(totals, aes(x = Habitat, y = total.number,
                   fill = morph_colour)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c(black = "black", red = "red"))

#Making the χ2 Test
totals

lady.mat <- xtabs(number ~ Habitat + morph_colour,
                  data = lady)
lady.mat

chisq.test(lady.mat)

#Two-sample t-test
ozone <- read.csv("ozone.csv")
glimpse(ozone)

ggplot(ozone, aes(x = Ozone)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ Garden.location, ncol = 1) +
  theme_bw()

# Do a t.test now....
t.test(Ozone ~ Garden.location, data = ozone)
