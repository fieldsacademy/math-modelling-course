# Code for basic linear regression examples
# author: Emma Kroell
# data source: Palmer penguins data set https://allisonhorst.github.io/palmerpenguins/

#-------------------------------------------------------------------------------
# SET-UP
# if this is your first time using either package, uncomment and run:
# install.packages("palmerpenguins")
# install.packages("tidyverse")
# libraries
library(palmerpenguins)
library(tidyverse)

#-------------------------------------------------------------------------------
# EXPLORE THE DATA
# check-out data
penguins

# uncomment to save as a csv to use in excel
# write_csv(penguins, file="penguins.csv")

# types of data
#numeric
ggplot(penguins,aes(species,bill_depth_mm)) + geom_violin(aes(fill=species)) +
  geom_boxplot(width=0.1)
ggplot(penguins,aes(species,bill_depth_mm)) +
  geom_boxplot()
ggplot(penguins,aes(species,fill=species)) + geom_bar()


#-------------------------------------------------------------------------------
# LINEAR REGRESSION
# linear example
ggplot(penguins,aes(x=body_mass_g,y=flipper_length_mm)) +
  geom_point(size=2.5) + xlab("Penguins' body mass (g)") +
  ylab("Penguins' flipper length (mm)") + theme_bw(base_size = 16)

my_model <- lm(flipper_length_mm ~ body_mass_g,data=penguins)
summary(my_model)

ggplot(penguins,aes(x=body_mass_g,y=flipper_length_mm))+
  geom_point(size=2.5)+
  geom_smooth(method='lm', formula= y~x,colour="black") +
  xlab("Penguins' body mass (g)") +
  ylab("Penguins' flipper length (mm)") + theme_bw(base_size = 16)

# good example with two groups
penguins %>% ggplot(aes(x=body_mass_g,y=bill_depth_mm))+
  geom_point(size=2.5) +
  xlab("Penguins' body mass (g)") +
  ylab("Penguins' bill depth (mm)") + theme_bw(base_size = 16)

penguins %>% ggplot(aes(x=body_mass_g,y=bill_depth_mm))+
  geom_point(size=2.5) + geom_smooth(method='lm', formula= y~x,colour="black")+
  xlab("Penguins' body mass (g)") +
  ylab("Penguins' bill depth (mm)") + theme_bw(base_size = 16)
#Uh oh!

my_bad_model <- lm(bill_depth_mm ~ body_mass_g,data=penguins_mod)
summary(my_bad_model)

penguins_mod <- penguins %>% 
  mutate(Gentoo = as.factor(ifelse(species == "Gentoo","yes","no")))
my_model2 <- lm(bill_depth_mm ~ body_mass_g + Gentoo,data=penguins_mod)
summary(my_model2)
penguins_mod %>% drop_na() %>%
  ggplot(aes(x=body_mass_g,y=bill_depth_mm,colour=Gentoo))+
  geom_point(size=2) + geom_smooth(method='lm', formula= y~x) +
  xlab("Penguins' body mass (g)") +
  ylab("Penguins' bill depth (mm)") + theme_bw(base_size = 16)


# more covariates
penguins %>% drop_na() %>%
  ggplot(aes(x=body_mass_g,y=flipper_length_mm,colour=species,shape=sex)) +
  geom_point() + xlab("Penguins' body mass (g)") +
  ylab("Penguins' flipper length (mm)") + theme_bw(base_size = 16)
my_model2 <- lm(flipper_length_mm ~ body_mass_g + sex + species,data=penguins)
summary(my_model2)



