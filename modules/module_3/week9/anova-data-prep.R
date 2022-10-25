# Data set generation for module 3 - anovas making bays
# based on code by Keaton Wilson and Ethan Davis
# modified by Mila Pruiett


# packages
library(tidyverse)
library(truncnorm)
library(faux)


fish_data_many_bays <- tibble(date =sort(rep(seq(as.Date("2021-03-01"), as.Date("2021-04-01"), by="days"), 60)), 
                    net = rep(1:5, 384),
                    time = rep(c(rep("AM", 5), rep("PM",5)), 192),
                    bay = rep(c(rep("Wilhelmenia", 10), rep("Marguarite", 10), rep("Emporer", 10), 
                                rep("Sulzberger", 10), rep("Iceberg", 10), rep("Hope", 10)), 32),
                    num_fish = ifelse(bay == "Wilhelmenia", round(rtruncnorm(n = 320, a=0, mean = 4, sd = 2)),
                                      ifelse(bay == "Marguarite", round(rtruncnorm(n = 320, a=0, mean = 4.05, sd = 2)), 
                                             ifelse(bay == "Emporer", round(rtruncnorm(n = 320, a=0, mean = 2.5, sd = 1)),
                                                    ifelse(bay == "Sulzberger", round(rtruncnorm(n = 320, a=0, mean = 5, sd = 3.2)),
                                                           ifelse(bay == "Iceberg", round(rtruncnorm(n = 320, a=0, mean = 4.03, sd = 1.4)),
                                                                  round(rtruncnorm(n = 320, a=0, mean = 4.1, sd = 1.5))))))))
                                                                 
                                  

# check if it looks like what we want
fish_data_many_bays %>% 
  ggplot(aes(x=bay, y=num_fish, fill = bay)) +
  geom_boxplot() 


seal_data_many_bays <- tibble(date = sort(rep(seq(as.Date("2021-03-01"), as.Date("2021-04-01"), by="days"), 60)),
                                time = rep(c(rep("AM", 5), rep("PM",5)), 192),
                                area = rep(1:5, 384),
                              bay = rep(c(rep("Wilhelmenia", 10), rep("Marguarite", 10), rep("Emporer", 10), 
                                          rep("Sulzberger", 10), rep("Iceberg", 10), rep("Hope", 10)), 32),
                                num_seals = ifelse(bay == "Wilhelmenia" & time == "AM", round(rtruncnorm(n = 160, a=0, mean = 6.2, sd = 2)),
                                                   ifelse(bay == "Wilhelmenia" & time == "PM", round(rtruncnorm(n = 160, a=0, mean = 5.5, sd = 2)),
                                                          ifelse(bay == "Marguarite" & time == "AM", round(rtruncnorm(n = 160, a=0, mean = 5.9, sd = 2)),
                                                                 ifelse(bay == "Emporer", round(rtruncnorm(n = 160, a=0, mean =  1.7, sd = .7)),
                                                                        ifelse(bay == "Sulzberger", round(rtruncnorm(n = 160, a=0, mean = 1.2, sd = .8)),
                                                                               ifelse(bay == "Iceberg", round(rtruncnorm(n = 160, a=0, mean = 6.2, sd = 2.2)),
                                                                                      ifelse(bay == "Hope", round(rtruncnorm(n = 160, a=0, mean = 10, sd = 1.2)), 
                                                                                                    round(rtruncnorm(n = 160, a=0, mean = 4.8, sd = 2))))))))))

# check if it looks like what we want
seal_data_many_bays %>% 
  ggplot(aes(x=bay, y=num_seals, fill = bay)) +
  geom_boxplot() 

# anova
summary(aov(data = seal_data_many_bays, num_seals ~ bay))
TukeyHSD(aov(data = seal_data_many_bays, num_seals ~ bay))
                                
write_csv(fish_data_many_bays, "antarctic_fish_many_bays.csv")
write_csv(seal_data_many_bays, "antarctic_seals_many_bays.csv")

