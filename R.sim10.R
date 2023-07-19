library (tidyverse)
library (ggplot2)
library(tidyvpc)

setwd("C:/NM75/pract_sim/scenerio3")

data<- read.table("sim10")
colnames (data) <- c ("ID","TIME","DOSE","ADDL","II","DV","MDV")

#ID 1-10
nsub <- 10
data$ID <- rep(c(1:nsub), each = nrow(data)/nsub)

#ID change to factor
simplot3 <- data |>
  mutate(ID=as.factor(ID)) |>
  filter(MDV != 1)

#concentration-time curve
ggplot(simplot3,aes(x=TIME, y=DV, color=ID)) +
  geom_line(alpha=1) +
  scale_x_continuous (limits = c (0,144),breaks = seq (0,144,24))+
  labs(x = "Time (h)", y = " Concentration(ng/ml)",
       col = "ID",
       title = "Simulation 10 Concentration-Time Curves") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA))


#ID change to factor
vpc <- observed(simplot3, x = TIME, y = DV)|>
  simulated(simplot3, y = DV) |>
  binless()|>
  vpcstats()

plot(vpc)



