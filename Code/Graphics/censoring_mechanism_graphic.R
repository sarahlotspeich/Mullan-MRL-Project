
# This script generates a plot to explain the censoring mechanism in the simulations.

set.seed(12623) #day i first wrote the plot
library(tidyverse)

x <- rweibull(n = 500, shape = 2, scale = 1.2) #to be censored covariate
e <- rnorm(n = n, mean = 0, sd = 1) #random error
y <- 1 + 0.5*x + e #continuous outcome
c <- rweibull(n = 500, shape = 5, scale = 2)
d <- as.numeric(x <= c) 


toy <- data.frame(x,y,c,d)

1 - mean(toy$d) #10% censored

toy %>% ggplot() +
  geom_density(aes(x = x), 
               fill = "#006ec9", 
               alpha = 0.5) +
  geom_density(aes(x = c), 
               fill = "yellow", 
               alpha = 0.5) +
  geom_text(x = 0.8, y = 0.375, 
            label="Observed X",
            color = "blue") +
  geom_text(x = 1.85, y = 0.5,
            label = "Extraneous",
            color = "#9f7a06") + 
  geom_text(x = 1.8, y = 0.468,
            label = "Info",
            color = "#9f7a06") + 
  geom_text(x = 1.6, y = 0.14,
            label = "Censored X",
            color = "darkgreen") + 
  theme_bw() +
  labs(title = "Censoring Mechanism in a Simulation",
       subtitle = "Light Censoring (10%)",
       x = "X",
       y = "")
 
