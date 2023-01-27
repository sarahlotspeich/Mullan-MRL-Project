
# This script generates a plot to explain the censoring mechanism in the simulations.

set.seed(12623) #day i first wrote the plot
library(tidyverse)

n <- 500
x <- rweibull(n = n, shape = 2, scale = 1.2) #to be censored covariate
e <- rnorm(n = n, mean = 0, sd = 1) #random error
y <- 1 + 0.5 * x + e #continuous outcome
c <- rweibull(n = n, shape = 5, scale = 2)
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
 
# Sarah's added plot #1 (this one does the fun *pop* art)
toy %>% 
  mutate(d = factor(x = d, levels = c(0, 1), 
                    labels = c("Censored", "Uncensored"))) %>%
  ggplot(aes(x = x, y = c, col = d)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + 
  scale_color_manual(name = "Status", 
                     values = c("#006ec9", "yellow")) + 
  geom_polygon(aes(fill = d))

# Sarah's added plot #2 (this one is boring but more informative)
toy %>% 
  mutate(d = factor(x = d, levels = c(0, 1), 
                    labels = c("Censored", "Uncensored"))) %>%
  ggplot() + 
  geom_polygon(data = data.frame(x = c(0, 0, 3.5), y = c(0, 3.5, 3.5)), aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  geom_polygon(data = data.frame(x = c(0, 3.5, 3.5), y = c(0, 0, 3.5)), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  geom_point(aes(x = x, y = c, col = d)) + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + ggthemes::scale_colour_colorblind(name = "Status") 
