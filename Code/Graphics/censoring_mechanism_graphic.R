
# This script generates a plot to explain the censoring mechanism in the simulations.

set.seed(12623) #day Ashley first wrote the plot
library(tidyverse)
library(ggthemes)
library(gridExtra)

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


#Ashley's riff off of Sarah's plot #2: varied censoring mechanisms
grid_toy <- data.frame(x,y)
censor_levels <- seq(from = 0.5, to = 3, by = 0.5) #set range of scale parameters
for(i in censor_levels) {
  new_censor <- rweibull(n = n, shape = 5, scale = i)
  grid_toy <- cbind(grid_toy, new_censor)
}

colnames(grid_toy) <- c("x", "y", "c1", "c2", "c3", "c4", "c5", "c6") #to counteract my hacky way of appending
grid_toy <- grid_toy %>% #to add event indicators
  mutate(d1 = as.numeric(x <= c1),
         d2 = as.numeric(x <= c2),
         d3 = as.numeric(x <= c3),
         d4 = as.numeric(x <= c4),
         d5 = as.numeric(x <= c5),
         d6 = as.numeric(x <= c6))

grid_toy <- grid_toy %>% 
  mutate(d1 = factor(x = d1, levels = c(0, 1), 
                    labels = c("Censored", "Uncensored")),
         d2 = factor(x = d2, levels = c(0, 1), 
                     labels = c("Censored", "Uncensored")),
         d3 = factor(x = d3, levels = c(0, 1), 
                     labels = c("Censored", "Uncensored")),
         d4 = factor(x = d4, levels = c(0, 1), 
                     labels = c("Censored", "Uncensored")),
         d5 = factor(x = d5, levels = c(0, 1), 
                     labels = c("Censored", "Uncensored")),
         d6 = factor(x = d6, levels = c(0, 1), 
                     labels = c("Censored", "Uncensored")))  #to relabel event indicators
  
plot1 <- grid_toy %>% ggplot() +
  geom_polygon(data = data.frame(x = c(0, 0, 3.5), y = c(0, 3.5, 3.5)), aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  geom_polygon(data = data.frame(x = c(0, 3.5, 3.5), y = c(0, 0, 3.5)), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  geom_point(aes(x = x, y = c1, col = d1)) + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + ggthemes::scale_colour_colorblind(name = "Status") +
  labs(x = "X", y = "C", title = "Who's Getting Censored?", subtitle = "Scale = 0.5")

plot2 <- grid_toy %>% ggplot() +
  geom_polygon(data = data.frame(x = c(0, 0, 3.5), y = c(0, 3.5, 3.5)), aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  geom_polygon(data = data.frame(x = c(0, 3.5, 3.5), y = c(0, 0, 3.5)), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  geom_point(aes(x = x, y = c2, col = d2)) + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + ggthemes::scale_colour_colorblind(name = "Status") +
  labs(x = "X", y = "C", title = "Who's Getting Censored?", subtitle = "Scale = 1")

plot3 <- grid_toy %>% ggplot() +
  geom_polygon(data = data.frame(x = c(0, 0, 3.5), y = c(0, 3.5, 3.5)), aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  geom_polygon(data = data.frame(x = c(0, 3.5, 3.5), y = c(0, 0, 3.5)), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  geom_point(aes(x = x, y = c3, col = d3)) + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + ggthemes::scale_colour_colorblind(name = "Status") +
  labs(x = "X", y = "C", title = "Who's Getting Censored?", subtitle = "Scale = 1.5")

plot4 <- grid_toy %>% ggplot() +
  geom_polygon(data = data.frame(x = c(0, 0, 3.5), y = c(0, 3.5, 3.5)), aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  geom_polygon(data = data.frame(x = c(0, 3.5, 3.5), y = c(0, 0, 3.5)), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  geom_point(aes(x = x, y = c4, col = d4)) + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + ggthemes::scale_colour_colorblind(name = "Status") +
  labs(x = "X", y = "C", title = "Who's Getting Censored?", subtitle = "Scale = 2")

plot5 <- grid_toy %>% ggplot() +
  geom_polygon(data = data.frame(x = c(0, 0, 3.5), y = c(0, 3.5, 3.5)), aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  geom_polygon(data = data.frame(x = c(0, 3.5, 3.5), y = c(0, 0, 3.5)), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  geom_point(aes(x = x, y = c5, col = d5)) + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + ggthemes::scale_colour_colorblind(name = "Status") +
  labs(x = "X", y = "C", title = "Who's Getting Censored?", subtitle = "Scale = 2.5 (my original)")

plot6 <- grid_toy %>% ggplot() +
  geom_polygon(data = data.frame(x = c(0, 0, 3.5), y = c(0, 3.5, 3.5)), aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  geom_polygon(data = data.frame(x = c(0, 3.5, 3.5), y = c(0, 0, 3.5)), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  geom_point(aes(x = x, y = c6, col = d6)) + 
  geom_abline(slope = 1, linetype = 2) +
  theme_bw() + ggthemes::scale_colour_colorblind(name = "Status") +
  labs(x = "X", y = "C", title = "Who's Getting Censored?", subtitle = "Scale = 3")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, 
             nrow = 2, common.legend = TRUE) #might want to plot zoom to see it
#the takeaway: as the scale parameter of the Weibull distribution increases, less data is censored.

ggpubr::ggarrange(plotlist = list(plot1, plot2, plot3, plot4, plot5, plot6), common.legend = TRUE)
