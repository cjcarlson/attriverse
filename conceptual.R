
library(tidyverse)
library(ggplot2)
library(MetBrewer)

met.brewer("Nizami")[c(6,1)]

df <- data.frame(x = c(1:200),
           y1 = sapply(c(1:200), function(x) {sin(x+runif(1,0,3)/3) + runif(1, -0.8, 0.8)}))

df$y2 <- df$y1 + sapply(c(1:200), function(x) {runif(1, -0.8, 0.8) + ((max(x,50))^1.7)/1500})

df %>%
  pivot_longer(cols = c("y1", "y2"), names_to = "variable", values_to = "y") %>%
  ggplot(aes(x = x, y = y, group = variable, color = variable)) + 
  geom_line(lwd = 1.3) + 
  theme_classic() + 
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x=element_line(linewidth=1.3),
        axis.line.y=element_line(linewidth=1.3),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold")) + 
  scale_color_manual(values = c("#7da7ea","#dd7867")) + 
  xlab(' \n Years \n ') + 
  ylab(' \n Deaths \n  ')


#####

mean_sim <- 10
std_sim <- 5

lcb <- ((mean_sim - (3 * std_sim)) - 5)
ucb <- (((2 * mean_sim) + (3 * (2 * std_sim))) + 5)

u <- seq(from = lcb,
         to = ucb,
         length.out = 1e+5)
v1 <- dnorm(x = u,
            mean = mean_sim,
            sd = std_sim)
v2 <- dnorm(x = u,
            mean = (2 * mean_sim),
            sd = (4/5 * std_sim))

data.frame(x = u, y1 = v1, y2 = v2) %>%
  pivot_longer(cols = c("y1", "y2"), names_to = "variable", values_to = "y") %>%
  ggplot(aes(x = x, y = y, group = variable, color = variable, fill = variable)) + 
  geom_density(lwd = 1.3, stat = "identity", alpha = 0.85) + 
  xlim(-5, 35) + 
  theme_classic() + 
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x=element_line(linewidth=1.3),
        axis.line.y=element_line(linewidth=1.3),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold")) + 
  scale_color_manual(values = c("#7da7ea","#dd7867")) + 
  scale_fill_manual(values = c("#7da7ea","#dd7867")) + 
  xlab(' \n Deaths \n ') + 
  ylab(' \n Probability \n  ')


#####

x = c(1:200)
y = dnorm(x-110, sd = 20)*20 + runif(1:200,-0.3,0.9)/7
y2 = dnorm(x-110, sd = 20)*40 + runif(1:200,-0.3,0.9)/7
df <- data.frame(x = x, y = y, group = 'a')
df2 <- data.frame(x = x, y = y2, group = 'b')
df <- rbind(df, df2)
df %>%
  ggplot(aes(x = x, y = y, group = group, color = group)) + 
  geom_line(lwd = 1.3) + 
  theme_classic() + 
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x=element_line(linewidth=1.3),
        axis.line.y=element_line(linewidth=1.3),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold")) + 
  scale_color_manual(values = c("#7da7ea","#dd7867")) + 
  xlab(' \n Days \n ') + 
  ylab(' \n Deaths \n  ')  +                              
  geom_segment(aes(x = 110,
                   y = 0.55,
                   xend = 110,
                   yend = 0.7),
               arrow = arrow(length = unit(0.5, "cm")), 
               color = 'black',
               linewidth = 1.2)



#####

#install.packages(c("waffle", "extrafont"))
library(waffle)
library(extrafont)
extrafont::font_import(path="/Users/carlson/Downloads/Font-Awesome-4.7.0/fonts/", pattern = "fontawesome", prompt =  FALSE)

waffle(c(26, 16), rows = 6, use_glyph = "child", glyph_size = 18) + 
  theme(legend.position = 'none') + 
  scale_color_manual(values = c("#7da7ea","#dd7867"))
