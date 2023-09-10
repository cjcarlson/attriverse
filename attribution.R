
library(ggplot2)
library(tidyverse)
library(MetBrewer)

cols <- met.brewer(name="Egypt", n=4, type="discrete")[c(3,1,2,4)]

df1 <- data.frame("Answer" = c("R", "Unspecified", "Not applicable"),
  "Studies" = c(5,6,2)) %>% 
  mutate(Answer = factor(Answer, levels = c("R", "Unspecified", "Not applicable"))) 

df1 %>% 
  mutate(percentage = round(Studies/sum(Studies),2)*100,
         lab.pos = 100-(cumsum(percentage)-.5*percentage)) %>%
  ggplot(aes(x = "", y = percentage, fill = Answer)) + 
  geom_bar(stat = "identity", width=1, color="white") +
  coord_polar("y", start = 0) + 
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=11.5, vjust = -1)
  ) + 
  theme(axis.text.x=element_blank()) + 
  geom_text(aes(y = rev(lab.pos), label = paste(rev(percentage),"%", sep = "")), col = "white")+
  scale_fill_manual(values = cols[c(1,3,4)]) +
  theme(legend.position = "bottom") + scale_y_reverse() + 
  ggtitle("Were programming languages used?") -> g1

##


df2 <- data.frame("Answer" = c("Yes", "No"),
                  "Studies" = c(4,9)) %>% 
  mutate(Answer = factor(Answer, levels = c("Yes","No"))) 

df2 %>% 
  mutate(percentage = round(Studies/sum(Studies),2)*100,
         lab.pos = 100-(cumsum(percentage)-.5*percentage)) %>%
  ggplot(aes(x = "", y = percentage, fill = Answer)) + 
  geom_bar(stat = "identity", width=1, color="white") +
  coord_polar("y", start = 0) + 
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=11.5, vjust = -1)
  ) + 
  theme(axis.text.x=element_blank()) + 
  geom_text(aes(y = rev(lab.pos), label = paste(rev(percentage),"%", sep = "")), col = "white")+
  scale_fill_manual(values = cols[c(1,2)]) +
  theme(legend.position = "bottom") + scale_y_reverse() + 
  ggtitle("Did the study share code?") -> g2

##


df2 <- data.frame("Answer" = c("Yes", "Unspecified"),
                  "Studies" = c(11,2)) %>% 
  mutate(Answer = factor(Answer, levels = c("Yes","No", "Unspecified"))) 

df2 %>% 
  mutate(percentage = round(Studies/sum(Studies),2)*100,
         lab.pos = 100-(cumsum(percentage)-.5*percentage)) %>%
  ggplot(aes(x = "", y = percentage, fill = Answer)) + 
  geom_bar(stat = "identity", width=1, color="white") +
  coord_polar("y", start = 0) + 
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=11.5, vjust = -1)
  ) + 
  theme(axis.text.x=element_blank()) + 
  geom_text(aes(y = rev(lab.pos), label = paste(rev(percentage),"%", sep = "")), col = "white")+
  scale_fill_manual(values = cols[c(1,3)]) +
  theme(legend.position = "bottom") + scale_y_reverse() + 
  ggtitle("Did the study reuse health data?") -> g3

##


df2 <- data.frame("Answer" = c("Yes", "No"),
                  "Studies" = c(3,10)) %>% 
  mutate(Answer = factor(Answer, levels = c("Yes","No"))) 

df2 %>% 
  mutate(percentage = round(Studies/sum(Studies),2)*100,
         lab.pos = 100-(cumsum(percentage)-.5*percentage)) %>%
  ggplot(aes(x = "", y = percentage, fill = Answer)) + 
  geom_bar(stat = "identity", width=1, color="white") +
  coord_polar("y", start = 0) + 
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=11.5, vjust = -1)
  ) + 
  theme(axis.text.x=element_blank()) + 
  geom_text(aes(y = rev(lab.pos), label = paste(rev(percentage),"%", sep = "")), col = "white")+
  scale_fill_manual(values = cols[c(1,2)]) +
  theme(legend.position = "bottom") + scale_y_reverse() + 
  ggtitle("Did the study share new health data?")  -> g4

library(patchwork)

(g3 + g4)/(g1 + g2)
