rm(list = ls())

library(ggplot2)
library(dplyr)

data.file <- "./data/lidarIntensity.csv"
data <- read.csv(data.file) %>% mutate(rel_intensity = (intensity - min(intensity))/(max(intensity) - min(intensity)),
                                       LI = case_when(load %in% c(0,1) ~ "Low",
                                                      load == 4 ~ "High",
                                                      TRUE ~ "else"))

data.select <- data %>% filter(LI %in% c("Low","High")) %>% mutate(LI = as.factor(LI))
data.select$LI <- factor(data.select$LI, levels = c("Low","High")) 

data.select %>% group_by(LI) %>% summarise(N = length(intensity))

summary(aov(data = data,formula = rel_intensity ~ as.factor(LI)))
summary(aov(data = data.select,formula = rel_intensity ~ as.factor(LI)))

ggplot(data = data.select) +
  geom_boxplot(aes(x = as.factor(LI),y = rel_intensity,
                   fill = as.factor(LI)),alpha = 0.4) +
  scale_fill_manual(values = c("#137300","#1E64C8")) +
  labs(x = "Liana infestation", y = "Relative intensity [-]") +
  guides(fill = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

ggsave(plot = last_plot(),dpi = 300, width = 12,height = 18, 
       filename = file.path("./Figures","Lidar.intensity.png"),units = "cm")
