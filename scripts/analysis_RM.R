rm(list = ls())

library(ggplot2)
library(purrr)
library(tidyr)
library(dplyr)
library(ggridges)

data.file <- file.path("/home/femeunier/Documents/projects/albedo/data/WV3refLianaCatCov_v2.rds")
# data.file <- file.path("/data/gent/vo/000/gvo00074/felicien/R/WV3refLianaCatCov.rds")
  
dataWV3 <- readRDS(data.file)

dataWV3_formatted <- dataWV3 %>% pivot_longer(-c(lianaCat,lianaCov))

dataW3_extr <- dataWV3_formatted %>% 
  mutate(Band = sub(".*\\.", "", name),
         wv = case_when(Band == 1 ~ 427.3,
                        Band == 2 ~ 477.9,
                        Band == 3 ~ 545.2,
                        Band == 4 ~ 607.8,
                        Band == 5 ~ 658.8,
                        Band == 6 ~ 723.7,
                        Band == 7 ~ 831.3,
                        Band == 8 ~ 950),
         wv_min = case_when(Band == 1 ~ 401,
                            Band == 2 ~ 447,
                            Band == 3 ~ 511,
                            Band == 4 ~ 588,
                            Band == 5 ~ 629,
                            Band == 6 ~ 704,
                            Band == 7 ~ 772,
                            Band == 8 ~ 862),
         wv_max = case_when(Band == 1 ~ 453,
                            Band == 2 ~ 508,
                            Band == 3 ~ 581,
                            Band == 4 ~ 627,
                            Band == 5 ~ 689,
                            Band == 6 ~ 744,
                            Band == 7 ~ 890,
                            Band == 8 ~ 1040)) %>% mutate(width = (wv_max - wv_min)) 

dataW3_extr_select <- rbind(dataW3_extr %>% filter(Band < 5) %>% mutate(LI = case_when(lianaCov >= 5 & lianaCov < 25  ~ 0,
                                                                                lianaCov >= 75 ~ 1,
                                                                                TRUE ~ 2)),
                     dataW3_extr %>% filter(Band >= 5) %>% mutate(LI = case_when(lianaCov >= 5 & lianaCov < 25  ~ 0,
                                                                                 lianaCov >= 75 ~ 1,
                                                                                 TRUE ~ 2))) %>% filter(LI < 2) #%>% filter(!(lianaCov %in% c(95)))

# Remove outliers
# dataW3_extr <- dataW3_extr %>% group_by(name,LI) %>% filter(value > quantile(value,0.25) - 1.5*(quantile(value,0.75) - quantile(value,0.25)) &
#                                                       value < quantile(value,0.25) + 1.5*(quantile(value,0.75) - quantile(value,0.25)))
# 

ggplot(data = dataW3_extr_select %>% filter(name == "layer.5"),
       aes(x = lianaCov,y = value)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw()

df.to.plot <- dataW3_extr_select %>% filter(Band > 1) %>% group_by(LI,wv) %>% add_count() %>%
  mutate(value = value/10000)
       
# Spectra                                                                                                                   
ggplot() +
  geom_boxplot(data = df.to.plot,
               aes(x = (wv),fill = as.factor(LI),y = value,group = interaction(LI,wv)),varwidth = FALSE) +
  scale_color_manual(values = c("#137300","#1E64C8")) +
  scale_fill_manual(values = c("#137300","#1E64C8")) +
  theme_bw()

pvals <- dataW3_extr_select %>% group_by(wv) %>% filter(Band > 1) %>% summarise(diff = mean(value[LI == 1])/10000 - mean(value[LI == 0])/10000,
                                                                pval = summary(aov(formula = value ~ as.factor(LI)))[[1]][1,5])
pvals

df.to.plot2 <- df.to.plot %>% group_by(wv,LI) %>% summarise(mean = mean(value),
                                                            median = median(value),
                                                            low = quantile(value,0.25),
                                                            verylow = quantile(value,0.05),
                                                            high = quantile(value,0.75),
                                                            veryhigh = quantile(value,0.95),
                                                            width = mean(width),
                                                            n = mean(n)) %>% ungroup() %>%
  mutate(wv_mod = case_when(LI == 0 ~ wv + width/4,
                            LI == 1 ~ wv - width/4),
         width = width/2) %>% mutate()

df.to.plot2_pval <- df.to.plot2 %>% left_join(pvals) %>% mutate(P_val = case_when(LI == 1 & pval < 0.01 ~ "***",
                                                                                  TRUE ~ ""),
                                                                wv = case_when(wv == 723.7 ~ 730,
                                                                               TRUE ~ wv)) 

ggplot(data = df.to.plot2_pval) +
  geom_rect(aes(xmin = wv_mod - width/2,xmax = wv_mod + width/2,ymin = low, ymax = high, fill = as.factor(LI)),alpha = 0.5) +
  geom_errorbar(aes(x = wv_mod,ymin = high,ymax = veryhigh,color = as.factor(LI)),width = 0) +
  geom_errorbar(aes(x = wv_mod,ymin = verylow,ymax = low,color = as.factor(LI)),width = 0) +
  geom_errorbarh(aes(xmin = (wv_mod - width/2)+1,xmax = (wv_mod + width/2)-1,y = mean,color = as.factor(LI))) +
  scale_color_manual(values = c("#137300","#1E64C8")) +
  scale_fill_manual(values = c("#137300","#1E64C8")) +
  theme_bw() + 
  guides(fill = FALSE, color = FALSE) +
  labs(x = "Wavelength [nm]",y = "Reflectance [-]") +
  geom_text(aes(x = wv,y = high*1.05,label=P_val),nudge_y = 0.01) +
  scale_x_continuous(limits = c(400,1050)) +
  theme(text = element_text(size = 18),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

ggsave(plot = last_plot(),dpi = 300, width = 15,height = 20, filename = file.path("./Figures","RM_Worldview2.png"),units = "cm")

dataW3_extr_select %>% mutate(type = case_when(wv < 650 ~ "short",
                                        TRUE ~ "long")) %>% group_by(type,LI) %>% add_count() %>% 
  summarise(S = sum(value)/mean(n),
            N = mean(n)/4)

# # ggridges
# ggplot(data = dataW3_extr_select,
#        aes(x = value, y = as.factor(LI), fill = as.factor(LI))) +
#   geom_density_ridges(alpha= 0.5,scale=0.95) +
#   facet_wrap(~wv,scales = "free") +
#   theme_bw()
#  

# ################################################################################
# dataWV3_formatted_group <- dataWV3_formatted %>% mutate(LI = case_when(lianaCat <= 1 ~ 0,
#                                                                        lianaCat >=5 ~1,
#                                                                        TRUE ~ 100))
# # 
# # dataWV3_formatted_OL <- dataWV3_formatted_group %>% group_by(name,LI) %>% filter(value > quantile(value,0.25) - 1.5*(quantile(value,0.75) - quantile(value,0.25)) &
# #                                                                                   value < quantile(value,0.25) + 1.5*(quantile(value,0.75) - quantile(value,0.25)))
# 
# 
# ggplot() +
#   geom_boxplot(data = dataWV3_formatted_group,
#            aes(x = name,fill = as.factor(lianaCat),y = value)) +
#   theme_bw()
# 
# 
# dataWV3_formatted_group %>% group_by(LI,name) %>% summarise(m = mean(value)) %>% arrange(name)
# 
# summary(aov(data = dataWV3_formatted_OL %>% filter(name == "layer.3",
#                                                       LI <= 1),formula = value ~ LI))[[1]][1,5]
# 
# dataWV3_formatted_group %>% filter(LI <=1) %>% group_by(name,LI) %>% summarise(pval = summary(aov(formula = value ~ as.factor(LI)))[[1]][1,5])
# 
# 
# ggplot(data = dataWV3_formatted_group %>% filter(name == "layer.3"),
#        aes(x = lianaCat,y = value)) +
#   geom_point() +
#   geom_smooth(method='lm') +
#   theme_bw()
# 
# 
# ggplot(data = dataWV3_formatted_group %>% filter(lianaCat %in%c(0,5)),
#        aes(x = value, y = as.factor(lianaCat), fill = as.factor(lianaCat))) +
#   geom_density_ridges(alpha= 0.5,scale=0.95) +
#     facet_wrap(~name,scales = "free") +
#     theme_bw()
# 
# ggplot(data = dataWV3_formatted_group %>% filter(name == "layer.3"),
#        aes(x = value, y = as.factor(lianaCat), fill = as.factor(lianaCat))) +
#   geom_density_ridges(alpha= 0.5,scale=0.95) +
#   facet_wrap(~name,scales = "free") +
#   theme_bw()
# 
# dataWV3_formatted_group %>% filter(name == "layer.3") %>% group_by(lianaCat) %>% summarise(q = quantile(value,0.95))
# 
# dataWV3_formatted_group %>% group_by(name,lianaCat) %>% add_count() %>% summarise(N = mean(n)) %>% filter(name == "layer.3")
# 
