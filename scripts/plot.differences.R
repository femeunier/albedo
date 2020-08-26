rm(list = ls())

library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(albedo)

pfts <- c(2,3,4,17,18)
vars <- c("gpp","npp",'leaf.par','hflxlc','wue','transp',"leaf.rshort","leaf.rlong",
          "leaf.temp")
names <- c("ref","liana")

files <- c(file.path(getwd(),"outputs","ref/","analysis.RData"),file.path(getwd(),"outputs","liana_all","analysis.RData"))
results <- data.frame()

for (ifile in seq(1,length(files))){
  file = files[ifile]
  load(file)
  results <- rbind(results,
                   datum2df(datum,vars = vars,name = names[ifile],pfts = pfts))
}

results.month <- results %>% group_by(simulation,month,var,pft) %>% summarise(value_m = mean(value),
                                                                              value_sd = sd(value))
ggplot(data = results.month) +
  geom_line(aes(x = month, y = value_m, color = as.factor(pft), linetype = simulation)) +
  facet_wrap( ~ var,scales = "free") +
  scale_color_manual(values = c("lightgreen","green","darkgreen","darkblue","black")) +
  labs(x = "") +
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = seq(1:12))+
  theme_bw()

ggsave(filename = file.path(getwd(),"Figures","Productivity.png"),
       plot = last_plot(),dpi = 300,height = 15,width = 25,units = "cm")


results.diff <- results %>% group_by(var,pft,time) %>% pivot_wider(names_from = simulation,
                                                    values_from = value) %>% mutate(diff = liana - ref) %>%
  group_by(month,var,pft) %>% summarise(diff_m = mean(diff),
                                        diff_sd = sd(diff),
                                        diff_rel = 100*mean(diff)/mean(ref))


ggsave(filename = "./Figures/cycles.png",dpi= 300,
       height = 25,width = 40,units = "cm")

ggplot(data = results.diff) +
  geom_line(aes(x = month, y = diff_m, color = as.factor(pft))) +
  facet_wrap( ~ var,scales = "free") +
  scale_color_manual(values = c("lightgreen","green","darkgreen","darkblue","black")) +
  theme_bw() +
  geom_abline(slope = 0,color = "black",linetype = 3) +
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = seq(1:12)) +
  labs(x = "")

ggsave(filename = "./Figures/differences.png",dpi= 300,
       height = 25,width = 40,units = "cm")
