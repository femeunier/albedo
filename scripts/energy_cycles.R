rm(list = ls())

library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(albedo)
library(stringr)

pfts <- c(18)
vars <- c("par.beam","par.diff","par.tot","parup","par.gnd","par.all",
          "nir.beam","nir.diff","nir.tot","nirup","nir.gnd","nir.all",
          #"rshort.beam","rshort.diff","rshort","rshortup","rshort.gnd","albedo.nir","nir.all","albedo.par"
          "rlong","rlong.gnd","rlongup","rlong.albedo","tir.all")

names <- c("ref","liana")

files <- c(file.path(getwd(),"outputs","ref","analysis.RData"),
           file.path(getwd(),"outputs","liana_all","analysis.RData"))
results <- data.frame()

for (ifile in seq(1,length(files))){
  file = files[ifile]
  load(file)
  results <- rbind(results,
                   datum2df(datum,vars = vars,name = names[ifile],pfts = pfts))
}



results <- results %>% mutate(value = case_when(
  var %in% c("par.gnd","nir.gnd","rlong.gnd","parup","nirup","rlongup") ~ - value,
  TRUE ~ value))


results.all <- results %>% mutate(band = as.factor(case_when(
  grepl("par", var) ~ "PAR",
  grepl("nir", var) ~ "NIR",
  grepl("rlong|tir", var) ~ "TIR"
))) %>% mutate(albedo = case_when(grepl("albedo", var) ~ TRUE,
                                 TRUE ~ FALSE)) %>% mutate(
                                   type = case_when(
                                     grepl("beam", var) ~ "Beam",
                                     grepl("diff", var) ~ "Diffuse",
                                     grepl("gnd", var) ~ "Ground",
                                     grepl("up", var) ~ "Up",
                                     grepl("all", var) ~ "Net",
                                     grepl("tot|rlong|rshort", var) ~ "Atmosphere",
                                     TRUE ~ 'other'
                                   )
                                 )

results.all$band <- factor(results.all$band, levels = c("PAR","NIR","TIR"))

# Corrections
results.all.corr <- results.all %>% mutate(value = case_when(
  band == "PAR" ~ value / 4.6,
  TRUE ~ value)) # µmol/m2/s --> W/m²

results.all.corr[results.all.corr$var == "rshortup","value"] <- -results.all.corr[results.all.corr$var == "albedo.nir","value"]*results.all.corr[results.all.corr$var == "rshort","value"] 

results.all.cycle <- results.all.corr %>% group_by(simulation,month,var) %>% summarise(value_m = mean(value),
                                                                                  value_sd = sd(value),
                                                                                  band = band[1],
                                                                                  albedo = albedo[1],
                                                                                  type = type[1]) 

ggplot(data = results.all.cycle %>% filter(!albedo,!(type %in% c("Beam","Diffuse")))) +
  geom_line(aes(x = month, y = value_m,color = type,linetype = simulation)) +
  facet_wrap(~band,scales = "free_y") +
  theme_bw() +
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = seq(1:12))+
  labs(x = "", y = "Energy flux [W/m²]") +
  scale_color_manual(values = c("Black","Blue","Brown","darkgreen"))

ggsave(filename = file.path(getwd(),"Figures","all.bands.png"),
       plot = last_plot(),dpi = 300,height = 15,width = 30,units = "cm")

results.net <- results.all.cycle %>% filter(type == "Net") %>% group_by(simulation,month) %>% summarise(Net = value_m[var == "par.all"] 
                                                                                             + value_m[var == "nir.all"]
                                                                                             + value_m[var == "tir.all"])
  
ggplot(data = results.net) + 
  geom_line(aes(x = (month), y = Net,color = simulation)) +
  labs(x = "", y = "Energy flux [W/m²]") +
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = seq(1:12)) +
  theme_bw()

ggsave(filename = file.path(getwd(),"Figures","balance.png"),
       plot = last_plot(),dpi = 300,height = 15,width = 20,units = "cm")


# plot(datum$emean$rnet,type='l')
# lines(datum$emean$rlong+datum$emean$rshort-datum$emean$rshortup-datum$emean$rlongup,col='red')
