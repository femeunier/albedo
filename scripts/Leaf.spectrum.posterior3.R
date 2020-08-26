rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(rrtm)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(pracma)

#####################################################################################################################
file2read <- "/home/femeunier/Documents/R/edr-da/data/smarts295_test.ext.txt"

# Irradiance (sun)
waves_all <- 400:2500 
data <- read.table(file2read, header = TRUE, sep = "", dec = ".")
colnames(data) <- c('wl','I') #Wavelength, Irradiance
data_interp <- data.frame(wl = waves_all,
                          I = interp1(data$wl,data$I,waves_all))

######################################################################################################################


file2load <- "~/Documents/R/edr-da/data/All_parameters.RDS"
All.parameters <- readRDS(file2load)

# Refs_for_Prospect <- c("Castro_PNM")
# Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Sanchez_PNM")
# Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Foster","Kalacska_RTM","Sanchez")
# Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Foster","Kalacska_RTM","Sanchez")
Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Foster","Marvin","Kalacska_RTM","Sanchez")


All.parameters <- All.parameters %>% filter(ref %in% Refs_for_Prospect)
Params_for_Prospect <- c("Cab","Car","Cw","Cm","Nlayers")

# ggplot(data = All.parameters %>% filter(Param == "Cab"),
#        aes(x = value, y = as.factor(pft), fill = as.factor(pft))) +
#   geom_density_ridges(alpha= 0.5, scale=0.95) +
#   facet_wrap(~ref,scales = "free") +
#   theme_bw()

Nsimulations <- 250
Npicks <- 1

df.spectra.all <- spectra.all <- df.diff.all <- diff.all <- data.frame()

for (isimu in seq(1,Nsimulations)){
  
  print(isimu/Nsimulations)
  Ref_prospect <- Refs_for_Prospect[sample(1:length(Refs_for_Prospect),1)]
  
  params_prospect <- All.parameters %>% filter(Param %in% Params_for_Prospect) %>% group_by(pft,Param) %>% sample_n(size = Npicks) %>%
    ungroup() %>% arrange(Param) %>% mutate(id = rep(1:Npicks,2*length(Params_for_Prospect))) %>% dplyr::select(-ref) %>% pivot_wider(names_from = Param,
                                                                                                                                      values_from = value)
  
  Lianas_params <- params_prospect %>% filter(pft == "Liana_optical") %>% dplyr::select(Params_for_Prospect)
  Trees_params <- params_prospect %>% filter(pft == "Tree_optical") %>% dplyr::select(Params_for_Prospect)
  
  # Lianas_m <- apply(Lianas_params,2,mean)
  # Trees_m <- apply(Trees_params,2,mean)
  # 
  # Lianas_params <- t(matrix(rep(Lianas_m,1),ncol=1))
  # Trees_params <- t(matrix(rep(Trees_m,1),ncol=1))

  df.spectra <- data.frame()
  
  for (i in seq(1,Npicks)){
    cparam_liana <- as.vector(t(Lianas_params[i,]))
    cparam_tree <- as.vector(t(Trees_params[i,]))
    cspectrum_liana <- prospect5(N = cparam_liana[5],cparam_liana[1],cparam_liana[2],cparam_liana[3],cparam_liana[4])
    cspectrum_tree <- prospect5(N = cparam_tree[5],cparam_tree[1],cparam_tree[2],cparam_tree[3],cparam_tree[4])
    
    cspectra <- rbind(data.frame(waves = 400:2500,
                                 reflectance = cspectrum_liana$reflectance,
                                 transmittance = cspectrum_liana$transmittance,
                                 w = data_interp$I,
                                 pft = "Liana",
                                 rep = i),
                      data.frame(waves = 400:2500,
                                 reflectance = cspectrum_tree$reflectance,
                                 transmittance = cspectrum_tree$transmittance,
                                 w = data_interp$I,
                                 pft = "Tree",
                                 rep = i))
    df.spectra <- bind_rows(list(df.spectra,
                                 cspectra %>% mutate(Band = case_when(waves >= 500 & waves <= 650 ~ 1,
                                                                      waves <= 2500 ~2)) %>% filter(Band %in%c(1,2)) %>% group_by(Band,pft) %>%
      summarise(reflectance = weighted.mean(reflectance,w),
                transmittance = weighted.mean(transmittance,w)) %>% mutate(rep = i)))
    
  }
  
  
  df.diff <- df.spectra %>% group_by(Band,rep) %>% summarise(diff_r = reflectance[pft == "Liana"] - reflectance[pft == "Tree"],
                                                             diff_t = transmittance[pft == "Liana"] - transmittance[pft == "Tree"])
  
  spectra.all <- bind_rows(list(spectra.all,
                                cspectra %>% mutate(simu = isimu)))
  
  diff.all <- bind_rows(list(diff.all,
                             cspectra %>% group_by(waves,rep) %>% summarise(diff_r = reflectance[pft == "Liana"] - reflectance[pft == "Tree"],
                                                                            diff_t = transmittance[pft == "Liana"] - transmittance[pft == "Tree"]) %>% mutate(simu = isimu,
                                                                                                                                                              ref = Ref_prospect)))
  
  df.spectra.all <- bind_rows(list(df.spectra.all,
                                   df.spectra %>% mutate(simu = isimu)))
  
  df.diff.all <- bind_rows(list(df.diff.all,
                                df.diff %>% mutate(simu = isimu,
                                                   ref = Ref_prospect)))
  
  # Ref <- ggplot(data = cspectra) +
  #   geom_line(aes(x = waves,y = reflectance,color = pft)) +
  #   theme_bw()
  # 
  # Tr <- ggplot(data = cspectra) +
  #   geom_line(aes(x = waves,y = transmittance,color = pft)) +
  #   theme_bw()
  # 
  # plot_grid(Ref,Tr,align = c("hv"),nrow=1)

}

# ggplot(diff.all) +
#   geom_line(aes(x = waves,y=diff_r,color = ref,group=interaction(ref,simu))) +
#   theme_bw()
# 
# ggplot(df.diff.all %>% filter(Band == 1),
#        aes(x = diff_r, y = ref, fill = ref)) +
#   stat_density_ridges(alpha= 0.5,quantile_lines = TRUE,jittered_points = FALSE,quantiles = c(0.025,0.5, 0.975)) +
#   geom_vline(xintercept = 0) +
#   theme_bw()
# 
# ggplot(df.diff.all %>% filter(Band == 1),
#        aes(x = diff_r,y = 0)) +
#   stat_density_ridges(alpha= 0.5,quantile_lines = TRUE,jittered_points = FALSE,quantiles = c(0.025,0.5, 0.975)) +
#   geom_vline(xintercept = 0,linetype=2) +
#   theme_bw()
# 
# ggplot(df.diff.all %>% filter(Band == 2),
#        aes(x = diff_t, y = ref, fill = ref)) +
#   stat_density_ridges(alpha= 0.5,quantile_lines = TRUE,jittered_points = FALSE,quantiles = c(0.025,0.5, 0.975)) +
#   geom_vline(xintercept = 0,linetype=2) +
#   theme_bw()
# 
# ggplot(df.diff.all %>% filter(Band == 2),
#        aes(x = diff_t,y = 0)) +
#   stat_density_ridges(alpha= 0.5,quantile_lines = TRUE,jittered_points = TRUE,quantiles = c(0.025,0.5,0.975)) +
#   geom_vline(xintercept = 0,linetype=2) +
#   theme_bw()

All <- spectra.all %>% group_by(waves,pft) %>% summarise(r_mean = median(reflectance),
                                                         t_mean = median(transmittance))

PI <- df.diff.all %>% group_by(Band) %>% summarise(r_verylow = quantile(diff_r,0.025),
                                                         r_low = quantile(diff_r,0.25),
                                                         r_m = mean(diff_r),
                                                         r_high = quantile(diff_r,0.75),
                                                         r_veryhigh = quantile(diff_r,0.975),
                                                         t_verylow = quantile(diff_t,0.025),
                                                         t_low = quantile(diff_t,0.25),
                                                         t_m = mean(diff_t),
                                                         t_high = quantile(diff_t,0.75),
                                                         t_veryhigh = quantile(diff_t,0.975))

CI <- df.diff.all %>% group_by(Band) %>% summarise(r_verylow = confint(lm(formula = diff_r ~ 1))[1,1],
                                             r_low = confint(lm(formula = diff_r ~ 1),level = 0.25)[1,1],
                                             r_m = mean(diff_r),
                                             r_high = confint(lm(formula = diff_r ~ 1),level = 0.25)[1,2],
                                             r_veryhigh = confint(lm(formula = diff_r ~1))[1,2],
                                             t_verylow = confint(lm(formula = diff_t ~ 1))[1,1],
                                             t_low = confint(lm(formula = diff_t ~ 1),level = 0.25)[1,1],
                                             t_m = mean(diff_t),
                                             t_high = confint(lm(formula = diff_t ~ 1),level = 0.25)[1,2],
                                             t_veryhigh = confint(lm(formula = diff_t ~1))[1,2],
                                             a_verylow =  confint(lm(formula = (-diff_r - diff_t) ~ 1))[1,1],
                                             a_m =  mean((-diff_r - diff_t)),
                                             a_veryhigh =  confint(lm(formula = (-diff_r - diff_t) ~ 1))[1,2])

df.spectra.all %>% group_by(Band) %>% summarise(pval_r = summary(aov(reflectance ~ pft))[[1]][1,5],
                                                pval_t = summary(aov(transmittance ~ pft))[[1]][1,5])

xmin1 = 450;xmin2=720
xmax1 = 680;xmax2=2500

ggplot() +
  geom_ribbon(aes(x = c(xmin1,xmax1),ymin= PI$r_verylow[1],ymax =PI$r_veryhigh[1]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin1,xmax1),ymin= CI$r_verylow[1],ymax =CI$r_veryhigh[1]),fill ="darkgrey",alpha = 0.8) +
  geom_segment(aes(x = xmin1,xend=xmax1,y=PI$r_m[1],yend=PI$r_m[1]),color = "black") +

  geom_ribbon(aes(x = c(xmin2,xmax2),ymin= PI$r_verylow[2],ymax =PI$r_veryhigh[2]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin2,xmax2),ymin= CI$r_verylow[2],ymax =CI$r_veryhigh[2]),fill ="darkgrey",alpha = 0.8) +
  geom_segment(aes(x = xmin2,xend=xmax2,y=PI$r_m[2],yend=PI$r_m[2]),color = "black") +

  geom_ribbon(aes(x = c(xmin1,xmax1),ymax= 1-PI$t_verylow[1],ymin =1-PI$t_veryhigh[1]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin1,xmax1),ymax= 1-CI$t_verylow[1],ymin =1-CI$t_veryhigh[1]),fill ="darkgrey",alpha = 0.8) +
  geom_segment(aes(x = xmin1,xend=xmax1,y=1-PI$t_m[1],yend=1-PI$t_m[1]),color = "black") +
  
  geom_ribbon(aes(x = c(xmin2,xmax2),ymax= 1-PI$t_verylow[2],ymin =1-PI$t_veryhigh[2]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin2,xmax2),ymax= 1-CI$t_verylow[2],ymin =1-CI$t_veryhigh[2]),fill ="darkgrey",alpha = 0.8) +
  geom_segment(aes(x = xmin2,xend=xmax2,y=1-PI$t_m[2],yend=1-PI$t_m[2]),color = "black") +
  
  geom_line(data = All, aes(x = waves,colour = pft,y = r_mean)) +
  geom_line(data = All, aes(x = waves,colour = pft,y = 1-t_mean),linetype = 1) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ 1 - 1 * ., name = "Transmittance [-]",breaks = seq(0,0.6,0.2)),
                     breaks = seq(0,0.6,0.2),expand = c(0,0)) +
  geom_hline(yintercept = c(0,1),linetype=3)+
  scale_color_manual(values = c("#1E64C8","#137300")) +
  labs(y = "Reflectance [-]",
       x = "Wavelength [nm]") +
  theme_bw() + theme(text = element_text(size = 14),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),hjust=0.25),
                     axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
                     axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

ggsave(plot = last_plot(),dpi = 300, width = 25,height = 12, filename = file.path("./Figures","Leaf.spectrum.posterior_ave.png"),units = "cm")


PI_all <- diff.all %>% group_by(waves) %>% summarise(r_verylow = quantile(diff_r,0.025),
                                                   r_low = quantile(diff_r,0.25),
                                                   r_m = mean(diff_r),
                                                   r_high = quantile(diff_r,0.75),
                                                   r_veryhigh = quantile(diff_r,0.975),
                                                   t_verylow = quantile(diff_t,0.025),
                                                   t_low = quantile(diff_t,0.25),
                                                   t_m = mean(diff_t),
                                                   t_high = quantile(diff_t,0.75),
                                                   t_veryhigh = quantile(diff_t,0.975))

CI_all <- diff.all %>% group_by(waves) %>% summarise(r_verylow = confint(lm(formula = diff_r ~ 1))[1,1],
                                                   r_low = confint(lm(formula = diff_r ~ 1),level = 0.25)[1,1],
                                                   r_m = mean(diff_r),
                                                   r_high = confint(lm(formula = diff_r ~ 1),level = 0.25)[1,2],
                                                   r_veryhigh = confint(lm(formula = diff_r ~1))[1,2],
                                                   t_verylow = confint(lm(formula = diff_t ~ 1))[1,1],
                                                   t_low = confint(lm(formula = diff_t ~ 1),level = 0.25)[1,1],
                                                   t_m = mean(diff_t),
                                                   t_high = confint(lm(formula = diff_t ~ 1),level = 0.25)[1,2],
                                                   t_veryhigh = confint(lm(formula = diff_t ~1))[1,2])

ggplot() +
  
  geom_ribbon(data = PI_all,aes(x =  waves,ymin= r_verylow,ymax = r_veryhigh),fill ="grey",alpha = 0.4) +
  geom_ribbon(data = CI_all,aes(x =  waves,ymin= r_verylow,ymax = r_veryhigh),fill ="darkgrey",alpha = 0.8) +
  geom_line(data = PI_all,aes(x =  waves,y= r_m),color = "black") +
  
  geom_ribbon(data = PI_all,aes(x =  waves,ymin= 1-t_veryhigh,ymax = 1-t_verylow),fill ="grey",alpha = 0.4) +
  geom_ribbon(data = CI_all,aes(x =  waves,ymin= 1-t_veryhigh,ymax = 1-t_verylow),fill ="darkgrey",alpha = 0.8) +
  geom_line(data = PI_all,aes(x =  waves,y= 1-t_m),color = "black") +
  
  geom_line(data = All, aes(x = waves,colour = pft,y = r_mean)) +
  geom_line(data = All, aes(x = waves,colour = pft,y = 1-t_mean),linetype = 1) +
  
  scale_y_continuous(sec.axis = sec_axis(trans = ~ 1 - 1 * ., name = "Transmittance [-]",breaks = seq(0,0.6,0.2)),
                     breaks = seq(0,0.6,0.2),expand = c(0,0)) +
  geom_hline(yintercept = c(0,1),linetype=3)+
  scale_color_manual(values = c("#1E64C8","#137300")) +
  labs(y = "Reflectance [-]",
       x = "Wavelength [nm]") +
  theme_bw() + theme(text = element_text(size = 14),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),hjust=0.25),
                     axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
                     axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

ggsave(plot = last_plot(),dpi = 300, width = 25,height = 12, filename = file.path("./Figures","Leaf.spectrum.posterior.png"),units = "cm")