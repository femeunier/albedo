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

file2load <- "~/Documents/R/edr-da/data/All_parameters.RDS"
All.parameters <- readRDS(file2load)

Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Sanchez_PNM","Sanchez_FTS","Foster","Marvin","Kalacska_RTM","Sanchez")
Params_for_Prospect <- c("Cab","Car","Cw","Cm","Nlayers")

wv_min_all <- 401
wv_max_all <- 2500
Delta_wv <- 2100

waves_all <- 400:2500 
Nruns <- ceiling((wv_max_all - wv_min_all)/Delta_wv)

Nsimulations <- 100
Npicks <- 100

diff_sum_all <- All <- data.frame()
for (irun in seq(1,Nruns)){
  
  print("---------------------------")
  print(irun/Nruns)
  
  df.spectra.all <- data.frame()
  
  wv_min <- wv_min_all + (irun - 1)*Delta_wv
  wv_max <- wv_min + Delta_wv - 1
  
  pos.waves <- which(waves_all %in% seq(wv_min,wv_max))
  
  for (isimu in seq(1,Nsimulations)){
    print(isimu/Nsimulations)
    #Pick up one reference for Prospect/ED_RTM
    Ref_prospect <- Refs_for_Prospect[sample(1:length(Refs_for_Prospect),1)]
    
    params_prospect <- All.parameters %>% filter(ref == Ref_prospect,
                                                 Param %in% Params_for_Prospect) %>% group_by(pft,Param) %>% sample_n(size = 10000,replace = TRUE) %>%
      ungroup() %>% arrange(Param) %>% mutate(id = rep(1:(10000),2*length(Params_for_Prospect))) %>% pivot_wider(names_from = Param,
                                                                                                                  values_from = value)
    Lianas_params <- params_prospect %>% filter(pft == "Liana_optical") %>% dplyr::select(Params_for_Prospect)
    Trees_params <- params_prospect %>% filter(pft == "Tree_optical") %>% dplyr::select(Params_for_Prospect)
  
    diff_params <- Lianas_params - Trees_params
    diff_params_m <- apply(diff_params,2,mean)
    diff_params_low <- apply(diff_params,2,quantile,0.25)
    diff_params_high <- apply(diff_params,2,quantile,0.75)
    IQR <- diff_params_high - diff_params_low
    params2low <- t(matrix(rep(diff_params_low - 1.5*IQR,10000),ncol = 10000))
    params2high <- t(matrix(rep(diff_params_high + 1.5*IQR,10000),ncol = 10000))
     
    pos2keep <- which(apply((diff_params < params2high) &   (diff_params > params2low),1,all))
    
    Lianas_params <- Lianas_params[pos2keep,]
    Trees_params  <- Trees_params[pos2keep,]
    
    diff_params <- Lianas_params - Trees_params
    
    df.spectra <- data.frame()
  
    for (i in seq(1,Npicks)){
      cparam_liana <- as.vector(t(Lianas_params[i,]))
      cparam_tree <- as.vector(t(Trees_params[i,]))
      cspectrum_liana <- prospect5(N = cparam_liana[5],cparam_liana[1],cparam_liana[2],cparam_liana[3],cparam_liana[4])
      cspectrum_tree <- prospect5(N = cparam_tree[5],cparam_tree[1],cparam_tree[2],cparam_tree[3],cparam_tree[4])
  
      
      df.spectra <- rbind(df.spectra,
                          rbind(data.frame(waves = wv_min:wv_max,
                                           reflectance = cspectrum_liana$reflectance[pos.waves],
                                           transmittance = cspectrum_liana$transmittance[pos.waves],
                                           pft = "Liana",
                                           simu = isimu,
                                           rep = i),
                                data.frame(waves = wv_min:wv_max,
                                           reflectance = cspectrum_tree$reflectance[pos.waves],
                                           transmittance = cspectrum_tree$transmittance[pos.waves],
                                           pft = "Tree",
                                           simu = isimu,
                                           rep = i)))
    }
  
    df.spectra.sum <- df.spectra %>% group_by(waves,pft) %>% summarise(reflectance = mean(reflectance),
                                                                       transmittance = mean(transmittance))
  
    df.spectra.all <- bind_rows(list(df.spectra.all,
                                     df.spectra.sum %>% mutate(simu = isimu)))
  }
  
  diff <- df.spectra.all %>% pivot_wider(names_from = pft,values_from = c(reflectance,transmittance)) %>% group_by(waves,simu) %>%
    summarise(diff_r = reflectance_Liana - reflectance_Tree,
              diff_t = transmittance_Liana - transmittance_Tree)
  
  diff_sum <- diff %>% group_by(waves) %>% summarise(diff_r_mean = mean(diff_r),
                                                     diff_t_mean = mean(diff_t),
                                                     diff_r_min = min(diff_r),
                                                     diff_r_max = max(diff_r),
                                                     diff_r_alphamin = quantile(diff_r,0.25),
                                                     diff_r_alphamax = quantile(diff_r,0.75),
                                                     diff_r_alphalow = quantile(diff_r,0.025),
                                                     diff_r_alphahigh = quantile(diff_r,0.975),
                                                     diff_t_min = min(diff_t),
                                                     diff_t_max = max(diff_t),
                                                     diff_t_alphamin = quantile(diff_t,0.25),
                                                     diff_t_alphamax = quantile(diff_t,0.75),
                                                     diff_t_alphalow = quantile(diff_t,0.025),
                                                     diff_t_alphahigh = quantile(diff_t,0.995))
  
  diff_sum_all <- rbind(diff_sum_all,
                        diff_sum)  
  
  All <- bind_rows(list(All,
               df.spectra.all %>% group_by(waves,pft) %>% summarise(r_mean = mean(reflectance),
                                                                    r_min = min(reflectance),
                                                                    r_max = max(reflectance),
                                                                    t_mean = mean(transmittance),
                                                                    t_min = min(transmittance),
                                                                    t_max = max(transmittance))))
}

saveRDS(file = "./data/diff_sum.RDS",object = diff_sum)
saveRDS(file = "./data/All.RDS",object = All)

# diff_sum <- readRDS(file = "./data/diff_sum.RDS")
# All <- readRDS(file = "./data/All.RDS")
All %>% mutate(Band = case_when(waves < 700 ~ "Vis",
                                waves < 1400 ~ "NIR",
                                waves <= 2500 ~ "SWIR")) %>% group_by(Band) %>% summarise(pval_r = summary(aov(r_mean ~ pft))[[1]][1,5],
                                                                                         pval_t = summary(aov(t_mean ~ pft))[[1]][1,5])

All %>% mutate(Band = case_when(waves < 700 ~ "Vis",
                                waves <= 2500 ~ "IR")) %>% group_by(Band) %>% summarise(pval_r = summary(aov(r_mean ~ pft))[[1]][1,5],
                                                                                          pval_t = summary(aov(t_mean ~ pft))[[1]][1,5])


ggplot() +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymin = diff_r_alphalow,ymax = diff_r_alphahigh),fill = "grey",alpha=0.5) +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymin = diff_r_alphamin,ymax = diff_r_alphamax),fill = "darkgrey",alpha=0.5) +
  geom_line(data = diff_sum_all, aes(x = waves,colour = pft,y = diff_r_mean),color = "black") +
  geom_line(data = All, aes(x = waves,colour = pft,y = r_mean)) +
  scale_y_continuous(breaks = seq(0,0.6,0.2),expand = c(0,0)) +
  geom_hline(yintercept = c(0),linetype=3)+
  scale_color_manual(values = c("#1E64C8","#137300")) +
  labs(y = "Reflectance [-]",
       x = "Wavelength [nm]") +
  theme_bw() + theme(text = element_text(size = 14),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),hjust=0.25),
                     axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))



ggplot() +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymin = diff_r_alphalow,ymax = diff_r_alphahigh),fill = "grey",alpha=0.5) +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymax = 1 - diff_t_alphahigh,ymin = 1 - diff_t_alphalow),fill = "grey",alpha=0.5) +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymin = diff_r_alphamin,ymax = diff_r_alphamax),fill = "darkgrey",alpha=0.5) +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymax = 1 - diff_t_alphamin,ymin = 1 - diff_t_alphamax),fill = "darkgrey",alpha=0.5) +
  geom_line(data = diff_sum_all, aes(x = waves,colour = pft,y = diff_r_mean),color = "black") +
  geom_line(data = diff_sum_all, aes(x = waves,colour = pft,y = 1-diff_t_mean),linetype = 1,color = "black") +
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

diff_sum_all2 <- All %>% mutate(Band = case_when(waves <= 700 ~ 1,
                                                 waves <= 2500 ~2)) %>% group_by(Band) %>% summarise(r_verylow = quantile(r_mean[pft == "Liana"] - r_mean[pft == "Tree"],0.025),
                                                                                                     r_low = quantile(r_mean[pft == "Liana"] - r_mean[pft == "Tree"],0.25),
                                                                                                     r_m = mean(r_mean[pft == "Liana"] - r_mean[pft == "Tree"]),
                                                                                                     r_high = quantile(r_mean[pft == "Liana"] - r_mean[pft == "Tree"],0.75),
                                                                                                     r_veryhigh = quantile(r_mean[pft == "Liana"] - r_mean[pft == "Tree"],0.975),
                                                                                                     t_verylow = quantile(t_mean[pft == "Liana"] - t_mean[pft == "Tree"],0.025),
                                                                                                     t_low = quantile(t_mean[pft == "Liana"] - t_mean[pft == "Tree"],0.25),
                                                                                                     t_m = mean(t_mean[pft == "Liana"] - t_mean[pft == "Tree"]),
                                                                                                     t_high = quantile(t_mean[pft == "Liana"] - t_mean[pft == "Tree"],0.75),
                                                                                                     t_veryhigh = quantile(t_mean[pft == "Liana"] - t_mean[pft == "Tree"],0.975))
          
All %>% mutate(Band = case_when(waves < 700 ~ "Vis",
                                waves < 1400 ~ "NIR",
                                waves <= 2500 ~ "SWIR")) %>% group_by(Band) %>% summarise(pval_r = summary(aov(r_mean ~ pft))[[1]][1,5],
                                                                                          pval_t = summary(aov(t_mean ~ pft))[[1]][1,5])

All %>% mutate(Band = case_when(waves < 700 ~ "Vis",
                                waves <= 2500 ~ "IR")) %>% group_by(Band) %>% summarise(pval_r = summary(aov(r_mean ~ pft))[[1]][1,5],
                                                                                        pval_t = summary(aov(t_mean ~ pft))[[1]][1,5])

df.spectra.all %>% mutate(Band = case_when(waves < 700 ~ "Vis",
                                waves < 1400 ~ "NIR",
                                waves <= 2500 ~ "SWIR")) %>% group_by(Band) %>% summarise(pval_r = summary(aov(reflectance ~ pft))[[1]][1,5],
                                                                                          pval_t = summary(aov(transmittance ~ pft))[[1]][1,5])

df.spectra.all %>% mutate(Band = case_when(waves < 700 ~ "Vis",
                                waves <= 2500 ~ "IR")) %>% group_by(Band) %>% summarise(pval_r = summary(aov(reflectance ~ pft))[[1]][1,5],
                                                                                        pval_t = summary(aov(transmittance ~ pft))[[1]][1,5])


# diff_sum_all2 <- df.spectra.all %>% mutate(Band = case_when(waves <= 700 ~ 1,
#                                                             waves <= 2500 ~2)) %>% group_by(Band) %>% summarise(r_verylow = quantile(reflectance[pft == "Liana"] - reflectance[pft == "Tree"],0.025),
#                                                                                                                 r_low = quantile(reflectance[pft == "Liana"] - reflectance[pft == "Tree"],0.25),
#                                                                                                                 r_m = mean(reflectance[pft == "Liana"] - reflectance[pft == "Tree"]),
#                                                                                                                 r_high = quantile(reflectance[pft == "Liana"] - reflectance[pft == "Tree"],0.75),
#                                                                                                                 r_veryhigh = quantile(reflectance[pft == "Liana"] - reflectance[pft == "Tree"],0.975),
#                                                                                                                 t_verylow = quantile(transmittance[pft == "Liana"] - transmittance[pft == "Tree"],0.025),
#                                                                                                                 t_low = quantile(transmittance[pft == "Liana"] - transmittance[pft == "Tree"],0.25),
#                                                                                                                 t_m = mean(transmittance[pft == "Liana"] - transmittance[pft == "Tree"]),
#                                                                                                                 t_high = quantile(transmittance[pft == "Liana"] - transmittance[pft == "Tree"],0.75),
#                                                                                                                 t_veryhigh = quantile(transmittance[pft == "Liana"] - transmittance[pft == "Tree"],0.975))



xmin1 = 450;xmin2=720
xmax1 = 680;xmax2=2500

ggplot() +
  geom_ribbon(aes(x = c(xmin1,xmax1),ymin= diff_sum_all2$r_verylow[1],ymax =diff_sum_all2$r_veryhigh[1]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin1,xmax1),ymin= diff_sum_all2$r_low[1],ymax =diff_sum_all2$r_high[1]),fill ="darkgrey",alpha = 0.4) +
  geom_segment(aes(x = xmin1,xend=xmax1,y=diff_sum_all2$r_m[1],yend=diff_sum_all2$r_m[1]),color = "black") +
  geom_ribbon(aes(x = c(xmin2,xmax2),ymin= diff_sum_all2$r_verylow[2],ymax =diff_sum_all2$r_veryhigh[2]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin2,xmax2),ymin= diff_sum_all2$r_low[2],ymax =diff_sum_all2$r_high[2]),fill ="darkgrey",alpha = 0.4) +
  geom_segment(aes(x = xmin2,xend=xmax2,y=diff_sum_all2$r_m[2],yend=diff_sum_all2$r_m[2]),color = "black") +
  geom_ribbon(aes(x = c(xmin1,xmax1),ymax= 1-diff_sum_all2$t_verylow[1],ymin =1-diff_sum_all2$t_veryhigh[1]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin1,xmax1),ymax= 1-diff_sum_all2$t_low[1],ymin =1-diff_sum_all2$t_high[1]),fill ="darkgrey",alpha = 0.4) +
  geom_segment(aes(x = xmin1,xend=xmax1,y=1-diff_sum_all2$t_m[1],yend=1-diff_sum_all2$t_m[1]),color = "black") +
  geom_ribbon(aes(x = c(xmin2,xmax2),ymax= 1-diff_sum_all2$t_verylow[2],ymin =1-diff_sum_all2$t_veryhigh[2]),fill ="grey",alpha = 0.4) +
  geom_ribbon(aes(x = c(xmin2,xmax2),ymax= 1-diff_sum_all2$t_low[2],ymin =1-diff_sum_all2$t_high[2]),fill ="darkgrey",alpha = 0.4) +
  geom_segment(aes(x = xmin2,xend=xmax2,y=1-diff_sum_all2$t_m[2],yend=1-diff_sum_all2$t_m[2]),color = "black") +
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


ggsave(plot = last_plot(),dpi = 300, width = 20,height = 12, filename = file.path("./Figures","Leaf.spectrum.posterior_large.png"),units = "cm")


ggplot() +
  # geom_ribbon(data = diff_sum, aes(x = waves,ymin = diff_r_alphalow,ymax = diff_r_alphahigh),fill = "darkgrey",alpha=0.3) +
  # geom_ribbon(data = diff_sum, aes(x = waves,ymax = 1 - diff_t_alphahigh,ymin = 1 - diff_t_alphalow),fill = "red",alpha=0.3) +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymin = diff_r_alphamin,ymax = diff_r_alphamax),fill = "darkgrey",alpha=0.5) +
  geom_ribbon(data = diff_sum_all, aes(x = waves,ymax = 1 - diff_t_alphamin,ymin = 1 - diff_t_alphamax),fill = "darkgrey",alpha=0.3) +
  geom_line(data = diff_sum_all, aes(x = waves,colour = pft,y = diff_r_mean),color = "black") +
  geom_line(data = diff_sum_all, aes(x = waves,colour = pft,y = 1-diff_t_mean),linetype = 1,color = "black") +
  geom_line(data = All, aes(x = waves,colour = pft,y = r_mean)) +
  geom_line(data = All, aes(x = waves,colour = pft,y = 1-t_mean),linetype = 1) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ 1 - 1 * ., name = "Transmittance [-]",breaks = seq(0,1,0.2)),
                     breaks = seq(0,1,0.2),expand = c(0,0)) +
  scale_x_continuous(limits = c(400,1000),expand = c(0,0)) +
  geom_hline(yintercept = c(0,1),linetype=3)+
  scale_color_manual(values = c("#1E64C8","#137300")) +
  labs(y = "Reflectance [-]",
       x = "Wavelength [nm]") +
  theme_bw() + theme(text = element_text(size = 14),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
                     axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

ggsave(plot = last_plot(),dpi = 300, width = 30,height = 20, filename = file.path("./Figures","Leaf.spectrum.posterior.png"),units = "cm")

